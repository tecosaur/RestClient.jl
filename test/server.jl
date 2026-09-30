# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# A local HTTP server whose routes each model one capability the client must
# handle. `with_server(f)` runs `f(baseurl, state)` against it on an ephemeral
# port; `state` exposes server-side counters for cross-request assertions.

using HTTP
using Sockets

const ETAG = "\"v1-abc\""

# Retry-After must exceed the test's launch stagger, so a shared backoff holds
# later callers off until the window closes.
const THROTTLE_WINDOW = 3.0
const THROTTLE_RETRY_AFTER = 3

mutable struct ServerState
    requests::Int            # total requests served
    ratelimit_hits::Int      # times /ratelimited returned 429
    revalidations::Int       # conditional requests seen at /cacheable and /revalidate
    forbidden_hits::Int      # times /forbidden/limited returned 403
    echo_limited_hits::Int   # times /echo/limited returned 429
    rotations::Int           # distinct versions served by /rotating
    # /throttled sees genuinely concurrent hits, so these are atomic
    @atomic throttle_429s::Int
    @atomic throttle_first::Float64  # time() of first hit, 0.0 until set
end
ServerState() = ServerState(0, 0, 0, 0, 0, 0, 0, 0.0)

# Tiny JSON encoders, just enough to avoid a JSON dep and escaped interpolation.
jsonval(s::AbstractString) = '"' * replace(s, '\\' => "\\\\", '"' => "\\\"") * '"'
jsonval(n::Integer) = string(n)
jsonval(b::Bool) = string(b)

json_response(body::AbstractString) =
    HTTP.Response(200, ["Content-Type" => "application/json"], body)
json_object(pairs::Pair...) =
    json_response('{' * join(("$(jsonval(k)):$(jsonval(v))" for (k, v) in pairs), ',') * '}')
json_array(elements::Vector{<:AbstractString}) =
    json_response('[' * join(elements, ',') * ']')
item_json(name::AbstractString, id::Integer) = "{\"name\":$(jsonval(name)),\"id\":$id}"

queryparam(req, key, default = "") = get(HTTP.queryparams(HTTP.URI(req.target)), key, default)

function build_router(state::ServerState)
    router = HTTP.Router()
    HTTP.register!(router, "GET", "/items/{id}", function (req)
        id = HTTP.getparams(req)["id"]
        json_object("name" => "item-$id", "id" => something(tryparse(Int, id), 0))
    end)
    HTTP.register!(router, "GET", "/search", function (req)
        q, limit = queryparam(req, "q"), parse(Int, queryparam(req, "limit", "1"))
        json_array([item_json("$q-$i", i) for i in 1:limit])
    end)
    HTTP.register!(router, "GET", "/paged", function (req)
        page = parse(Int, queryparam(req, "page", "1"))
        json_response("{\"items\":[$(item_json("p$page", page))],\
                        \"page\":$page,\"more\":$(page < 3)}")
    end)
    # Reflects the decoded `?q=` back, so a test can prove the client encoded it.
    HTTP.register!(router, "GET", "/reflect",
                   req -> json_object("name" => queryparam(req, "q"), "id" => 0))
    HTTP.register!(router, "GET", "/headers",
                   req -> json_object("accept" => HTTP.header(req, "Accept", ""),
                                      "authorization" => HTTP.header(req, "Authorization", "")))
    # Echoes the body + method + Content-Type, so payload formats are observable.
    echo_any(req) = echo(req.method)(req)
    echo(method) = req -> json_object("method" => method,
                                      "content_type" => HTTP.header(req, "Content-Type", ""),
                                      "body" => String(req.body))
    for method in ("GET", "POST", "PUT", "PATCH", "DELETE")
        HTTP.register!(router, method, "/echo", echo(method))
    end
    HTTP.register!(router, "GET", "/error", _ -> HTTP.Response(500, "boom"))
    HTTP.register!(router, "GET", "/notfound", _ -> HTTP.Response(404, "nope"))
    HTTP.register!(router, "GET", "/nostore",
                   _ -> HTTP.Response(200, ["Content-Type" => "application/json",
                                            "Cache-Control" => "no-store"],
                                      item_json("fresh", 1)))
    HTTP.register!(router, "HEAD", "/exists", _ -> HTTP.Response(200, ""))
    HTTP.register!(router, "HEAD", "/missing", _ -> HTTP.Response(404, ""))
    HTTP.register!(router, "HEAD", "/broken", _ -> HTTP.Response(500, ""))
    # 429 + Retry-After on the first hit, then 200 — drives the client's backoff.
    HTTP.register!(router, "GET", "/ratelimited", function (req)
        state.ratelimit_hits < 1 || return json_response(item_json("ok", 1))
        state.ratelimit_hits += 1
        HTTP.Response(429, ["Retry-After" => "1", "Content-Type" => "application/json"], "{}")
    end)
    # 429s every request within THROTTLE_WINDOW of the first hit, then 200.
    HTTP.register!(router, "GET", "/throttled", function (req)
        now = time()
        @atomicreplace state.throttle_first 0.0 => now
        if now - (@atomic state.throttle_first) < THROTTLE_WINDOW
            @atomic state.throttle_429s += 1
            return HTTP.Response(429, ["Retry-After" => string(THROTTLE_RETRY_AFTER),
                                       "Content-Type" => "application/json"], "{}")
        end
        json_response(item_json("ok", 1))
    end)
    # Fresh response carries max-age + ETag; a conditional re-request gets 304.
    HTTP.register!(router, "GET", "/cacheable", function (req)
        if HTTP.header(req, "If-None-Match", "") == ETAG
            state.revalidations += 1
            return HTTP.Response(304, ["ETag" => ETAG])
        end
        HTTP.Response(200, ["Content-Type" => "application/json",
                            "Cache-Control" => "max-age=60", "ETag" => ETAG],
                      item_json("cached", 1))
    end)
    # A plain 403 (not a rate limit), and one carrying exhausted rate-limit headers once.
    HTTP.register!(router, "GET", "/forbidden", _ -> HTTP.Response(403, "no"))
    HTTP.register!(router, "GET", "/forbidden/limited", function (req)
        state.forbidden_hits < 1 || return json_response(item_json("ok", 1))
        state.forbidden_hits += 1
        HTTP.Response(403, ["X-RateLimit-Remaining" => "0", "X-RateLimit-Reset" => "1"], "{}")
    end)
    HTTP.register!(router, "GET", "/ratelimited/long",
                   _ -> HTTP.Response(429, ["Retry-After" => "3600"], "{}"))
    HTTP.register!(router, "GET", "/always429",
                   _ -> HTTP.Response(429, ["Retry-After" => "0"], "{}"))
    # 429 once, then echo: the retry must resend the whole payload.
    HTTP.register!(router, "POST", "/echo/limited", function (req)
        if state.echo_limited_hits < 1
            state.echo_limited_hits += 1
            return HTTP.Response(429, ["Retry-After" => "0"], "{}")
        end
        echo("POST")(req)
    end)
    # "See Other": the answer is at /seen, which reports how it was requested.
    HTTP.register!(router, "*", "/see-other", _ -> HTTP.Response(303, ["Location" => "/seen"], ""))
    HTTP.register!(router, "*", "/seen", echo_any)
    HTTP.register!(router, "GET", "/text", _ -> HTTP.Response(200, "plain words"))
    HTTP.register!(router, "GET", "/number", _ -> HTTP.Response(200, "42"))
    # Stored already stale; a 304 revalidation extends its freshness.
    HTTP.register!(router, "GET", "/revalidate", function (req)
        if HTTP.header(req, "If-None-Match", "") == ETAG
            state.revalidations += 1
            return HTTP.Response(304, ["ETag" => ETAG, "Cache-Control" => "max-age=60"])
        end
        HTTP.Response(200, ["Content-Type" => "application/json",
                            "Cache-Control" => "max-age=0", "ETag" => ETAG],
                      item_json("revalidated", 1))
    end)
    # A new version (and ETag) every request, so revalidation replaces the entry.
    HTTP.register!(router, "GET", "/rotating", function (req)
        state.rotations += 1
        HTTP.Response(200, ["Content-Type" => "application/json", "Cache-Control" => "max-age=0",
                            "ETag" => "\"v$(state.rotations)\""],
                      item_json("rotation", state.rotations))
    end)
    router
end

"""
    with_server(f) -> f's return value

Run `f(baseurl, state)` against the test server on an ephemeral loopback port,
tearing it down afterwards (even on error). `state` is the live [`ServerState`].
"""
function with_server(f)
    state = ServerState()
    router = build_router(state)
    counting(req) = (state.requests += 1; router(req))
    # Bind our own socket to read the OS-assigned port; `serve!(…, 0)` records
    # the requested port (0), not the real one.
    sock = Sockets.listen(Sockets.localhost, 0)
    port = Int(Sockets.getsockname(sock)[2])
    server = HTTP.serve!(counting, Sockets.localhost, port;
                         server = sock, access_log = nothing, verbose = -1)
    try
        f("http://127.0.0.1:$port", state)
    finally
        close(server)
    end
end

# Streaming routes: handlers receive an `HTTP.Stream` and write incrementally, so
# the client observes frames as they arrive (and connection drops mid-stream).
const SLOW_INTERVAL = 0.15
# Well beyond a 1 s silence window plus curl's ~6 s rate-averaging lag.
const STALL_SECONDS = 10

# Times /sse/limited has answered 429; reset by each `with_stream_server`.
const STREAM_LIMITED_HITS = Threads.Atomic{Int}(0)

function write_frames(http::HTTP.Stream, frames; contenttype, interval = 0.0, abortafter = 0)
    HTTP.setstatus(http, 200)
    HTTP.setheader(http, "Content-Type" => contenttype)
    HTTP.startwrite(http)
    for (i, frame) in enumerate(frames)
        abortafter > 0 && i > abortafter && error("aborting mid-stream")
        write(http, frame)
        flush(http)
        interval > 0 && sleep(interval)
    end
end

function stream_router(http::HTTP.Stream)
    read(http)   # consume any request body, which HTTP.jl requires before responding
    path = HTTP.URI(http.message.target).path
    sseframes = ["event: tick\ndata: {\"n\":1}\nid: a\n\n",
                 "data: {\"n\":2}\n\n",
                 "data: {\"n\":3}\n\n"]
    ndframes = ["{\"n\":1}\n", "{\"n\":2}\n", "{\"n\":3}\n"]
    if path == "/sse"
        write_frames(http, sseframes; contenttype = "text/event-stream")
    elseif path == "/sse/done"
        write_frames(http, ["data: {\"n\":1}\n\n", "data: [DONE]\n\n", "data: {\"n\":9}\n\n"];
                     contenttype = "text/event-stream")
    elseif path == "/sse/stop"
        write_frames(http, ["data: {\"n\":1}\n\n", "event: stop\ndata: x\n\n"];
                     contenttype = "text/event-stream")
    elseif path == "/sse/limited"        # 429 once, then a normal stream
        if Threads.atomic_add!(STREAM_LIMITED_HITS, 1) == 0
            HTTP.setstatus(http, 429)
            HTTP.setheader(http, "Retry-After" => "0")
            HTTP.startwrite(http)
        else
            write_frames(http, sseframes; contenttype = "text/event-stream")
        end
    elseif path == "/stall"              # a frame, silence, then another
        write_frames(http, ndframes[1:2]; contenttype = "application/x-ndjson", interval = STALL_SECONDS)
    elseif path == "/heartbeat"          # 2-byte heartbeats 3 s apart: under 1 byte/s
        write_frames(http, ["data: {\"n\":1}\n\n", ":\n", ":\n", ":\n", ":\n", "data: {\"n\":2}\n\n"];
                     contenttype = "text/event-stream", interval = 3)
    elseif path == "/steady"             # frames a second apart, for a few seconds
        write_frames(http, [ndframes; "{\"n\":4}\n"]; contenttype = "application/x-ndjson", interval = 1)
    elseif path == "/ndjson"
        write_frames(http, ndframes; contenttype = "application/x-ndjson")
    elseif path == "/slow"
        write_frames(http, ndframes; contenttype = "application/x-ndjson", interval = SLOW_INTERVAL)
    elseif path == "/abort"
        write_frames(http, ndframes; contenttype = "application/x-ndjson",
                     interval = SLOW_INTERVAL, abortafter = 2)
    elseif path == "/empty"
        HTTP.setstatus(http, 200)
        HTTP.startwrite(http)
    elseif path == "/error500"
        HTTP.setstatus(http, 500)
        HTTP.startwrite(http)
        write(http, "boom")
    else
        HTTP.setstatus(http, 404)
        HTTP.startwrite(http)
    end
end

"""
    with_stream_server(f) -> f's return value

Like [`with_server`], but serves the streaming routes (`/sse`, `/ndjson`, `/slow`,
`/abort`, `/empty`, `/error500`, `/sse/done`, `/sse/stop`, `/sse/limited`, `/stall`,
`/steady`) in HTTP stream mode.
"""
function with_stream_server(f)
    STREAM_LIMITED_HITS[] = 0
    sock = Sockets.listen(Sockets.localhost, 0)
    port = Int(Sockets.getsockname(sock)[2])
    server = HTTP.serve!(stream_router, Sockets.localhost, port;
                         server = sock, stream = true, access_log = nothing, verbose = -1)
    try
        f("http://127.0.0.1:$port")
    finally
        close(server)
    end
end
