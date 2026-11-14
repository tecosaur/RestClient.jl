# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

const HTTP_DATE_FORMAT = dateformat"e, d u Y H:M:S G\MT"
const USER_AGENT = "RestClient.jl/" * string(pkgversion(@__MODULE__))

# The core API

"""
    perform(req::Request{kind}) -> Any

Validate and perform the request `req`, and return the result.

The `kind` of the request is its HTTP method, one of: $(join(SUPPORTED_HTTP_METHODS, ", ")).
The result depends on the endpoint: the response parsed as its
[`responsetype`](@ref) (read incrementally, for a [`Stream`](@ref)),
passed through [`postprocess`](@ref). A `:head` request's result is whether
the resource exists: `true` on success, `false` on 404 or 410.

Redirects are followed, a 303 (See Other) with a GET as it asks. Every request
identifies itself as `User-Agent: RestClient.jl/<version>` unless the
endpoint's [`headers`](@ref) give their own.

# Retries and rate limits

Requests are sent no faster than their config's [`RateLimit`](@ref), and wait
out any deadline set by a response to the same host with the same key: one its
headers give (a `Retry-After`, or an `X-RateLimit-Reset` with
`X-RateLimit-Remaining: 0`), or [`nextallowed`](@ref) does. Responses served
from the cache are not held back.

A 429 is retried at its deadline, or failing that after an exponential backoff
(about 0.5, 1, 2 then 4 seconds, with jitter). A 403 or 503 is retried when its
headers give a deadline, and any other unsuccessful response when `nextallowed`
does. A gateway failure (502, 503 or 504) is retried after the backoff, for
methods safe to repeat (not POST or PATCH), unless its body reads as the
endpoint's [`errortype`](@ref): then the API gave it, and it stands. A request is
attempted at most five times.

No wait is longer than the config's `timeout`. A retry that would need a longer
one isn't made, and the response is raised instead; a request that would be held
back for longer isn't sent, and raises a [`RateLimitError`](@ref). With the
default `timeout = Inf`, a wait can be long (until an hourly quota resets, say),
so set a `timeout` to bound it.

# Errors

- [`MalformedRequest`](@ref) if the request fails validation.
- [`ResponseError`](@ref) if the server responds with an unsuccessful status,
  once any retries are spent.
- [`RateLimitError`](@ref) if the request would wait longer than its `timeout`
  to be sent.
- [`MalformedResponse`](@ref) if a successful response can't be read as the
  endpoint's [`responsetype`](@ref).
- `ArgumentError` if `kind` is not a supported HTTP method.

See also: [`Request`](@ref).
"""
perform(req::Request) = send(req, outgoing(req))

function send(req::Request{kind}, out::Outgoing) where {kind}
    # A caller's IO payload can only be resent if it can be rewound
    isretryable = !(out.payload isa IO) || !isnothing(out.payloadstart)
    attempt, incoming = 1, transmit(req, out)
    while true
        retry = retrydeadline(req, incoming, kind ∉ (:post, :patch), attempt)
        isretryable && attempt < MAX_ATTEMPTS && !isnothing(retry) && retry - time() <= req.config.timeout || break
        close(incoming)
        hold!(req, retry)
        attempt, incoming = attempt + 1, transmit(req, out)
    end
    try
        deliver(req, incoming)
    catch err
        # A successful response that can't be read mustn't be replayed from the cache
        cfile = cachefile(req, out)
        err isa Union{MalformedResponse, ResponseError} && err.response.status ∈ 200:299 &&
            !isnothing(cfile) && rm(cfile; force = true)
        rethrow()
    end
end


# Generic request functionality

"""
    MAX_ATTEMPTS

How many times a request asked to retry, or failed at a gateway, is attempted
before giving up.
"""
const MAX_ATTEMPTS = 5

"""
    NO_SECRETS

The secrets of a request whose auth scheme places no key (see `authsecrets`).
"""
const NO_SECRETS = (header = nothing, param = nothing)

"""
    DEFAULT_IDLE_SECONDS

How long a request (not a stream) may go without sending or receiving data,
once its response has begun, before it is abandoned, unless its `timeout` asks
for longer (Downloads' own default).
"""
const DEFAULT_IDLE_SECONDS = 20

"""
    DEFAULT_RESPONSE_SECONDS

How long a request may wait for its response to begin before it is abandoned,
unless its silence limit is longer.
"""
const DEFAULT_RESPONSE_SECONDS = 120

# `CURLOPT_FOLLOWLOCATION` mode (libcurl ≥ 8.13; older versions treat it as plain
# following): a redirect rewrites the method as its status says, so a 303 turns a
# request with a body into a GET, rather than resending the same method without it.
const CURLFOLLOW_OBEYCODE = 2

# Abandons a transfer once no data has moved for its `silencelimit`, as reported by
# curl's progress callback (at least once a second).
mutable struct Watchdog
    const limit::Float64
    bytes::Int
    lastactive::Float64
    responding::Bool
    fired::Bool
end

Watchdog(limit::Real) = Watchdog(Float64(limit), 0, time(), false, false)

silencelimit(w::Watchdog) = if w.responding w.limit else max(w.limit, DEFAULT_RESPONSE_SECONDS) end

# Never less than the total `timeout`, which curl enforces
idlelimit(timeout::Real) = if isfinite(timeout) max(timeout, DEFAULT_IDLE_SECONDS) else DEFAULT_IDLE_SECONDS end

# Per-transfer settings, applied by `transferhook` (see `curlrequest`).
struct TransferOptions
    watchdog::Watchdog
    handlesink::Union{Nothing, Channel{Downloads.Curl.Easy}}
end

const TRANSFER_OPTIONS = ScopedValue{Union{Nothing, TransferOptions}}(nothing)

# RestClient's own Downloader, created in `__init__`, with `transferhook` as its easy_hook.
const DOWNLOADER = Ref{Downloads.Downloader}()

"""
    encode_uri_component(io::IO, str::AbstractString)

Encode `str` according to RFC3986 Section 2, writing the result to `io`.

This escapes all characters outside of `A-Z`, `a-z`, `0-9`, `-_.~`, and `,`, which
APIs read as separating the items of a list.

# Examples

```julia
julia> encode_uri_component(stdout, "Hello, world!")
Hello,%20world%21
```
"""
function encode_uri_component(io::IO, s::AbstractString)
    nib2hex(n::UInt8) = n + 0x30 + (((n + 0x6) >> 4) * 0x7)
    # RFC3986 Section 2.1
    # RFC3986 Section 2.3
    issafe(b::UInt8) =
        UInt8('A') <= b <= UInt8('Z') ||
        UInt8('a') <= b <= UInt8('z') ||
        UInt8('0') <= b <= UInt8('9') ||
        b ∈ UInt8.(('-', '_', '.', '~', ','))
    for b in codeunits(s)
        if issafe(b)
            write(io, b)
        else
            write(io, UInt8('%'), nib2hex(b >> 4), nib2hex(b & 0xf))
        end
    end
end

encode_uri_component(s::AbstractString) = sprint(encode_uri_component, s)

# The inverse of `encode_uri_component`; a `%` without two hex digits after it is kept
function decode_uri_component(s::AbstractString)
    bytes, decoded, i = codeunits(s), UInt8[], 1
    while i <= length(bytes)
        if bytes[i] == UInt8('%') && i + 2 <= length(bytes) && all(isxdigit ∘ Char, bytes[i+1:i+2])
            push!(decoded, parse(UInt8, String(bytes[i+1:i+2]); base = 16))
            i += 3
        else
            push!(decoded, bytes[i])
            i += 1
        end
    end
    String(decoded)
end

"""
    url_parameters(params)

Return the query string `key1=value1&key2=value2&...` for the `key => value`
string pairs in `params`, with keys and values encoded by `encode_uri_component`.

# Examples

```julia
julia> url_parameters(["foo" => "bar", "baz" => "qux"])
"foo=bar&baz=qux"

julia> url_parameters(Pair{String, String}[])
""
```
"""
url_parameters(params) =
    join((encode_uri_component(key) * '=' * encode_uri_component(val) for (key, val) in params), '&')

const MultiValue = Union{AbstractVector, Tuple, AbstractSet}

function querypairs(fields)
    pairs = Pair{String, String}[]
    for (key, val) in fields
        vals = if val isa MultiValue val else (val,) end
        append!(pairs, string(key) => string(v) for v in vals if !isnothing(v))
    end
    pairs
end

function url(req::Request)
    path = try
        chopprefix(String(urlpath(req.config, req.endpoint)), "/")
    catch err
        err isa NothingInPath || rethrow()
        throw(MalformedRequest(req, ["`{$(err.expr)}` in the URL path is `nothing`"]))
    end
    base = if isempty(path) req.config.baseurl else string(req.config.baseurl, '/', path) end
    query = url_parameters(parameters(req))
    if isempty(query) base else string(base, ifelse('?' in path, '&', '?'), query) end
end

# `headers(req)` plus the Content-Type/Accept implied by the payload and response formats
function mimeheaders(req::Request, fmt::AbstractFormat, in::Type)
    hdrs = headers(req)
    out = responsetype(req.endpoint)
    inmime = if in !== Nothing @something(mimetype(req.endpoint, fmt, in), Some(defaultlabel(fmt, in))) end
    outmime = if out !== Nothing mimetype(req.endpoint, dataformat(req.endpoint, out), out) end
    isset(name) = any(h -> lowercase(first(h)) == name, hdrs)
    extras = Pair{String, String}[]
    isnothing(inmime) || isset("content-type") || push!(extras, "Content-Type" => string(inmime))
    isnothing(outmime) || isset("accept") || push!(extras, "Accept" => string(outmime))
    isset("user-agent") || push!(extras, "User-Agent" => USER_AGENT)
    isempty(extras) && hdrs isa Vector{Pair{String, String}} && return hdrs
    append!(Pair{String, String}[h for h in hdrs], extras)
end

# When to retry `req` after `incoming`, its `attempt`th (see `perform`), or `nothing` not to
function retrydeadline(req::Request, incoming::Incoming, isidempotent::Bool, attempt::Int)
    (; response) = incoming
    (; status) = response
    status >= 400 || return
    (; asked, given) = deadlines(req, response)
    isretry = asked > -Inf || status == 429 || status ∈ (403, 503) && given > -Inf ||
        isidempotent && status ∈ (502, 503, 504) && !isownerror(req, incoming)
    isretry || return
    deadline = max(asked, given)
    if deadline == -Inf time() + 2^(attempt - 1) * (0.5 + rand()) elseif isfinite(deadline) deadline end
end

# Whether `incoming`'s body reads as `req`'s error type: the API's own answer, not a gateway's
function isownerror(req::Request, (; response, body)::Incoming)
    errortype(req.endpoint) !== Nothing && body isa IOBuffer || return false
    detail = ResponseError(req, response, read(body)).detail
    seekstart(body)
    !isnothing(detail)
end

# The deadlines `response` sets for requests sharing `req`'s throttle: the one
# `nextallowed` asks for, and the one its headers give (each `-Inf` for none)
deadlines(req::Request, response::Response) =
    (asked = unixtime(something(nextallowed(req.endpoint, response), -Inf)),
     given = retryafter(response.headers))

"""
    throttled(exchange::Function, req::Request) -> Incoming

Run `exchange()`, `req`'s exchange with the network, once its config's
[`Throttle`](@ref) admits it under the config's [`RateLimit`](@ref), then hold
the throttle to the deadlines its response sets. It is in flight until its
response has been received: for a stream, until its transfer ends.
"""
function throttled(exchange::F, req::Request) where {F <: Function}
    (; ratelimit, timeout) = req.config
    throttle = throttler(req.config)
    claimslot!(req, throttle)
    release() = @lock throttle.slots (throttle.inflight -= 1; notify(throttle.slots))
    incoming = try
        # Wait (up to `timeout`) for the time the GCRA admits a request, and claim it
        allowance = (ratelimit.burst - 1) / ratelimit.rate
        while true
            now, arrival = time(), @atomic throttle.arrival
            pause = arrival - allowance - now
            pause > timeout && throw(RateLimitError(req, unix2datetime(arrival - allowance)))
            pause > 0 && (sleep(pause); continue)
            @atomicreplace(throttle.arrival, arrival => max(arrival, now) + 1 / ratelimit.rate).success && break
        end
        exchange()::Incoming
    catch
        release()
        rethrow()
    end
    if isnothing(incoming.transfer)
        release()
    else
        # Its failure is the stream's to report
        Threads.@spawn (wait(incoming.transfer; throw = false); release())
    end
    quota = quotaof(incoming.response.headers)
    isnothing(quota) || @atomic throttle.quota = quota
    hold!(req, max(deadlines(req, incoming.response)...))
    incoming
end

function claimslot!(req::Request, throttle::Throttle)
    (; ratelimit, timeout) = req.config
    giveup = time() + timeout
    # A task, not a `Timer` callback, whose error printing can't be trimmed
    alarm = if isfinite(timeout) Timer(timeout) end
    isnothing(alarm) || Threads.@spawn (wait(alarm); @lock throttle.slots notify(throttle.slots))
    try
        @lock throttle.slots begin
            while throttle.inflight >= ratelimit.inflight
                time() >= giveup && throw(RateLimitError(req, nothing))
                wait(throttle.slots)
            end
            throttle.inflight += 1
        end
    finally
        isnothing(alarm) || close(alarm)
    end
end

# Hold requests sharing `req`'s throttle back until `deadline`, if it's finite
function hold!(req::Request, deadline::Float64)
    isfinite(deadline) || return
    (; ratelimit, baseurl) = req.config
    throttle = throttler(req.config)
    allowance = (ratelimit.burst - 1) / ratelimit.rate
    before, after = @atomic throttle.arrival max deadline + allowance
    after - before >= 1 &&
        @info S"$baseurl asked for a pause of {emphasis:$(round(deadline - time(), digits = 1))} seconds"
end

"""
    retryafter(headers::Headers) -> Float64

Return the time (as a `time()` value) that `headers` ask requests to wait until,
or `-Inf` if they set none.

Checks (in order):
- `Retry-After`: seconds from now, or an HTTP date
- `X-RateLimit-Remaining`: unless zero, the limit is not exhausted (`-Inf`)
- `X-RateLimit-Reset`: seconds from now, a Unix time, or an HTTP date
"""
function retryafter(headers::Headers)
    after = headertime(get(headers, "retry-after", ""))
    after > -Inf && return after
    something(tryparse(Float64, get(headers, "x-ratelimit-remaining", "")), Inf) > 0 && return -Inf
    headertime(get(headers, "x-ratelimit-reset", ""))
end

# A header's time (seconds from now, a Unix time, or an HTTP date) as a `time()` value
function headertime(value::String)
    seconds = if all(isdigit, value) tryparse(Int, value) end
    isnothing(seconds) || return unixtime(seconds)
    date = tryparse(DateTime, value, HTTP_DATE_FORMAT)
    if isnothing(date); -Inf else datetime2unix(date) end
end

function quotaof(headers::Headers)
    function requests(name)
        n = tryparse(Float64, get(headers, name, ""))
        if !isnothing(n) && 0 <= n < typemax(Int) floor(Int, n) end
    end
    reset = headertime(get(headers, "x-ratelimit-reset", ""))
    fields = (requests("x-ratelimit-limit"), requests("x-ratelimit-remaining"),
              if isfinite(reset) unix2datetime(reset) end)
    if !all(isnothing, fields) Quota(fields...) end
end

"""
    quota(config::RequestConfig) -> Union{Quota, Nothing}

Return the rate limit the server last reported to requests through `config`, or
any other config for the same host and key, or `nothing` if none has.

It is read from the response's `X-RateLimit-Limit`, `X-RateLimit-Remaining` and
`X-RateLimit-Reset` headers, as a [`Quota`](@ref): the requests allowed and
those left, and when the count renews. Responses from the cache don't
count, as their headers are as old as the response.

# Examples

```julia
julia> RestClient.quota(config)
RestClient.Quota: 4987 of 5000 left, renewing 2026-11-14T13:00:00 UTC
```
"""
quota(config::RequestConfig) = @atomic throttler(config).quota

# `t` seconds from now as a Unix time, or `t` itself if it's over a year's worth
unixtime(t::Real) = if t < 365 * 24 * 60 * 60 time() + t else Float64(t) end


# Utility functions

# Where `req`'s auth scheme puts its key: the names of the header and query parameter
# (each `nothing` if unused) whose values are hidden in debug output and errors
function authsecrets(req::Request)
    key = req.config.key
    (isnothing(key) || authpolicy(req.endpoint) === :off) && return NO_SECRETS
    scheme = authscheme(req.endpoint)
    header, param = authheaders(scheme, key), authparams(scheme, key)
    (header = if !isnothing(header) first(header) end,
     param = if !isnothing(param) first(param) end)
end

# `url` with the value of the auth query parameter hidden
function redactsecrets(url::AbstractString, secrets::NamedTuple)
    isnothing(secrets.param) && return String(url)
    name = encode_uri_component(secrets.param)
    replace(url, Regex("([?&]\\Q$name\\E=)[^&#]*") => s"\1*****")
end

redactsecrets(res::Response, secrets::NamedTuple) =
    Response(redactsecrets(res.url, secrets), res.status, res.message, res.headers, res.source)

const CURL_REASONS = let C = Downloads.Curl
    reasons = (resolve = (C.CURLE_COULDNT_RESOLVE_HOST, C.CURLE_COULDNT_RESOLVE_PROXY),
               connect = (C.CURLE_COULDNT_CONNECT,),
               tls = (C.CURLE_SSL_CONNECT_ERROR, C.CURLE_SSL_CERTPROBLEM, C.CURLE_PEER_FAILED_VERIFICATION),
               timeout = (C.CURLE_OPERATION_TIMEDOUT,),
               dropped = (C.CURLE_PARTIAL_FILE, C.CURLE_GOT_NOTHING, C.CURLE_SEND_ERROR, C.CURLE_RECV_ERROR),
               redirects = (C.CURLE_TOO_MANY_REDIRECTS,))
    Dict{Int, Symbol}(code => reason for (reason, codes) in pairs(reasons) for code in codes)
end

# A transport failure as a `TransportError`, with `req`'s secrets removed from its URL
function transporterror(req::Request, err::Downloads.RequestError, watchdog::Watchdog)
    reason, message = if watchdog.fired
        waited = silencelimit(watchdog)
        :silence, if watchdog.responding "No data received for $waited seconds"
                  else "No response for $waited seconds" end
    else
        get(CURL_REASONS, err.code, :other), err.message
    end
    TransportError(req, redactsecrets(err.url, authsecrets(req)), reason, message)
end

const DEBUG_DIR = Ref("")
const DEBUG_DIR_LOCK = ReentrantLock()

# A private (0o700), per-session directory for debug dumps, created on first use.
function debugfile(name::String)
    dir = @lock DEBUG_DIR_LOCK begin
        isempty(DEBUG_DIR[]) && (DEBUG_DIR[] = mktempdir(prefix = "restclient-debug-"))
        DEBUG_DIR[]
    end
    joinpath(dir, name)
end

function debug_request(method::String, url::String, headers, payload::Union{IO, Nothing} = nothing;
                       secrets::NamedTuple = NO_SECRETS)
    url = redactsecrets(url, secrets)
    shown(name, value) =
        if !isnothing(secrets.header) && lowercase(name) == lowercase(secrets.header) "*****" else value end
    payloadinfo = if isnothing(payload)
        S""
    else
        dumpfile = debugfile("request.http")
        nb = open(dumpfile, "w") do io
            println(io, method, ' ', url)
            for (k, v) in headers
                println(io, k, ": ", shown(k, v))
            end
            println(io)
            mark(payload)
            nbytes = write(io, payload)
            reset(payload)
            nbytes
        end
        S"$(Base.format_bytes(nb)) (saved to {bright_magenta:$dumpfile}) sent to "
    end
    strheaders = join((S"\n       {emphasis:$k:} $(shown(k, v))" for (k, v) in headers), "")
    S"{inverse,bold,magenta: $method } $payloadinfo{light,underline:$url}$strheaders"
end

const MUNDANE_HEADERS = (
    "server", "date", "transfer-encoding", "connection", "keep-alive", "vary",
    "access-control-allow-origin", "referrer-policy", "strict-transport-security", "alt-svc",
    # CDN headers
    "report-to", "reporting-endpoints", "nel", "via", "cf-cache-status", "cf-ray")

function debug_response(url::String, res::Response, buf::Union{IOBuffer, Nothing} = nothing;
                        secrets::NamedTuple = NO_SECRETS)
    url = redactsecrets(url, secrets)
    face = ifelse(res.status ∈ 200:299, :success, :warning)
    msg = if isnothing(buf)
        S"streaming from"
    else
        dumpfile = debugfile("response.http")
        nb = open(io -> dumpresponse(io, redactsecrets(res, secrets), buf), dumpfile, "w")
        S"$(Base.format_bytes(nb)) (saved to {bright_magenta:$dumpfile}) from"
    end
    headers = [h for h in res.headers if first(h) ∉ MUNDANE_HEADERS]
    nhiddenheaders = length(res.headers) - length(headers)
    strheaders = join((S"\n       {emphasis:$k:} $v" for (k, v) in headers), "")
    strhiddenheaders = if iszero(nhiddenheaders)
        S""
    else
        S"\n       {emphasis:⋮} {italic,light:$nhiddenheaders hidden (mundane)}"
    end
    S"{inverse,bold,$face: $(res.status) } $msg {light,underline:$url}$strheaders$strhiddenheaders"
end


# Preparing requests

# Resolve `req` to what goes on the wire, once: validate it, encode its payload,
# and build its URL and headers.
function outgoing(req::Request{kind}) where {kind}
    kind ∈ SUPPORTED_HTTP_METHODS ||
        throw(ArgumentError("Cannot perform a :$kind request: the supported HTTP methods are \
                             $(join(SUPPORTED_HTTP_METHODS, ", "))"))
    check_validation(req)
    content = payload(req)
    kind === :query && isnothing(content) &&
        throw(MalformedRequest(req, ["a QUERY request needs a payload, as its content is the query (RFC 10008)"]))
    fmt = dataformat(req.endpoint, typeof(content))
    body = if isnothing(content) || content isa IO
        content
    else
        buf = IOBuffer()
        writepayload(buf, fmt, content)
        take!(buf)
    end
    start = if content isa IO && applicable(position, content) position(content) end
    Outgoing(uppercase(String(kind)), url(req), mimeheaders(req, fmt, typeof(content)),
             body, start)
end

# The payload IO for one send of `out`: encoded bytes are sent afresh, and a
# caller's seekable IO is rewound to where it started.
function payloadio(out::Outgoing)
    out.payload isa Vector{UInt8} && return IOBuffer(out.payload)
    isnothing(out.payloadstart) || seek(out.payload, out.payloadstart)
    out.payload
end

function check_validation(req::Request)
    result = validate(req)
    issues = if isnothing(result) || result === true
        String[]
    elseif result === false
        ["Request is not well-formed"]
    elseif result isa AbstractString
        [String(result)]
    elseif result isa AbstractVector{<:AbstractString}
        collect(String, result)
    else
        throw(ArgumentError("validate returned $(typeof(result)); expected Nothing, Bool, AbstractString, or AbstractVector{<:AbstractString}"))
    end
    if authpolicy(req.endpoint) === :required && isnothing(req.config.key)
        push!(issues, "this endpoint requires authentication, but the request config has no key set (RequestConfig(url; key = ...))")
    end
    isempty(issues) || throw(MalformedRequest(req, issues))
    nothing
end



# Transmission

# A `Stream` result is read incrementally (but a HEAD request's never is); otherwise the
# whole response is read, through the HTTP cache when it may be stored.
function transmit(req::Request{kind}, out::Outgoing) where {kind}
    kind !== :head && responsetype(req.endpoint) <: Stream && return throttled(() -> open_stream(req, out), req)
    cfile = cachefile(req, out)
    if isnothing(cfile)
        incoming = http_request(req, out)
        # RFC 9111 §4.4: a successful unsafe request invalidates what is cached for its target
        kind ∈ (:get, :head, :query, :options) || incoming.response.status >= 400 ||
            rm(cachefolder(out.url); recursive = true, force = true)
        return incoming
    end
    incoming = http_cached(cfile, req, out)
    # A partial response (206) must not stand in for the full one (RFC 9111 §3.3)
    (; status, source) = incoming.response
    source === :stored || status ∉ 200:299 || status == 206 ||
        cachesave(req, cfile, out.url, incoming.response, incoming.body)
    incoming
end

http_request(req::Request, out::Outgoing, headers::Vector{Pair{String, String}} = out.headers) =
    throttled(req) do
        buf, input = IOBuffer(), payloadio(out)
        @debug debug_request(out.method, out.url, headers, input; secrets = authsecrets(req)) _file=nothing
        (; timeout) = req.config
        options = TransferOptions(Watchdog(idlelimit(timeout)), nothing)
        res = curlrequest(out.url, options; out.method, output = buf, input, headers, timeout,
                          throw = false)
        res isa Downloads.RequestError && throw(transporterror(req, res, options.watchdog))
        seekstart(buf)
        response = Response(res)
        @debug debug_response(out.url, response, buf; secrets = authsecrets(req)) _file=nothing
        Incoming(response, buf)
    end


"""
    curlrequest(url::String, options::TransferOptions; kwargs...) -> Union{Downloads.Response, Downloads.RequestError}

Call `Downloads.request(url; kwargs...)` on RestClient's downloader, whose
`easy_hook` applies `options` to the transfer.
"""
curlrequest(url::String, options::TransferOptions; kwargs...) =
    with(TRANSFER_OPTIONS => options) do
        Downloads.request(url; kwargs..., downloader = DOWNLOADER[])
    end

# Whether the transfer may continue, given the bytes moved so far
function (w::Watchdog)(dlnow::Integer, ulnow::Integer)
    now = time()
    w.responding |= dlnow > 0
    if dlnow + ulnow != w.bytes
        w.bytes, w.lastactive = dlnow + ulnow, now
    elseif now - w.lastactive > silencelimit(w)
        w.fired = true
    end
    !w.fired
end

# Curl's progress callback (see `transferhook`); a nonzero result aborts the transfer
watchdog_xferinfo(watchdog::Ptr{Cvoid}, _dltotal::Integer, dlnow::Integer,
                  _ultotal::Integer, ulnow::Integer)::Cint =
    !(unsafe_pointer_to_objref(watchdog)::Watchdog)(dlnow, ulnow)

# Configure a transfer's curl handle from the `TRANSFER_OPTIONS` in scope. The
# hook runs in the task calling `Downloads.request`, so the options are that
# request's. A user's global `Downloads.EASY_HOOK` still applies, ours after it.
function transferhook(easy::Downloads.Curl.Easy, info::NamedTuple)
    userhook = Downloads.EASY_HOOK[]
    isnothing(userhook) || userhook(easy, info)
    options = TRANSFER_OPTIONS[]
    isnothing(options) && return
    # Curl's rate-based check counts sparse data (e.g. heartbeats) as silence, so it's
    # made unreachable, but left armed: it wakes curl each second to call the watchdog.
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_LOW_SPEED_TIME, typemax(Int32))
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_XFERINFOFUNCTION,
                          @cfunction(watchdog_xferinfo, Cint, (Ptr{Cvoid}, Downloads.Curl.curl_off_t,
                              Downloads.Curl.curl_off_t, Downloads.Curl.curl_off_t, Downloads.Curl.curl_off_t)))
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_XFERINFODATA, pointer_from_objref(options.watchdog))
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_NOPROGRESS, false)
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_FOLLOWLOCATION, CURLFOLLOW_OBEYCODE)
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_ACCEPT_ENCODING, "")
    Downloads.Curl.setopt(easy, Downloads.Curl.CURLOPT_TCP_KEEPALIVE, 1)
    isnothing(options.handlesink) || put!(options.handlesink, easy)
    nothing
end



# Delivery

function deliver(req::Request, incoming::Incoming)
    responsetype(req.endpoint) <: Stream && return deliver_stream(req, incoming)
    (; response, body) = incoming
    try
        response.status ∈ 200:299 || throw(ResponseError(req, response, read(body)))
        dtype = responsetype(req.endpoint)
        data = if dtype !== Nothing
            interpret(req, response, read(body), dataformat(req.endpoint, dtype), dtype)
        end
        result = try
            postprocess(response, req, data)
        catch err
            E = errortype(req.endpoint)
            err isa E || rethrow()
            throw(ResponseError{E, typeof(req)}(req, response, read(seekstart(body)), err))
        end
        restructure(response, req, result)
    finally
        close(incoming)
    end
end

function interpret(req::Request, response::Response, bytes::AbstractVector{UInt8},
                   fmt::AbstractFormat, ::Type{T}) where {T}
    try
        interpretresponse(bytes, fmt, T)
    catch err
        err isa InterruptException && rethrow()
        failure = ResponseError(req, response, collect(bytes))
        failure isa ResponseError{Nothing} || throw(failure)
        throw(MalformedResponse(req, response, T, failure.body, err::Exception))
    end
end

function ResponseError(req::Request, response::Response, body::Vector{UInt8})
    E = errortype(req.endpoint)
    E === Nothing && return ResponseError(req, response, body, nothing)
    fmt = dataformat(req.endpoint, E)
    try
        detail = interpretresponse(copy(body), fmt, E)
        ResponseError{E, typeof(req)}(req, response, body, detail)
    catch err
        err isa InterruptException && rethrow()
        if fmt isa RawFormat
            @warn "Error responses for $(endpointlabel(req.endpoint)) can't be read as $E, which has no data format: \
                   give it a `dataformat` method, or the API one with `@globalconfig … format = …`" maxlog = 1
        else
            @debug "The response for $(endpointlabel(req.endpoint)) doesn't read as its error type $E either" exception = err
        end
        ResponseError(req, response, body, nothing)
    end
end

# A HEAD request asks whether the resource exists: a missing resource is a
# `false`, not an error, but any other failure still is.
function deliver(req::Request{:head}, incoming::Incoming)
    close(incoming)
    (; response) = incoming
    exists = response.status ∈ 200:299
    exists || response.status ∈ (404, 410) || throw(ResponseError(req, response, UInt8[], nothing))
    postprocess(response, req, exists)
end
