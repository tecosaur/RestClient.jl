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

# Errors

- [`MalformedRequest`](@ref) if the request fails validation.
- [`ResponseError`](@ref) if the server responds with an unsuccessful status. A
  response asking to be retried later (a 429, or a 403 or 503 with `Retry-After`)
  is retried a few times first, as is a gateway failure (502, 503 or 504) for a
  method that is safe to repeat (not POST or PATCH).
- [`MalformedResponse`](@ref) if a successful response can't be read as the
  endpoint's [`responsetype`](@ref).
- `ArgumentError` if `kind` is not a supported HTTP method.

See also: [`Request`](@ref).
"""
perform(req::Request) = send(req, outgoing(req))

function send(req::Request{kind}, out::Outgoing) where {kind}
    # A caller's IO payload can only be resent if it can be rewound
    isretryable = !(out.payload isa IO) || !isnothing(out.payloadstart)
    incoming = withretries(req.config.rategate; isretryable, isidempotent = kind ∉ (:post, :patch),
                           maxwait = req.config.timeout) do
        transmit(req, out)
    end
    deliver(req, incoming)
end


# Generic request functionality

"""
    ANSI_CLEAR_LINE

String escape sequence to clear the current line in a terminal.
"""
const ANSI_CLEAR_LINE = "\e[A\e[2K"

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

How long a transfer may go without sending or receiving data before it is
abandoned, unless the request's `timeout` asks for longer (Downloads' own default).
"""
const DEFAULT_IDLE_SECONDS = 20

# `CURLOPT_FOLLOWLOCATION` mode (libcurl ≥ 8.13; older versions treat it as plain
# following): a redirect rewrites the method as its status says, so a 303 turns a
# request with a body into a GET, rather than resending the same method without it.
const CURLFOLLOW_OBEYCODE = 2

# Abandons a transfer once no data has moved for `limit` seconds, as reported by
# curl's progress callback (at least once a second).
mutable struct Watchdog
    const limit::Float64
    bytes::Int
    lastactive::Float64
    fired::Bool
end

# The watchdog for a request's `timeout`: at least `atleast`, and the default if unlimited
Watchdog(timeout::Real; atleast::Real = 0) =
    Watchdog(if isfinite(timeout) max(Float64(timeout), atleast) else Float64(DEFAULT_IDLE_SECONDS) end,
             0, time(), false)

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

This escapes all characters outside of `A-Z`, `a-z`, `0-9`, and `-_.~`.

# Examples

```julia
julia> encode_uri_component(stdout, "Hello, world!")
Hello%2C%20world%21
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
        b ∈ UInt8.(('-', '_', '.', '~'))
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

"""
    withretries(send::Function, gate::RateGate; isretryable::Bool = true,
                isidempotent::Bool = false, maxwait::Real = Inf) -> Incoming

Call `send()`, retrying while the response asks to be retried later (a 429, or a
403 or 503 with `Retry-After` or rate-limit headers, see [`retrydelay`](@ref)), or,
when `isidempotent`, while a gateway fails (a 502, 503 or 504).

Retries wait the delay the headers ask for, or back off exponentially with jitter,
up to `MAX_ATTEMPTS` attempts; a discarded attempt is closed. When not
`isretryable`, out of attempts, or asked to wait longer than `maxwait` seconds, the
last response is returned as-is.
Calls sharing `gate` share one backoff (see [`RateGate`](@ref)).
"""
function withretries(send::F, gate::RateGate; isretryable::Bool = true,
                     isidempotent::Bool = false, maxwait::Real = Inf) where {F <: Function}
    attempt = 1
    while true
        while (remaining = (@atomic gate.nextallowed) - time()) > 0
            sleep(remaining)
        end
        deadline = @atomic gate.nextallowed
        incoming = send()::Incoming
        (; status, headers) = incoming.response
        delay = retrydelay(headers)
        isretry = status == 429 || status ∈ (403, 503) && !isnothing(delay) ||
            isidempotent && status ∈ (502, 503, 504)
        pause = something(delay, 2^(attempt - 1) * (0.5 + rand()))
        isretry && isretryable && attempt < MAX_ATTEMPTS && pause <= maxwait ||
            return incoming
        close(incoming)
        backoff!(gate, pause, deadline)
        attempt += 1
    end
end

# Move the gate's deadline, unless another task has done so since `deadline` was read
# before sending; either way, the next attempt waits out the gate
function backoff!(gate::RateGate, delay::Real, deadline::Float64)
    _, claimed = @atomicreplace gate.nextallowed deadline => time() + max(delay, 0.1)
    claimed && delay > 0 && isinteractive() && @noinline alert_sleep(delay)
    nothing
end

function alert_sleep(delay::Real)
    @info S"Asked to wait {emphasis:$delay} seconds before retrying, obliging..."
    tty = stdout
    if tty isa Base.TTY
        print(tty, '\n')
        for waited in 1:delay
            sleep(1)
            print(tty, ANSI_CLEAR_LINE, S" Waited {emphasis:$waited} seconds\n")
        end
        print(tty, ANSI_CLEAR_LINE)
    else
        sleep(delay)
    end
end

"""
    retrydelay(headers::Vector{Pair{String, String}}) -> Union{Int, Nothing}

Examine `headers` to find how many seconds should be waited before retrying a
rate-limited request, or `nothing` if they carry no rate-limit information.

Checks (in order):
- `Retry-After`: which may be a timestamp or seconds delta
- `X-RateLimit-Remaining`: if not zero, the limit is not exhausted (`nothing`)
- `X-RateLimit-Reset`: which may be a timestamp, seconds delta, or unix time
"""
function retrydelay(headers::Vector{Pair{String, String}})
    retryafter = secondsuntil(getheader(headers, "retry-after"))
    isnothing(retryafter) || return retryafter
    something(tryparse(Int, getheader(headers, "x-ratelimit-remaining")), 0) == 0 || return nothing
    secondsuntil(getheader(headers, "x-ratelimit-reset"); allowunix = true)
end

# Seconds until the time `value` gives: a delta, an HTTP date, or (`allowunix`) a Unix
# time, told from a delta by being over a year; `nothing` if it is none of these.
function secondsuntil(value::String; allowunix::Bool = false)
    if !isempty(value) && all(isdigit, value)
        seconds = tryparse(Int, value)
        isnothing(seconds) && return nothing
        allowunix && seconds >= 365 * 24 * 60 * 60 || return seconds
        max(0, seconds - floor(Int, time()))
    else
        date = tryparse(DateTime, value, HTTP_DATE_FORMAT)
        if !isnothing(date) max(0, ceil(Int, datetime2unix(date) - time())) end
    end
end

# The first header called `name` (received headers are lowercased by Downloads)
function getheader(headers, name::String, default::String = "")
    for (key, val) in headers
        key == name && return val
    end
    default
end


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

redactsecrets(res::Downloads.Response, secrets::NamedTuple) =
    Downloads.Response(res.proto, redactsecrets(res.url, secrets), res.status, res.message, res.headers)

# A transport failure's error as callers see it: `req`'s auth secrets removed from its
# URLs, and an abort by `watchdog` reported as curl reports its own timeouts
function transfererror(req::Request, err::Downloads.RequestError, watchdog::Watchdog)
    secrets = authsecrets(req)
    code, reason = if watchdog.fired
        Downloads.Curl.CURLE_OPERATION_TIMEDOUT,
        "No data received for $(watchdog.limit) seconds; if the API is slow to respond, \
         raise the `timeout` of its RequestConfig"
    else
        err.code, err.message
    end
    Downloads.RequestError(redactsecrets(err.url, secrets), code, reason,
                           redactsecrets(err.response, secrets))
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

function debug_response(url::String, res::Downloads.Response, buf::IOBuffer;
                        secrets::NamedTuple = NO_SECRETS)
    url = redactsecrets(url, secrets)
    dumpfile = debugfile("response.http")
    nb = open(io -> dumpresponse(io, redactsecrets(res, secrets), buf), dumpfile, "w")
    face = ifelse(res.status ∈ 200:299, :success, :warning)
    msg = S"$(Base.format_bytes(nb)) (saved to {bright_magenta:$dumpfile}) from"
    headers = filter(h -> lowercase(first(h)) ∉ MUNDANE_HEADERS, res.headers)
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
    kind !== :head && responsetype(req.endpoint) <: Stream && return open_stream(req, out)
    # RFC 9111 §3, RFC 10008 §2.7: GET, HEAD, and QUERY (keyed on its content) are cacheable
    iscacheable = req.config.cache && responsetype(req.endpoint) !== Nothing &&
        kind ∈ (:get, :head, :query) && !(out.payload isa IO)
    iscacheable || return http_request(req, out)
    cfile = joinpath(cachedir(), cachekey(out.url, out.method, out.headers, out.payload) * ".http")
    incoming, isstored = http_cached(cfile, req, out)
    # A partial response (206) must not stand in for the full one (RFC 9111 §3.3)
    (; status) = incoming.response
    isstored || status ∉ 200:299 || status == 206 ||
        cachesave(req, cfile, out.url, incoming.response, incoming.body)
    incoming
end

function http_request(req::Request, out::Outgoing, headers::Vector{Pair{String, String}} = out.headers)
    buf, input = IOBuffer(), payloadio(out)
    @debug debug_request(out.method, out.url, headers, input; secrets = authsecrets(req)) _file=nothing
    (; timeout) = req.config
    # Never less than the total `timeout`, which curl enforces
    options = TransferOptions(Watchdog(timeout; atleast = DEFAULT_IDLE_SECONDS), nothing)
    res = curlrequest(out.url, options; out.method, output = buf, input, headers, timeout,
                      throw = false)
    res isa Downloads.RequestError && throw(transfererror(req, res, options.watchdog))
    seekstart(buf)
    @debug debug_response(out.url, res, buf; secrets = authsecrets(req)) _file=nothing
    Incoming(res, buf)
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
    if dlnow + ulnow != w.bytes
        w.bytes, w.lastactive = dlnow + ulnow, now
    elseif now - w.lastactive > w.limit
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
        postprocess(response, req, data)
    finally
        close(incoming)
    end
end

# `interpretresponse`, with an exception raised as a `MalformedResponse` naming the request
function interpret(req::Request, response::Downloads.Response, bytes::AbstractVector{UInt8},
                   fmt::AbstractFormat, ::Type{T}) where {T}
    try
        interpretresponse(bytes, fmt, T)
    catch err
        err isa InterruptException && rethrow()
        throw(MalformedResponse(req, response, T, collect(bytes), err::Exception))
    end
end

# A HEAD request asks whether the resource exists: a missing resource is a
# `false`, not an error, but any other failure still is.
function deliver(req::Request{:head}, incoming::Incoming)
    close(incoming)
    (; response) = incoming
    exists = response.status ∈ 200:299
    exists || response.status ∈ (404, 410) || throw(ResponseError(req, response, UInt8[]))
    postprocess(response, req, exists)
end
