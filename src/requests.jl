# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

const HTTP_DATE_FORMAT = dateformat"e, d u Y H:M:S G\MT"

# The core API

"""
    perform(req::Request{kind}) -> Any

Validate and perform the request `req`, and return the result.

The `kind` of the request is its HTTP method, one of: $(join(SUPPORTED_HTTP_METHODS, ", ")).
The result depends on the endpoint: the response parsed as its
[`responsetype`](@ref) (a [`Stream`](@ref) for a [`StreamEndpoint`](@ref)),
passed through [`postprocess`](@ref). A `:head` request's result is whether
the resource exists: `true` on success, `false` on 404 or 410.

# Errors

- [`MalformedRequest`](@ref) if the request fails validation.
- [`ResponseError`](@ref) if the server responds with an unsuccessful status
  (still rate-limited after retries included).
- `ArgumentError` if `kind` is not a supported HTTP method.

See also: [`Request`](@ref).
"""
function perform(req::Request)
    out = outgoing(req)
    isretryable = !(out.payload isa IO) || !isnothing(out.payloadstart)
    incoming = catch_ratelimit(req.config.rategate; isretryable, maxwait = req.config.timeout) do
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
    RATELIMIT_MAX_ATTEMPTS

How many times a rate-limited request is attempted before giving up.
"""
const RATELIMIT_MAX_ATTEMPTS = 5

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

"""
    url_parameters(params::Vector{Pair{String, String}})

Return a URL query string from a vector of key-value pairs `params`.

The returned string is of the form `?key1=value1&key2=value2&...`,
with keys and values encoded by `encode_uri_component`. If `params` is empty,
the empty string is returned.

# Examples

```julia
julia> url_parameters([("foo", "bar"), ("baz", "qux")])
"?foo=bar&baz=qux"


julia> url_parameters(Pair{String, String}[])
""
```
"""
function url_parameters(params::Vector{Pair{String, String}})
    if isempty(params)
        ""
    else
        iob = IOBuffer()
        print(iob, '?')
        for (i, (key, val)) in enumerate(params)
            i > 1 && print(iob, '&')
            encode_uri_component(iob, key)
            print(iob, '=')
            encode_uri_component(iob, val)
        end
        String(take!(iob))
    end
end

function url(req::Request)
    path, query = urlpath(req.config, req.endpoint)::String, url_parameters(parameters(req))
    '?' in path && !isempty(query) && (query = "&" * chopprefix(query, "?"))
    string(req.config.baseurl, '/', path, query)
end

# `headers(req)` plus the Content-Type/Accept implied by the payload and response formats
function mimeheaders(req::Request, in::Type)
    hdrs = headers(req)
    out = responsetype(req.endpoint)
    inmime = if in !== Nothing payloadmime(dataformat(req.endpoint, in), in) end
    outmime = if out !== Nothing mimetype(typeof(dataformat(req.endpoint, out))) end
    isset(name) = any(h -> lowercase(first(h)) == name, hdrs)
    extras = Pair{String, String}[]
    isnothing(inmime) || isset("content-type") || push!(extras, "Content-Type" => inmime)
    isnothing(outmime) || isset("accept") || push!(extras, "Accept" => outmime)
    isempty(extras) && hdrs isa Vector{Pair{String, String}} && return hdrs
    append!(Pair{String, String}[h for h in hdrs], extras)
end

"""
    catch_ratelimit(send::Function, gate::RateGate; isretryable::Bool = true, maxwait::Real = Inf) -> Incoming

Call `send()`, retrying while the response is rate-limited: a 429, or a 403
with rate-limit headers (see [`retrydelay`](@ref)).

Retries wait the delay the headers ask for, or back off exponentially, up to
`RATELIMIT_MAX_ATTEMPTS` attempts; a discarded attempt is closed. When not
`isretryable`, out of attempts, or asked to wait longer than `maxwait` seconds, the
rate-limited response is returned as-is.
Calls sharing `gate` share one backoff (see [`RateGate`](@ref)).
"""
function catch_ratelimit(send::F, gate::RateGate; isretryable::Bool = true,
                         maxwait::Real = Inf) where {F <: Function}
    attempt = 1
    while true
        while (@atomic gate.nextallowed) - time() > 0
            lock(gate.lock) # block until the in-progress backoff releases
            unlock(gate.lock)
        end
        incoming = send()::Incoming
        (; status, headers) = incoming.response
        delay = retrydelay(headers)
        islimited = status == 429 || (status == 403 && !isnothing(delay))
        wait = something(delay, 2^(attempt - 1))
        islimited && isretryable && attempt < RATELIMIT_MAX_ATTEMPTS && wait <= maxwait ||
            return incoming
        close(incoming)
        backoff!(gate, wait)
        attempt += 1
    end
end

function backoff!(gate::RateGate, delay::Real)
    @lock gate.lock begin
        (@atomic gate.nextallowed) - time() > 0 && return
        @atomic gate.nextallowed = max(gate.nextallowed, time() + delay)
        if delay <= 0
            sleep(0.1)
        elseif isinteractive()
            @noinline alert_sleep(delay)
        else
            sleep(delay)
        end
    end
end

function alert_sleep(delay::Real)
    @info S"Rate limited :( asked to wait {emphasis:$delay} seconds, obliging..."
    if isa(stdout, Base.TTY)
        print('\n')
        for waited in 1:delay
            sleep(1)
            print(ANSI_CLEAR_LINE, S" Waited {emphasis:$waited} seconds\n")
        end
        print(ANSI_CLEAR_LINE)
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
    retryafter = getheader(headers, "retry-after")
    if isempty(retryafter)
    elseif all(isdigit, retryafter)
        delay = tryparse(Int, retryafter)
        isnothing(delay) || return delay
    else
        resettime = tryparse(DateTime, retryafter, HTTP_DATE_FORMAT)
        isnothing(resettime) ||
            return max(0, ceil(Int, datetime2unix(resettime) - time()))
    end
    something(tryparse(Int, getheader(headers, "x-ratelimit-remaining")), 0) == 0 ||
        return nothing
    xreset = getheader(headers, "x-ratelimit-reset")
    if isempty(xreset)
    elseif all(isdigit, xreset)
        resetsec = tryparse(Int, xreset)
        if isnothing(resetsec)
        elseif resetsec < 365 * 24 * 60 * 60 # Infer <1y as a delta
            return resetsec
        else # Infer >=1y as an absolute time
            return max(0, resetsec - floor(Int, time()))
        end
    else
        resettime = tryparse(DateTime, xreset, HTTP_DATE_FORMAT)
        isnothing(resettime) || return max(0, ceil(Int, datetime2unix(resettime) - time()))
    end
    nothing
end

# The first header called `name` (received headers are lowercased by Downloads)
function getheader(headers, name::String, default::String = "")
    for (key, val) in headers
        key == name && return val
    end
    default
end


# Utility functions

# The header and (URI-encoded) parameter values `req`'s auth adds, to redact from debug output
function authsecrets(req::Request)
    secrets = String[]
    authpolicy(req.endpoint) === :off && return secrets
    scheme, key = authscheme(req.endpoint), req.config.key
    authheader = authheaders(scheme, key)
    isnothing(authheader) || push!(secrets, last(authheader))
    authparam = authparams(scheme, key)
    isnothing(authparam) || push!(secrets, encode_uri_component(last(authparam)))
    filter!(!isempty, secrets)
end

redactsecrets(str::AbstractString, secrets::Vector{String}) =
    if isempty(secrets) str else replace(str, (secret => "*****" for secret in secrets)...) end

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
                       secrets::Vector{String} = String[])
    url = redactsecrets(url, secrets)
    payloadinfo = if isnothing(payload)
        S""
    else
        dumpfile = debugfile("request.http")
        nb = open(dumpfile, "w") do io
            println(io, method, ' ', url)
            for (k, v) in headers
                println(io, k, ": ", redactsecrets(v, secrets))
            end
            println(io)
            mark(payload)
            nbytes = write(io, payload)
            reset(payload)
            nbytes
        end
        S"$(Base.format_bytes(nb)) (saved to {bright_magenta:$dumpfile}) sent to "
    end
    strheaders = if isempty(headers)
        S""
    else
        join((S"\n       {emphasis:$k:} $(redactsecrets(v, secrets))" for (k, v) in headers), "")
    end
    S"{inverse,bold,magenta: $method } $payloadinfo{light,underline:$url}$strheaders"
end

const MUNDANE_HEADERS = (
    "server", "date", "transfer-encoding", "connection", "keep-alive", "vary",
    "access-control-allow-origin", "referrer-policy", "strict-transport-security", "alt-svc",
    # CDN headers
    "report-to", "reporting-endpoints", "nel", "via", "cf-cache-status", "cf-ray")

function debug_response(url::String, res::Downloads.Response, buf::IOBuffer;
                        secrets::Vector{String} = String[])
    url = redactsecrets(url, secrets)
    dumpfile = debugfile("response.http")
    shownres = Downloads.Response(res.proto, redactsecrets(res.url, secrets), res.status, res.message, res.headers)
    nb = open(io -> dumpresponse(io, shownres, buf), dumpfile, "w")
    face = ifelse(res.status ∈ 200:299, :success, :warning)
    msg = S"$(Base.format_bytes(nb)) (saved to {bright_magenta:$dumpfile}) from"
    nhiddenheaders = sum(h -> lowercase(first(h)) ∈ MUNDANE_HEADERS, res.headers; init = 0)
    headers = filter(h -> lowercase(first(h)) ∉ MUNDANE_HEADERS, res.headers)
    strheaders = if isempty(headers)
        S""
    else
        join((S"\n       {emphasis:$k:} $v" for (k, v) in headers), "")
    end
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
    responsetype(req.endpoint) <: Stream && !(req.endpoint isa StreamEndpoint) &&
        throw(ArgumentError("$(typeof(req.endpoint)) returns a Stream, so it must subtype \
                             StreamEndpoint (for an API-wide supertype, declare it as \
                             e.g. `abstract type MyAPIStream <: StreamEndpoint end`)"))
    content = payload(req)
    check_validation(req, content)
    body = if isnothing(content) || content isa IO
        content
    else
        buf = IOBuffer()
        writepayload(buf, dataformat(req.endpoint, typeof(content)), content)
        take!(buf)
    end
    start = if content isa IO && applicable(position, content) position(content) end
    Outgoing(uppercase(String(kind)), url(req), mimeheaders(req, typeof(content)),
             body, start, req.config.timeout)
end

# The payload IO for one send of `out`: encoded bytes are sent afresh, and a
# caller's seekable IO is rewound to where it started.
function payloadio(out::Outgoing)
    out.payload isa Vector{UInt8} && return IOBuffer(out.payload)
    isnothing(out.payloadstart) || seek(out.payload, out.payloadstart)
    out.payload
end

function check_validation(req::Request{kind}, content) where {kind}
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
    if kind === :query && isnothing(content)
        push!(issues, "a QUERY request needs a payload, as its content is the query (RFC 10008)")
    end
    isempty(issues) || throw(MalformedRequest(req, issues))
    nothing
end



# Transmission

# Buffered: the whole response is read, through the HTTP cache when it may be stored.
function transmit(req::Request, out::Outgoing)
    # RFC 9111 §3, RFC 10008 §2.7: GET, HEAD, and QUERY (keyed on its content) are cacheable
    iscacheable = req.config.cache && responsetype(req.endpoint) !== Nothing &&
        out.method ∈ ("GET", "HEAD", "QUERY") && !(out.payload isa IO)
    iscacheable || return http_request(req, out)
    cfile = joinpath(cachedir(), cachekey(out.url, out.method, out.headers, out.payload) * ".http")
    incoming, isstored = http_cached(cfile, req, out)
    isstored || incoming.response.status ∉ 200:299 ||
        cachesave(req, cfile, incoming.response, incoming.body)
    incoming
end

function http_request(req::Request, out::Outgoing, headers::Vector{Pair{String, String}} = out.headers)
    buf, input = IOBuffer(), payloadio(out)
    @debug debug_request(out.method, out.url, headers, input; secrets = authsecrets(req)) _file=nothing
    # Never less than the total `timeout`, which curl enforces
    options = TransferOptions(Watchdog(out.timeout; atleast = DEFAULT_IDLE_SECONDS), nothing)
    res = curlrequest(out.url, options; out.method, output = buf, input, headers, out.timeout,
                      throw = false)
    options.watchdog.fired && throw(IdleTimeout(req, options.watchdog.limit))
    res isa Downloads.RequestError && throw(res)
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
    (; response, body) = incoming
    try
        response.status ∈ 200:299 || throw(ResponseError(req, response, read(body)))
        dtype = responsetype(req.endpoint)
        data = if dtype !== Nothing
            interpretresponse(read(body), dataformat(req.endpoint, dtype), dtype)
        end
        postprocess(response, req, data)
    finally
        close(incoming)
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
