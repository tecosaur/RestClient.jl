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
- [`ResponseError`](@ref) if the server responds with an unsuccessful status,
  including a rate limit that outlasts the retries.
- `ArgumentError` if `kind` is not a supported HTTP method.

See also: [`Request`](@ref).
"""
function perform(req::Request)
    out = outgoing(req)
    # A caller's IO payload can only be resent if it can be rewound
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
    NO_SECRETS

The secrets of a request whose auth scheme places no key (see `authsecrets`).
"""
const NO_SECRETS = (header = nothing, param = nothing)

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
        deadline = @atomic gate.nextallowed
        incoming = send()::Incoming
        (; status, headers) = incoming.response
        delay = retrydelay(headers)
        islimited = status == 429 || (status == 403 && !isnothing(delay))
        pause = something(delay, 2^(attempt - 1))
        islimited && isretryable && attempt < RATELIMIT_MAX_ATTEMPTS && pause <= maxwait ||
            return incoming
        close(incoming)
        backoff!(gate, pause, deadline)
        attempt += 1
    end
end

# Skip the wait when another task has backed off since `deadline` was read before sending
function backoff!(gate::RateGate, delay::Real, deadline::Float64)
    @lock gate.lock begin
        (@atomic gate.nextallowed) == deadline || return
        @atomic gate.nextallowed = time() + delay
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
    strheaders = if isempty(headers)
        S""
    else
        join((S"\n       {emphasis:$k:} $(shown(k, v))" for (k, v) in headers), "")
    end
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
    check_validation(req)
    content = payload(req)
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

# Buffered: the whole response is read, through the HTTP cache when it may be stored.
function transmit(req::Request, out::Outgoing)
    iscacheable = req.config.cache && responsetype(req.endpoint) !== Nothing &&
        out.method ∈ ("GET", "HEAD") && !(out.payload isa IO) # RFC 9111 §3: only GET/HEAD
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
    res = Downloads.request(out.url; out.method, output = buf, input, headers, out.timeout)
    seekstart(buf)
    @debug debug_response(out.url, res, buf; secrets = authsecrets(req)) _file=nothing
    Incoming(res, buf)
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
