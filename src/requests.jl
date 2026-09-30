# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

const HTTP_DATE_FORMAT = dateformat"e, d u Y H:M:S G\MT"

# The core API

"""
    perform(req::Request{kind}) -> Any

Validate and perform the request `req`, and return the result.

The specific behaviour is determined by the `kind` of the request, which
corresponds to an HTTP method name (`:get`, `:post`, etc.).

See also: [`Request`](@ref).
"""
function perform end


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

url(req::Request) = string(req.config.baseurl, '/', urlpath(req.config, req.endpoint)::String,
                           url_parameters(parameters(req)))

# `headers(req)` plus the Content-Type/Accept implied by the payload and response formats
function mimeheaders(req::Request, in::Type)
    hdrs = headers(req)
    out = responsetype(req.endpoint)
    inmime = if in !== Nothing payloadmime(dataformat(req.endpoint, in), in) end
    outmime = if out !== Nothing mimetype(typeof(dataformat(req.endpoint, out))) end
    setnames = Set(lowercase(first(h)) for h in hdrs)
    extras = Pair{String, String}[]
    isnothing(inmime) || "content-type" ∈ setnames || push!(extras, "Content-Type" => inmime)
    isnothing(outmime) || "accept" ∈ setnames || push!(extras, "Accept" => outmime)
    if isempty(extras) hdrs else [collect(hdrs); extras] end
end

"""
    catch_ratelimit(f::Function, gate::RateGate, args...; maxwait::Real = Inf, kwargs...)

Call `f(args...; kwargs...)`, retrying while the response is rate-limited: a
429, or a 403 with rate-limit headers (see [`retrydelay`](@ref)).

`f` must return a `Downloads.Response`, optionally followed by its body (closed
when a retry discards it). Retries wait the delay the headers ask for, or back
off exponentially, up to `RATELIMIT_MAX_ATTEMPTS` attempts; a wait longer than
`maxwait` seconds isn't made, returning the rate-limited response at once. Calls
sharing `gate` share one backoff (see [`RateGate`](@ref)).
"""
function catch_ratelimit(f::F, gate::RateGate, args...; maxwait::Real = Inf, kwargs...) where {F <: Function}
    attempt = 1
    while true
        while (@atomic gate.nextallowed) - time() > 0
            lock(gate.lock) # block until the in-progress backoff releases
            unlock(gate.lock)
        end
        res, rest... = f(args...; kwargs...)
        delay = retrydelay(res.headers)
        islimited = res.status == 429 || (res.status == 403 && !isnothing(delay))
        wait = something(delay, 2^(attempt - 1))
        islimited && attempt < RATELIMIT_MAX_ATTEMPTS && wait <= maxwait || return res, rest...
        isempty(rest) || close(first(rest))
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
        for waited in 0:delay
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
        open(dumpfile, "w") do io
            println(io, method, ' ', url)
            for (k, v) in headers
                println(io, k, ": ", redactsecrets(v, secrets))
            end
            println(io)
            mark(payload)
            write(io, payload)
            reset(payload)
        end
        S"$(Base.format_bytes(position(payload))) (saved to {bright_magenta:$dumpfile}) sent to "
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

function handle_response(req::Request, res::Downloads.Response, body::IO)
    dtype = responsetype(req.endpoint)
    data = if dtype !== Nothing
        interpretresponse(read(body), dataformat(req.endpoint, dtype), dtype)
    end
    postprocess(res, req, data)
end



# HTTP method implementations

function http_request(method::String, url::String, payload::Union{IO, Nothing} = nothing;
                      headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                      timeout::Float64 = Inf, secrets::Vector{String} = String[])
    buf = IOBuffer()
    @debug debug_request(method, url, headers, payload; secrets) _file=nothing
    res = Downloads.request(url; method, output=buf, input=payload, headers, timeout)
    seekstart(buf)
    @debug debug_response(url, res, buf; secrets) _file=nothing
    res, buf
end

function format_payload(endpoint::AbstractEndpoint, payload)
    isnothing(payload) && return
    payload isa IO && return payload
    fmt = dataformat(endpoint, typeof(payload))
    buf = IOBuffer()
    writepayload(buf, fmt, payload)
    seekstart(buf)
end

# A thunk yielding `pldio` for each (re)send of a request, rewound to its start
# (a rate-limit retry resends the whole payload)
function resendable(pldio::Union{IO, Nothing})
    isnothing(pldio) && return Returns(nothing)
    start = if applicable(position, pldio) position(pldio) end
    issent = false
    function ()
        if issent
            isnothing(start) &&
                throw(ArgumentError("Cannot resend a request payload read from a non-seekable \
                                     $(typeof(pldio)) (to retry after rate limiting); provide \
                                     the payload as bytes or a seekable IO instead"))
            seek(pldio, start)
        end
        issent = true
        pldio
    end
end

function cached_request(req::Request, method::String, pldtype::Type, pldio::Union{IO, Nothing})
    rurl, headers = url(req), mimeheaders(req, pldtype)
    timeout, secrets = req.config.timeout, authsecrets(req)
    iscacheable = req.config.cache && responsetype(req.endpoint) !== Nothing &&
        method ∈ ("GET", "HEAD") # RFC 9111 §3: only GET/HEAD are cacheable
    iscacheable || return http_request(method, rurl, pldio; headers, timeout, secrets)
    cfile = joinpath(cachedir(), cachekey(rurl, method, headers, pldio) * ".http")
    res, body, isstored = http_cached(cfile, method, rurl, pldio; headers, timeout, secrets)
    isstored || res.status ∉ 200:299 || cachesave(req, cfile, res, body)
    res, body
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

function payload_request(req::Request, method::String, pld)
    check_validation(req)
    nextpayload = resendable(format_payload(req.endpoint, pld))
    res, body = catch_ratelimit(req.config.rategate; maxwait = req.config.timeout) do
        cached_request(req, method, typeof(pld), nextpayload())
    end
    try
        res.status ∈ 200:299 || throw(ResponseError(req, res, read(body)))
        handle_response(req, res, body)
    finally
        close(body)
    end
end



# Specific HTTP methods

perform(req::Request{:get}) = payload_request(req, "GET", nothing)
perform(req::Request{:options}) = payload_request(req, "OPTIONS", nothing)

perform(req::Request{:post}) = payload_request(req, "POST", payload(req))
perform(req::Request{:put}) = payload_request(req, "PUT", payload(req))
perform(req::Request{:patch}) = payload_request(req, "PATCH", payload(req))
perform(req::Request{:delete}) = payload_request(req, "DELETE", payload(req))

function perform(req::Request{:head})
    check_validation(req)
    rurl, hdrs, secrets = url(req), headers(req), authsecrets(req)
    res, = catch_ratelimit(req.config.rategate; maxwait = req.config.timeout) do
        @debug debug_request("HEAD", rurl, hdrs; secrets) _file=nothing
        headres = Downloads.request(rurl; method = "HEAD", headers = hdrs, timeout = req.config.timeout)
        @debug debug_response(rurl, headres, IOBuffer(); secrets) _file=nothing
        (headres,)
    end
    postprocess(res, req, res.status ∈ 200:299)
end
