# The core API

"""
    perform(req::Request{kind}) -> Any

Validate and perform the request `req`, and return the result.

The specific behaviour is determined by the `kind` of the request, which
corresponds to an HTTP method name (`:get`, `:post`, etc.).
"""
function perform end

# Generic request functionality

"""
    ANSI_CLEAR_LINE

String escape sequence to clear the current line in a terminal.
"""
const ANSI_CLEAR_LINE = "\e[A\e[2K"

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
            print(io, '%', uppercase(string(b, base=16)))
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

function url((; config, endpoint)::Request)
    isnothing(config.baseurl) && throw(ArgumentError("Base URL is not set"))
    params = parameters(config, endpoint)
    string(config.baseurl, '/', pagename(config, endpoint)::String, url_parameters(params))
end

function addmimes!(headers::AbstractVector{Pair{String, String}}, ::In, ::Out) where {In, Out}
    setheaders = map(lowercase ∘ first, headers)
    inmime = mimetype(In)
    outmime = mimetype(Out)
    if "content-type" ∉ setheaders && !isnothing(inmime)
        push!(headers, "Content-Type" => inmime)
    end
    if "accept" ∉ setheaders && !isnothing(outmime)
        push!(headers, "Accept" => outmime)
    end
    headers
end

function addmimes!(headers::AbstractDict{<:AbstractString}, ::In, ::Out) where {In, Out}
    setheaders = [lowercase(k) for k in keys(headers)]
    inmime = mimetype(In)
    outmime = mimetype(Out)
    if "content-type" ∉ setheaders && !isnothing(inmime)
        headers["Content-Type"] = inmime
    end
    if "accept" ∉ setheaders && !isnothing(outmime)
        headers["Accept"] = outmime
    end
    headers
end

function mimeheaders(req::Request, in::Type)
    addmimes!(copy(headers(req)),
              if in !== Nothing dataformat(req.endpoint, in) end,
              dataformat(req.endpoint, responsetype(req.config, req.endpoint)))
end

"""
    catch_ratelimit(f::Function, reqlock::ReentrantLock, args...; kwargs...)

Call `f(args...; kwargs...)`, handling rate-limit headers appropriately.

If the request is rate-limited, this function will wait until the rate limit
is reset before retrying the request.
"""
function catch_ratelimit(f::F, reqlock::ReentrantLock, args...; kwargs...) where {F <: Function}
    islocked(reqlock) && @lock reqlock nothing
    local data
    try
        f(args...; kwargs...)
    catch err
        if islocked(reqlock)
            @lock reqlock nothing
            return catch_ratelimit(f, reqlock, args...; kwargs...)
        end
        @lock reqlock if err isa RequestError && err.response.status ∈ (403, 429)
            headers = Dict(err.response.headers)
            delay = @something(
                tryparse(Int, get(headers, "retry-after", "")::String),
                let ratelimit = tryparse(Int, get(headers, "x-ratelimit-remaining", "-1"))
                    if ratelimit === 0
                        reset = tryparse(Int, get(headers, "x-ratelimit-reset", "-1"))
                        if !isnothing(reset)
                            ceil(Int, resettime - time())
                        end
                    end
                end,
                rethrow())
            @info S"Rate limited :( asked to wait {emphasis:$delay} seconds, obliging..."
            if isa(stdout, Base.TTY)
                print('\n')
                for wait in 0:delay
                    sleep(1)
                    print(ANSI_CLEAR_LINE, S" Waited {emphasis:$wait} seconds\n")
                end
                print(ANSI_CLEAR_LINE)
            else
                sleep(delay)
            end
            return catch_ratelimit(f, reqlock, args...; kwargs...)
        end
        rethrow()
    end
end

# Utility functions

function debug_request(method::String, url::String, headers, body::Union{IO, Nothing} = nothing)
    bodyinfo = if isnothing(body)
        S""
    else
        dumpfile = joinpath(tempdir(), "rest-body.dump")
        @static if isdefined(Base.Filesystem, :temp_cleanup_later)
            isfile(dumpfile) || Base.Filesystem.temp_cleanup_later(dumpfile)
        end
        p0 = position(body)
        write(dumpfile, body)
        seek(body, p0)
        S"$(Base.format_bytes(position(body))) (saved to {bright_magenta:$dumpfile}) sent to "
    end
    strheaders = if isempty(headers)
        S""
    else
        join((S"\n       {emphasis:$k:} $v" for (k, v) in headers), "")
    end
    S"{inverse,bold,magenta: $method } $bodyinfo{light,underline:$url}$strheaders"
end

const MUNDANE_HEADERS = (
    "server", "date", "transfer-encoding", "connection", "keep-alive", "vary",
    "access-control-allow-origin", "strict-transport-security")

function debug_response(url::String, res, buf::IOBuffer)
    face, status, headers, msg = if res isa Downloads.RequestError
        :error, res.response.status, res.response.headers, res.response.message
    else
        dumpfile = joinpath(tempdir(), "rest-response.dump")
        @static if isdefined(Base.Filesystem, :temp_cleanup_later)
            isfile(dumpfile) || Base.Filesystem.temp_cleanup_later(dumpfile)
        end
        write(dumpfile, seekstart(buf))
        statuscolor = ifelse(200 <= res.status <= 299, :success, :warning)
        statuscolor, res.status, res.headers, S"$(Base.format_bytes(position(buf))) (saved to {bright_magenta:$dumpfile}) from"
    end
    headers = filter(h -> lowercase(first(h)) ∉ MUNDANE_HEADERS, headers)
    strheaders = if isempty(headers)
        S""
    else
        join((S"\n       {emphasis:$k:} $v" for (k, v) in headers), "")
    end
    S"{inverse,bold,$face: $status } $msg {light,underline:$url}$strheaders"
end

function handle_response(req::Request, res::Downloads.Response, buf::IO)
    dtype = responsetype(req.config, req.endpoint)
    fmt = dataformat(req.endpoint, dtype)
    seekstart(buf)
    data = interpretresponse(buf, fmt, dtype)
    postprocess(res, req, data)
end

# HTTP method implementations

# GET

function http_get_directly(url::String;
                          headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                          timeout::Float64 = Inf)
    buf = IOBuffer()
    @debug debug_request("GET", url, headers) _file=nothing
    res = Downloads.request(url; method="GET", output=buf, headers, timeout)
    @debug debug_response(url, res, buf) _file=nothing
    res isa Downloads.RequestError && throw(req)
    res, buf
end

function perform(req::Request{:get})
    validate(req) || throw(ArgumentError("Request is not well-formed"))
    res, buf = catch_ratelimit(http_get_directly, req.config.reqlock, url(req);
                               headers=mimeheaders(req, Nothing),
                               timeout=req.config.timeout)
    handle_response(req, res, buf)
end

# POST

function http_post_directly(url::String, payload::Union{<:IO, <:AbstractString, Nothing} = nothing;
                           headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                           timeout::Float64 = Inf)
    buf = IOBuffer()
    input = if payload isa IO
        payload
    elseif payload isa AbstractString
        IOBuffer(payload)
    end
    @debug debug_request("POST", url, headers, payload) _file=nothing
    res = Downloads.request(url; method="POST", output=buf, input, headers, timeout)
    @debug debug_response(url, res, buf) _file=nothing
    res isa Downloads.RequestError && throw(req)
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

function perform(req::Request{:post})
    validate(req) || throw(ArgumentError("Request is not well-formed"))
    pldtype, pldio = let pld = payload(req)
        typeof(pld), format_payload(req.endpoint, pld)
    end
    res, buf = catch_ratelimit(http_post_directly, req.config.reqlock, url(req), pldio;
                               headers=mimeheaders(req, pldtype),
                               timeout=req.config.timeout)
    handle_response(req, res, buf)
end
