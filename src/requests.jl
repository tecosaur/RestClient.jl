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
    string(config.baseurl, '/', urlpath(config, endpoint)::String, url_parameters(params))
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

function debug_request(method::String, url::String, headers, payload::Union{IO, Nothing} = nothing)
    payloadinfo = if isnothing(payload)
        S""
    else
        dumpfile = joinpath(tempdir(), "rest-request.http")
        @static if isdefined(Base.Filesystem, :temp_cleanup_later)
            isfile(dumpfile) || Base.Filesystem.temp_cleanup_later(dumpfile)
        end
        open(dumpfile, "w") do io
            println(io, method, ' ', url)
            for (k, v) in headers
                println(io, k, ": ", v)
            end
            println(io)
            mark(payload)
            write(dumpfile, payload)
            reset(payload)
        end
        S"$(Base.format_bytes(position(payload))) (saved to {bright_magenta:$dumpfile}) sent to "
    end
    strheaders = if isempty(headers)
        S""
    else
        join((S"\n       {emphasis:$k:} $v" for (k, v) in headers), "")
    end
    S"{inverse,bold,magenta: $method } $payloadinfo{light,underline:$url}$strheaders"
end

const MUNDANE_HEADERS = (
    "server", "date", "transfer-encoding", "connection", "keep-alive", "vary",
    "access-control-allow-origin", "strict-transport-security")

function debug_response(url::String, res, buf::IOBuffer)
    face, status, headers, msg = if res isa Downloads.RequestError
        :error, res.response.status, res.response.headers, res.response.message
    else
        dumpfile = joinpath(tempdir(), "rest-response.http")
        @static if isdefined(Base.Filesystem, :temp_cleanup_later)
            isfile(dumpfile) || Base.Filesystem.temp_cleanup_later(dumpfile)
        end
        open(dumpfile, "w") do io
            mark(buf)
            dumpresponse(io, res, buf)
            reset(buf)
        end
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

function handle_response(req::Request, res::Downloads.Response, body::IO)
    dtype = responsetype(req.config, req.endpoint)
    fmt = dataformat(req.endpoint, dtype)
    data = interpretresponse(body, fmt, dtype)
    close(body)
    postprocess(res, req, data)
end


# HTTP method implementations

function http_request(method::String, url::String, ::Nothing = nothing;
                      headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                      timeout::Float64 = Inf)
    buf = IOBuffer()
    @debug debug_request(method, url, headers) _file=nothing
    res = Downloads.request(url; method, output=buf, headers, timeout)
    seekstart(buf)
    @debug debug_response(url, res, buf) _file=nothing
    res isa Downloads.RequestError && throw(res)
    res, buf
end

function http_request(method::String, url::String, payload::Union{<:IO, <:AbstractString, Nothing};
                      headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                      timeout::Float64 = Inf)
    buf = IOBuffer()
    input = if payload isa IO
        payload
    elseif payload isa AbstractString
        IOBuffer(payload)
    end
    @debug debug_request(method, url, headers, payload) _file=nothing
    res = Downloads.request(url; method, output=buf, input, headers, timeout)
    seekstart(buf)
    @debug debug_response(url, res, buf) _file=nothing
    res isa Downloads.RequestError && throw(res)
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

function cached_request(req::Request, method::String, payload)
    pldtype, pldio = if isnothing(payload)
        Nothing, nothing
    else
        typeof(payload), format_payload(req.endpoint, payload)
    end
    rurl, headers = url(req), mimeheaders(req, pldtype)
    if !req.config.cache
        return http_request(method, rurl, pldio; headers, timeout=req.config.timeout)
    end
    res, body, wascached = http_cached(
        method, rurl, pldio; headers, timeout=req.config.timeout)
    if !wascached
        cachesave(req, rurl, headers,
                  if !isnothing(pldio) seekstart(pldio) end,
                  res, body)
    end
    res, body
end

function bare_request(req::Request, method::String)
    validate(req) || throw(ArgumentError("Request is not well-formed"))
    res, body = catch_ratelimit(
        cached_request, req.config.reqlock, req, method, nothing)
    handle_response(req, res, body)
end

function payload_request(req::Request, method::String)
    validate(req) || throw(ArgumentError("Request is not well-formed"))
    res, body = catch_ratelimit(
        cached_request, req.config.reqlock, req, method, payload(req))
    handle_response(req, res, body)
end


# Specific HTTP methods

perform(req::Request{:get}) = bare_request(req, "GET")
perform(req::Request{:options}) = bare_request(req, "OPTIONS")

perform(req::Request{:post}) = payload_request(req, "POST")
perform(req::Request{:put}) = payload_request(req, "PUT")
perform(req::Request{:patch}) = payload_request(req, "PATCH")
perform(req::Request{:delete}) = payload_request(req, "DELETE")

function perform(req::Request{:head})
    function head_request(url::String; headers, timeout)
        @debug debug_request("HEAD", url, headers) _file=nothing
        res = Downloads.request(url; method, headers, timeout)
        @debug debug_response(url, res, IOBuffer()) _file=nothing
        res
    end
    res = catch_ratelimit(
        head_request, req.config.reqlock, url(req);
        headers=headers(req),
        timeout=req.config.timeout)
    success = 200 <= res.status <= 299
    postprocess(res, req, success)
end
