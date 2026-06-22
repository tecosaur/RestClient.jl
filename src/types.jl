# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

"""
    AbstractEndpoint

Abstract supertype for API endpoints.

Usually you will want to subtype either `SingleEndpoint` or `ListEndpoint`,
which share the same interface as `AbstractEndpoint` but have additional
semantics.

Cross-cutting concerns (auth, logging, shared validation) are best expressed
by defining an API-specific abstract subtype and attaching generic methods to it.

# Interface

```
urlpath([config::RequestConfig], endpoint::AbstractEndpoint) -> String
headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
responsetype(endpoint::AbstractEndpoint) -> Union{Type, Nothing}
validate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool
postprocess([response::Downloads.Response], request::Request, data) -> Any
```

All of these functions but [`urlpath`](@ref) have default implementations.

See also: [`Request`](@ref), [`dataformat`](@ref), [`interpretresponse`](@ref).
"""
abstract type AbstractEndpoint end

"""
    SingleEndpoint <: AbstractEndpoint

Abstract supertype for API endpoints that return a single value.

See also: [`AbstractEndpoint`](@ref), [`SingleResponse`](@ref), [`Single`](@ref).
"""
abstract type SingleEndpoint <: AbstractEndpoint end

"""
    ListEndpoint <: AbstractEndpoint

Abstract supertype for API endpoints that return a list of values.

See also: [`AbstractEndpoint`](@ref), [`ListResponse`](@ref), [`List`](@ref).
"""
abstract type ListEndpoint <: AbstractEndpoint end


# Datatype singletons

"""
    AbstractFormat

Abstract supertype for response formats.

Typically, you will want to create a singleton subtype and then implement
the following methods for it:
- An [`interpretresponse`](@ref) method so a response in that format
  can be parsed/interpreted as a specific type.
- A [`writepayload`](@ref) method so data can be sent in that format.
- A [`mimetype`](@ref) method so that appropriate headers can be set
  when sending or receiving data in this format.

See also: [`RawFormat`](@ref), [`JSONFormat`](@ref).
"""
abstract type AbstractFormat end

"""
    RawFormat <: AbstractFormat

Singleton type for raw response formats.
"""
struct RawFormat <: AbstractFormat end

"""
    JSONFormat{backend} <: AbstractFormat

Singleton type for JSON request/response formats. The `backend` parameter
selects the JSON implementation: `:json` for JSON.jl, `:json3` for JSON3.jl.

The zero-argument constructor `JSONFormat()` picks an available backend
at the time of construction.
"""
struct JSONFormat{backend} <: AbstractFormat end

"""
    XMLFormat <: AbstractFormat

Singleton type for XML request/response formats.
"""
struct XMLFormat <: AbstractFormat end


# Authentication

# A scheme describes *how* an API expects its secret to be presented — as a Bearer
# header, Basic credential, a custom header, or a query parameter. This is an API fact,
# so a wrapper declares it once with an `authscheme` method on its endpoint (like
# `urlpath` or `responsetype`); the *secret* itself is the runtime `key` on a
# `RequestConfig`. The library combines the two and applies the result to every request
# automatically (see `headers`/`parameters` for a `Request`), so the common schemes need
# no per-endpoint `headers` method. A scheme the library does not model (request signing,
# OAuth flows) is still served by defining a `headers`/`parameters` method directly,
# reading `config.key`.

# The authentication policies an endpoint may follow (see `authpolicy`): never send
# auth, send the key when present, or require a key before sending.
const AUTH_POLICIES = (:off, :optional, :required)

"""
    AuthScheme

Supertype for authentication schemes: *how* an API presents its secret on the wire.
A scheme holds only API-specific shape (e.g. a header name), never the secret — the
secret is the `key` of a [`RequestConfig`](@ref). Declare a scheme for an endpoint
with [`authscheme`](@ref), and it is applied to `key` on every request.

The provided schemes are [`BearerAuth`](@ref), [`BasicAuth`](@ref),
[`HeaderAuth`](@ref), [`QueryAuth`](@ref), and [`NoAuth`](@ref) (the default).

Non-standard authentication schemes can be handled by adjusting
[`headers`](@ref) and [`parameters`](@ref) of endpoints on a case-by-case basis.
"""
abstract type AuthScheme end

"""
    NoAuth() <: AuthScheme

No authentication: the default [`authscheme`](@ref). Contributes nothing, regardless
of `key`.
"""
struct NoAuth <: AuthScheme end

"""
    BearerAuth() <: AuthScheme

Bearer-token authentication: the `key` is sent as `Authorization: Bearer <key>`. The
common scheme for OAuth2 access tokens and most modern token-based APIs.
"""
struct BearerAuth <: AuthScheme end

"""
    BasicAuth() <: AuthScheme

HTTP Basic authentication (RFC 7617): the `key` (a `"username:password"` string) is
sent as `Authorization: Basic <base64>`. Set `key = "user:pass"` on the config.
"""
struct BasicAuth <: AuthScheme end

"""
    HeaderAuth(name) <: AuthScheme

The `key` is sent in the named request header, e.g. `HeaderAuth("X-API-Key")`. For
APIs that authenticate with a custom header rather than `Authorization`.
"""
struct HeaderAuth <: AuthScheme
    name::String
end

"""
    QueryAuth(name) <: AuthScheme

The `key` is sent as the named URL query parameter, e.g. `QueryAuth("api_key")`. For
APIs that authenticate via the query string. Prefer a header scheme where the API
allows it: query parameters are more likely to be logged by proxies and servers.
"""
struct QueryAuth <: AuthScheme
    name::String
end


# Requests

"""
    RateGate

Per-config coordination point for rate-limit backoff.

`nextallowed` is a shared deadline (a `time()` value, `0.0` when open) read
atomically on the request fast path. When ratelimiting occurs, concurrent
requests can share a single backoff, coordinated by the lock.
"""
mutable struct RateGate
    const lock::ReentrantLock
    @atomic nextallowed::Float64
end

RateGate() = RateGate(ReentrantLock(), 0.0)

"""
    RequestConfig

The general configuration for a request to the API, not tied to any specific endpoint.

At a minimum, this holds the base URL of the API and a lock for handling rate-limiting.

Other fields are optional, but may be useful for changing the way requests are
performed or handling authentication. The following additional fields can be
provided as keyword arguments:

- `key::Union{Nothing, String}`: The secret used to authenticate requests.
  How it is presented on the wire is declared by endpoints.
- `timeout::Float64`: The timeout for the request, in seconds. This is passed to
  `Downloads.download` and defaults to `Inf`.
- `cache::Bool`: Whether to cache the response, using lifetime information from
  standard HTTP response headers. Defaults to `true`.

See also: [`@globalconfig`](@ref), [`Request`](@ref).

# Examples

```julia-repl
julia> RequestConfig("https://api.example.com", timeout = 5, cache = false)
RequestConfig("https://api.example.com"; timeout = 5.0, cache = false)

julia> RequestConfig("https://api.example.com", key = ENV["API_SECRET_KEY"])
RequestConfig("https://api.example.com"; key = "*****", cache = true)
```
"""
struct RequestConfig
    baseurl::String
    rategate::RateGate
    key::Union{Nothing, String}
    timeout::Float64
    cache::Bool
end

RequestConfig(baseurl::String; key::Union{Nothing, String} = nothing,
              timeout::Real = Inf, cache::Bool = true) =
    RequestConfig(baseurl, RateGate(), key, Float64(timeout), cache)

function Base.show(io::IO, rc::RequestConfig)
    show(io, RequestConfig)
    print(io, '(')
    show(io, rc.baseurl)
    if isnothing(rc.key)
        print(io, "; ")
    elseif get(io, :limit, false) === true
        print(io, S"; key = \"{shadow:*****}\", ")
    else
        print(io, "; key = ")
        show(io, rc.key)
        print(io, ", ")
    end
    if rc.timeout != Inf
        print(io, "timeout = ")
        show(io, rc.timeout)
        print(io, ", ")
    end
    print(io, "cache = ", rc.cache, ')')
end

"""
    Request{kind, E<:AbstractEndpoint}

A request to an API endpoint, with a specific configuration.

This is the complete set of information required to make a `kind` HTTP request
to an endpoint `E`. This consists of the request configuration and target
endpoint itself. The `kind` and endpoint`::E` are put as type parameters
so that they may participate in method dispatch.

See also: [`AbstractEndpoint`](@ref), [`RequestConfig`](@ref), [`perform`](@ref).

# Examples

```julia-repl
julia> Request{:get}(RequestConfig(...), MyEndpoint(...))
Request{:get, MyEndpoint}(RequestConfig(...),MyEndpoint(...))
```

# Data flow

```
         ╭─╴config╶────────────────────────────╮
         │     ╎                               │
         │     ╎        ╭─▶ responsetype ╾─────┼────────────────┬──▶ dataformat ╾───╮
Request╶─┤     ╰╶╶╶╶╶╶╶╶│                      │                ╰─────────╮         │
         │              ├─▶ urlpath ╾────╮     │      ╓┄┄*debug*┄┄╖       │  ╭──────╯
         │              │                ├──▶ url ╾─┬─━─▶ request ┊ ╭─▶ interpret ╾──▶ data
         ├─╴endpoint╶───┼─▶ parameters ╾─╯          │ ┊      ┠────━─┤                   │
         │              │                           ├─━──▶ cache  ┊ │             ╭─────╯
         │              ├─▶ parameters ╾────────────┤ ┊           ┊ ╰─────────╮   │
         │              │                           │ ╙┄┄┄┄┄┄┄┄┄┄┄╜        postprocess ╾──▶ result
         │             *╰─▶ payload ─▶ writepayload╶╯                           │
         │                    ╰─▶ dataformat ╾╯                                 │
         ╰─────────┬────────────────────────────────────────────────────────────╯
                   ╰────▶ validate (before initiating the request)

 * Only for POST requests   ╶╶ Optional first argument
```
"""
struct Request{kind,E<:AbstractEndpoint}
    config::RequestConfig
    endpoint::E
end

Request{kind}(config::RequestConfig, endpoint::E) where {kind, E <: AbstractEndpoint} =
    Request{kind, E}(config, endpoint)

"""
    MalformedRequest <: Exception

An exception type for requests that fail validation.

See also: [`validate`](@ref), [`Request`](@ref).
"""
struct MalformedRequest{R <: Request, S <: AbstractString} <: Exception
    request::R
    issues::Vector{S}
end

function Base.showerror(io::IO, @nospecialize(ex::MalformedRequest))
    print(io, "Malformed request:")
    if isempty(ex.issues)
        print(io, " (no details provided)")
    elseif length(ex.issues) == 1
        print(io, ' ', ex.issues[1])
    else
        for issue in ex.issues
            print(io, "\n  • ", issue)
        end
    end
end

"""
    ResponseError <: Exception

An exception type for requests that complete but return an unsuccessful HTTP
status (outside `200:299`).

Unlike a bare transport failure, the server's response is available: `response`
holds the status and headers, and `body` holds the raw response body, which APIs
routinely use to explain *why* a request failed.

See also: [`perform`](@ref), [`Request`](@ref).
"""
struct ResponseError{R <: Request} <: Exception
    request::R
    response::Downloads.Response
    body::Vector{UInt8}
end

function Base.showerror(io::IO, @nospecialize(ex::ResponseError))
    print(io, "HTTP ", ex.response.status)
    isempty(ex.response.message) || print(io, ' ', ex.response.message)
    print(io, " from ", ex.response.url)
    isempty(ex.body) && return
    bodystr = map(c -> if isvalid(c) && (isprint(c) || isspace(c)); c else '�' end, String(ex.body))
    if ncodeunits(bodystr) <= 512
        print(io, "\n  ", rstrip(bodystr))
    else
        print(io, "\n  ", SubString(bodystr, 1, thisind(bodystr, 512)), " …")
    end
end


# Generic response forms

"""
    Single{T, E<:SingleEndpoint}

Holds a single value of type `T` returned from an API endpoint,
along with request information and metadata.

See also: [`SingleEndpoint`](@ref), [`SingleResponse`](@ref).
"""
struct Single{T, E<:SingleEndpoint}
    request::Request{<:Any, E}
    data::T
    meta::Dict{Symbol, Any}
end

Base.getindex(single::Single) = single.data

"""
    List{T, E<:ListEndpoint}

Holds a list of values of type `T` returned from an API endpoint,
along with request information and metadata.

See also: [`ListEndpoint`](@ref), [`ListResponse`](@ref), [`nextpage`](@ref).
"""
struct List{T, E<:ListEndpoint}
    request::Request{<:Any, E}
    items::Vector{T}
    meta::Dict{Symbol, Any}
end

"""
    SingleResponse{T}

Abstract supertype for responses that contain
a single `T` item and (optionally) metadata.

# Interface

Subtypes of `SingleResponse` may need to define these two methods:

```julia
contents(single::SingleResponse{T}) -> T
metadata(single::SingleResponse{T}) -> Dict{Symbol, Any}
```

Both have generic implementations that are sufficient for simple cases.
"""
abstract type SingleResponse{T} end

"""
    ListResponse{T}

Abstract supertype for responses that contain
a list of `T` items and (optionally) metadata.

# Interface

Subtypes of `ListResponse` may need to define these two methods:

```julia
contents(list::ListResponse{T}) -> Vector{T}
metadata(list::ListResponse{T}) -> Dict{Symbol, Any}
```

Both have generic implementations that are sufficient for simple cases.
"""
abstract type ListResponse{T} end

Base.getindex(list::List, idx::Int) = list.items[idx]
Base.firstindex(list::List) = firstindex(list.items)
Base.lastindex(list::List) = lastindex(list.items)
Base.length(list::List) = length(list.items)
Base.eltype(::List{T}) where {T} = T
function Base.iterate(list::List, i::Int = firstindex(list))
    firstindex(list) <= i <= lastindex(list) || return nothing
    list.items[i], i + 1
end

function Base.show(io::IO, m::MIME"text/plain", list::List{I, E}) where {I, E}
    show(io, List)
    pageno, rempages = thispagenumber(list), remainingpages(list)
    print(io, S"\{{yellow:$(sprint(show, I))}\} holding \
    {emphasis:$(length(list.items))} item$(ifelse(length(list.items) == 1, \"\", \"s\"))")
    if !isnothing(pageno) && !isnothing(rempages)
        print(io, S", page {emphasis:$(pageno)} of {emphasis:$(pageno+rempages)}")
    elseif !isnothing(pageno)
        print(io, S", page {emphasis:$(pageno)}")
    end
    print(io, ':')
    drows = first(displaysize(io)) - 4
    if length(list.items) <= drows
        for item in list.items
            print(io, "\n  • ")
            show(IOContext(io, :compact => true, :typeinfo => I), m, item)
        end
    else
        drows -= 5 + drows ÷ 5
        for item in list.items[1:drows÷2]
            print(io, "\n  • ")
            show(IOContext(io, :compact => true, :typeinfo => I), m, item)
        end
        print(io, S"\n  {shadow:⋮}\n  \
                   {shadow,italic:$(length(list.items)-drows) items omitted}\n  \
                   {shadow:⋮}")
        for dataset in list.items[end-drows÷2:end]
            print(io, "\n  • ")
            show(IOContext(io, :compact => true, :typeinfo => I), m, dataset)
        end
    end
end
