# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

"""
    AbstractEndpoint

Abstract supertype for API endpoints.

What a request to an endpoint returns follows from its [`responsetype`](@ref):
a [`ListResponse`](@ref) is wrapped in a [`List`](@ref), a [`SingleResponse`](@ref)
gives its [`contents`](@ref), and a [`Stream`](@ref) is read incrementally.

To share behaviour across an API's endpoints (auth, validation, logging), define
an abstract subtype for the API and give it the shared methods.

# Interface

```
urlpath([config::RequestConfig], endpoint::AbstractEndpoint) -> String
headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
responsetype(endpoint::AbstractEndpoint) -> Type
validate([config::RequestConfig], endpoint::AbstractEndpoint) -> Union{Nothing, Bool, AbstractString, AbstractVector{<:AbstractString}}
postprocess([response::Downloads.Response], request::Request, data) -> Any
```

All of these functions but [`urlpath`](@ref) have default implementations.

See also: [`Request`](@ref), [`dataformat`](@ref), [`interpretresponse`](@ref).
"""
abstract type AbstractEndpoint end



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

Singleton type for raw text or bytes: strings and byte vectors are sent and
received as-is, numbers as their decimal text. Other types need a format of their own.
"""
struct RawFormat <: AbstractFormat end

"""
    UnspecifiedFormat <: AbstractFormat

The format of a type that declares none, given by the default `dataformat(::Type)`.
An endpoint takes it as its API's format (see [`@globalconfig`](@ref)), which is
[`RawFormat`](@ref) unless declared otherwise.
"""
struct UnspecifiedFormat <: AbstractFormat end

"""
    JSONFormat <: AbstractFormat

Singleton type for JSON request/response formats, read and written with JSON.jl
(which must be loaded).
"""
struct JSONFormat <: AbstractFormat end

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
# no per-endpoint `headers` method. Other header or query parameter schemes are new
# `AuthScheme` subtypes; those the library does not model at all (request signing, OAuth
# flows) are still served by defining a `headers`/`parameters` method directly, reading
# `config.key`.

# The authentication policies an endpoint may follow (see `authpolicy`): never send
# auth, send the key when present, or require a key before sending.
const AUTH_POLICIES = (:off, :optional, :required)

"""
    AuthScheme

Supertype for authentication schemes, which describe *how* an API expects its
secret: in which header or query parameter, and in what form. A scheme never holds
the secret itself, which is the `key` of a [`RequestConfig`](@ref). Declare an
endpoint's scheme with [`authscheme`](@ref), and every request applies it to `key`.

The provided schemes are [`BearerAuth`](@ref), [`BasicAuth`](@ref),
[`HeaderAuth`](@ref), [`QueryAuth`](@ref), and [`NoAuth`](@ref) (the default).

For any other header or query parameter scheme, define a subtype with an
[`authheaders`](@ref RestClient.authheaders) or
[`authparams`](@ref RestClient.authparams) method. A scheme's header or parameter
is hidden in debug output and errors, while a key an endpoint adds itself (in its
own [`headers`](@ref), [`parameters`](@ref), or [`urlpath`](@ref)) is not.

```julia
struct TokenAuth <: AuthScheme end
RestClient.authheaders(::TokenAuth, key::String) = "Authorization" => "Token \$key"
```
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

The rate-limit backoff shared by every request with the same config.

`nextallowed` is the deadline (a `time()` value, `0.0` when open) that requests
wait out before sending. The first rate-limited request moves it, and any others
limited at the same time wait for that same deadline rather than adding their own.
"""
mutable struct RateGate
    @atomic nextallowed::Float64
end

RateGate() = RateGate(0.0)

"""
    RequestConfig

The general configuration for a request to the API, not tied to any specific endpoint.

At a minimum, this holds the base URL of the API, and the rate-limit state its
requests share.

Other fields are optional, but may be useful for changing the way requests are
performed or handling authentication. The following additional fields can be
provided as keyword arguments:

- `key::Union{Nothing, String}`: The secret used to authenticate requests.
  How it is presented on the wire is declared by endpoints.
- `timeout::Float64`: The longest a request may take, in seconds, or for a
  [`Stream`](@ref), the longest it may go without receiving data. Defaults to
  `Inf`. Even so, a request that sends and receives nothing for 20 seconds (or
  `timeout`, if longer) is abandoned, as is a stream silent for 20 seconds when no
  `timeout` is set. Either limit raises a `Downloads.RequestError` with curl's
  timeout code (28).
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

function RequestConfig(baseurl::String; key::Union{Nothing, String} = nothing,
                       timeout::Real = Inf, cache::Bool = true)
    timeout > 0 || throw(ArgumentError("A RequestConfig's timeout must be positive (seconds), \
                                        got $timeout; leave it unset for no limit"))
    RequestConfig(String(rstrip(baseurl, '/')), RateGate(), key, Float64(timeout), cache)
end

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

const SUPPORTED_HTTP_METHODS = (:get, :post, :put, :patch, :delete, :head, :options, :query)

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
         │              ├─▶ headers ╾───────────────┤ ┊           ┊ ╰─────────╮   │
         │              │                           │ ╙┄┄┄┄┄┄┄┄┄┄┄╜        postprocess ╾──▶ result
         │             *╰─▶ payload ─▶ writepayload╶╯                           │
         │                    ╰─▶ dataformat ╾╯                                 │
         ╰─────────┬────────────────────────────────────────────────────────────╯
                   ╰────▶ validate (before initiating the request)

 * Only with a payload      ╶╶ Optional first argument
```
"""
struct Request{kind,E<:AbstractEndpoint}
    config::RequestConfig
    endpoint::E
end

Request{kind}(config::RequestConfig, endpoint::E) where {kind, E <: AbstractEndpoint} =
    Request{kind, E}(config, endpoint)

"""
    Outgoing

A request resolved to what goes on the wire, computed once however often it is
sent. A caller-supplied `IO` payload is sent as-is (from `payloadstart`, when
seekable); any other payload is encoded to bytes up front.
"""
struct Outgoing{P <: Union{Nothing, Vector{UInt8}, IO}}
    method::String
    url::String
    headers::Vector{Pair{String, String}}
    payload::P
    payloadstart::Union{Int, Nothing}
end

"""
    Incoming{B <: IO, T <: Union{Task, Nothing}}

A response head with its body, which for a stream is still arriving over
`transfer`. Closing it closes the body, and ends a stream's transfer at once by
notifying its `interrupt`.
"""
struct Incoming{B <: IO, T <: Union{Task, Nothing}}
    response::Downloads.Response
    body::B
    transfer::T
    interrupt::Union{Base.Event, Nothing}
end

Incoming(response::Downloads.Response, body::IO) = Incoming(response, body, nothing, nothing)

function Base.close(incoming::Incoming)
    close(incoming.body)
    isnothing(incoming.interrupt) || notify(incoming.interrupt)
end

"""
    MalformedRequest <: Exception

An exception type for requests that fail validation.

See also: [`validate`](@ref), [`Request`](@ref).
"""
struct MalformedRequest{R <: Request} <: Exception
    request::R
    issues::Vector{String}
end

MalformedRequest(request::Request, issues::AbstractVector{<:AbstractString}) =
    MalformedRequest{typeof(request)}(request, collect(String, issues))

function Base.showerror(io::IO, @nospecialize(ex::MalformedRequest{<:Request{kind}})) where {kind}
    print(io, "Malformed :", kind, " request to ", nameof(typeof(ex.request.endpoint)), ':')
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
    reason = replace(ex.response.message, r"^HTTP/\S+\s+\d+\s*" => "")
    print(io, "HTTP ", ex.response.status)
    isempty(reason) || print(io, ' ', reason)
    print(io, " from ", redactsecrets(ex.response.url, authsecrets(ex.request)))
    isempty(ex.body) || print(io, "\n  ", bodypreview(ex.body))
end

"""
    MalformedResponse <: Exception

An exception type for successful responses that cannot be read as the expected
type, such as malformed JSON or a missing required key.

`request` and `response` say where the response came from, `type` is what it was
being read as, `body` holds the bytes that failed (for a [`Stream`](@ref), the
frame's payload), and `error` is the underlying exception.

See also: [`perform`](@ref), [`interpretresponse`](@ref).
"""
struct MalformedResponse{R <: Request} <: Exception
    request::R
    response::Downloads.Response
    type::Type
    body::Vector{UInt8}
    error::Exception
end

function Base.showerror(io::IO, @nospecialize(ex::MalformedResponse))
    print(io, "Could not read a ", ex.type, " from the HTTP ", ex.response.status,
          " response to ", nameof(typeof(ex.request.endpoint)), " at ",
          redactsecrets(ex.response.url, authsecrets(ex.request)), "\n  ",
          replace(rstrip(sprint(showerror, ex.error; context = io)), '\n' => "\n  "))
    isempty(ex.body) || print(io, "\n  Body: ", bodypreview(ex.body))
end

function bodypreview(body::Vector{UInt8})
    shown = map(c -> if isvalid(c) && (isprint(c) || isspace(c)) c else '�' end,
                String(body[1:min(end, 512)]))
    replace(rstrip(shown), '\n' => "\n  ") * ifelse(length(body) > 512, " …", "")
end


# Generic response forms

"""
    List{T, E<:AbstractEndpoint, R<:Request}

Holds a list of values of type `T` returned by an `R` request to an `E` endpoint,
along with request information, metadata, the URL of the next page when the
response links to one (`Link: <…>; rel="next"`), and the page's number. A page
reached through that link keeps the request that started the listing, and is
numbered one on from the page that linked to it.

See also: [`ListResponse`](@ref), [`nextpage`](@ref).
"""
struct List{T, E<:AbstractEndpoint, R<:Request{<:Any, E}}
    request::R
    items::Vector{T}
    meta::Dict{Symbol, Any}
    nextlink::Union{String, Nothing}
    page::Int
end

List{T}(request::Request{<:Any, E}, items, meta, nextlink = nothing,
        page = something(thispagenumber(request), 1)) where {T, E} =
    List{T, E, typeof(request)}(request, items, meta, nextlink, page)

"""
    SingleResponse{T}

Abstract supertype for responses that wrap a single `T` item, such as an
envelope like `{"data": {...}}`. A request returns the item itself.

# Interface

Subtypes of `SingleResponse` may need to define:

```julia
contents(single::SingleResponse{T}) -> T
```

The generic implementation takes the response's one field of type `T`.
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
    function showitem(item)
        print(io, "\n  • ")
        show(IOContext(io, :compact => true, :typeinfo => I), m, item)
    end
    nitems = length(list.items)
    maxrows = first(displaysize(io)) - 4
    if nitems <= maxrows || !get(io, :limit, false)
        foreach(showitem, list.items)
    else
        nshown = max(maxrows - 3, 2) # 3 rows for the omission marker
        nhead, ntail = cld(nshown, 2), fld(nshown, 2)
        foreach(showitem, view(list.items, 1:nhead))
        print(io, S"\n  {shadow:⋮}\n  \
                   {shadow,italic:$(nitems - nshown) items omitted}\n  \
                   {shadow:⋮}")
        foreach(showitem, view(list.items, nitems-ntail+1:nitems))
    end
end
