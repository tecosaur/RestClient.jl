# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

"""
    AbstractEndpoint

Abstract supertype for API endpoints.

Usually you will want to subtype either `SingleEndpoint` or `ListEndpoint`,
which share the same interface as `AbstractEndpoint` but have additional
semantics.

# Interface

```
pagename([config::RequestConfig], endpoint::AbstractEndpoint) -> String
headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
responsetype(endpoint::AbstractEndpoint) -> Union{Type, Nothing}
validate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool
postprocess([response::Downloads.Response], request::Request, data) -> Any
```

All of these functions but [`pagename`](@ref) have default implementations.

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
    JSONFormat <: AbstractFormat

Singleton type for JSON request/response formats.
"""
struct JSONFormat <: AbstractFormat end

"""
    XMLFormat <: AbstractFormat

Singleton type for XML request/response formats.
"""
struct XMLFormat <: AbstractFormat end


# Requests

"""
    RequestConfig

The general configuration for a request to the API, not tied to any specific endpoint.

At a minimum, this holds the base URL of the API and a lock for handling rate-limiting.

Other fields are optional, but may be useful for changing the way requests are
performed or handling authentication. The following additional fields can be
provided as keyword arguments:

- `key::Union{Nothing, String}`: An authentication key used to authorise requests.
  This only serves to hold the key, because of the varied ways that authentication
  may be performed implementation is left to individual endpoints.
- `timeout::Float64`: The timeout for the request, in seconds. This is passed to
  `Downloads.download` and defaults to `Inf`.
- `cache::Bool`: Whether to cache the response, using lifetime information from
  standard HTTP response headers. Defaults to `true`.

See also: [`@globalconfig`](@ref), [`Request`](@ref).

# Examples

```julia-repl
julia> RequestConfig("https://api.example.com")
RequestConfig("https://api.example.com", ReentrantLock(), nothing, Inf, true)

julia> RequestConfig("https://api.example.com", key = ENV["API_SECRET_KEY"])
RequestConfig("https://api.example.com", ReentrantLock(), "*****", Inf, true)
```
"""
struct RequestConfig
    baseurl::String
    reqlock::ReentrantLock
    key::Union{Nothing, String}
    timeout::Float64
    cache::Bool
end

RequestConfig(baseurl::String; key::Union{Nothing, String}=nothing, timeout::Real = Inf, cache::Bool = true) =
    RequestConfig(baseurl, ReentrantLock(), key, Float64(timeout), cache)

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
         │              ├─▶ pagename ╾───╮     │      ╓┄┄*debug*┄┄╖       │  ╭──────╯
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

See also: [`ListEndpoint`](@ref), [`ListResponse`](@ref).
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
