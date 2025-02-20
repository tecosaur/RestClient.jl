# Endpoint API

"""
    globalconfig(::Val{::Module}) -> RequestConfig

Return the global configuration for the given module.

This is used in [`@endpoint`](@ref) generated API functions.

!!! warning
    Be careful not to accidentally define this function in a way that generates
    a new `RequestConfig` every time it is called, as this will cause state information
    (like rate limits) to be lost between requests.
"""
function globalconfig end

"""
    pagename([config::RequestConfig], endpoint::AbstractEndpoint) -> String

Return the name of the page for the given `endpoint`.

This is combined with the base URL and parameters to form the full URL for the
request.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function pagename end

pagename(::RequestConfig, endpoint::AbstractEndpoint) = pagename(endpoint)

"""
    headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}

Return headers for the given `endpoint`.

The default implementation returns an empty list.

!!! note "Default Content-Type"

    When no `Content-Type` header is provided and `mimetype` is defined for
    the format, the `Content-Type` header is set to the value from `mimetype`.
"""
function headers(::AbstractEndpoint)
    Pair{String, String}[]
end

headers(::RequestConfig, endpoint::AbstractEndpoint) = headers(endpoint)
headers((; config, endpoint)::Request) = headers(config, endpoint)

"""
    parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}

Return URI parameters for the given `endpoint`.

This are combined with the endpoint URL to form the full query URL.

The default implementation returns an empty list.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function parameters(::AbstractEndpoint)
    Pair{String, String}[]
end

parameters(::RequestConfig, endpoint::AbstractEndpoint) = parameters(endpoint)

"""
    payload([config::RequestConfig], endpoint::AbstractEndpoint) -> Any

Return the payload for the given `endpoint`.

This is used for POST requests, and is sent as the body of the request.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function payload end

payload(::RequestConfig, endpoint::AbstractEndpoint) = payload(endpoint)
payload((; config, endpoint)::Request) = payload(config, endpoint)

"""
    responsetype(endpoint::AbstractEndpoint) -> Type

Return the type of the response for the given endpoint.

Together with `dataformat`, this is used to parse the response.

If `IO` (the default implementation), the response is not parsed at all.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function responsetype(::AbstractEndpoint)
    IO
end

"""
    dataformat([endpoint::AbstractEndpoint], ::Type{T}) -> AbstractFormat

Return the expected format that `T` is represented by in requests to and
responses from `endpoint`.

Using the default `dataformat(::Type)` method, the format is [`RawFormat`](@ref).

A `dataformat(::Type)` method is automatically defined when invoking [`@jsondef`](@ref).
"""
function dataformat(::Type{IO})
    RawFormat()
end

dataformat(::AbstractEndpoint, T::Type) = dataformat(T)

dataformat(::Type{Vector{T}}) where T = dataformat(T)

dataformat(::Type{<:Dict{<:Union{String, Integer, Float64}, T}}) where T = dataformat(T)

"""
    mimetype(::Type{<:AbstractFormat}) -> Union{String, Nothing}

Return the MIME type for the given format, if known.
"""
mimetype(::Type) = nothing

mimetype(::Type{RawFormat}) = "text/plain"

"""
    interpretresponse(data::IO, fmt::AbstractFormat, ::Type{T}) -> value::T

Interpret `data` as a response of type `T` according to `fmt`.
"""
function interpretresponse end

function interpretresponse(data::IO, ::RawFormat, ::Type)
    data
end

"""
    writepayload(dest::IO, fmt::AbstractFormat, data)

Write `data` to `dest` according to `fmt`.
"""
function writepayload(dest::IO, ::RawFormat, data)
    print(dest, data)
end

"""
    validate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool

Check if the request to `endpoint` according to `config` is valid.

This is called before the request is made, and can be used to check
if the request is well-formed. This is the appropriate place to
emit warnings about potential issues with the request.

Return `true` if the request should proceed, `false` otherwise.

The default implementation always returns `true`.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function validate(::AbstractEndpoint)
    true
end

validate(::RequestConfig, endpoint::AbstractEndpoint) = validate(endpoint)
validate((; config, endpoint)::Request) = validate(config, endpoint)

"""
    postprocess([response::Downloads.Response], request::Request, data) -> Any

Post-process the data returned by the request.

There are three generic implementations provided:
- For `SingleEndpoint` requests that return a `SingleResponse`,
  the `data` is wrapped in a `Single` object.
- For `ListEndpoint` requests that return a `ListResponse`,
  the `data` are wrapped in a `List` object.
- For all other endpoints, the data is returned as-is.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function postprocess(::Request, data)
    data
end

postprocess(::Downloads.Response, req::Request, data) =
    postprocess(req, data)

function postprocess(req::Request{kind, E}, data::ListResponse{T}) where {kind, E<:ListEndpoint, T}
    List{T, E}(req, contents(data), metadata(data))
end

function postprocess(req::Request{kind, E}, data::SingleResponse{T}) where {kind, E<:SingleEndpoint, T}
    Single{T, E}(req, contents(data), metadata(data))
end

# Response API

function contents end

"""
    content(response::SingleResponse{T}) -> T

Return the content of the response.
"""
function contents(r::R) where {T, R<:SingleResponse{T}}
    # The determination of `cfield` can be done at compile-time,
    # and at runtime this is a simple field access.
    cfield = nothing
    for f in fieldnames(R)
        if fieldtype(R, f) == T
            if isnothing(cfield)
                cfield = f
            else
                throw(ArgumentError("Multiple fields of type $T found in $T, contents(::$(typeof(r))) must be explicitly defined"))
            end
        end
    end
    if isnothing(cfield)
        throw(ArgumentError("No field of type $T found in $T, contents(::$(typeof(r))) must be explicitly defined"))
    else
        getfield(r, cfield)
    end
end

"""
    content(response::ListResponse{T}) -> Vector{T}

Return the items of the response.
"""
function contents(r::R) where {T, R<:ListResponse{T}}
    # The determination of `cfield` can be done at compile-time,
    # and at runtime this is a simple field access.
    cfield = nothing
    for f in fieldnames(R)
        if fieldtype(R, f) == Vector{T}
            if isnothing(cfield)
                cfield = f
            else
                throw(ArgumentError("Multiple fields of type Vector{$T} found in $R, contents(::$(typeof(r))) must be explicitly defined"))
            end
        end
    end
    if isnothing(cfield)
        throw(ArgumentError("No field of type Vector{$T} found in $R, contents(::$(typeof(r))) must be explicitly defined"))
    else
        getfield(r, cfield)
    end
end

"""
    metadata(response::SingleResponse) -> Dict{Symbol, Any}
    metadata(response::ListResponse) -> Dict{Symbol, Any}

Return metadata for the given response.

The default implementation returns an empty dictionary.
"""
function metadata end
metadata(::SingleResponse) = Dict{Symbol, Any}()
metadata(::ListResponse) = Dict{Symbol, Any}()

"""
    nextpage(response::List) -> Union{List, Nothing}

Fetch the next page of results after `response`.

If there are no more pages, or this method is not available for the given
endpoint, return `nothing`.
"""
function nextpage(l::List)
    nothing
end

"""
    thispagenumber(response::List) -> Union{Int, Nothing}

Return the current page number of `response`, if known.
"""
function thispagenumber(l::List)
    nothing
end

"""
    remainingpages(response::List) -> Union{Int, Nothing}

Return the number of remaining pages after `response`, if known.
"""
function remainingpages(l::List)
    nothing
end
