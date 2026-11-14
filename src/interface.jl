# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# Endpoint API

"""
    globalconfig(::Val{::Module}) -> RequestConfig

Return the global configuration for the given module.

This is used in [`@endpoint`](@ref) generated API functions.

See also: [`@globalconfig`](@ref), [`RequestConfig`](@ref).

!!! warning
    If you define this yourself rather than with `@globalconfig`, return the same
    `RequestConfig` each time (e.g. a `const`): a new one on every call loses state,
    such as rate limits, between requests.
"""
function globalconfig end

globalauthdefault(::Val) = (NoAuth(), :off)
globalformatdefault(::Val) = RawFormat()

"""
    urlpath([config::RequestConfig], endpoint::AbstractEndpoint) -> String

Return the name of the page for the given `endpoint`.

This is combined with the base URL and parameters to form the full URL for the
request.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function urlpath end

urlpath(::RequestConfig, endpoint::AbstractEndpoint) = urlpath(endpoint)

"""
    headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
    headers(config::RequestConfig, ::Type{T}) -> collection of `Pair{String, String}`

Return headers for the given `endpoint`, or for every endpoint of type `T`.

A request sends the endpoint's own headers, then those for its type and for each
of its supertypes in turn, so headers for a whole API belong on its supertype.
Define these for the type itself (`Type{MyAPI}`, not `Type{<:MyAPI}`), or they
are sent again for each type below it.

The default implementations return an empty list, or `()` for a type.

!!! note "Default Content-Type"

    When no `Content-Type` (or `Accept`) header is provided, it is set from the
    payload's (or result's) [`mimetype`](@ref).
"""
function headers(::AbstractEndpoint)
    Pair{String, String}[]
end

headers(::RequestConfig, endpoint::AbstractEndpoint) = headers(endpoint)
headers(::RequestConfig, ::Type) = ()

headers((; config, endpoint)::Request) =
    tieredattrs(headers, config, endpoint,
                if authpolicy(endpoint) !== :off authheaders(authscheme(endpoint), config.key) end)

"""
    parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}
    parameters(config::RequestConfig, ::Type{T}) -> collection of `Pair{String, String}`

Return URI parameters for the given `endpoint`, or for every endpoint of type `T`.

These are combined with the endpoint URL to form the full query URL: the
endpoint's own parameters, then those for its type and for each of its
supertypes in turn. As with [`headers`](@ref), parameters for a whole API belong
on its supertype, defined for the type itself.

The default implementations return an empty list, or `()` for a type.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function parameters(::AbstractEndpoint)
    Pair{String, String}[]
end

parameters(::RequestConfig, endpoint::AbstractEndpoint) = parameters(endpoint)
parameters(::RequestConfig, ::Type) = ()

parameters(req::Request) = tieredattrs(parameters, req.config, req.endpoint, authparam(req))

authparam((; config, endpoint)::Request) =
    if authpolicy(endpoint) !== :off authparams(authscheme(endpoint), config.key) end

function tieredattrs(fn::F, config::RequestConfig, endpoint::E, extra) where {F, E}
    own = fn(config, endpoint)
    own isa Vector{Pair{String, String}} && isnothing(extra) && !hasgenericattr(fn, config, E) &&
        return own
    attrs = collect(Pair{String, String}, own)
    isnothing(extra) || push!(attrs, extra)
    addgenericattrs!(attrs, fn, config, E)
end

# `isa`, not `isempty`, so this resolves at compile time
hasgenericattr(fn::F, conf::RequestConfig, ::Type{E}) where {F, E} =
    !(fn(conf, E) isa Tuple{}) || hasgenericattr(fn, conf, supertype(E))
hasgenericattr(::Function, ::RequestConfig, ::Type{Any}) = false

function addgenericattrs!(attrs::Vector{Pair{String, String}}, fn::F, conf::RequestConfig, ::Type{E}) where {F, E}
    append!(attrs, fn(conf, E))
    addgenericattrs!(attrs, fn, conf, supertype(E))
end
addgenericattrs!(attrs::Vector{Pair{String, String}}, ::Function, ::RequestConfig, ::Type{Any}) = attrs

"""
    authscheme(endpoint::AbstractEndpoint) -> AuthScheme

Return how `endpoint` authenticates: the [`AuthScheme`](@ref) describing where the
request's secret (`config.key`) is placed on the wire. The library applies it to
`key` automatically, so a standard scheme needs no [`headers`](@ref) method.

The default is the scheme given by `@globalconfig auth=` in the endpoint type's
module, else [`NoAuth`](@ref). Alternatively, declare a scheme once per API on a
shared endpoint supertype:

```julia
RestClient.authscheme(::MyAPIEndpoint) = BearerAuth()          # Authorization: Bearer <key>
RestClient.authscheme(::OtherEndpoint) = HeaderAuth("X-API-Key")
```

For a scheme the provided ones do not cover, define a new [`AuthScheme`](@ref); for
one that can't be expressed as a header or query parameter (request signing, OAuth
flows), read `config.key` in a [`headers`](@ref) method directly instead.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
authscheme(endpoint::AbstractEndpoint) = first(globalauthdefault(Val(parentmodule(typeof(endpoint)))))

"""
    authpolicy(endpoint::AbstractEndpoint) -> Symbol

How `endpoint` uses authentication, one of:

- `:off` — never send auth (the default);
- `:optional` — apply the [`authscheme`](@ref) to the request's `key` if one is set,
  but do not require it;
- `:required` — apply it, and reject a request whose `config.key` is `nothing` during
  validation, before sending, rather than relying on the server to return 401.

The default is the policy given by `@globalconfig auth=` in the endpoint type's
module (`:off` without one), looked up when a request is made. The
[`@endpoint`](@ref) macro overrides it per endpoint with an `:auth` (required),
`:authoption` (optional), or `:noauth` (off) marker.
"""
authpolicy(endpoint::AbstractEndpoint) = last(globalauthdefault(Val(parentmodule(typeof(endpoint)))))

"""
    validate([config::RequestConfig], endpoint::AbstractEndpoint) ->
        Union{Nothing, Bool, AbstractString, AbstractVector{<:AbstractString}}

Check if the request to `endpoint` according to `config` is valid.

The return value determines whether the request proceeds and, if not, what
problem to report:
- `nothing` or `String[]` means the request is valid and should proceed
- a string or vector of strings means the request is invalid and should be aborted, with the string(s) as the reason(s)
- `true` or `false` means valid or invalid, without a reason

This is also the appropriate place to emit warnings about non-critical potential
issues with the request, using `@warn` or `@debug`.

The default implementation returns `nothing` (proceed).

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function validate(@nospecialize ::AbstractEndpoint)
    nothing
end

validate(::RequestConfig, endpoint::AbstractEndpoint) = validate(endpoint)
validate((; config, endpoint)::Request) = validate(config, endpoint)

"""
    payload([config::RequestConfig], endpoint::AbstractEndpoint) -> Any

Return the payload for the given `endpoint`.

This is sent as the body of the request, whatever its method (e.g. `:post`, `:put`,
`:patch`, or `:query`).

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function payload end

payload(@nospecialize ::AbstractEndpoint) = nothing
payload(::RequestConfig, endpoint::AbstractEndpoint) = payload(endpoint)
payload((; config, endpoint)::Request) = payload(config, endpoint)


# Interpretation

"""
    responsetype(endpoint::AbstractEndpoint) -> Type

Return the type of the response for the given endpoint.

Together with `dataformat`, this is used to parse the response.

If `Vector{UInt8}` (the default implementation), the response data is provided raw.
If `Nothing`, the response body is discarded (and never cached), and the result is
`nothing`.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function responsetype(::AbstractEndpoint)
    Vector{UInt8}
end

"""
    errortype(endpoint::AbstractEndpoint) -> Type

Return the type an error response to `endpoint` is read as, in its own
[`dataformat`](@ref), or `Nothing` (the default) to leave error bodies unread.

The [`ResponseError`](@ref) raised for an unsuccessful status then carries the
body read as this type, as its `detail`. A body that doesn't read as it leaves
the `detail` as `nothing`, and the error is raised all the same.

A successful response that can't be read as the endpoint's
[`responsetype`](@ref) is tried as this type too, so an API that reports errors
inside a `200` raises them as a `ResponseError` (as does a [`Stream`](@ref)
element, partway through the stream). For that, the error type needs
a required field that a successful body lacks, or it would read from any body.

Define this for an API's endpoint supertype to read its errors from every
endpoint, or give one endpoint its own with [`@endpoint`](@ref)'s
`-> Result | Error`. An abstract error type, with a subtype chosen per body (as
by JSON.jl's `@choosetype`), is matched as declared: `ResponseError{APIError}`.
Make it an `Exception` with a `showerror` method for the error to show the API's
own explanation.

!!! note
    Part of the `AbstractEndpoint` interface.

# Examples

```julia
@jsondef struct EnsemblError
    message."error"::String
end

RestClient.errortype(::EnsemblEndpoint) = EnsemblError
```
"""
errortype(@nospecialize(::AbstractEndpoint)) = Nothing

"""
    dataformat([endpoint::AbstractEndpoint], ::Type{T}) -> AbstractFormat

Return the expected format that `T` is represented by in requests to and
responses from `endpoint`.

A type without a format of its own has [`UnspecifiedFormat`](@ref), which for an
endpoint becomes its API's format: [`RawFormat`](@ref), unless declared otherwise
with `@globalconfig … format = …` (see [`@globalconfig`](@ref)). Bytes and `IO`
are always `RawFormat`, as is any type whose own `dataformat` method says so, and
a string is text whatever the API's format (though a `Vector{String}` takes it).

A `dataformat(::Type)` method is automatically defined when invoking [`@jsondef`](@ref).
"""
dataformat(@nospecialize(::Type)) = UnspecifiedFormat()

function dataformat(endpoint::AbstractEndpoint, ::Type{T}) where {T}
    T <: Stream && return StreamFormat{framingtype(T)}(dataformat(endpoint, elementpayload(eltype(T))))
    fmt = dataformat(T)
    if !(fmt isa UnspecifiedFormat)
        fmt
    elseif T <: AbstractString
        RawFormat()
    else
        globalformatdefault(Val(parentmodule(typeof(endpoint))))
    end
end

dataformat(::Type{<:AbstractVector{UInt8}}) = RawFormat()
dataformat(::Type{<:IO}) = RawFormat()
dataformat(::Type{<:AbstractVector{T}}) where T = dataformat(T)
dataformat(::Type{<:Dict{<:Any, T}}) where T = dataformat(T)

"""
    mimetype(fmt::AbstractFormat, ::Type{T}) -> Union{String, MIME, Nothing}
    mimetype(endpoint::AbstractEndpoint, fmt::AbstractFormat, ::Type{T}) -> Union{String, MIME, Nothing}

The media type a `T` in `fmt` is labelled with, or `nothing` for none: the
`Content-Type` of a payload, and the `Accept` of a result. An endpoint labels a
type as its format does, unless a method for it (or its supertype) says
otherwise, such as for an API's vendor type:

```julia
RestClient.mimetype(::GitHubAPI, ::JSONFormat, ::Type) = MIME("application/vnd.github+json")
```

[`RawFormat`](@ref) has no label: a raw payload is sent as `text/plain` text or
`application/octet-stream` bytes, and a raw result accepts anything.
"""
mimetype(::AbstractFormat, ::Type) = nothing
mimetype(::AbstractEndpoint, fmt::AbstractFormat, ::Type{T}) where {T} = mimetype(fmt, T)

defaultlabel(::RawFormat, ::Type{<:Union{AbstractString, Number}}) = "text/plain; charset=utf-8"
defaultlabel(::RawFormat, ::Type) = "application/octet-stream"
defaultlabel(::AbstractFormat, ::Type) = nothing

"""
    interpretresponse(data::AbstractVector{UInt8}, fmt::AbstractFormat, ::Type{T}) -> value::T

Interpret `data` as a response of type `T` according to `fmt`.

For `JSONFormat`, the methods are implemented in the JSON.jl package extension.

A method must leave `data` intact, as a body that fails to read is tried as the
endpoint's [`errortype`](@ref), and kept in the error raised. Beware `String(data)`,
which takes `data`'s memory and leaves it empty: use `String(copy(data))`.
"""
function interpretresponse end

interpretresponse(data::AbstractVector{UInt8}, ::RawFormat, ::Type{>:Vector{UInt8}}) =
    Vector{UInt8}(data)

interpretresponse(data::AbstractVector{UInt8}, ::RawFormat, ::Type{T}) where {T <: AbstractString} =
    convert(T, String(copy(data)))

interpretresponse(data::AbstractVector{UInt8}, ::RawFormat, ::Type{T}) where {T <: Number} =
    parse(T, String(copy(data)))

interpretresponse(::AbstractVector{UInt8}, ::RawFormat, ::Type{T}) where {T} =
    throw(ArgumentError("Cannot interpret a raw (text/bytes) response as a $T. \
                         Declare the API's format (e.g. `@globalconfig … format = :json`), \
                         or $T's with a `RestClient.dataformat(::Type{$T})` method (e.g. via \
                         @jsondef), or define `interpretresponse` for it."))

"""
    writepayload(dest::IO, fmt::AbstractFormat, data)

Write `data` to `dest` according to `fmt`.
"""
writepayload(dest::IO, ::RawFormat, data::Union{AbstractString, AbstractVector{UInt8}, IO}) =
    write(dest, data)

writepayload(dest::IO, ::RawFormat, data::Number) = print(dest, data)

writepayload(::IO, ::RawFormat, data::T) where {T} =
    throw(ArgumentError("Cannot send a $T as a raw (text/bytes) payload. \
                         Declare the API's format (e.g. `@globalconfig … format = :json`), \
                         or $T's with a `RestClient.dataformat(::Type{$T})` method (e.g. via \
                         @jsondef), or define `writepayload` for it."))

const FORMAT_PACKAGES = (JSONFormat = :JSON, XMLFormat = :XML)

interpretresponse(::AbstractVector{UInt8}, fmt::Union{JSONFormat, XMLFormat}, ::Type{T}) where {T} =
    throw(ArgumentError("Cannot read a $T as $(nameof(typeof(fmt))) without its package loaded: \
                         add `using $(FORMAT_PACKAGES[nameof(typeof(fmt))])`"))

writepayload(::Any, fmt::Union{JSONFormat, XMLFormat}, data::T) where {T} =
    throw(ArgumentError("Cannot send a $T as $(nameof(typeof(fmt))) without its package loaded: \
                         add `using $(FORMAT_PACKAGES[nameof(typeof(fmt))])`"))

"""
    postprocess([response], request::Request, data) -> Any

Post-process the data returned by the request, by default returning it as-is.

`data` is the response body already read as the endpoint's
[`responsetype`](@ref): reading happens first, and a body that can't be read
raises a [`MalformedResponse`](@ref) without reaching `postprocess`. An error
status raises a [`ResponseError`](@ref) before either. To raise an error an API
reports inside a successful response, throw it here, or declare its
[`errortype`](@ref).

The result is then restructured: a [`ListResponse`](@ref) is wrapped in a
[`List`](@ref), and a [`SingleResponse`](@ref) gives its [`contents`](@ref). So
a method may check or unwrap the data, and a `ListResponse` it returns is still
paged.

The three-argument form also sees the `response`, with its `status` and
`headers`; by default it calls the two-argument form.

!!! note
    Part of the `AbstractEndpoint` interface.
"""
function postprocess(@nospecialize(::Request), data)
    data
end

postprocess(_, req::Request, data) = postprocess(req, data)

restructure(::Response, ::Request, data) = data
restructure(::Response, ::Request, single::SingleResponse) = contents(single)

restructure(response::Response, req::Request, list::ListResponse{T}) where {T} =
    List{T}(req, contents(list), list, response)


# Response handling

function contents end

"""
    contents(response::SingleResponse{T}) -> Union{T, Nothing}

Return the content of the response: its `T` field, or failing that its
`Union{T, Nothing}` field, which may be `nothing`.
"""
function contents(r::R) where {T, R <: SingleResponse{T}}
    if isempty(fieldslots(R, T)) && !isempty(fieldslots(R, Union{T, Nothing}))
        typedfield(r, Union{T, Nothing})
    else
        typedfield(r, T)
    end
end

"""
    contents(response::ListResponse{T}) -> Vector{T}

Return the items of the response: its `Vector{T}` field, or failing that its
`Union{Vector{T}, Nothing}` field, with `nothing` as no items.
"""
function contents(r::R) where {T, R <: ListResponse{T}}
    if isempty(fieldslots(R, Vector{T})) && !isempty(fieldslots(R, Union{Vector{T}, Nothing}))
        @something(typedfield(r, Union{Vector{T}, Nothing}), T[])
    else
        typedfield(r, Vector{T})
    end
end

function typedfield(r::R, ::Type{F}) where {R, F}
    slots = fieldslots(R, F)
    isempty(slots) &&
        throw(ArgumentError("No field of type $F found in $R, contents(::$R) must be explicitly defined"))
    length(slots) == 1 ||
        throw(ArgumentError("Multiple fields of type $F found in $R, contents(::$R) must be explicitly defined"))
    getfield(r, only(slots))
end

# Foldable, so `typedfield` compiles to a plain `getfield`
Base.@assume_effects :foldable fieldslots(::Type{R}, ::Type{F}) where {R, F} =
    filter(i -> fieldtype(R, i) == F, ntuple(identity, fieldcount(R)))

"""
    nextpage(list::List) -> Union{List, Nothing}
    nextpage([response], endpoint::AbstractEndpoint, data) -> Union{AbstractEndpoint, AbstractString, Nothing}

Fetch the page after `list`, or return `nothing` if it is the last.

An endpoint gives where its next page is with the second form, from the `data`
of the page before (its [`ListResponse`](@ref), after [`postprocess`](@ref)),
and in the three-argument form its `response` too: the next page's endpoint, its
URL, or `nothing` for none. Without such a method, the page the response's
`Link: <…>; rel="next"` header gives is fetched (RFC 8288). A URL may be
absolute or start with `/`, and must lie under the API's `baseurl`.

The `endpoint` is that of the request for the page before, or for a page reached
by URL, of the first page. Pages are numbered on from the first (see
[`thispagenumber`](@ref)), and [`pagecount`](@ref) may give their total.

# Examples

```julia
# A page number, until the response says there are no more
RestClient.thispagenumber(search::SearchEndpoint) = search.page
RestClient.nextpage(search::SearchEndpoint, results::SearchResults) =
    if results.has_more SearchEndpoint(search; page = search.page + 1) end

# A cursor
RestClient.nextpage(works::WorksEndpoint, page::WorksPage) =
    if !isnothing(page.next_cursor) WorksEndpoint(works; cursor = page.next_cursor) end

# The next page's URL, given in the response
RestClient.nextpage(::ResourcesEndpoint, page::ResourcePage) = page.next
```
"""
function nextpage(list::List{<:Any, <:Any, <:Request{kind}}) where {kind}
    (; request, response, page) = list
    target = nexttarget(list)
    next = if target isa AbstractEndpoint
        perform(Request{kind}(request.config, target))
    elseif !isnothing(target)
        absolute = @something(underbase(request.config, target), throw(ArgumentError(
            "The next page of $(endpointlabel(request.endpoint)), \
             $(redactsecrets(target, authsecrets(request))), is not under the API's base URL \
             $(request.config.baseurl); `nextpage` should give a URL under it, or an endpoint")))
        url = withauthparam(request, absolute)
        url == response.url && return
        out = outgoing(request)
        send(request, Outgoing(out.method, url, out.headers, out.payload, out.payloadstart))
    end
    if !isnothing(next) typeof(next)(next.request, next.items, next.data, next.response, page + 1) end
end

nextpage(_, endpoint::AbstractEndpoint, data) = nextpage(endpoint, data)
nextpage(@nospecialize(::AbstractEndpoint), _) = nothing

function nexttarget((; request, response, data)::List)
    next = nextpage(response, request.endpoint, data)
    if isnothing(next) nextlink(request.config, response.headers) else next end
end

# The `rel="next"` URL of a `Link` header (RFC 8288), if it lies under the base URL
function nextlink(config::RequestConfig, headers)
    isnextrel(param) = '=' ∈ param && let (key, rels) = split(lowercase(param), '='; limit = 2)
        strip(key) == "rel" && "next" ∈ split(strip(rels, (' ', '"', ',')))
    end
    # A delimiter within a quoted value is text (RFC 8288 §3)
    masked(value) = replace(value, r"\"[^\"]*\"" => quoted -> replace(quoted, r"[<;]" => ' '))
    for (name, value) in headers, link in eachsplit(masked(value), '<')
        name == "link" && '>' ∈ link || continue
        target, params = split(link, '>'; limit = 2)
        any(isnextrel, eachsplit(params, ';')) || continue
        return underbase(config, target)
    end
end

function underbase((; baseurl)::RequestConfig, url::AbstractString)
    absolute = if startswith(url, '/') join(first(split(baseurl, '/'), 3), '/') * url else String(url) end
    if startswith(absolute, baseurl * '/') absolute end
end

# `url` with `req`'s query auth, unless the server already included it
function withauthparam(req::Request, url::String)
    auth = authparam(req)
    isnothing(auth) && return url
    query = if '?' in url last(split(url, '?'; limit = 2)) else "" end
    any(startswith(url_parameters([first(auth) => ""])), eachsplit(query, '&')) && return url
    string(url, ifelse('?' in url, '&', '?'), url_parameters([auth]))
end

"""
    thispagenumber(list::List) -> Union{Int, Nothing}
    thispagenumber(endpoint::AbstractEndpoint) -> Union{Int, Nothing}

Return the number of the page `list` is, if it is one of several: counted on
from the number the first page's endpoint gives, else 1.

An endpoint that starts partway through a listing, such as by an offset, gives
its page's number with the second form; by default `nothing`.
"""
function thispagenumber(list::List)
    paged = list.page > 1 || !isnothing(thispagenumber(list.request.endpoint)) || !isnothing(nexttarget(list))
    if paged list.page end
end

thispagenumber(@nospecialize ::AbstractEndpoint) = nothing

"""
    remainingpages(list::List) -> Union{Int, Nothing}

Return how many pages follow `list`, if [`pagecount`](@ref) gives their total.
"""
function remainingpages((; request, response, data, page)::List)
    total = pagecount(response, request.endpoint, data)
    if !isnothing(total) max(total - page, 0) end
end

"""
    pagecount([response], endpoint::AbstractEndpoint, data) -> Union{Int, Nothing}

Return the total number of pages in the listing that `data`, read from a
request to `endpoint`, is a page of, if known: for instance from a count of
items in `data`, or from the three-argument form's `response` headers. By
default `nothing`.
"""
pagecount(_, endpoint::AbstractEndpoint, data) = pagecount(endpoint, data)
pagecount(@nospecialize(::AbstractEndpoint), _) = nothing

"""
    eachpage(list::List)

Iterate over `list` and the pages after it (see [`nextpage`](@ref)), fetching each
only when it is reached.

# Examples

```julia
for page in eachpage(users()), user in page
    println(user.name)
end
```
"""
eachpage(list::List) = Pages(list)

struct Pages{L <: List}
    first::L
end

Base.IteratorSize(::Type{<:Pages}) = Base.SizeUnknown()
Base.iterate(pages::Pages) = (pages.first, pages.first)
Base.iterate(::Pages, page) = let next = nextpage(page); if !isnothing(next); (next, next) end end


# Rate limits

"""
    nextallowed(endpoint::AbstractEndpoint, response::Response) -> Union{Real, Nothing}

Return when the API next allows a request after `response` to a request to
`endpoint`, in seconds from now or as a Unix time (a `time()` value), or
`nothing` to set no time. A time worth more than a year of seconds is taken as a
Unix time.

Each response from the network is given to this, and requests to the same host
with the same key wait until the later of this and the time its rate-limit
headers set (`Retry-After`, or `X-RateLimit-Reset` with `X-RateLimit-Remaining: 0`), so a
method can only lengthen the wait. An unsuccessful response with a time is
retried then, and with `Inf` is raised without a retry. By default `nothing`.

See also: [`RateLimit`](@ref), for limits known ahead of time.

# Examples

```julia
# A full minute's pause after any 429
RestClient.nextallowed(::LichessEndpoint, response) = if response.status == 429 60 end
```
"""
nextallowed(@nospecialize(::AbstractEndpoint), _) = nothing
