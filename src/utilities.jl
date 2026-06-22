# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using Base64: base64encode

"""
    JSONFormat()

Construct a `JSONFormat` singleton for whichever JSON backend is loaded.
JSON.jl takes priority if both are loaded; throws if neither is.
"""
function JSONFormat()
    hasjson  = !isnothing(Base.get_extension(@__MODULE__, :JSONExt))
    hasjson3 = !isnothing(Base.get_extension(@__MODULE__, :JSON3Ext))
    hasjson  && return JSONFormat{:json}()
    hasjson3 && return JSONFormat{:json3}()
    throw(ArgumentError("No JSON backend loaded; load JSON or JSON3"))
end

mimetype(::Type{RawFormat}) = "text/plain"
mimetype(::Type{<:JSONFormat}) = "application/json"

"""
    authheaders(scheme::AuthScheme, key) -> Union{Pair{String, String}, Nothing}

The header `scheme` contributes for secret `key`, or `nothing` for a parameter-based
or absent scheme, or when `key` is `nothing`. Appended to an endpoint's own
[`headers`](@ref) when the endpoint's [`authpolicy`](@ref) is not `:off`.
"""
function authheaders(::AuthScheme, ::Union{String, Nothing}) end

authheaders(::BearerAuth, key::String) = "Authorization" => "Bearer $key"
authheaders(::BasicAuth, key::String) = "Authorization" => "Basic " * base64encode(key)
authheaders(a::HeaderAuth, key::String) = a.name => key

"""
    authparams(scheme::AuthScheme, key) -> Union{Pair{String, String}, Nothing}

The query parameter `scheme` contributes for secret `key`, or `nothing` for a
header-based or absent scheme, or when `key` is `nothing`. Appended to an endpoint's
own [`parameters`](@ref) when the endpoint's [`authpolicy`](@ref) is not `:off`.
"""
function authparams(::AuthScheme, ::Union{String, Nothing}) end

authparams(a::QueryAuth, key::String) = a.name => key

"""
    setfield(x::T, field::Symbol, value) -> ::T
"""
function setfield(x::T, field::Symbol, value) where {T}
    fvals = ((getfield(x, f) for f in fieldnames(T))...,)
    fidx = findfirst(==(field), fieldnames(T))
    isnothing(fidx) && throw(ArgumentError("Field $field not found in $T"))
    fvals = Base.setindex(fvals, convert(fieldtype(T, field), value), fidx)
    T(fvals...)
end

"""
    @globalconfig config [auth=scheme | auth=(scheme, policy)]

Define [`globalconfig`](@ref) for the current module as `config`, and optionally a
module-wide authentication default applied to endpoints defined with [`@endpoint`](@ref).

This avoids having to write the slightly awkward
`RestClient.globalconfig(::Val{@__MODULE__}) = config` by hand. Similarly, the
optional `auth` clause defines a
`RestClient.globalauthdefault(::Val{@__MODULE__})` method. The `auth` argument
is may take one of two forms:

- `auth = scheme`: the [`AuthScheme`](@ref) endpoints in this module use (e.g.
  `BearerAuth()`); the request's `key` is applied to it. With no policy given, the
  policy is `:off` — declare the scheme, but send it only where an endpoint opts in.
- `auth = (scheme, policy)`: also set the [`authpolicy`](@ref), one of:
  - `:off` — never send auth (the scheme is declared but unused here);
  - `:optional` — send the `key` if one is set, but do not require it;
  - `:required` — send the `key`, and reject a request that has none before sending.

See also: [`globalconfig`](@ref), [`RequestConfig`](@ref), [`authscheme`](@ref).

# Examples

```julia
@globalconfig RequestConfig("https://api.example.com")
# Send a Bearer token on every endpoint when a key is set:
@globalconfig RequestConfig("https://api.example.com") auth=(BearerAuth(), :optional)
# Require a key on every endpoint (mark public ones `:noauth`):
@globalconfig RequestConfig("https://api.example.com") auth=(BearerAuth(), :required)
```
"""
macro globalconfig(config::Expr, opts::Expr...)
    scheme, policy = :($(@__MODULE__).NoAuth()), QuoteNode(:off)
    for opt in opts
        Meta.isexpr(opt, :(=), 2) && first(opt.args) === :auth ||
            throw(ArgumentError("@globalconfig accepts only an `auth=...` option, got `$opt`"))
        val = opt.args[2]
        if Meta.isexpr(val, :tuple, 2)
            scheme, policy = val.args
            policy isa QuoteNode && policy.value ∈ AUTH_POLICIES ||
                throw(ArgumentError("auth policy must be one of $AUTH_POLICIES, got `$policy`"))
        else
            scheme = val
        end
    end
    quote
        $(@__MODULE__).globalconfig(::Val{$__module__}) = $(esc(config))
        $(@__MODULE__).globalauthdefault(::Val{$__module__}) = ($(esc(scheme)), $policy)
    end
end

"""
    _jsondef_expand(::Val{backend}, source::LineNumberNode, mod::Module, option::Symbol, kind::Symbol, struc::Expr) -> Expr

Expand a `@jsondef` struct definition for the given JSON `backend`
(`:json` for JSON.jl, `:json3` for JSON3.jl).

Implemented by a package extension.
"""
function _jsondef_expand end

"""
    _jsondef_wrapper_field(kind::Symbol, fieldtypes, mod::Module) -> Union{Any, Nothing}

Return the field type expression for a single-field container wrapper, or
`nothing` for any other shape. The type is resolved in `mod`; a forward
reference (unevaluable) falls back to `nothing`.
"""
function _jsondef_wrapper_field(kind::Symbol, fieldtypes, mod::Module)
    kind ∈ (:Dict, :Array, :Vector) || return nothing
    length(fieldtypes) == 1 || return nothing
    ftype = only(fieldtypes)
    isnothing(ftype) && return nothing
    T = try Core.eval(mod, ftype) catch; return nothing end
    T isa Type || return nothing
    iswrapper = if kind === :Dict
        T <: AbstractDict
    else
        T <: AbstractVector
    end
    if iswrapper ftype end
end

"""
    _jsondef_wrapper_methods(structname, fmtexpr) -> Vector{Expr}

Emit `interpretresponse`/`writepayload` methods for a single-field container
wrapper, delegating to the wrapped field's own type (`fieldtype(T, 1)`).
"""
function _jsondef_wrapper_methods(structname, fmtexpr)
    fmttype = :(typeof($fmtexpr))
    Expr[
        :(function RestClient.interpretresponse(data::AbstractVector{UInt8}, fmt::$fmttype, ::Type{T}) where {T <: $structname}
              T(RestClient.interpretresponse(data, fmt, fieldtype(T, 1)))
          end),
        :(function RestClient.writepayload(io::IO, fmt::$fmttype, x::$structname)
              RestClient.writepayload(io, fmt, getfield(x, 1))
          end),
    ]
end

function _jsondef_backend(mod::Module)
    hasjson  = !isnothing(Base.get_extension(@__MODULE__, :JSONExt))
    hasjson3 = !isnothing(Base.get_extension(@__MODULE__, :JSON3Ext))
    hasjson || hasjson3 ||
        throw(ArgumentError("@jsondef requires JSON or JSON3 to be loaded"))
    hasjson  || return :json3
    hasjson3 || return :json
    # `jl_module_usings` is an internal API but stable since at least 1.0.
    usings = ccall(:jl_module_usings, Vector{Any}, (Any,), mod)
    inmodjson  = any(m -> nameof(m) === :JSON,  usings)
    inmodjson3 = any(m -> nameof(m) === :JSON3, usings)
    inmodjson && !inmodjson3 && return :json
    inmodjson3 && !inmodjson && return :json3
    throw(ArgumentError(
        "@jsondef in $mod is ambiguous: both JSON and JSON3 extensions are loaded. \
         `using` only one of them in this module."))
end

"""
    @jsondef [:noshow] [kind] struct ... end

Define a struct that can be read from and written as JSON.

This macro conveniently combines the following pieces:
- `@kwdef` to define keyword constructors for the struct.
- Custom `Base.show` method to show the struct with keyword arguments,
  omitting default values (suppress with the `:noshow` option).
- Registration of JSON field-name mapping with the active JSON backend.
- `RestClient.dataformat` to declare that this struct is JSON-formatted.

Note the `name."json_field"` syntax demonstrated in the examples, that allows
for declaration of the JSON object key that should be mapped to the field.

The optional `kind` argument selects the JSON representation, defaulting
to `Struct`. Both backends support `Struct`, `Dict`, `Array`, `Vector`,
`Nothing`, and `String`; for `String`, the user must define `Base.string`
and a `T(::AbstractString)` constructor. The JSON3 backend additionally
supports `Number` and `Bool`; with the JSON.jl backend these require
user-defined `StructUtils.lift` / `StructUtils.lower` methods.

For an open-ended JSON object or array, a struct wrapping a *single*
`AbstractDict` (`kind=Dict`) or `AbstractVector` (`kind=Array`/`Vector`) field
decodes the whole payload into that field (recursively) and wraps it. Any other
shape declares only `dataformat`, leaving the backend integration to the user.

```julia
@jsondef Dict struct Translations
    entries::Dict{String, String}   # {"en": "Hello", "fr": "Bonjour"} → Translations(...)
end
```

!!! warning "Soft JSON dependency"
    This macro is implemented in package extensions, and so
    requires either `JSON` or `JSON3` to be loaded before it can be used.
    If both are loaded, the backend is chosen based on which of `JSON`
    or `JSON3` is `using`'d in the calling module.

# Examples

```julia
@jsondef struct DocumentStatus
    exists::Bool  # Required, goes by 'exists' in JSON too
    status."document_status"::Union{String, Nothing} = nothing
    url."document_url"::Union{String, Nothing} = nothing
    age::Int = 0  # Known as 'age' in JSON too, defaults to 0
end
```
"""
macro jsondef(args...)
    option, kind, struc = _jsondef_parse_args(args)
    struc isa Expr ||
        throw(ArgumentError("@jsondef expects a struct definition"))
    _jsondef_expand(Val(_jsondef_backend(__module__)), __source__, __module__, option, kind, struc)
end

function _jsondef_parse_args(args::Tuple)
    option, kind, struc = :default, :Struct, nothing
    for arg in args
        if arg isa QuoteNode && arg.value isa Symbol
            option = arg.value
        elseif arg isa Symbol
            kind = arg
        elseif arg isa Expr
            struc = arg
        end
    end
    option, kind, struc
end

"""
    @xmldef struct ... end

Define a struct that can be used with `XML`.

This macro conveniently combines the following pieces:
- XML deserialization
- `@kwdef` to define keyword constructors for the struct.
- `RestClient.dataformat` to declare that this struct is XML-formatted.

Note the `name."xpath"` syntax demonstrated in the examples, that allows for
declaration of the (simple) XPath that should be used to extract the field. The
subset of supported XPath components are:
- `nodetag` to extract all immediate children with a given tag name
- `relative/node/paths`
- `*` to extract all children
- `text()` to extract the text content of a node
- `@attr` to extract the value of an attribute
- `nodetag[i]` to extract the `i`-th child of type `nodetag`
- `nodetag[last()]` to extract the last child of type `nodetag`

!!! warning "Soft XML dependency"
    This macro is implemented in a package extension, and so
    requires `XML` to be loaded before it can be used.

# Examples

```julia
@xmldef struct DocumentStatus
    exists."status/@exists"::Bool
    status."status/text()"::String
    url."status/@url"::Union{String, Nothing} = nothing
    age."status/@age"::Int
end
```
"""
macro xmldef(arg::Any)
    if arg isa Expr
        throw(ArgumentError("@xmldef requires XML to be loaded"))
    else
        throw(ArgumentError("@xmldef expects a struct definition"))
    end
end
