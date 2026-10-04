# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using Base64: base64encode

mimetype(::Type{JSONFormat}) = "application/json"

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
    setfield(x::T, field::Symbol, value) -> T

Return a copy of `x` with `field` set to `value` (converted to the field's type),
leaving `x` itself unchanged. Useful for deriving the next request from an
immutable endpoint, as in a [`nextpage`](@ref) method.

# Errors

`ArgumentError` if `T` has no field named `field`.

# Examples

```julia
nextpage(e::PagedEndpoint) = setfield(e, :page, e.page + 1)
```
"""
function setfield(x::T, field::Symbol, value) where {T}
    fvals = ((getfield(x, f) for f in fieldnames(T))...,)
    fidx = findfirst(==(field), fieldnames(T))
    isnothing(fidx) && throw(ArgumentError("Field $field not found in $T"))
    fvals = Base.setindex(fvals, convert(fieldtype(T, field), value), fidx)
    T(fvals...)
end

"""
    @globalconfig config [auth=scheme | auth=(scheme, policy)] [format=fmt]

Define [`globalconfig`](@ref) for the current module as `config`, and optionally
module-wide authentication and format defaults for its endpoints.

This avoids having to write the slightly awkward
`RestClient.globalconfig(::Val{@__MODULE__}) = config` by hand. Similarly, the
optional `auth` clause defines a
`RestClient.globalauthdefault(::Val{@__MODULE__})` method. The `auth` argument
may take one of two forms:

- `auth = scheme`: the [`AuthScheme`](@ref) endpoints in this module use (e.g.
  `BearerAuth()`); the request's `key` is applied to it. With no policy given, the
  policy is `:off` — declare the scheme, but send it only where an endpoint opts in.
- `auth = (scheme, policy)`: also set the [`authpolicy`](@ref), one of:
  - `:off` — never send auth (the scheme is declared but unused here);
  - `:optional` — send the `key` if one is set, but do not require it;
  - `:required` — send the `key`, and reject a request that has none before sending.

The `format` clause sets the [`AbstractFormat`](@ref) of values that have none of
their own, such as a `Vector{String}` or `Dict{String, Any}` result, or a `String`
to decode as JSON, in place of [`RawFormat`](@ref). Bytes (`Vector{UInt8}`, `IO`)
stay raw. It may be a format, such as `JSONFormat()`, or the shorthand `:json` or
`:xml`. A [`dataformat`](@ref) method for an endpoint and type still takes precedence.

See also: [`globalconfig`](@ref), [`RequestConfig`](@ref), [`authscheme`](@ref).

# Examples

```julia
@globalconfig RequestConfig("https://api.example.com")
# Send a Bearer token on every endpoint when a key is set:
@globalconfig RequestConfig("https://api.example.com") auth=(BearerAuth(), :optional)
# Require a key on every endpoint (mark public ones `:noauth`):
@globalconfig RequestConfig("https://api.example.com") auth=(BearerAuth(), :required)
# Read and send plain values (e.g. a `Vector{String}`) as JSON:
@globalconfig RequestConfig("https://api.example.com") format=:json
```
"""
macro globalconfig(config, opts::Expr...)
    scheme, policy, format = :($(@__MODULE__).NoAuth()), QuoteNode(:off), RawFormat()
    for opt in opts
        Meta.isexpr(opt, :(=), 2) && first(opt.args) ∈ (:auth, :format) ||
            throw(ArgumentError("@globalconfig accepts only `auth=...` and `format=...` options, got `$opt`"))
        name, val = opt.args
        if name === :format
            format = if val isa QuoteNode
                get(FORMAT_SHORTHANDS, val.value) do
                    throw(ArgumentError("unknown format `$val` in @globalconfig; use one of \
                                         $(join(map(repr, keys(FORMAT_SHORTHANDS)), ", ")) or a format such as `JSONFormat()`"))
                end
            else
                esc(val)
            end
        elseif Meta.isexpr(val, :tuple, 2)
            scheme, policy = val.args
            policy isa QuoteNode && policy.value ∈ AUTH_POLICIES ||
                throw(ArgumentError("auth policy must be one of $AUTH_POLICIES, got `$policy`"))
        else
            scheme = val
        end
    end
    # `config` is evaluated per call (so e.g. an `ENV` key isn't baked into the
    # precompile image), but every call shares one rate-limit gate.
    gate = esc(gensym("rategate"))
    quote
        const $gate = $RateGate()
        $(@__MODULE__).globalconfig(::Val{$__module__}) =
            let c = $(esc(config))::$RequestConfig
                $RequestConfig(c.baseurl, $gate, c.key, c.timeout, c.cache)
            end
        $(@__MODULE__).globalauthdefault(::Val{$__module__}) = ($(esc(scheme)), $policy)
        $(@__MODULE__).globalformatdefault(::Val{$__module__}) = $format
    end
end

const FORMAT_SHORTHANDS = (json = JSONFormat(), xml = XMLFormat())

"""
    parsefield(line) -> (; name::Symbol, key, type, default)

Parse the struct field declaration `name[."key"][::type][ = default]`, with
`nothing` for the parts not given.

Throws an `ArgumentError` when `line` is not of that form.
"""
function parsefield(line)
    field, key, type, default = line, nothing, nothing, nothing
    Meta.isexpr(field, :(=), 2) && ((field, default) = field.args)
    Meta.isexpr(field, :(::), 2) && ((field, type) = field.args)
    if Meta.isexpr(field, :., 2) && field.args[2] isa QuoteNode && field.args[2].value isa String
        field, key = field.args[1], field.args[2].value
    end
    field isa Symbol ||
        throw(ArgumentError("Malformed field $line, expected `name[.\"key\"][::Type][ = default]`"))
    (; name = field, key, type, default)
end

"""
    parsefields(struc::Expr, source::LineNumberNode)
        -> (; name::Symbol, fields, structdef::Expr)

Parse the fields of the struct definition `struc` with [`parsefield`](@ref),
giving those typed `Union{…, Nothing}` a default of `nothing`.

`structdef` is `struc` with each field rewritten as `name[::Type][ = default]`, ready
for `@kwdef`. Docstrings, line numbers and inner constructors in the
body are kept.
"""
function parsefields(struc, source::LineNumberNode)
    Meta.isexpr(struc, :struct, 3) && Meta.isexpr(struc.args[3], :block) ||
        throw(ArgumentError("Expected a struct definition, not $struc"))
    mutable, decl, body = struc.args
    label = if Meta.isexpr(decl, :(<:), 2) first(decl.args) else decl end
    name = if Meta.isexpr(label, :curly) first(label.args) else label end
    name isa Symbol || throw(ArgumentError("Malformed struct name in $decl"))
    fields = @NamedTuple{name::Symbol, key::Union{String, Nothing}, type::Any, default::Any}[]
    newbody = map(body.args) do line
        line isa Union{LineNumberNode, String} && return line
        isconstructor = Meta.isexpr(line, :function) ||
            Meta.isexpr(line, :(=), 2) && Meta.isexpr(line.args[1], (:call, :where))
        isconstructor && return line
        field = parsefield(line)
        if isnothing(field.default) && allowsnothing(field.type)
            field = (; field..., default = :nothing)
        end
        push!(fields, field)
        fielddecl(field)
    end
    (; name, fields, structdef = Expr(:struct, mutable, decl, Expr(:block, newbody...)))
end

function fielddecl((; name, type, default))
    decl = if isnothing(type) name else :($name::$type) end
    if isnothing(default) decl else Expr(:(=), decl, default) end
end

"""
    _jsondef_expand(source::LineNumberNode, option::Symbol, kind::Symbol, struc::Expr) -> Expr

Expand a `@jsondef` struct definition. Implemented by the JSON.jl extension.
"""
function _jsondef_expand end

"""
    @jsondef [:noshow] [kind] struct ... end

Define a struct that can be read from and written as JSON.

This macro conveniently combines the following pieces:
- `@kwdef` to define keyword constructors for the struct.
- Custom `Base.show` method to show the struct with keyword arguments,
  omitting default values (suppress with the `:noshow` option).
- Registration of the JSON field-name mapping with JSON.jl.
- `RestClient.dataformat` to declare that this struct is JSON-formatted.

Note the `name."json_field"` syntax demonstrated in the examples, that allows
for declaration of the JSON object key that should be mapped to the field.
A field typed `Union{…, Nothing}` defaults to `nothing`.

The optional `kind` argument selects the JSON representation, one of `Struct`
(the default), `Dict`, `Array`, `Vector`, `Nothing`, and `String`. For `String`,
the user must define `Base.string` and a `T(::AbstractString)` constructor. A
number or boolean representation needs user-defined `StructUtils.lift` /
`StructUtils.lower` methods instead.

For an open-ended JSON object or array, a struct with a *single* field of kind
`Dict` or `Array`/`Vector` reads the whole object or array into that field, whether
it is the response itself or nested within another struct. A container kind with
more fields is read and written as a struct of its fields, like `Struct`.

```julia
@jsondef Dict struct Translations
    entries::Dict{String, String}   # {"en": "Hello", "fr": "Bonjour"} → Translations(...)
end
```

!!! warning "Soft JSON dependency"
    This macro is implemented in a package extension, and so
    requires `JSON` to be loaded before it can be used.

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
    isnothing(Base.get_extension(@__MODULE__, :JSONExt)) &&
        throw(ArgumentError("@jsondef requires JSON to be loaded"))
    _jsondef_expand(__source__, option, kind, struc)
end

function _jsondef_parse_args(args::Tuple)
    option, kind, struc = :default, :Struct, nothing
    for arg in args
        if arg isa QuoteNode && arg.value isa Symbol
            arg.value === :noshow ||
                throw(ArgumentError("Unknown @jsondef option `:$(arg.value)`; the only option is `:noshow`"))
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

A field whose XPath matches nothing takes its default value. Unlike plain
`@kwdef`, a default can't refer to other fields. A `Vector` field takes every
matching element, a `Union{…, Nothing}` field the first of several, and any
other field requires exactly one.

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
