# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

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
    @globalconfig expr

Define [`globalconfig`](@ref) for the current module as `expr`.

This is a minor convenience macro to avoid having to write the slightly awkward
`RestClient.globalconfig(::Val{@__MODULE__}) = expr`.

See also: [`globalconfig`](@ref), [`RequestConfig`](@ref).

# Examples

```julia
@globalconfig RequestConfig("https://api.example.com")
```
"""
macro globalconfig(expr::Expr)
    :($(@__MODULE__).globalconfig(::Val{$__module__}) = $expr)
end

"""
    _jsondef_expand(::Val{backend}, source::LineNumberNode, option::Symbol, kind::Symbol, struc::Expr) -> Expr

Expand a `@jsondef` struct definition for the given JSON `backend`
(`:json` for JSON.jl, `:json3` for JSON3.jl).

Implemented by a package extension.
"""
function _jsondef_expand end

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
    _jsondef_expand(Val(_jsondef_backend(__module__)), __source__, option, kind, struc)
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
