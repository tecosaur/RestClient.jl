"""
    setfield(x::T, field::Symbol, value) -> ::T
"""
function setfield(x::T, field::Symbol, value) where {T}
    fvals = ((getfield(x, f) for f in fieldnames(T))...,)
    fidx = findfirst(==(field), fieldnames(T))
    isnothing(fidx) && throw(ArgumentError("Field $field not found in $T"))
    value isa fieldtype(T, field) || throw(ArgumentError("Field $field must be of type $(fieldtype(T, field)), got $(typeof(value))"))
    fvals = Base.setindex(fvals, value, fidx)
    T(fvals...)
end

"""
    @globalconfig expr

Define [`globalconfig`](@ref) for the current module as `expr`.

This is a minor convenience macro to avoid having to write the slightly awkward
`RestClient.globalconfig(::Val{@__MODULE__}) = expr`.

# Examples

```julia
@globalconfig RequestConfig("https://api.example.com")
```
"""
macro globalconfig(expr::Expr)
    :($(@__MODULE__).globalconfig(::Val{$__module__}) = $expr)
end

"""
    @jsondef [kind] struct ... end

Define a struct that can be used with `JSON3`.

This macro conveniently combines the following pieces:
- `@kwdef` to define keyword constructors for the struct.
- Custom `Base.show` method to show the struct with keyword arguments,
  omitting default values.
- `StructTypes.StructType` to define the struct type, and
  `StructTypes.names` (if needed) to define the JSON field mapping.
- `RestClient.dataformat` to declare that this struct is JSON-formatted.

Note the `name."json_field"` syntax demonstrated in the examples, that allows
for declaration of the JSON object key that should be mapped to the field.

Optionally the JSON representation `kind` can be specified. It
defaults to `Struct`, but can any of: `Struct`, `Dict`, `Array`,
`Vector`, `String`, `Number`, `Bool`, `Nothing`.

!!! warning "Soft JSON3 dependency"
    This macro is implemented in a package extension, and so
    requires `JSON3` to be loaded before it can be used.

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
macro jsondef(arg::Any)
    if arg isa Expr
        throw(ArgumentError("@jsondef requires JSON3 to be loaded"))
    else
        throw(ArgumentError("@jsondef expects a struct definition"))
    end
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
