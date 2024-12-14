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
    @globalconfig value

Store `value` as the global request configuration, and define [`globalconfig`](@ref) to return it.

# Examples

```julia
@globalconfig RequestConfig("https://api.example.com")
```
"""
macro globalconfig(expr::Expr)
    confvar = gensym("global-request-config")
    quote
        const $confvar = $expr
        $(@__MODULE__).globalconfig(::Val{$__module__}) = $confvar
    end
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

