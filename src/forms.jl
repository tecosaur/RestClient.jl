# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# Form-encoded and multipart request bodies. `FormFormat` flattens a struct
# to `key=value` pairs; `MultipartFormat` serialises each property as a MIME
# part, recursing through `dataformat` so e.g. an `@jsondef` field becomes a
# JSON part and a `Vector{UInt8}` field becomes an octet-stream part.

"""
    FormFormat <: AbstractFormat

`application/x-www-form-urlencoded` payloads.
"""
struct FormFormat <: AbstractFormat end

"""
    MultipartFormat <: AbstractFormat

`multipart/form-data` payloads. Each property of the serialised value becomes
a MIME part, encoded with the field type's own [`dataformat`](@ref).
"""
struct MultipartFormat <: AbstractFormat end

"""
    BinaryFormat <: AbstractFormat

`application/octet-stream` payloads. Default [`dataformat`](@ref) for
`Vector{UInt8}` and `IO`.
"""
struct BinaryFormat <: AbstractFormat end

mimetype(::Type{FormFormat}) = "application/x-www-form-urlencoded"
mimetype(::Type{BinaryFormat}) = "application/octet-stream"

# Set to a random string in `__init__`
MULTIPART_BOUNDARY::String = ""
MULTIPART_MIME::String = ""
mimetype(::Type{MultipartFormat}) = MULTIPART_MIME

# RFC 3986 percent-encoding (via `encode_uri_component`) is universally
# accepted; we don't bother with the legacy `+`-for-space substitution.
function writepayload(io::IO, ::FormFormat, data)
    firstpair = true
    for name in propertynames(data)
        val = getproperty(data, name)
        val === nothing && continue
        firstpair || print(io, '&')
        firstpair = false
        encode_uri_component(io, form_key(typeof(data), name))
        print(io, '=')
        encode_uri_component(io, string(val))
    end
end

# Each property becomes one MIME part: a boundary line, a Content-Disposition
# header, then a body shaped by `write_multipart_body` dispatch (binary fields
# get octet-stream; the generic method consults the field type's own
# `dataformat`, falling back to raw text for types without one — keeps the
# global `dataformat` strict outside the multipart context).
function writepayload(io::IO, ::MultipartFormat, data)
    for name in propertynames(data)
        val = getproperty(data, name)
        val === nothing && continue
        print(io, "--", MULTIPART_BOUNDARY, "\r\n",
                  "Content-Disposition: form-data; name=\"",
                  form_key(typeof(data), name), "\"")
        write_multipart_body(io, val)
    end
    print(io, "--", MULTIPART_BOUNDARY, "--\r\n")
end

const MultipartBinary = Union{Vector{UInt8}, IO}

function write_multipart_body(io::IO, value::MultipartBinary)
    # `write(io, value)` is polymorphic over `Vector{UInt8}` and `IO`.
    print(io, "\r\nContent-Type: application/octet-stream\r\n\r\n")
    write(io, value)
    print(io, "\r\n")
end

function write_multipart_body(io::IO, value)
    T = typeof(value)
    hasmethod(dataformat, Tuple{Type{T}}) || return print(io, "\r\n\r\n", value, "\r\n")
    fmt = dataformat(T)
    mime = mimetype(typeof(fmt))
    isnothing(mime) || print(io, "\r\nContent-Type: ", mime)
    print(io, "\r\n\r\n")
    writepayload(io, fmt, value)
    print(io, "\r\n")
end

"""
    form_key(::Type{T}, name::Symbol) -> String

The form-field name for field `name` of type `T`. Defaults to `string(name)`;
`@formdef` / `@multipartdef` emit overrides for fields that use the
`name."form_key"::Type` syntax.
"""
form_key(::Type, name::Symbol) = string(name)

"""
    @formdef struct ... end

Define a struct that serialises as [`FormFormat`](@ref). Supports the
`name."form_key"::Type` syntax for remapping a field's form name.

# Example
```julia
@formdef struct Login
    username::String
    password::String
    remember_me."remember-me"::Bool = false
end
```
"""
macro formdef(struc::Expr)
    formlike_expand(__source__, struc, FormFormat)
end

"""
    @multipartdef struct ... end

Define a struct that serialises as [`MultipartFormat`](@ref). Each field is
encoded with its type's own [`dataformat`](@ref): `@jsondef` fields become
JSON parts, `Vector{UInt8}` / `IO` fields become octet-stream parts, others
become text parts.

# Example
```julia
@multipartdef struct Upload
    file::Vector{UInt8}
    metadata::Metadata    # `@jsondef struct Metadata ... end`
    note::Union{String, Nothing} = nothing
end
```
"""
macro multipartdef(struc::Expr)
    formlike_expand(__source__, struc, MultipartFormat)
end

# Shared expansion for `@formdef` / `@multipartdef`. Strips `name."form_key"`
# syntax from field declarations, then emits a `@kwdef` struct plus a
# `dataformat` method and (if any keys were remapped) a `form_key` method.
function formlike_expand(source::LineNumberNode, struc::Expr, fmt::Type)
    Meta.isexpr(struc, :struct, 3) ||
        throw(ArgumentError("expected a struct definition"))
    _, decl, body = struc.args
    Meta.isexpr(body, :block) ||
        throw(ArgumentError("expected a block as the struct body"))
    structname = struct_name(decl)
    form_keys = Tuple{Symbol, String}[]
    fields = map(body.args) do line
        line isa LineNumberNode && return line
        rewritten, keypair = rewrite_field(line)
        isnothing(keypair) || push!(form_keys, keypair)
        rewritten
    end
    sname = esc(structname)
    kwstruct = Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), source,
                    Expr(:struct, false, decl, Expr(:block, fields...)))
    out = Any[source, esc(kwstruct),
              :(RestClient.dataformat(::Type{$sname}) = RestClient.$(nameof(fmt))())]
    isempty(form_keys) ||
        push!(out, :(RestClient.form_key(::Type{$sname}, name::Symbol) =
                     $(form_key_chain(form_keys))))
    Expr(:block, out...)
end

# Peel through `[ = default][::Type][name."form_key"]`, returning the rewritten
# field (with the form-key syntax replaced by a bare identifier) and an
# optional `(:field, "form_key")` pair to register.
function rewrite_field(line)
    default = nothing
    field = line
    if Meta.isexpr(field, :(=), 2)
        field, default = field.args
    end
    ftype = nothing
    if Meta.isexpr(field, :(::), 2)
        field, ftype = field.args
    end
    fname, keypair = if Meta.isexpr(field, :., 2)
        base, qkey = field.args
        base isa Symbol && qkey isa QuoteNode && qkey.value isa AbstractString ||
            throw(ArgumentError("malformed form-key syntax in $line"))
        base, (base, String(qkey.value))
    else
        field, nothing
    end
    rebuilt = fname
    isnothing(ftype) || (rebuilt = Expr(:(::), rebuilt, ftype))
    isnothing(default) || (rebuilt = Expr(:(=), rebuilt, default))
    rebuilt, keypair
end

# Extract a bare struct-name `Symbol` from a struct declaration, peeling
# `<: Super` and `{T,...}` wrappers.
struct_name(decl::Symbol) = decl
function struct_name(decl::Expr)
    if decl.head === :<:    return struct_name(decl.args[1]) end
    if decl.head === :curly return struct_name(decl.args[1]) end
    throw(ArgumentError("could not extract struct name from $decl"))
end

# Build an `if name === :foo "foo_form" elseif ... else string(name) end`
# chain from the remapping list. Order doesn't affect correctness (names are
# unique within a struct) but we keep declaration order for readability of
# `@code_lowered` output.
function form_key_chain(pairs::Vector{Tuple{Symbol, String}})
    chain = :(string(name))
    for (name, formname) in Iterators.reverse(pairs)
        chain = Expr(:if, :(name === $(QuoteNode(name))), formname, chain)
    end
    chain
end
