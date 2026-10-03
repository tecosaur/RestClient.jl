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

mimetype(::Type{FormFormat}) = "application/x-www-form-urlencoded"

# Set to a random string in `__init__`
MULTIPART_BOUNDARY::String = ""
MULTIPART_MIME::String = ""
mimetype(::Type{MultipartFormat}) = MULTIPART_MIME

# RFC 3986 percent-encoding (via `encode_uri_component`) is universally
# accepted; we don't bother with the legacy `+`-for-space substitution.
function writepayload(io::IO, ::FormFormat, data)
    print(io, url_parameters(querypairs(form_key(typeof(data), name) => getproperty(data, name)
                                        for name in propertynames(data))))
end

const MultipartBinary = Union{AbstractVector{UInt8}, IO}

# Binary values are sent as files named after their field, and other values without a
# format as their text (as in a URL-encoded form), one part per element of a collection
function writepayload(io::IO, ::MultipartFormat, data)
    function writepart(fieldname::String, val)
        fmt = dataformat(typeof(val))
        fmt isa RawFormat && !(val isa MultipartBinary) && (val = string(val))
        mime = payloadmime(fmt, typeof(val))
        print(io, "--", MULTIPART_BOUNDARY, "\r\n",
              "Content-Disposition: form-data; name=\"", fieldname, '"')
        val isa MultipartBinary && print(io, "; filename=\"", fieldname, '"')
        isnothing(mime) || print(io, "\r\nContent-Type: ", mime)
        print(io, "\r\n\r\n")
        writepayload(io, fmt, val)
        print(io, "\r\n")
    end
    for name in propertynames(data)
        val = getproperty(data, name)
        # Percent-encode what would break the quoted name (RFC 7578 §4.2)
        fieldname = replace(form_key(typeof(data), name), '"' => "%22", '\r' => "%0D", '\n' => "%0A")
        issplit = val isa MultiValue && !(val isa MultipartBinary) && dataformat(typeof(val)) isa RawFormat
        for part in (if issplit val else (val,) end)
            isnothing(part) || writepart(fieldname, part)
        end
    end
    print(io, "--", MULTIPART_BOUNDARY, "--\r\n")
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
`name."form_key"::Type` syntax for remapping a field's form name. A field typed
`Union{…, Nothing}` defaults to `nothing`, and is left out of the form when
`nothing`. A collection (a `Vector`, `Tuple`, or `Set`) is sent as one field per
element, as in `tags=a&tags=b`.

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
become text parts. A collection without a format of its own, such as a
`Vector{String}` or a `Vector{Vector{UInt8}}`, is sent as one part per element
under the field's name. As with [`@formdef`](@ref), a field typed
`Union{…, Nothing}` defaults to `nothing`, and is left out when `nothing`.

# Example
```julia
@multipartdef struct Upload
    file::Vector{UInt8}
    metadata::Metadata    # `@jsondef struct Metadata ... end`
    note::Union{String, Nothing}
end
```
"""
macro multipartdef(struc::Expr)
    formlike_expand(__source__, struc, MultipartFormat)
end

# Shared expansion for `@formdef` / `@multipartdef`: a `@kwdef` struct plus a
# `dataformat` method and (if any keys were remapped) a `form_key` method.
function formlike_expand(source::LineNumberNode, struc::Expr, fmt::Type)
    (; name, fields, structdef) = parsefields(struc, source)
    form_keys = Tuple{Symbol, String}[(f.name, f.key) for f in fields if !isnothing(f.key)]
    sname = esc(name)
    out = Any[source, esc(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), source, structdef)),
              :(RestClient.dataformat(::Type{<:$sname}) = RestClient.$(nameof(fmt))())]
    isempty(form_keys) ||
        push!(out, :(RestClient.form_key(::Type{<:$sname}, name::Symbol) =
                     $(form_key_chain(form_keys))))
    Expr(:block, out...)
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
