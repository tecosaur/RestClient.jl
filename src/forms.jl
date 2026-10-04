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

MULTIPART_KEY::NTuple{2, UInt64} = (0, 0)
multipartboundary(::Type{T}) where {T} =
    "----" * string(first(speck128(MULTIPART_KEY, (hash(T), zero(UInt64)))); base = 16)

function speck128((k, l)::NTuple{2, UInt64}, (x, y)::NTuple{2, UInt64})
    round(x, y, k) = (x = (bitrotate(x, -8) + y) ⊻ k; (x, bitrotate(y, 3) ⊻ x))
    for i in zero(UInt64):UInt64(30)
        x, y = round(x, y, k)
        l, k = round(l, k, i)
    end
    round(x, y, k)
end

mimetype(::FormFormat, ::Type) = "application/x-www-form-urlencoded"
mimetype(::MultipartFormat, ::Type{T}) where {T} = "multipart/form-data; boundary=" * multipartboundary(T)

# RFC 3986 percent-encoding (via `encode_uri_component`) is universally
# accepted; we don't bother with the legacy `+`-for-space substitution.
function writepayload(io::IO, ::FormFormat, data)
    print(io, url_parameters(querypairs(form_key(typeof(data), name) => getproperty(data, name)
                                        for name in propertynames(data))))
end

"""
    MultipartFile(data; filename = nothing, mimetype = "application/octet-stream")

A file in a [`@multipartdef`](@ref) payload: `data` (bytes, a string, or an `IO`)
sent with the given `filename` (by default, its field's name) and `Content-Type`,
given as a string or a `MIME`.

Throws an `ArgumentError` if `mimetype` contains a line break.

# Example
```julia
@multipartdef struct Upload
    report::MultipartFile
end
Upload(report = MultipartFile(read("q3.csv"); filename = "q3.csv", mimetype = "text/csv"))
```
"""
struct MultipartFile{D <: Union{AbstractVector{UInt8}, AbstractString, IO}}
    data::D
    filename::Union{String, Nothing}
    mimetype::String
end

function MultipartFile(data; filename::Union{AbstractString, Nothing} = nothing,
                       mimetype::Union{AbstractString, MIME} = "application/octet-stream")
    mime = string(mimetype)
    any(∈(('\r', '\n')), mime) &&
        throw(ArgumentError("A multipart file's mimetype can't contain a line break, got $(repr(mime))"))
    MultipartFile(data, if !isnothing(filename) String(filename) end, mime)
end

const MultipartBinary = Union{AbstractVector{UInt8}, IO}

# Binary values are sent as files (by default named after their field), and other values
# without a format as their text (as in a URL-encoded form), a part per collection element
function writepayload(io::IO, ::MultipartFormat, data)
    boundary = multipartboundary(typeof(data))
    function writepart(disposition::String, mime, fmt::AbstractFormat, val)
        print(io, "--", boundary, "\r\nContent-Disposition: form-data; ", disposition)
        isnothing(mime) || print(io, "\r\nContent-Type: ", string(mime))
        print(io, "\r\n\r\n")
        writepayload(io, fmt, val)
        print(io, "\r\n")
    end
    # Percent-encode what would break a quoted name (RFC 7578 §4.2)
    quoted(name) = replace(name, '"' => "%22", '\r' => "%0D", '\n' => "%0A")
    for name in propertynames(data)
        val = getproperty(data, name)
        fieldname = quoted(form_key(typeof(data), name))
        issplit = val isa MultiValue && !(val isa MultipartBinary) &&
            dataformat(typeof(val)) isa Union{RawFormat, UnspecifiedFormat}
        for part in (if issplit val else (val,) end)
            isnothing(part) && continue
            part isa MultipartBinary && (part = MultipartFile(part))
            if part isa MultipartFile
                filename = quoted(something(part.filename, fieldname))
                writepart("name=\"$fieldname\"; filename=\"$filename\"", part.mimetype, RawFormat(), part.data)
            else
                fmt = dataformat(typeof(part))
                fmt isa UnspecifiedFormat && (fmt = RawFormat(); part = string(part))
                mime = @something(mimetype(fmt, typeof(part)), Some(defaultlabel(fmt, typeof(part))))
                writepart("name=\"$fieldname\"", mime, fmt, part)
            end
        end
    end
    print(io, "--", boundary, "--\r\n")
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
JSON parts, `Vector{UInt8}` / `IO` fields become octet-stream file parts, and
others become text parts. A [`MultipartFile`](@ref) field sets its file's name and
`Content-Type`. A collection without a format of its own, such as a
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
