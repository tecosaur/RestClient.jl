# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module JSONExt

using RestClient
using JSON, StructUtils

RestClient.interpretresponse(data::AbstractVector{UInt8}, ::RestClient.JSONFormat, ::Type{T}) where {T} =
    JSON.parse(data, T)

RestClient.writepayload(dest::IO, ::RestClient.JSONFormat, data) =
    JSON.json(dest, data)

const JSON_KINDS = (:Struct, :Dict, :Array, :Vector, :Nothing, :String)

function RestClient._jsondef_expand(source::LineNumberNode, option::Symbol, kind::Symbol, struc::Expr)
    kind ∈ JSON_KINDS || throw(ArgumentError(
        if kind ∈ (:Number, :Bool)
            "@jsondef does not support `kind=$kind`; define \
             `StructUtils.lift(::Type{T}, x)` and `StructUtils.lower(x::T)` for it instead."
        else
            "@jsondef expects a valid kind, one of: $(join(JSON_KINDS, ", ")) (got $kind)"
        end))
    (; name, fields, structdef) = RestClient.parsefields(struc, source)
    T = esc(name)
    body = Expr[esc(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), source, structdef))]
    if kind === :Nothing
        push!(body, :($(GlobalRef(StructUtils, :nulllike))(::Type{<:$T}) = true))
    elseif kind === :String
        append!(body, jsonvalue(T, :(S(x)), :(string(x))))
    elseif kind !== :Struct && length(fields) == 1
        # A single-field container is the whole JSON object or array, read into its field
        append!(body, jsonvalue(T, :(S($(GlobalRef(StructUtils, :make))(fieldtype(S, 1), x))),
                                :(getfield(x, 1))))
    else
        push!(body, fielddefaults_method(name, fields),
              :($(GlobalRef(JSON, :omit_null))(::Type{<:$T}) = true))
        # Not under `json`, so the names also hold in the default style container `lift`s read in
        renamed = [Expr(:kw, f.name, :((name = $(f.key),))) for f in fields
                   if !isnothing(f.key) && String(f.name) != f.key]
        isempty(renamed) ||
            push!(body, :($(GlobalRef(StructUtils, :fieldtags))(::$(GlobalRef(StructUtils, :StructStyle)), ::Type{<:$T}) =
                              $(Expr(:tuple, Expr(:parameters, renamed...)))))
    end
    option === :noshow || push!(body, kwshow(name, fields))
    push!(body, :(RestClient.dataformat(::Type{<:$T}) = RestClient.JSONFormat()))
    # Compiled as defined, so nested structs come before those holding them: compiling an
    # outer struct first makes inference re-infer the inner ones from every level, slowly
    kind === :Struct &&
        push!(body, :(isconcretetype($T) && precompile($(GlobalRef(JSON, :parse)), (Vector{UInt8}, Type{$T}))))
    Expr(:block, body...)
end

# `T` as a single JSON value rather than an object of its fields: read from the parsed
# value `x` by `lift`, and written as `lower`
jsonvalue(T, lift, lower) = Expr[
    :($(GlobalRef(StructUtils, :structlike))(::Type{<:$T}) = false),
    :($(GlobalRef(StructUtils, :lift))(::Type{S}, x) where {S <: $T} = $lift),
    :($(GlobalRef(StructUtils, :lower))(x::$T) = $lower)]

# StructUtils' `@kwarg` evaluates defaults without the fields they may use, so this
# fills each field absent from the JSON in order, as `@kwdef` would
function fielddefaults_method(name::Symbol, fields)
    fill = map(enumerate(fields)) do (i, field)
        absent = if isnothing(field.default)
            jsonkey = something(field.key, String(field.name))
            :(throw(ArgumentError($("Cannot read $name from JSON without its required key \"$jsonkey\"; \
                                     give the field a default if the API may leave it out"))))
        else
            esc(field.default)
        end
        :($(esc(field.name)) = if isassigned(vals, $i) vals[$i] else $absent end)
    end
    defaults = [Expr(:kw, f.name, esc(f.name)) for f in fields if !isnothing(f.default)]
    :(function $(GlobalRef(StructUtils, :fielddefaults))(::$(GlobalRef(StructUtils, :StructStyle)),
                                                       ::Type{<:$(esc(name))}, vals)
          $(fill...)
          $(Expr(:tuple, Expr(:parameters, defaults...)))
      end)
end

# `show` as `Name(field=value, …)`, leaving out fields at their default; the fields are
# bound first, so a default may refer to them
function kwshow(structname::Symbol, fields)
    showfields = map(fields) do (; name, default)
        showfield = :(print(io, ifelse(needscomma, ", ", ""), $("$name=")); show(io, $(esc(name))); needscomma = true)
        if isnothing(default) showfield else :($(esc(name)) === $(esc(default)) || $showfield) end
    end
    bindings = [:($(esc(name)) = x.$name) for (; name) in fields]
    quote
        function Base.show(io::IO, x::$(esc(structname)))
            show(io, $(esc(structname)))
            print(io, '(')
            needscomma = false
            $(Expr(:let, Expr(:block, bindings...), Expr(:block, showfields...)))
            print(io, ')')
        end
    end
end

end
