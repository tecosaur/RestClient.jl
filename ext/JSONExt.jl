# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module JSONExt

using RestClient
using JSON, StructUtils

RestClient.interpretresponse(data::AbstractVector{UInt8}, ::RestClient.JSONFormat{:json}, ::Type{T}) where {T} =
    JSON.parse(data, T)

RestClient.writepayload(dest::IO, ::RestClient.JSONFormat{:json}, data) =
    JSON.json(dest, data)

const JSON_KINDS_SUPPORTED = (:Struct, :Dict, :Array, :Vector, :Nothing, :String)
const JSON_KINDS_NEED_USER_METHODS = (:Number, :Bool)

function RestClient._jsondef_expand(::Val{:json}, source::LineNumberNode, mod::Module,
                                    option::Symbol, kind::Symbol, struc::Expr)
    kind ∈ JSON_KINDS_SUPPORTED ||
        if kind ∈ JSON_KINDS_NEED_USER_METHODS
            throw(ArgumentError(
                "@jsondef via JSON.jl does not support `kind=$kind` directly. \
                 Define `StructUtils.lift(::Type{T}, x)` and `StructUtils.lower(x::T)` \
                 manually, or load JSON3 instead."))
        else
            throw(ArgumentError(
                "@jsondef expects a valid kind, one of: \
                 $(join(JSON_KINDS_SUPPORTED, ", ")) (got $kind)"))
        end
    # `name::type &(json = (name = "key",),) = default`, the field tag `@kwarg` reads
    function tagged(field)
        renamed = !isnothing(field.key) && String(field.name) != field.key
        RestClient.fielddecl(field, if renamed d -> :($d & (json = (name = $(field.key),),)) else identity end)
    end
    (; name, fields, structdef) = RestClient.parsefields(struc, source; declare = tagged)
    structref = Expr(:<:, esc(name))
    # `@kwarg` is the StructUtils equivalent of `@kwdef`, honouring the field tags
    body = Expr[esc(Expr(:macrocall, GlobalRef(StructUtils, Symbol("@kwarg")), source, structdef))]
    fmtexpr = :(RestClient.JSONFormat{:json}())
    if kind ∈ (:Dict, :Array, :Vector)
        wrapfield = RestClient._jsondef_wrapper_field(kind, [f.type for f in fields], mod)
        isnothing(wrapfield) ||
            append!(body, RestClient._jsondef_wrapper_methods(esc(name), fmtexpr))
    elseif kind === :Nothing
        push!(body, :($(GlobalRef(StructUtils, :nulllike))(::Type{$structref}) = true))
    elseif kind === :String
        # structlike=false opts out of field-iteration parsing so JSON.jl uses `lift`.
        push!(body, :($(GlobalRef(StructUtils, :structlike))(::Type{$structref}) = false))
        push!(body, :($(GlobalRef(StructUtils, :lower))(x::$(esc(name))) = string(x)))
        push!(body, :($(GlobalRef(StructUtils, :lift))(::Type{$structref}, x::AbstractString) = $(esc(name))(x)))
    end
    option != :noshow && push!(body, RestClient.kwshow(name, fields))
    push!(body, :(RestClient.dataformat(::Type{$structref}) = $fmtexpr))
    Expr(:block, body...)
end

end
