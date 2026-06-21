# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module JSONExt

using RestClient
using JSON, StructUtils

RestClient.interpretresponse(data::IO, ::RestClient.JSONFormat{:json}, ::Type{T}) where {T} =
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
    Meta.isexpr(struc, :struct, 3) ||
        throw(ArgumentError("@jsondef expects a struct definition"))
    Meta.isexpr(struc.args[3], :block) ||
        throw(ArgumentError("@jsondef expects a block definition within the struct"))
    # Extract field info and rewrite the body into @tags-compatible form
    _, structdecl, structdef = struc.args
    structlabel = if Meta.isexpr(structdecl, :(<:)) first(structdecl.args) else structdecl end
    structname, isparametric = if Meta.isexpr(structlabel, :curly)
        first(structlabel.args)::Symbol, true
    else
        structlabel::Symbol, false
    end
    fields = @NamedTuple{
        name::Symbol, json::Union{String, Nothing},
        type::Union{Symbol, Expr, Nothing}, default::Any, line::LineNumberNode}[]
    newbody = Union{Expr, Symbol, LineNumberNode}[]
    lastline = source
    for line in structdef.args
        if line isa LineNumberNode
            lastline = line
            push!(newbody, line)
            continue
        end
        json, type, default = nothing, nothing, nothing
        if Meta.isexpr(line, :(=), 2)
            line, default = line.args
        end
        if Meta.isexpr(line, :(::), 2)
            line, type = line.args
        end
        if isnothing(default) && Meta.isexpr(type, :curly) && :Nothing in type.args
            default = :nothing
        end
        if Meta.isexpr(line, :(.), 2)
            name, qjson::QuoteNode = line.args
            json = qjson.value
        else
            name = line
        end
        push!(fields, (name, json, type, default, lastline))
        # Rebuild as `name::type &(json=(name="json",),) = default`
        field = if isnothing(type) name else :($name::$type) end
        if !isnothing(json) && String(name) != json
            field = :($field & (json = (name = $json,),))
        end
        if !isnothing(default)
            field = :($field = $default)
        end
        push!(newbody, field)
    end
    taggedstruct = Expr(:struct, false, structdecl, Expr(:block, newbody...))
    structref = if isparametric Expr(:(<:), esc(structname)) else esc(structname) end
    # `@kwarg` is the StructUtils equivalent of `@kwdef`: it generates a
    # keyword-argument constructor *and* honours the `&(json=...)` field tags
    # we attach below. `@tags` alone only registers tags — no kwarg ctor.
    body = Expr[
        esc(Expr(:macrocall, GlobalRef(StructUtils, Symbol("@kwarg")), source, taggedstruct)),
    ]
    fmtexpr = :(RestClient.JSONFormat{:json}())
    if kind ∈ (:Dict, :Array, :Vector)
        wrapfield = RestClient._jsondef_wrapper_field(kind, [f.type for f in fields], mod)
        isnothing(wrapfield) ||
            append!(body, RestClient._jsondef_wrapper_methods(esc(structname), fmtexpr))
    elseif kind === :Nothing
        push!(body, :($(GlobalRef(StructUtils, :nulllike))(::Type{$structref}) = true))
    elseif kind === :String
        # structlike=false opts out of field-iteration parsing so JSON.jl uses `lift`.
        push!(body, :($(GlobalRef(StructUtils, :structlike))(::Type{$structref}) = false))
        push!(body, :($(GlobalRef(StructUtils, :lower))(x::$(esc(structname))) = string(x)))
        push!(body, :($(GlobalRef(StructUtils, :lift))(::Type{$structref}, x::AbstractString) = $(esc(structname))(x)))
    end
    if option != :noshow
        fieldtuples = map(fields) do (; name, default)
            :(($(QuoteNode(name)), x.$name, $(esc(default))))
        end
        push!(body, quote
            function Base.show(io::IO, x::$(esc(structname)))
                show(io, $(esc(structname)))
                print(io, '(')
                needscomma = false
                for (name, value, default) in $(Expr(:tuple, fieldtuples...))
                    value === default && continue
                    needscomma && print(io, ", ")
                    needscomma = true
                    print(io, name, '=')
                    show(io, value)
                end
                print(io, ')')
            end
        end)
    end
    push!(body, :(RestClient.dataformat(::Type{$structref}) = $fmtexpr))
    Expr(:block, body...)
end

end
