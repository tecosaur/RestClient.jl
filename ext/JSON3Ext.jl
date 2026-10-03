# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module JSON3Ext

using RestClient
using JSON3, StructTypes

RestClient.interpretresponse(data::AbstractVector{UInt8}, ::RestClient.JSONFormat{:json3}, ::Type{T}) where {T} =
    JSON3.read(data, T)

RestClient.writepayload(dest::IO, ::RestClient.JSONFormat{:json3}, data) =
    JSON3.write(dest, data)

function RestClient._jsondef_expand(::Val{:json3}, source::LineNumberNode, mod::Module,
                                    option::Symbol, kind::Symbol, struc::Expr)
    kindmap = (
        Struct = :Struct,
        Dict = :DictType,
        Array = :ArrayType,
        Vector = :ArrayType,
        String = :StringType,
        Number = :NumberType,
        Bool = :BoolType,
        Nothing = :NullType
    )
    kind ∈ propertynames(kindmap) ||
        throw(ArgumentError("@jsondef expects a valid kind, one of: $(join(propertynames(kindmap), ", "))"))
    (; name, fields, structdef) = RestClient.parsefields(struc, source)
    body = Expr[esc(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), source, structdef))]
    option != :noshow && push!(body, RestClient.kwshow(name, fields))
    structref = Expr(:<:, esc(name))
    fmtexpr = :(RestClient.JSONFormat{:json3}())
    wrapfield = RestClient._jsondef_wrapper_field(kind, [f.type for f in fields], mod)
    if !isnothing(wrapfield)
        append!(body, RestClient._jsondef_wrapper_methods(esc(name), fmtexpr))
    elseif kind ∈ (:Dict, :Array, :Vector)
        # Non-wrapper: `dataformat` only — a StructType here would parse a
        # fielded struct as dict/array-like, which is wrong.
    else
        stt = GlobalRef(StructTypes, :StructType)
        stn = GlobalRef(StructTypes, :names)
        push!(body, :($stt(::Type{$structref}) = $(GlobalRef(StructTypes, kindmap[kind]))()))
        if any(f -> !isnothing(f.key) && String(f.name) != f.key, fields)
            namemap = [:(($(QuoteNode(f.name)), $(QuoteNode(Symbol(something(f.key, f.name)))))) for f in fields]
            push!(body, :($stn(::Type{$structref}) = $(Expr(:tuple, namemap...))))
        end
    end
    # Declare that this struct is JSON formatted
    push!(body, :(RestClient.dataformat(::Type{$structref}) = $fmtexpr))
    Expr(:block, body...)
end

end
