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
    Meta.isexpr(struc, :struct, 3) ||
        throw(ArgumentError("@jsondef expects a struct definition"))
    Meta.isexpr(struc.args[3], :block) ||
        throw(ArgumentError("@jsondef expects a block definition within the struct"))
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
    structkind = getproperty(kindmap, kind)
    fields = @NamedTuple{
        name::Symbol,
        json::Union{String, Nothing},
        type::Union{Symbol, Expr, Nothing},
        default::Any,
        line::LineNumberNode
    }[]
    # Collect information from `struc`
    _, structdecl, structdef = struc.args
    structlabel = if Meta.isexpr(structdecl, :(<:))
        first(structdecl.args)
    else structdecl end
    structname, isparametric = if Meta.isexpr(structlabel, :curly)
        first(structlabel.args)::Symbol, true
    else
        structlabel::Symbol, false
    end
    lastline = source
    for line in structdef.args
        if line isa LineNumberNode
            lastline = line
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
    end
    # Generate the struct definition
    body = Expr[]
    structbody = Union{Expr, Symbol, LineNumberNode}[]
    for (; name, type, default, line) in fields
        push!(structbody, line,
              if isnothing(default)
                  if isnothing(type)
                      name
                  else
                      :($name::$type)
                  end
              else
                  if isnothing(type)
                      :($name = $default)
                  else
                      :($name::$type = $default)
                  end
              end)
    end
    structredef = Expr(:struct, false, structdecl, Expr(:block, structbody...))
    push!(body, esc(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), source, structredef)))
    # Show with kwargs
    if option != :noshow
        fieldvaldefaults = map(fields) do (; name, default)
            :(($(QuoteNode(name)), x.$name, $(esc(default))))
        end
        push!(body,
            quote
                function Base.show(io::IO, x::$(esc(structname)))
                    show(io, $(esc(structname)))
                    print(io, '(')
                    fieldvaldefaults = $(Expr(:tuple, fieldvaldefaults...))
                    needscomma = false
                    for (name, value, default) in fieldvaldefaults
                        value === default && continue
                        if needscomma
                            print(io, ", ")
                        else
                            needscomma = true
                        end
                        print(io, name, '=')
                        show(io, value)
                    end
                    print(io, ')')
                end
            end)
    end
    structref = if isparametric
        Expr(:(<:), esc(structname))
    else
        esc(structname)
    end
    fmtexpr = :(RestClient.JSONFormat{:json3}())
    wrapfield = RestClient._jsondef_wrapper_field(kind, [f.type for f in fields], mod)
    if !isnothing(wrapfield)
        append!(body, RestClient._jsondef_wrapper_methods(esc(structname), fmtexpr))
    elseif kind ∈ (:Dict, :Array, :Vector)
        # Non-wrapper: `dataformat` only — a StructType here would parse a
        # fielded struct as dict/array-like, which is wrong.
    else
        stt = GlobalRef(StructTypes, :StructType)
        stn = GlobalRef(StructTypes, :names)
        stkind = GlobalRef(StructTypes, structkind)
        push!(body, :($stt(::Type{$structref}) = $stkind()))
        if any(f -> !isnothing(f.json) && String(f.name) != f.json, fields)
            namemap = Expr[]
            for (; name, json) in fields
                jname = if isnothing(json) name else Symbol(json) end
                push!(namemap, :(($(QuoteNode(name)), $(QuoteNode(jname)))))
            end
            push!(body, :($stn(::Type{$structref}) = $(Expr(:tuple, namemap...))))
        end
    end
    # Declare that this struct is JSON formatted
    push!(body, :(RestClient.dataformat(::Type{$structref}) = $fmtexpr))
    Expr(:block, body...)
end

end
