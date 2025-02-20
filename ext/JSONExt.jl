module JSONExt

using RestClient
import RestClient: @jsondef
using JSON3, StructTypes

function RestClient.interpretresponse(data::IO, ::RestClient.JSONFormat, ::Type{T}) where {T}
    JSON3.read(data, T)
end

RestClient.mimetype(::Type{RestClient.JSONFormat}) = "application/json"

function RestClient.writepayload(dest::IO, ::RestClient.JSONFormat, data)
    JSON3.write(dest, data)
end

macro jsondef(opt::QuoteNode, kindname::Symbol, struc::Expr)
    option = opt.value
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
    kindname ∈ propertynames(kindmap) ||
        throw(ArgumentError("@jsondef expects a valid kind, one of: $(join(propertynames(kindmap), ", "))"))
    structkind = getproperty(kindmap, kindname)
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
    lastline = __source__
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
    push!(body, esc(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), __source__, structredef)))
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
    # Create the StructType definition
    structref = if isparametric
        Expr(:(<:), esc(structname))
    else
        esc(structname)
    end
    push!(body, :(StructTypes.StructType(::Type{$structref}) = StructTypes.$structkind()))
    if any(f -> !isnothing(f.json) && String(f.name) != f.json, fields)
        namemap = Expr[]
        for (; name, json) in fields
            jname = if isnothing(json) name else Symbol(json) end
            push!(namemap, :(($(QuoteNode(name)), $(QuoteNode(jname)))))
        end
        push!(body, :(StructTypes.names(::Type{$structref}) = $(Expr(:tuple, namemap...))))
    end
    # Declare that this struct is JSON formatted
    push!(body, :($RestClient.dataformat(::Type{$structref}) = JSONFormat()))
    # Return the generated code
    Expr(:block, body...)
end

macro jsondef(kind::Symbol, struc::Expr)
    Expr(:macrocall, GlobalRef(RestClient, Symbol("@jsondef")),
         __source__, QuoteNode(:default), kind, struc) |> esc
end

macro jsondef(struc::Expr)
    Expr(:macrocall, GlobalRef(RestClient, Symbol("@jsondef")),
         __source__, :Struct, struc) |> esc
end

macro jsondef(opt::QuoteNode, struc::Expr)
    Expr(:macrocall, GlobalRef(RestClient, Symbol("@jsondef")),
         __source__, opt, :Struct, struc) |> esc
end

end
