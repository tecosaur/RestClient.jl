module XMLExt

using RestClient
import RestClient: @xmldef
using XML

const AbstractNode = XML.AbstractXMLNode

function parsexml end

function RestClient.interpretresponse(data::IO, ::RestClient.XMLFormat, ::Type{T}) where {T}
    xml = read(data, LazyNode)
    parsexml(T, xml)
end

function writexml end

function RestClient.writepayload(dest::IO, ::RestClient.XMLFormat, data)
    writexml(dest, data)
end

# Parsing

abstract type XMLParseException <: Exception end

struct XMLMissingAttribute{N <: AbstractNode} <: XMLParseException
    node::N
    attr::String
end

struct XMLMissingChild{N <: AbstractNode} <: XMLParseException
    node::N
    tag::String
end

struct XMLMultipleChildren{N <: AbstractNode} <: XMLParseException
    node::N
    tag::String
end

function extract_attr(node::AbstractNode, attr::String)
    get(attributes(node), attr, nothing)
end

extract_attr(::Nothing, ::String) = nothing

function extract_text(node::AbstractNode)
    textnode = nothing
    for child in children(node)
        if tag(child) == nothing
            textnode = child
            break
        end
    end
    if !isnothing(textnode)
        value(textnode)
    end
end

extract_text(::Nothing) = nothing

function extract_child(node::AbstractNode, ctag::String)
    childs = children(node)
    found = nothing
    for child in childs
        tag(child) == ctag || continue
        if isnothing(found)
            found = child
        else
            throw(XMLMultipleChildren(node, ctag))
        end
    end
    found
end

extract_child(::Nothing, ::String) = nothing

function extract_children(node::AbstractNode, ctag::String)
    childs = children(node)
    found = empty(childs)
    for child in childs
        tag(child) == ctag || continue
        push!(found, child)
    end
    found
end

extract_children(::Nothing, ::String) = nothing

"""
    @xpath String -> Function

Create a function that extracts a value from an XML node using a simple subset
of XPath syntax. The function takes a single argument, the XML node and returns
either the extracted value or `nothing` if the path could not be found.

Supported components:
- `name` to extract a child node with the given tag name
- `*` to extract all children
- `name/text()` to extract the text content of a child node
- `name/@attr` to extract the value of an attribute
- `name[i]` to extract the i-th child node
- `name[last()]` to extract the last child node
"""
macro xpath(spec::String)
    body = :node
    terminated = false
    for component in eachsplit(spec, '/')
        if terminated
            throw(ArgumentError("Unreachable component in XPath: $component"))
        elseif isempty(component)
            throw(ArgumentError("Ungrounded selectors are not supported"))
        elseif component == "text()"
            body = :(extract_text($body))
            terminated = true
        elseif startswith(component, '@')
            body = :(extract_attr($body, $(String(component[2:end]))))
            terminated = true
        elseif component == "*"
            body = :(children($body))
        elseif '[' in component &&endswith(component, ']')
            path, idx = split(component[1:prevind(component, end)], '[', limit=2)
            if idx == "last()"
                body = :(let childs = extract_children($body, $(String(path)))
                             if !isnothing(childs) last(childs) end
                         end)
            else
                intidx = tryparse(Int, idx)
                isnothing(intidx) && throw(ArgumentError("Unrecognised index form: [$idx]"))
                body = :(get(extract_children($body, $(String(path))), $intidx, nothing))
            end
        else
            body = :(extract_children($body, $(String(component))))
        end
    end
    Expr(:->, :node, body)
end

parsexml(::Type{Union{T, Nothing}}, ::Nothing) where {T} = nothing
parsexml(::Type{String}, content::String) = XML.unescape(content)
parsexml(::Type{Symbol}, content::String) = Symbol(parsexml(String, content))
parsexml(::Type{T}, content::String) where {T} = parse(T, parsexml(String, content))
parsexml(::Type{Union{T, Nothing}}, content::String) where {T} = parsexml(T, content)
parsexml(::Type{Any}, value) = value
parsexml(::Type{Vector{T}}, nodes::Vector{<:AbstractNode}) where {T} = [parsexml(T, n) for n in nodes]

# Construction

macro xmldef(struc::Expr)
    Meta.isexpr(struc, :struct, 3) ||
        throw(ArgumentError("@xmldef expects a struct definition"))
    Meta.isexpr(struc.args[3], :block) ||
        throw(ArgumentError("@xmldef expects a block definition within the struct"))
    fields = @NamedTuple{
        name::Symbol,
        xml::Union{String, Nothing},
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
        xml, type, default = nothing, nothing, nothing
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
            name, qxml::QuoteNode = line.args
            xml = qxml.value
        else
            name = line
        end
        push!(fields, (name, xml, type, default, lastline))
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
    # Create the parse rule
    structref = if isparametric
        Expr(:(<:), esc(structname))
    else
        esc(structname)
    end
    fieldparsers = Any[]
    for (; xml, type, default) in fields
        node = if !isnothing(xml)
            ex = Expr(:macrocall, GlobalRef(@__MODULE__, Symbol("@xpath")),
                      __source__, xml)
            :($(esc(ex))(node))
        else
            :node
        end
        ftype = if isnothing(type) Any else esc(type) end
        push!(fieldparsers, :(parsexml($ftype, $node)))
    end
    push!(body, :(function $(esc(:($(@__MODULE__).parsexml)))(::Type{$structref}, node::$AbstractNode)
                      $(structref)($(fieldparsers...))
                  end))
    # Declare that this struct is XML formatted
    push!(body, :($RestClient.dataformat(::Type{$structref}) = XMLFormat()))
    # Return the generated code
    Expr(:block, body...)
end

end
