# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module XMLExt

using RestClient
import RestClient: @xmldef
using XML
using Dates: DateTime, TimeType, Minute

# XML 0.4 unescapes text and attribute values as it reads them; 0.3 leaves them as written
const XML_UNESCAPES = pkgversion(XML) >= v"0.4"
textvalue(raw::AbstractString) = if XML_UNESCAPES String(raw) else XML.unescape(raw) end
textvalue(::Nothing) = nothing

function parsexml end

function RestClient.interpretresponse(data::AbstractVector{UInt8}, ::RestClient.XMLFormat, ::Type{T}) where {T}
    root = read(IOBuffer(data), LazyNode)
    # `read` returns a document wrapper with a nil tag — descend to its root element.
    if isnothing(tag(root))
        for child in children(root)
            isnothing(tag(child)) && continue
            root = child
            break
        end
    end
    parsexml(T, root)
end

RestClient.writepayload(::IO, ::RestClient.XMLFormat, ::T) where {T} =
    throw(ArgumentError("Cannot send a $T as XML: @xmldef types are only read from responses. \
                         To send one, define `RestClient.writepayload(io::IO, ::RestClient.XMLFormat, data::$T)`."))

RestClient.mimetype(::RestClient.XMLFormat, ::Type) = "application/xml"

# Parsing

abstract type XMLParseException <: Exception end

struct XMLMultipleChildren{N <: LazyNode} <: XMLParseException
    node::N
    tag::String
end

Base.showerror(io::IO, ex::XMLMultipleChildren) =
    print(io, "Expected one <", ex.tag, "> in <", tag(ex.node), ">, but found several. \
               Select one with an index (`", ex.tag, "[1]`), or take them all with a \
               Vector-typed field.")

function extract_attr(node::LazyNode, attr::String)
    attrs = attributes(node)
    if !isnothing(attrs) textvalue(get(attrs, attr, nothing)) end
end

extract_attr(::Nothing, ::String) = nothing

function extract_text(node::LazyNode)
    texts = [value(child) for child in children(node) if XML.nodetype(child) == XML.Text]
    if !isempty(texts) textvalue(join(texts)) end
end

extract_text(::Nothing) = nothing

function printtext(io::IO, node::LazyNode)
    for child in children(node)
        kind = XML.nodetype(child)
        if kind == XML.Text
            print(io, value(child))
        elseif kind == XML.Element
            printtext(io, child)
        end
    end
end

function extract_child(node::LazyNode, ctag::String)
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

extract_children(node::LazyNode, ctag::String) = filter(child -> tag(child) == ctag, children(node))

extract_children(::Nothing, ::String) = nothing

"""
    @xpath String -> Function

Create a function that extracts a value from an XML node using a simple subset
of XPath syntax. The function takes a single argument, the XML node and returns
either the extracted value or `nothing` if the path could not be found.

Supported components:
- `name` to extract a child node with the given tag name
- `.` to extract the node itself
- `*` to extract all children
- `name/text()` to extract a child node's own text, without that of elements within it
- `name/@attr` to extract the value of an attribute
- `name[i]` to extract the i-th child node
- `name[last()]` to extract the last child node
"""
macro xpath(spec::String, many::Bool = false)
    body = if many; :([node]) else :node end
    # With `many`, each step applies to every node so far, as in XPath
    every(ex) = :([found for n in $body for found in $ex])
    some(ex) = :([found for n in $body for found in ($ex,) if !isnothing(found)])
    terminated = false
    components = collect(eachsplit(spec, '/'))
    for (i, component) in enumerate(components)
        is_last = i == length(components)
        if terminated
            throw(ArgumentError("Unreachable component in XPath: $component"))
        elseif isempty(component)
            throw(ArgumentError("Ungrounded selectors are not supported"))
        elseif component == "."
        elseif component == "text()"
            body = if many some(:(extract_text(n))) else :(extract_text($body)) end
            terminated = true
        elseif startswith(component, '@')
            attr = String(component[2:end])
            body = if many some(:(extract_attr(n, $attr))) else :(extract_attr($body, $attr)) end
            terminated = true
        elseif component == "*"
            body = if many every(:(children(n))) else :(children($body)) end
        elseif '[' in component && endswith(component, ']')
            path, idx = split(component[1:prevind(component, end)], '[', limit=2)
            pick = if idx == "last()"
                :(last(childs))
            else
                intidx = tryparse(Int, idx)
                isnothing(intidx) && throw(ArgumentError("Unrecognised index form: [$idx]"))
                :(get(childs, $intidx, nothing))
            end
            select(from) = :(let childs = extract_children($from, $(String(path)))
                                 if !isnothing(childs) && !isempty(childs)
                                     $pick
                                 end
                             end)
            body = if many some(select(:n)) else select(body) end
        elseif many
            body = every(:(extract_children(n, $(String(component)))))
        else
            # Terminal segment returns the matching child vector (so unwrapped
            # arrays work); intermediate segments return a single child for
            # `text()` / `@attr` to consume.
            body = if is_last
                :(extract_children($body, $(String(component))))
            else
                :(extract_child($body, $(String(component))))
            end
        end
    end
    Expr(:->, :node, body)
end

parsexml(::Type{Any}, value) = value
parsexml(::Type{Any}, content::String) = content
parsexml(::Type{Any}, ::Nothing) = nothing
parsexml(::Type{String}, content::String) = content
parsexml(::Type{Symbol}, content::String) = Symbol(content)
parsexml(::Type{T}, content::String) where {T} = parse(T, content)
# ISO 8601, as JSON reads it, with any `Z` or offset taken to UTC
function parsexml(::Type{DateTime}, content::String)
    stamp, sign, hours, minutes = match(r"^(.+?)(?:Z|([+-])(\d\d):?(\d\d))?$", content)
    moment = DateTime(stamp)
    isnothing(sign) && return moment
    offset = Minute(60 * parse(Int, hours) + parse(Int, minutes))
    if sign == "+" moment - offset else moment + offset end
end
parsexml(::Type{Vector{T}}, nodes::Vector{<:LazyNode}) where {T} = [parsexml(T, n) for n in nodes]
parsexml(::Type{Vector{T}}, values::Vector{String}) where {T} = [parsexml(T, v) for v in values]
parsexml(::Type{Vector{T}}, ::Nothing) where {T} = T[]
parsexml(::Type{T}, node::LazyNode) where {T <: Union{AbstractString, Number, TimeType}} =
    parsexml(T, textvalue(sprint(printtext, node)))
parsexml(::Type{T}, ::Nothing) where {T} =
    throw(ArgumentError("Required XML field of type `$T` is missing or empty"))

# A single-valued field takes exactly one of the matching children
function parsexml(::Type{T}, nodes::Vector{<:LazyNode}) where {T}
    isempty(nodes) && return parsexml(T, nothing)
    length(nodes) == 1 ||
        throw(ArgumentError("Expected one <$(tag(first(nodes)))> element for a field of type \
                             `$T`, but found $(length(nodes)); use a Vector-typed field to take all"))
    parsexml(T, only(nodes))
end
parsexml(::Type{Any}, nodes::Vector{<:LazyNode}) = nodes

parsexml(::Type{Union{T, Nothing}}, ::Nothing) where {T} = nothing
parsexml(::Type{Union{T, Nothing}}, content::String) where {T} = parsexml(T, content)
function parsexml(::Type{Union{T, Nothing}}, nodes::Vector{<:LazyNode}) where {T}
    isempty(nodes) && return nothing
    parsexml(T, if T <: Vector nodes else first(nodes) end)
end
parsexml(::Type{Union{T, Nothing}}, x) where {T} = parsexml(T, x)

function parsexml(::Type{T}, found, tags::NamedTuple) where {T}
    F = Base.nonnothingtype(T)
    lifted(::Type{E}, text::String) where {E} = if haskey(tags, :lift)
        tags.lift(text)
    elseif haskey(tags, :dateformat)
        E(text, tags.dateformat)
    else
        parsexml(E, text)
    end
    if F <: AbstractVector
        F(map(text -> lifted(eltype(F), text), parsexml(Vector{String}, found)))
    else
        lifted(F, parsexml(String, found))
    end
end

# Construction

macro xmldef(struc::Expr)
    (; name, fields, structdef) = RestClient.parsefields(struc, __source__)
    body = Expr[esc(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), __source__, structdef))]
    # Create the parse rule
    fieldparsers = Any[]
    for (i, (; key, default, tags)) in enumerate(fields)
        one, all = (esc(Expr(:macrocall, GlobalRef(@__MODULE__, Symbol("@xpath")), __source__,
                             something(key, String(fields[i].name)), many))
                    for many in (false, true))
        node = :(if fieldtype(S, $i) <: AbstractVector; $all(node) else $one(node) end)
        parsefield(from) = if isnothing(tags)
            :(parsexml(fieldtype(S, $i), $from))
        else
            :(parsexml(fieldtype(S, $i), $from, $(esc(tags))))
        end
        push!(fieldparsers, if isnothing(default) parsefield(node) else
            :(let found = $node
                  if isnothing(found) || found isa Vector && isempty(found)
                      $(esc(default))
                  else
                      $(parsefield(:found))
                  end
              end)
        end)
    end
    push!(body, RestClient.kwshow(name, fields))
    push!(body, :(function $(esc(:($(@__MODULE__).parsexml)))(::Type{S}, node::$LazyNode) where {S <: $(esc(name))}
                      S isa UnionAll &&
                          throw(ArgumentError("Cannot parse XML as $S without its type parameters (e.g. $S{Int})"))
                      S($(fieldparsers...))
                  end))
    # Declare that this struct is XML formatted
    push!(body, :($RestClient.dataformat(::Type{<:$(esc(name))}) = XMLFormat()))
    # Return the generated code
    Expr(:block, body...)
end

end
