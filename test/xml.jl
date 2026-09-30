# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# Exercise the XML extension (ext/XMLExt.jl): the `@xmldef` macro, its xpath
# subset, and the `parsexml` dispatch. Parsing is pure (body → typed struct), so
# these feed `interpretresponse` an `IOBuffer` fixture and need no network.

using RestClient
using XML

xmlparse(::Type{T}, body::AbstractString) where {T} =
    RestClient.interpretresponse(codeunits(body), RestClient.dataformat(T), T)

# Endpoints/types declared as a user would, in their own module so the xpath
# field syntax and generated `parsexml` methods are realistic.
module XMLClient
    using RestClient, XML
    @xmldef struct Book
        title."title/text()"::String
        year."@year"::Int
        ratio."@ratio"::Float64
        isbn."isbn/text()"::Union{String, Nothing} = nothing
    end
    @xmldef struct Chapter
        name."name/text()"::String
    end
    @xmldef struct Volume
        chapters."chapter"::Vector{Chapter}
        first_chapter."chapter[1]/name/text()"::String
        last_chapter."chapter[last()]/name/text()"::String
    end
    @xmldef struct Maybe
        present."@a"::Union{String, Nothing} = nothing
        absent."@b"::Union{String, Nothing} = nothing
        count."@n"::Union{Int, Nothing} = nothing
        # optional scalar via a child-node path: exercises node-unwrap, not the String path
        size."size"::Union{Int, Nothing} = nothing
    end
    @xmldef struct NothingDefaults
        maybe."@m"::Union{Int, Nothing}     # defaults to nothing
        flags."flag"::Vector{Nothing}       # doesn't: Nothing is only a parameter
    end
    @xmldef struct Untyped
        text."@t"             # untyped field holding an attribute string
        node."child"          # untyped field holding child node(s)
    end
end

const XC = XMLClient

@testset "format" begin
    @test RestClient.dataformat(XC.Book) == RestClient.XMLFormat()
    @test RestClient.mimetype(RestClient.XMLFormat) == "application/xml"
end

@testset "scalars" begin
    book = xmlparse(XC.Book, """
        <book year="1937" ratio="0.5">
          <title>The Hobbit</title>
          <isbn>978-0</isbn>
        </book>""")
    @test book isa XC.Book
    @test book.title == "The Hobbit"          # text()
    @test book.year == 1937                   # @attr parsed to Int
    @test book.ratio == 0.5                   # @attr parsed to Float64
    @test book.isbn == "978-0"                # optional present
end

@testset "`nothing` defaults" begin
    @test XC.NothingDefaults(flags = Nothing[]).maybe === nothing
    @test_throws UndefKeywordError XC.NothingDefaults()
end

@testset "optional" begin
    @testset "absent" begin
        m = xmlparse(XC.Maybe, "<m/>")
        @test m.present === nothing
        @test m.absent === nothing
        @test m.count === nothing
        @test m.size === nothing
    end
    # Regression: a present optional terminal value once hit a parsexml
    # ambiguity (Union{T,Nothing} vs String).
    @testset "present" begin
        m = xmlparse(XC.Maybe, """<m a="hi" n="7"><size>42</size></m>""")
        @test m.present == "hi"
        @test m.count == 7
        @test m.size == 42          # optional scalar via a child-node path
        @test m.absent === nothing
    end
end

@testset "untyped" begin
    # Regression: an untyped field holding a string once hit a parsexml
    # ambiguity ((Any, value) vs (T, ::String)).
    u = xmlparse(XC.Untyped, """<u t="a &amp; b"><child>x</child></u>""")
    @test u.text == "a & b"                 # string value, unescaped
    @test u.node isa Vector                 # node value passes through untouched
end

@testset "vectors" begin
    vol = xmlparse(XC.Volume, """
        <volume>
          <chapter><name>Alpha</name></chapter>
          <chapter><name>Beta</name></chapter>
          <chapter><name>Gamma</name></chapter>
        </volume>""")
    @test length(vol.chapters) == 3
    @test vol.chapters[2].name == "Beta"      # nested @xmldef in a Vector
    @test vol.first_chapter == "Alpha"        # [1] index form
    @test vol.last_chapter == "Gamma"         # [last()] index form
    @testset "single element" begin
        one = xmlparse(XC.Volume, "<volume><chapter><name>Solo</name></chapter></volume>")
        @test length(one.chapters) == 1
    end
end

@testset "missing required" begin
    # `title` has no default and no matching node ⇒ ArgumentError from parsexml.
    @test_throws ArgumentError xmlparse(XC.Book, """<book year="1" ratio="0"/>""")
end

@testset "document wrapper" begin
    # `read(io, LazyNode)` yields a nil-tag document node; interpretresponse must
    # descend to the real root element before parsing.
    book = xmlparse(XC.Book, """<?xml version="1.0"?>
        <book year="2001" ratio="1.0"><title>Odyssey</title></book>""")
    @test book.title == "Odyssey"
    @test book.year == 2001
end

@testset "xpath errors" begin
    # The xpath subset rejects malformed specs at macro-expansion time.
    @test_throws Exception @macroexpand @xmldef struct Bad1
        x."/leading/slash"::String   # ungrounded selector
    end
    @test_throws Exception @macroexpand @xmldef struct Bad2
        x."a/text()/more"::String    # component after a terminal text()
    end
end
