# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# Exercise `@jsondef` (ext/JSONExt.jl) directly; integration.jl covers it end to end.

using RestClient
using JSON

module JSONClient
    using RestClient, JSON, Dates
    @jsondef struct Mixed
        pair::@NamedTuple{a::String, b::Vector{Int}}
        counts::Dict{String, Vector{Int}} = Dict{String, Vector{Int}}()
        id::Union{Int, String}
        score::Float64
        when::Union{Date, Nothing} = nothing
    end
    @jsondef struct Item
        name::String
        id::Int
    end
    @jsondef struct Tagged
        # Remapped wire key, plus an optional field defaulting to nothing.
        kind."item_kind"::String
        note."item_note"::Union{String, Nothing} = nothing
    end
    @jsondef struct Patch
        name::Union{String, Nothing} = nothing
        cleared::Union{Nothing, JSON.Null} = nothing
    end
    @jsondef :noshow struct Quiet
        value::Int
    end
    @jsondef Dict struct ItemMap
        entries::Dict{String, Item}
    end
    @jsondef Vector struct ItemList
        items::Vector{Item}
    end
    @jsondef Vector struct TaggedList
        items::Vector{Tagged}
    end
    @jsondef Dict struct TaggedMap
        entries::Dict{String, Tagged}
    end
    @jsondef Dict struct Scores
        values::Dict{String, Int}
    end
    @jsondef struct Catalogue
        byname::ItemMap
        listed::ItemList
    end
    @jsondef struct Shelf
        label::String
        top::Item
        items::Vector{Item}
        spare::Union{Tagged, Nothing} = nothing
    end
    @jsondef struct NothingDefaults
        maybe::Union{Int, Nothing}          # defaults to nothing
        flags::Dict{String, Nothing}        # doesn't: Nothing is only a parameter
    end
end

const JC = JSONClient
jsonparse(::Type{T}, body::AbstractString) where {T} =
    RestClient.interpretresponse(codeunits(body), RestClient.dataformat(T), T)
jsonwrite(x) = sprint(io -> RestClient.writepayload(io, RestClient.dataformat(typeof(x)), x))

@testset "backend" begin
    @test RestClient.dataformat(JC.Item) == RestClient.JSONFormat()
    @test RestClient.mimetype(RestClient.JSONFormat(), JC.Item) == "application/json"
end

@testset "parse" begin
    it = jsonparse(JC.Item, """{"name":"widget","id":7}""")
    @test it isa JC.Item
    @test it.name == "widget"
    @test it.id == 7
    @testset "remapped and optional" begin
        t = jsonparse(JC.Tagged, """{"item_kind":"book","item_note":"hi"}""")
        @test t.kind == "book"          # read from "item_kind"
        @test t.note == "hi"
    end
    @testset "optional absent" begin
        t = jsonparse(JC.Tagged, """{"item_kind":"pen"}""")
        @test t.kind == "pen"
        @test t.note === nothing
    end
    @testset "nested structs" begin
        json = """{"label":"a","top":{"name":"x","id":1},"items":[{"name":"y","id":2}],"spare":{"item_kind":"k"}}"""
        shelf = jsonparse(JC.Shelf, json)
        @test shelf.top == JC.Item(name = "x", id = 1)
        @test shelf.items == [JC.Item(name = "y", id = 2)]
        @test shelf.spare == JC.Tagged(kind = "k")
        @test jsonparse(JC.Shelf, """{"label":"b","top":{"name":"x","id":1},"items":[]}""").spare === nothing
        @test jsonwrite(jsonparse(JC.Shelf, jsonwrite(shelf))) == jsonwrite(shelf)
        @test (@inferred JSON.parse(codeunits(json), JC.Shelf)) isa JC.Shelf
    end
    @testset "field values" begin
        m = jsonparse(JC.Mixed, """{"pair":{"a":"x","b":[1,2]},"counts":{"k":[3]},"id":"s1","score":2,"when":"2024-02-29"}""")
        @test m.pair == (a = "x", b = [1, 2])
        @test m.counts == Dict("k" => [3])
        @test m.id == "s1"
        @test m.score === 2.0
        @test m.when == JC.Date(2024, 2, 29)
        m2 = jsonparse(JC.Mixed, """{"pair":{"b":[],"a":"y"},"id":5,"score":1.5,"when":null}""")
        @test m2.id === 5 && m2.when === nothing && isempty(m2.counts)
    end
    @testset "unknown and missing keys" begin
        @test jsonparse(JC.Item, """{"name":"w","extra":{"deep":[1]},"id":3}""") == JC.Item(name = "w", id = 3)
        @test_throws ArgumentError JSON.parse(codeunits("""{"name":"w","id":3,"extra":1}"""), JC.Item; unknown_fields = :error)
        @test_throws "required key \"id\"" jsonparse(JC.Item, """{"name":"w"}""")
        @test_throws ArgumentError jsonparse(JC.Mixed, """{"pair":{"b":[]},"id":1,"score":1}""")
    end
end

@testset "write" begin
    @test jsonwrite(JC.Item(name = "y", id = 4)) == """{"name":"y","id":4}"""
    # The remapped key appears on the wire; a nothing-valued field is left out
    @test jsonwrite(JC.Tagged(kind = "k")) == """{"item_kind":"k"}"""
    @test jsonwrite(JC.Patch()) == "{}"
    @test jsonwrite(JC.Patch(name = "x")) == """{"name":"x"}"""
    @test jsonwrite(JC.Patch(cleared = JSON.Null())) == """{"cleared":null}"""   # an explicit null
    @test jsonparse(JC.Tagged, """{"item_kind":"k","item_note":null}""").note === nothing
    @testset "round-trip" begin
        orig = JC.Item(name = "round", id = 99)
        back = jsonparse(JC.Item, jsonwrite(orig))
        @test back.name == orig.name && back.id == orig.id
    end
end

@testset "`nothing` defaults" begin
    @test JC.NothingDefaults(flags = Dict{String, Nothing}()).maybe === nothing
    @test_throws UndefKeywordError JC.NothingDefaults()
end

@testset "show" begin
    @test sprint(show, JC.Tagged(kind = "k")) == "$(JC.Tagged)(kind=\"k\")"
    @test occursin("note=\"n\"", sprint(show, JC.Tagged(kind = "k", note = "n")))
    # :noshow opts out of the custom show, leaving the default tuple form.
    @test sprint(show, JC.Quiet(value = 1)) == "$(JC.Quiet)(1)"
end

@testset "containers" begin
    @testset "Dict" begin
        m = jsonparse(JC.ItemMap, """{"a":{"name":"x","id":1},"b":{"name":"y","id":2}}""")
        @test m isa JC.ItemMap
        @test length(m.entries) == 2
        @test m.entries["a"] == JC.Item(name = "x", id = 1)   # values parsed recursively
        @test jsonparse(JC.ItemMap, jsonwrite(m)).entries == m.entries
    end
    @testset "renamed keys within" begin
        tagged = JC.Tagged(kind = "a", note = "b")
        @test jsonparse(JC.TaggedList, """[{"item_kind":"a","item_note":"b"}]""").items == [tagged]
        @test jsonparse(JC.TaggedMap, """{"x":{"item_kind":"a","item_note":"b"}}""").entries == Dict("x" => tagged)
    end
    @testset "scalar Dict" begin
        s = jsonparse(JC.Scores, """{"alice":10,"bob":20}""")
        @test s.values == Dict("alice" => 10, "bob" => 20)
        @test jsonwrite(JC.Scores(values = Dict("x" => 1))) == """{"x":1}"""
    end
    @testset "Vector" begin
        l = jsonparse(JC.ItemList, """[{"name":"x","id":1},{"name":"y","id":2}]""")
        @test l isa JC.ItemList
        @test l.items == [JC.Item(name = "x", id = 1), JC.Item(name = "y", id = 2)]
        @test jsonparse(JC.ItemList, jsonwrite(l)).items == l.items
    end
    @testset "plain Dict" begin
        T = Dict{Symbol, JC.Item}
        @test RestClient.dataformat(T) == RestClient.JSONFormat()     # any key type
        @test jsonparse(T, """{"a":{"name":"x","id":1}}""") == Dict(:a => JC.Item(name = "x", id = 1))
    end
    @testset "nested" begin
        json = """{"byname":{"a":{"name":"x","id":1}},"listed":[{"name":"y","id":2}]}"""
        c = jsonparse(JC.Catalogue, json)
        @test c.byname.entries["a"] == JC.Item(name = "x", id = 1)
        @test c.listed.items == [JC.Item(name = "y", id = 2)]
        @test jsonwrite(c) == json
    end
end

# A fielded struct declared `:Dict` is not a wrapper, so the macro declares only
# `dataformat` and the user supplies the backend integration themselves.
module JSONCaseB
    using RestClient, JSON
    using JSON: StructUtils
    @jsondef Dict struct Pair2
        a::Int
        b::Int
    end
    StructUtils.dictlike(::Type{Pair2}) = false
    StructUtils.structlike(::Type{Pair2}) = true
end

@testset "non-wrapper containers" begin
    @test RestClient.dataformat(JSONCaseB.Pair2) == RestClient.JSONFormat()
    p = RestClient.interpretresponse(codeunits("""{"a":1,"b":2}"""),
                                     RestClient.dataformat(JSONCaseB.Pair2), JSONCaseB.Pair2)
    @test p == JSONCaseB.Pair2(a = 1, b = 2)
end

@testset "errors" begin
    @test_throws MethodError RestClient.interpretresponse(IOBuffer("{}"), RestClient.JSONFormat(), JC.Item)
    @test_throws "Malformed field" macroexpand(JSONClient, :(@jsondef struct J; x.y::Int; end))
    # Number/Bool need user-defined lift/lower under JSON.jl, so the macro
    # refuses them with guidance rather than emitting broken code.
    @test_throws Exception @eval module BadNumber
        using RestClient, JSON
        @jsondef Number struct N; x::Int end
    end
    @test_throws Exception @eval module BadKind
        using RestClient, JSON
        @jsondef Bogus struct B; x::Int end
    end
end
