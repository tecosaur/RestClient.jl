# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# Exercise the JSON.jl backend of `@jsondef` (ext/JSONExt.jl); the integration
# suite covers only JSON3. The backend follows the calling module's `using`s, so
# everything here lives in a JSON-only module to force `:json`.

using RestClient
using JSON

module JSONClient
    using RestClient, JSON
    @jsondef struct Item
        name::String
        id::Int
    end
    @jsondef struct Tagged
        # Remapped wire key, plus an optional field defaulting to nothing.
        kind."item_kind"::String
        note."item_note"::Union{String, Nothing} = nothing
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
    @jsondef Dict struct Scores
        values::Dict{String, Int}
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
    @test RestClient.dataformat(JC.Item) == RestClient.JSONFormat{:json}()
    @test RestClient.mimetype(RestClient.JSONFormat{:json}) == "application/json"
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
end

@testset "write" begin
    @test jsonwrite(JC.Item(name = "y", id = 4)) == """{"name":"y","id":4}"""
    # The remapped key appears on the wire; a nothing-valued field is null.
    @test jsonwrite(JC.Tagged(kind = "k")) == """{"item_kind":"k","item_note":null}"""
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
end

# A fielded struct declared `:Dict` is not a wrapper, so the macro declares only
# `dataformat` and the user supplies the backend integration themselves.
module JSONCaseB
    using RestClient, JSON
    import StructUtils
    @jsondef Dict struct Pair2
        a::Int
        b::Int
    end
    StructUtils.dictlike(::Type{Pair2}) = false
    StructUtils.structlike(::Type{Pair2}) = true
end

@testset "non-wrapper containers" begin
    @test RestClient.dataformat(JSONCaseB.Pair2) == RestClient.JSONFormat{:json}()
    p = RestClient.interpretresponse(codeunits("""{"a":1,"b":2}"""),
                                     RestClient.dataformat(JSONCaseB.Pair2), JSONCaseB.Pair2)
    @test p == JSONCaseB.Pair2(a = 1, b = 2)
end

@testset "errors" begin
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
