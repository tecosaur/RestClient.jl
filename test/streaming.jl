# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using RestClient: SSEvent, NDJSONFrame, StreamFormat, RawFormat, readframe,
    framestate, framedata, remake, elementpayload

# A bytes-element StreamFormat for the given framing symbol (framing in isolation).
sfmt(framing::Symbol) = StreamFormat{framing}(RawFormat())

# Feed `chunks` into a BufferStream from a task, closing at the end — the producer
# side of `readframe`. `bytesize` splits the whole payload into fixed-size writes.
function fed_stream(payload::AbstractString; bytesize::Int = 0)
    bs = Base.BufferStream()
    bytes = collect(codeunits(payload))
    Threads.@spawn begin
        if bytesize <= 0
            write(bs, bytes)
        else
            for i in 1:bytesize:length(bytes)
                write(bs, bytes[i:min(i + bytesize - 1, end)])
                flush(bs)
            end
        end
        close(bs)
    end
    bs
end

# Drain every frame `readframe` yields, returning the public frames.
function collectframes(framing::Symbol, payload::AbstractString; bytesize::Int = 0)
    fmt = sfmt(framing)
    bs = fed_stream(payload; bytesize)
    state = framestate(fmt)
    frames = []
    while (fr = readframe(bs, state, fmt)) !== nothing
        push!(frames, fr)
    end
    frames
end

@testset "framing types" begin
    @test framedata(NDJSONFrame([1,2])) == [1,2]
    @test framedata(SSEvent("e", [1], "7")) == [1]
    @test remake(SSEvent("e", UInt8[1], "7"), "x") == SSEvent("e", "x", "7")
    @test remake(NDJSONFrame(UInt8[1]), "x") == NDJSONFrame("x")
    # framedata/remake locate the payload by its type-parameter field, so remake
    # re-types it and disambiguates even when a metadata field shares the type.
    @test remake(SSEvent("e", UInt8[1], "7"), 42) isa SSEvent{Int}
    let ev = SSEvent{Union{String,Nothing}}("e", "body", "7")   # all three fields same type
        @test framedata(ev) == "body"
        @test remake(ev, 9) == SSEvent("e", 9, "7")
    end
    @test elementpayload(SSEvent{Int}) === Int
    @test elementpayload(NDJSONFrame{Int}) === Int
    @test elementpayload(Float64) === Float64
    # SSEvent destructures (event, data[, id]) and exposes fields
    let ev = SSEvent("msg", [0x01], "42")
        e, d = ev
        @test (e, d) == ("msg", [0x01])
        e2, d2, i2 = ev
        @test (e2, d2, i2) == ("msg", [0x01], "42")
        @test ev.event == "msg" && ev.data == [0x01] && ev.id == "42"
    end
end

@testset "NDJSON framing" begin
    payload = "{\"a\":1}\n{\"a\":2}\n{\"a\":3}\n"
    expected = [b"{\"a\":1}", b"{\"a\":2}", b"{\"a\":3}"]
    # Byte-identical output across every chunk size from 1 up (split-across-reads).
    for bytesize in (0, 1, 2, 3, 5, 8, 100)
        frames = collectframes(:NDJSON, payload; bytesize)
        @test framedata.(frames) == expected
    end
    # Trailing line with no final newline is still emitted.
    @test framedata.(collectframes(:NDJSON, "{\"a\":1}\n{\"a\":2}")) ==
        [b"{\"a\":1}", b"{\"a\":2}"]
end

@testset "SSE framing — fields" begin
    payload = "event: greeting\ndata: hello\nid: 1\n\ndata: world\n\n"
    for bytesize in (0, 1, 2, 7, 100)
        frames = collectframes(:SSE, payload; bytesize)
        @test length(frames) == 2
        @test frames[1].event == "greeting"
        @test frames[1].data == b"hello"
        @test frames[1].id == "1"
        # id is sticky: second event keeps id "1" though it sets none
        @test frames[2].event === nothing
        @test frames[2].data == b"world"
        @test frames[2].id == "1"
    end
end

@testset "SSE framing — multi-line data joined with \\n" begin
    frames = collectframes(:SSE, "data: line one\ndata: line two\n\n")
    @test length(frames) == 1
    @test frames[1].data == b"line one\nline two"
end

@testset "SSE framing — leading space stripped once" begin
    # one leading space removed; a second space is preserved
    frames = collectframes(:SSE, "data:  two-spaces\n\n")
    @test frames[1].data == b" two-spaces"
    frames = collectframes(:SSE, "data:nospace\n\n")
    @test frames[1].data == b"nospace"
end

@testset "SSE framing — comments and retry" begin
    # `:`-comment heartbeats skipped; `retry:` parsed and discarded (not data)
    frames = collectframes(:SSE, ": keep-alive\ndata: x\nretry: 5000\n\n")
    @test length(frames) == 1
    @test frames[1].data == b"x"
end

@testset "SSE framing — event resets, id persists" begin
    payload = "event: a\nid: 10\ndata: 1\n\ndata: 2\n\nevent: b\ndata: 3\n\n"
    frames = collectframes(:SSE, payload)
    @test [f.event for f in frames] == ["a", nothing, "b"]
    @test [f.id for f in frames] == ["10", "10", "10"]
    @test [f.data for f in frames] == [b"1", b"2", b"3"]
end

@testset "SSE framing — trailing event without final blank line" begin
    frames = collectframes(:SSE, "data: a\n\ndata: b\n")
    @test [f.data for f in frames] == [b"a", b"b"]
end
