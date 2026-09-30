# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module StreamTransportTest

using RestClient: @jsondef, open_stream, decode_stream, StreamFormat, dataformat,
    AbstractEndpoint, SSEvent, Stream
import RestClient
using JSON3, StructTypes
using Test
using Downloads: RequestError

@jsondef struct Tok
    n::Int
end

struct SEP <: AbstractEndpoint end

# Drive open_stream → decode_stream against a live route, returning the channel.
function stream_route(url, framing::Symbol, ::Type{Elt}; ep = SEP()) where {Elt}
    res, body, transfer = open_stream("GET", url, nothing; headers = Pair{String,String}[], timeout = Inf)
    @assert res.status == 200 "status $(res.status)"
    fmt = StreamFormat{framing}(dataformat(Tok))
    cancelled = Threads.Atomic{Bool}(false)
    items = decode_stream(body, ep, fmt, Elt, transfer, cancelled)
    (; items, body, cancelled, res)
end

# Closing the body mid-transfer collapses the transfer task (the cancel path); a
# test that only cares about the head shouldn't re-raise that expected collapse.
quietclose(body, transfer) = (close(body); try wait(transfer) catch end)

function run(with_stream_server, slow_interval)
@testset "status rendezvous" begin
    with_stream_server() do base
        # normal stream: head resolved before frames drain
        res, body, transfer = open_stream("GET", "$base/ndjson", nothing;
                                          headers = Pair{String,String}[], timeout = Inf)
        @test res.status == 200
        quietclose(body, transfer)
        # empty-200: rendezvous must still reach take! without hanging
        res2, body2, transfer2 = open_stream("GET", "$base/empty", nothing;
                                             headers = Pair{String,String}[], timeout = Inf)
        @test res2.status == 200
        quietclose(body2, transfer2)
    end
end

@testset "transport failure" begin
    # nothing is listening on this port → RequestError, not a bogus Response
    @test_throws RequestError open_stream("GET", "http://127.0.0.1:1/x", nothing;
                                          headers = Pair{String,String}[], timeout = 2.0)
end

@testset "incremental delivery" begin
    with_stream_server() do base
        # Measure at the raw body so timing reflects the wire, not channel buffering:
        # /slow sleeps SLOW_INTERVAL between frames, so arrivals must be spread out.
        res, body, transfer = open_stream("GET", "$base/slow", nothing;
                                          headers = Pair{String,String}[], timeout = Inf)
        t0 = time()
        offsets = Float64[]
        while !eof(body)
            line = readline(body; keep = false)
            isempty(line) && continue
            push!(offsets, time() - t0)
        end
        wait(transfer)
        @test length(offsets) == 3
        @test last(offsets) - first(offsets) > slow_interval
    end
end

@testset "decode" begin
    with_stream_server() do base
        @test collect(stream_route("$base/ndjson", :NDJSON, Tok).items) ==
            [Tok(n=1), Tok(n=2), Tok(n=3)]
        @test collect(stream_route("$base/sse", :SSE, Tok).items) ==
            [Tok(n=1), Tok(n=2), Tok(n=3)]
        # whole-frame SSE: metadata preserved
        evs = collect(stream_route("$base/sse", :SSE, SSEvent{Tok}).items)
        @test evs[1].event == "tick"
        @test evs[1].id == "a"
        @test [e.data for e in evs] == [Tok(n=1), Tok(n=2), Tok(n=3)]
    end
end

@testset "truncation" begin
    with_stream_server() do base
        # /abort writes 2 frames then drops the connection mid-stream
        r = stream_route("$base/abort", :NDJSON, Tok)
        got = Tok[]
        @test_throws RequestError for tok in r.items; push!(got, tok); end
        @test got == [Tok(n=1), Tok(n=2)]
    end
end

@testset "cancellation" begin
    with_stream_server() do base
        r = stream_route("$base/slow", :NDJSON, Tok)
        first(r.items)                          # take one frame
        r.cancelled[] = true; close(r.items); close(r.body)
        # draining the remainder ends cleanly (no throw); tasks unwind
        for _ in r.items; end
        @test true
    end
end
end  # run

end  # module
