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

# Open a GET stream on `url` directly, below the request pipeline.
open_get(url; timeout = Inf) =
    open_stream(RestClient.Request{:get}(RestClient.RequestConfig(url), SEP()),
                RestClient.Outgoing("GET", url, Pair{String, String}[], nothing, nothing, timeout))

# Drive open_stream → decode_stream against a live route, returning the channel.
function stream_route(url, framing::Symbol, ::Type{Elt}; ep = SEP()) where {Elt}
    (; response, body, transfer) = open_get(url)
    @assert response.status == 200 "status $(response.status)"
    fmt = StreamFormat{framing}(dataformat(Tok))
    cancelled = Threads.Atomic{Bool}(false)
    items = decode_stream(body, ep, fmt, Elt, transfer, cancelled)
    (; items, body, cancelled, response)
end

# Closing the body mid-transfer collapses the transfer task (the cancel path); a
# test that only cares about the head shouldn't re-raise that expected collapse.
quietclose(body, transfer) = (close(body); try wait(transfer) catch end)

function run(with_stream_server, slow_interval)
@testset "status rendezvous" begin
    with_stream_server() do base
        # normal stream: head resolved before frames drain
        (; response, body, transfer) = open_get("$base/ndjson")
        @test response.status == 200
        quietclose(body, transfer)
        # empty-200: rendezvous must still reach take! without hanging
        empty = open_get("$base/empty")
        @test empty.response.status == 200
        quietclose(empty.body, empty.transfer)
    end
end

# The transfer can't finish (and free its curl handle) while the head is read,
# because its first body write is held until then.
@testset "head gate" begin
    body = Base.BufferStream()
    gate = RestClient.HeadGate(body)
    writer = Threads.@spawn (write(gate, "first"); write(gate, "second"))
    @test take!(gate.signal) === :body
    sleep(0.1)
    @test !istaskdone(writer)                   # held at the first write
    @test bytesavailable(body) == 0
    notify(gate.released)
    wait(writer)
    close(body)
    @test read(body, String) == "firstsecond"   # then everything passes through
end

# Short bodies finish soonest, so many in turn exercise reading the head of a
# finishing transfer (which once yielded garbage statuses, or crashed).
@testset "head of a finishing transfer" begin
    with_stream_server() do base
        statuses = map(1:500) do _
            (; response, body, transfer) = open_get("$base/ndjson")
            quietclose(body, transfer)
            response.status
        end
        @test all(==(200), statuses)
    end
end

@testset "transport failure" begin
    # nothing is listening on this port → RequestError, not a bogus Response
    @test_throws RequestError open_get("http://127.0.0.1:1/x"; timeout = 2.0)
end

@testset "incremental delivery" begin
    with_stream_server() do base
        # Measure at the raw body so timing reflects the wire, not channel buffering:
        # /slow sleeps SLOW_INTERVAL between frames, so arrivals must be spread out.
        (; body, transfer) = open_get("$base/slow")
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
