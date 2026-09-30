# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module StreamDecodeTest

using RestClient: @jsondef, AbstractEndpoint, SSEvent, Stream, StreamFormat,
    decode_stream, isstreamend, dataformat, mimetype, framingtype, interpretresponse
import RestClient
using JSON3, StructTypes
using Test
using Downloads: RequestError, Response

@jsondef struct Tok
    n::Int
end

# Endpoints exercising the three termination conventions.
struct PlainEP <: AbstractEndpoint end                  # default: stop at EOF
struct DoneEP <: AbstractEndpoint end                   # ends on a `[DONE]` data sentinel
struct StopEP <: AbstractEndpoint end                   # ends on a terminal event type
RestClient.isstreamend(::DoneEP, fr::SSEvent) = fr.data == codeunits("[DONE]")
RestClient.isstreamend(::StopEP, fr::SSEvent) = fr.event == "stop"

# A completed transfer Task standing in for `Downloads.request`'s outcome.
okxfer() = (t = Threads.@spawn nothing; wait(t); t)
errxfer() = (t = Threads.@spawn RequestError("u", 18, "truncated", Response("http", "u", 18, "", [])); wait(t); t)

# Feed `payload` into a BufferStream, optionally leaving it open (no close) so the
# framer blocks — used by the cancellation tests.
function feed(payload::AbstractString; close_after::Bool = true)
    bs = Base.BufferStream()
    write(bs, collect(codeunits(payload)))
    close_after && close(bs)
    bs
end

function run_decode(ep, framing::Symbol, ::Type{Elt}, payload; transfer = okxfer(),
                    cancelled = Threads.Atomic{Bool}(false), close_after = true) where {Elt}
    bs = feed(payload; close_after)
    fmt = StreamFormat{framing}(dataformat(Tok))
    items = decode_stream(bs, ep, fmt, Elt, transfer, cancelled)
    (; items, bs, cancelled)
end

function run()
@testset "format composition" begin
    @test dataformat(Stream{Tok, :NDJSON}) isa StreamFormat{:NDJSON}
    @test dataformat(Stream{SSEvent{Tok}, :SSE}) isa StreamFormat{:SSE}
    @test mimetype(typeof(dataformat(Stream{Tok, :SSE}))) == "text/event-stream"
    @test mimetype(typeof(dataformat(Stream{Tok, :NDJSON}))) == "application/x-ndjson"
    @test framingtype(Stream{Tok, :NDJSON}) === :NDJSON
    @test eltype(Stream{Tok, :SSE}) === Tok
    @test Base.IteratorSize(Stream{Tok, :SSE}) === Base.SizeUnknown()
end

@testset "drain to completion (NDJSON, bare payload)" begin
    r = run_decode(PlainEP(), :NDJSON, Tok, "{\"n\":1}\n{\"n\":2}\n{\"n\":3}\n")
    @test collect(r.items) == [Tok(n=1), Tok(n=2), Tok(n=3)]
end

@testset "element selection: bare T vs whole frame" begin
    sse = "data: {\"n\":1}\n\ndata: {\"n\":2}\n\n"
    # Stream{Tok} yields the decoded payload
    bare = collect(run_decode(PlainEP(), :SSE, Tok, sse).items)
    @test bare == [Tok(n=1), Tok(n=2)]
    # Stream{SSEvent{Tok}} yields whole frames, destructurable
    whole = collect(run_decode(PlainEP(), :SSE, SSEvent{Tok}, sse).items)
    @test all(f -> f isa SSEvent{Tok}, whole)
    @test [f.data for f in whole] == [Tok(n=1), Tok(n=2)]
end

@testset "termination predicate stops without emitting terminator" begin
    # [DONE] data sentinel
    done = run_decode(DoneEP(), :SSE, Tok, "data: {\"n\":1}\n\ndata: [DONE]\n\ndata: {\"n\":9}\n\n")
    @test collect(done.items) == [Tok(n=1)]
    # terminal `event: stop`
    stop = run_decode(StopEP(), :SSE, Tok, "data: {\"n\":1}\n\nevent: stop\ndata: x\n\n")
    @test collect(stop.items) == [Tok(n=1)]
end

@testset "truncation vs clean EOF" begin
    # Clean transfer → iterating completes normally
    clean = run_decode(PlainEP(), :NDJSON, Tok, "{\"n\":1}\n")
    @test collect(clean.items) == [Tok(n=1)]
    # Errored transfer → consumer's loop throws after the buffered items
    trunc = run_decode(PlainEP(), :NDJSON, Tok, "{\"n\":1}\n"; transfer = errxfer())
    got = Tok[]
    @test_throws RequestError for t in trunc.items; push!(got, t); end
    @test got == [Tok(n=1)]
end

@testset "decode error surfaces to consumer" begin
    bad = run_decode(PlainEP(), :NDJSON, Tok, "{\"n\":1}\nnot json\n")
    got = Tok[]
    @test_throws Exception for t in bad.items; push!(got, t); end
    @test got == [Tok(n=1)]
end

@testset "cancellation (framer blocked reading) — clean exit, no throw" begin
    # Items buffered, stream left open so the framer parks in readframe; then cancel
    # with the transfer reporting a RequestError (as a real cancel collapse would).
    cancelled = Threads.Atomic{Bool}(false)
    r = run_decode(PlainEP(), :NDJSON, Tok, "{\"n\":1}\n{\"n\":2}\n";
                   transfer = errxfer(), cancelled = cancelled, close_after = false)
    sleep(0.05)                              # let the framer buffer + park
    cancelled[] = true; close(r.items); close(r.bs)   # mirrors close(::Stream)
    # Draining must NOT throw the truncation error — it was a deliberate cancel.
    got = Tok[]
    for t in r.items; push!(got, t); end
    @test got == [Tok(n=1), Tok(n=2)]
end

@testset "cancellation (framer parked in put!) — clean exit, no throw" begin
    # buffer=1 with no consumer: the framer decodes one item, parks in put! on the
    # full channel, then we cancel with the transfer reporting a RequestError.
    cancelled = Threads.Atomic{Bool}(false)
    bs = Base.BufferStream()
    write(bs, collect(codeunits("{\"n\":1}\n{\"n\":2}\n{\"n\":3}\n")))
    fmt = StreamFormat{:NDJSON}( dataformat(Tok))
    items = decode_stream(bs, PlainEP(), fmt, Tok, errxfer(), cancelled; buffer = 1)
    sleep(0.05)                              # framer fills the 1-slot channel, parks in put!
    cancelled[] = true; close(items); close(bs)
    # Draining must not throw: the cancel routes through the catch-guard, not close(items, err).
    got = Tok[]
    for x in items; push!(got, x); end
    @test !isempty(got)                      # at least the buffered item delivered, no throw
end
end  # run

end  # module
