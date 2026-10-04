# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# Incrementally-consumed HTTP responses: a lazy iterator of parsed elements over a
# framed body. Framing (bytes → message units) and interpretation (message → T) are
# orthogonal; they meet only at the point a frame's payload bytes are decoded.
#
# A framing is a `Symbol` tag carried in a stream's type (`Stream{T, :ndjson}`) and
# in its `StreamFormat{framing}`. The generic framework below knows nothing of any
# particular framing; the shipped `:SSE` and `:NDJSON` implementations are at the
# foot of the file, and a new framing is added the same way from outside.

const STREAM_BUFFER = 256


# Generic framework

"""
    StreamFormat{F, E<:AbstractFormat} <: AbstractFormat

The format of a streamed body: framing `F` (a symbol, e.g. `:SSE`/`:NDJSON`) over
an element format `E`. [`readframe`](@ref)/[`framestate`](@ref)/[`mimetype`](@ref)
dispatch on it by `F`.
"""
struct StreamFormat{F, E<:AbstractFormat} <: AbstractFormat
    element::E
end

StreamFormat{F}(element::E) where {F, E} = StreamFormat{F, E}(element)

# Foldable, so `framedata` and `remake` compile to plain field access
Base.@assume_effects :foldable function payloadslot(F::Type)
    wrapper = F.name.wrapper
    slot = if wrapper isa UnionAll
        findfirst(==(wrapper.var), fieldtypes(Base.unwrap_unionall(wrapper)))
    end
    isnothing(slot) &&
        throw(ArgumentError("no payload field in $F; define `framedata` and `remake` for it"))
    slot
end

"""
    framedata(frame) -> payload

Extract a frame's payload: the field whose type is the frame's type parameter. A
framing whose payload is located differently can override this.
"""
framedata(frame) = getfield(frame, payloadslot(typeof(frame)))

"""
    remake(frame, data, ::Type{P} = typeof(data)) -> frame

Return a copy of `frame` with its payload replaced by `data`, as a frame of payload
type `P`, preserving the other fields. This is the decode step: `{bytes} → {P}`. The
payload field is located as in [`framedata`](@ref).
"""
function remake(frame::F, data, ::Type{P}) where {F, P}
    slot = payloadslot(F)
    F.name.wrapper{P}(ntuple(i -> if i == slot data else getfield(frame, i) end, Val(fieldcount(F)))...)
end

remake(frame, data) = remake(frame, data, typeof(data))

"""
    elementpayload(::Type{Elt}) -> Type

The decode target for a stream element type. Defaults to `Elt`; a framing whose
element type wraps its payload (e.g. `SSEvent{T}` ⇒ `T`) overrides it.
"""
elementpayload(::Type{T}) where {T} = T

"""
    framestate(fmt::StreamFormat)

The per-stream state [`readframe`](@ref) threads across calls: `nothing` by
default, overridden by a framing that carries state across frames.
"""
framestate(::StreamFormat) = nothing

"""
    readframe(bs::IO, state, fmt::StreamFormat) -> frame | nothing

Read one complete frame from `bs`, with its payload still **undecoded** bytes, or
return `nothing` at a clean EOF. `state` is
[`framestate`](@ref)'s value, reused across calls. Dispatched on `fmt`'s framing.

The read blocks on `bs`, reassembling a frame split across network reads. As `bs` is
written concurrently, read it with calls that lock against the writer (`readuntil`,
`read`, `readavailable`), not `copyuntil` or `readline` (which uses it).
"""
function readframe(::IO, _, ::StreamFormat) end

# A stream's MIME is its framing's wire type, falling back to the element format's.
# A framing with a wire MIME adds a `mimetype(::Type{<:StreamFormat{:framing}})`.
mimetype(::Type{<:StreamFormat{F, E}}) where {F, E} = mimetype(E)


# Disposition

"""
    StreamEndpoint <: AbstractEndpoint

Supertype for endpoints returning a [`Stream`](@ref). The `@endpoint` macro selects
it for any `Stream`-typed output; [`perform`](@ref) dispatches on it to the
streaming request path.
"""
abstract type StreamEndpoint <: AbstractEndpoint end

"""
    Stream{T, F}

A lazily consumed stream of elements of type `T`, framed by `F` (`:SSE`, the
default, or `:NDJSON`). Like [`Single`](@ref) and [`List`](@ref), it is chosen
through an endpoint's output type.

Iterating it blocks until the next element has arrived and been decoded. Any task
may `close` it, which ends iteration without an error (see the manual on
cancellation), and a stream that is dropped before it is drained is reclaimed by
its finalizer.

See also: [`isstreamend`](@ref).
"""
mutable struct Stream{T, F}
    const items::Channel{T}
    const response::Downloads.Response
    const body::Base.BufferStream
    const cancelled::Threads.Atomic{Bool}
    function Stream{T, F}(items, response, body, cancelled) where {T, F}
        finalizer(closefinalized, new{T, F}(items, response, body, cancelled))
    end
end

Base.IteratorSize(::Type{<:Stream}) = Base.SizeUnknown()
Base.eltype(::Type{<:Stream{T}}) where {T} = T
Base.iterate(s::Stream, st...) = iterate(s.items, st...)

framingtype(::Type{<:Stream{T, F}}) where {T, F} = F

# Flag the cancel before closing items, so the framer's EOF reconciliation can tell
# a deliberate cancel from a server truncation (both collapse the transfer alike).
Base.close(s::Stream) = (s.cancelled[] = true; close(s.items); close(s.body))

# A finalizer must not wait on a lock, so close inline when both locks are free,
# and only otherwise from a task (always spawning can crash the JIT at exit).
function closefinalized(s::Stream)
    if trylock(s.items)
        try
            if trylock(s.body.lock)
                try close(s) finally unlock(s.body.lock) end
                return
            end
        finally
            unlock(s.items)
        end
    end
    Threads.@spawn close(s)
    nothing
end

function dataformat(S::Type{<:Stream})
    fmt = dataformat(elementpayload(eltype(S)))
    StreamFormat{framingtype(S)}(if fmt isa UnspecifiedFormat RawFormat() else fmt end)
end

"""
    isstreamend(endpoint::AbstractEndpoint, frame) -> Bool

Whether `frame` ends the stream from `endpoint`. It sees each frame before it is
decoded, so the terminating frame never reaches the consumer. The default is
`false`, ending a stream only when the connection closes; an API that marks its
end in band needs a method checking the frame's fields:

```julia
RestClient.isstreamend(::ChatEndpoint, fr::SSEvent)     = fr.data == codeunits("[DONE]")  # a data sentinel
RestClient.isstreamend(::MessagesEndpoint, fr::SSEvent) = fr.event == "message_stop"      # a terminal event
```
"""
isstreamend(@nospecialize(::AbstractEndpoint), @nospecialize(frame)) = false

# Decode `req`'s streamed body into a channel of `Elt` (the decoded payload, or a whole
# frame like `SSEvent{P}`). Spawns a framer task; the transfer is read only at EOF to
# distinguish a clean end from a server truncation, and `cancelled` (shared with the
# owning `Stream`) keeps a deliberate cancel from being reported as that truncation.
function decode_stream(req::Request, incoming::Incoming{Base.BufferStream, Task},
                       fmt::StreamFormat, ::Type{Elt}, cancelled::Threads.Atomic{Bool};
                       buffer::Int = STREAM_BUFFER) where {Elt}
    (; response, body, transfer) = incoming
    P = elementpayload(Elt)
    items = Channel{Elt}(buffer)
    state = framestate(fmt)
    Threads.@spawn try
        while (frame = readframe(body, state, fmt)) !== nothing
            if isstreamend(req.endpoint, frame)
                cancelled[] = true # end the transfer as a deliberate stop
                break
            end
            data = interpret(req, response, framedata(frame), fmt.element, P)
            put!(items, if P === Elt data else remake(frame, data, P) end)
        end
        if cancelled[]
            close(items)
        else
            result = fetch(transfer)
            if result isa Exception; close(items, result) else close(items) end
        end
    catch err
        if cancelled[]; close(items) else close(items, err) end
    finally
        close(incoming)
    end
    items
end


# Transport

"""
    HeadGate(body::Base.BufferStream) <: IO

The output of a streaming transfer: writes pass through to `body`, except that
the first is held until `released`, after announcing itself on `signal`
(`:body`; the transfer adds `:done` when it ends).
"""
mutable struct HeadGate <: IO
    const body::Base.BufferStream
    const signal::Channel{Symbol}
    const released::Base.Event
    isheld::Bool
end

HeadGate(body::Base.BufferStream) = HeadGate(body, Channel{Symbol}(2), Base.Event(), true)

# Only the transfer's writer task writes, so `isheld` needs no lock.
function Base.unsafe_write(gate::HeadGate, p::Ptr{UInt8}, n::UInt)
    if gate.isheld
        put!(gate.signal, :body)
        wait(gate.released)
        gate.isheld = false
    end
    unsafe_write(gate.body, p, n)
end

# Open a streaming transfer: run the request on a task, block until the response
# head is known, and return it with the incrementally filled body and the transfer.
# Cancellation is `close(body)`, which collapses the transfer.
function open_stream(req::Request, out::Outgoing)
    bs = Base.BufferStream()
    gate = HeadGate(bs)
    handle, interrupt = Channel{Downloads.Curl.Easy}(1), Base.Event()
    # A stream is bounded by silence, not total time
    options = TransferOptions(Watchdog(req.config.timeout), handle)
    transfer = Threads.@spawn try
        result = curlrequest(out.url, options; out.method, out.headers, input = payloadio(out),
                             output = gate, interrupt, throw = false)
        if result isa Downloads.RequestError transfererror(req, result, options.watchdog) else result end
    finally
        close(bs)
        put!(gate.signal, :done)
    end
    # Without a body (empty, or a transport failure), the transfer's result is final
    if take!(gate.signal) === :done
        result = fetch(transfer)
        result isa Exception && throw(result)
        return Incoming(result, bs, transfer, interrupt)
    end
    # The held first write keeps the transfer from freeing the curl handle while its
    # head is read (relying on Downloads internals: its Multi's lock and `get_response_info`)
    res = try
        easy = take!(handle)
        @lock DOWNLOADER[].multi.lock Downloads.Response(Downloads.Curl.get_response_info(easy)...)
    finally
        notify(gate.released)
    end
    Incoming(res, bs, transfer, interrupt)
end

transmit(req::Request{<:Any, <:StreamEndpoint}, out::Outgoing) = open_stream(req, out)

# Rate-limiting acts on the head status only: once frames flow there is nothing to replay.
function deliver(req::Request{<:Any, <:StreamEndpoint}, incoming::Incoming)
    (; response, body) = incoming
    response.status ∈ 200:299 || throw(ResponseError(req, response, read(body)))
    dtype = responsetype(req.endpoint)
    Elt, F = eltype(dtype), framingtype(dtype)
    cancelled = Threads.Atomic{Bool}(false)
    items = decode_stream(req, incoming, dataformat(req.endpoint, dtype), Elt, cancelled)
    stream = Stream{Elt, F}(items, response, body, cancelled)
    try
        postprocess(response, req, stream)
    catch
        close(stream)
        rethrow()
    end
end

# A HEAD request to a streaming endpoint is an ordinary existence check.
transmit(req::Request{:head, <:StreamEndpoint}, out::Outgoing) =
    invoke(transmit, Tuple{Request, Outgoing}, req, out)
deliver(req::Request{:head, <:StreamEndpoint}, incoming::Incoming) =
    invoke(deliver, Tuple{Request{:head}, Incoming}, req, incoming)


# Server-Sent Events (`:SSE`)

"""
    SSEvent{T}

A Server-Sent Event with payload `data::T` (undecoded bytes off the wire, or the
decoded `T`), the dispatching `event` type, and the sticky last-event-`id`.

Destructures as `(event, data)` or `(event, data, id)`, and exposes each as a field.
"""
struct SSEvent{T}
    event::Union{String, Nothing}
    data::T
    id::Union{String, Nothing}
end

Base.iterate(ev::SSEvent, s::Int = 1) = if s <= 3 (getfield(ev, s), s + 1) end

elementpayload(::Type{SSEvent{T}}) where {T} = T

mutable struct SSEState
    const data::IOBuffer
    id::Union{String, Nothing}
    started::Bool
end

framestate(::StreamFormat{:SSE}) = SSEState(IOBuffer(), nothing, false)

# WHATWG SSE, with LF or CRLF line endings: a leading byte order mark is skipped, an
# event without data is not dispatched, nor is one unterminated at EOF; `id` is
# sticky, `retry` and comments are ignored. Lines are read with `readuntil`, which
# BufferStream locks against its writer.
function readframe(bs::IO, state::SSEState, ::StreamFormat{:SSE})
    event, hasdata = nothing, false
    while !eof(bs)
        line = readuntil(bs, UInt8('\n'))
        !isempty(line) && last(line) == UInt8('\r') && pop!(line)
        if !state.started
            state.started = true
            length(line) >= 3 && line[1:3] == codeunits("\ufeff") && deleteat!(line, 1:3)
        end
        if isempty(line)
            hasdata && return SSEvent(event, take!(state.data), state.id)
            event = nothing
            continue
        end
        first(line) == UInt8(':') && continue
        colon = findfirst(==(UInt8(':')), line)
        name, val = if isnothing(colon)
            line, view(line, 1:0)
        else
            vstart = colon + 1 + (colon < length(line) && line[colon+1] == UInt8(' '))
            view(line, 1:colon-1), view(line, vstart:length(line))
        end
        if name == b"data"
            hasdata && write(state.data, UInt8('\n'))
            write(state.data, val)
            hasdata = true
        elseif name == b"event"
            event = String(val)
        elseif name == b"id" && UInt8(0) ∉ val
            state.id = String(val)
        end
    end
    nothing
end

mimetype(::Type{<:StreamFormat{:SSE}}) = "text/event-stream"

# Make `Stream{T}` (framing unspecified) default to :SSE.
framingtype(::Type{Stream{T}}) where {T} = :SSE


# Newline-delimited JSON (`:NDJSON`)

"""
    NDJSONFrame{T}

A single NDJSON record with payload `data::T`.
"""
struct NDJSONFrame{T}
    data::T
end

elementpayload(::Type{NDJSONFrame{T}}) where {T} = T

# `readuntil`, not `copyuntil`: Base's BufferStream only locks the former against
# its writer, and the latter can lose or duplicate data read mid-transfer.
function readframe(bs::IO, _, ::StreamFormat{:NDJSON})
    while !eof(bs)
        line = readuntil(bs, UInt8('\n'))
        !isempty(line) && last(line) == UInt8('\r') && pop!(line)
        isempty(line) || return NDJSONFrame(line)
    end
    nothing
end

mimetype(::Type{<:StreamFormat{:NDJSON}}) = "application/x-ndjson"
