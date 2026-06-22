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

"""
    framedata(frame) -> payload

Extract a frame's payload: the field whose type is the frame's type parameter. A
framing whose payload is located differently can override this.
"""
@generated function framedata(frame)
    slot = let T = TypeVar(:T)
        findfirst(==(T), fieldtypes(frame.name.wrapper{T}))
    end
    isnothing(slot) &&
        return :(throw(ArgumentError("no payload field in $frame; define framedata")))
    :(getfield(frame, $slot))
end

"""
    remake(frame, data) -> frame

Return a copy of `frame` with its payload replaced by `data` (re-typed to `data`'s
type), preserving the other fields. This is the decode step: `{bytes} → {T}`. The
payload field is located as in [`framedata`](@ref).
"""
@generated function remake(frame, data)
    slot = let T = TypeVar(:T)
        findfirst(==(T), fieldtypes(frame.name.wrapper{T}))
    end
    isnothing(slot) &&
        return :(throw(ArgumentError("no payload field in $frame; define remake")))
    args = [if i == slot; :data else :(getfield(frame, $i)) end for i in 1:fieldcount(frame)]
    :($(frame.name.wrapper)($(args...)))
end

"""
    elementpayload(::Type{Elt}) -> Type

The decode target for a stream element type. Defaults to `Elt`; a framing whose
element type wraps its payload (e.g. `SSEvent{T}` ⇒ `T`) overrides it.
"""
elementpayload(::Type{T}) where {T} = T

"""
    framestate(fmt::StreamFormat)

The reusable per-stream state [`readframe`](@ref) threads across calls. Defaults to
a fresh `IOBuffer` (the reused scratch for delimiter framings); a framing that
carries state across frames overrides it.
"""
framestate(::StreamFormat) = IOBuffer()

"""
    readframe(bs::IO, state, fmt::StreamFormat) -> frame | nothing

Read one complete frame from `bs`, returning the framing's public frame with its
**undecoded** payload bytes, or `nothing` at clean EOF. `state` is
[`framestate`](@ref)'s value, reused across calls. Dispatched on `fmt`'s framing.

The read blocks on `bs`, reassembling a frame split across network reads via Base's
`copyuntil`/`read`; it touches no stream internals.
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

A lazily-consumed stream of elements of type `T`, framed by `F` (a symbol, e.g.
`:SSE`/`:NDJSON`). A peer of [`Single`](@ref)/[`List`](@ref), selected through an
endpoint's output type; `F` defaults to `:SSE`.

Iterate it to pull decoded elements as bytes arrive (blocking per frame). It is a
plain value: `close` it from any task to end iteration cleanly (see the manual on
cancellation), and a dropped, undrained stream is reclaimed by its finalizer.

See also: [`isstreamend`](@ref).
"""
mutable struct Stream{T, F}
    const items::Channel{T}
    const response::Downloads.Response
    const body::Base.BufferStream
    const cancelled::Threads.Atomic{Bool}
    function Stream{T, F}(items, response, body, cancelled) where {T, F}
        finalizer(close, new{T, F}(items, response, body, cancelled))
    end
end

Base.IteratorSize(::Type{<:Stream}) = Base.SizeUnknown()
Base.eltype(::Type{<:Stream{T}}) where {T} = T
Base.iterate(s::Stream, st...) = iterate(s.items, st...)

framingtype(::Type{<:Stream{T, F}}) where {T, F} = F

# Flag the cancel before closing items, so the framer's EOF reconciliation can tell
# a deliberate cancel from a server truncation (both collapse the transfer alike).
Base.close(s::Stream) = (s.cancelled[] = true; close(s.items); close(s.body))

dataformat(S::Type{<:Stream}) =
    StreamFormat{framingtype(S)}(dataformat(elementpayload(eltype(S))))

"""
    isstreamend(endpoint::AbstractEndpoint, frame) -> Bool

Whether `frame` terminates the stream from `endpoint`. Applied to the undecoded
public frame before it is decoded or emitted, so a terminator is never seen by the
consumer. The default is `false` (stop only at EOF, correct for any stream); a
client library defines one method per API over the frame's public accessors:

```julia
isstreamend(::ChatEndpoint, fr::SSEvent)     = fr.data == codeunits("[DONE]")  # OpenAI
isstreamend(::MessagesEndpoint, fr::SSEvent) = fr.event == "message_stop"      # Anthropic
```
"""
isstreamend(@nospecialize(::AbstractEndpoint), @nospecialize(frame)) = false

# Decode a streamed body into a channel of `Elt` (the decoded payload, or a whole
# frame like `SSEvent{P}`). Spawns a framer task; `transfer` is read only at EOF to
# distinguish a clean end from a server truncation, and `cancelled` (shared with the
# owning `Stream`) keeps a deliberate cancel from being reported as that truncation.
function decode_stream(bs::Base.BufferStream, endpoint::AbstractEndpoint,
                       fmt::StreamFormat, ::Type{Elt},
                       transfer::Task, cancelled::Threads.Atomic{Bool};
                       buffer::Int = STREAM_BUFFER) where {Elt}
    P = elementpayload(Elt)
    items = Channel{Elt}(buffer)
    state = framestate(fmt)
    Threads.@spawn try
        while (frame = readframe(bs, state, fmt)) !== nothing
            isstreamend(endpoint, frame) && break
            decoded = remake(frame, interpretresponse(framedata(frame), fmt.element, P))
            put!(items, if decoded isa Elt; decoded else framedata(decoded) end)
        end
        if cancelled[]
            close(items)
        else
            result = fetch(transfer)
            if result isa Downloads.RequestError; close(items, result) else close(items) end
        end
    catch err
        if cancelled[]; close(items) else close(items, err) end
    finally
        close(bs)
    end
    items
end


# Transport

# Open a streaming transfer: run the request on a task, block until the response
# head is known, and return (response, body, transfer). The body is a BufferStream
# filled incrementally; `payload` is already-encoded IO/Nothing. Cancellation is
# `close(bs)`, which collapses the transfer.
function open_stream(method::String, url::String, payload::Union{IO, Nothing};
                     headers, timeout)
    bs = Base.BufferStream()
    handle = Channel{Downloads.Curl.Easy}(1)
    dl = Downloads.Downloader()
    dl.easy_hook = (e, _info) -> (put!(handle, e); nothing)
    transfer = Threads.@spawn try
        Downloads.request(url; method, headers, timeout, input = payload,
                          output = bs, downloader = dl, throw = false)
    finally
        close(bs)
    end
    eof(bs)                                    # blocks until first byte or empty close
    # If the transfer already finished (an empty/instant body, or a transport
    # failure with no HTTP head), its return value is the authoritative result: a
    # RequestError to surface, or the fully-resolved Response. Only while the body
    # is still flowing do we read the status off the live handle.
    if istaskdone(transfer)
        result = fetch(transfer)
        result isa Downloads.RequestError && throw(result)
        return result, bs, transfer
    end
    res = Downloads.Response(Downloads.Curl.get_response_info(take!(handle))...)
    res, bs, transfer
end

# The streaming sibling of bare_request/payload_request: the only layer holding both
# the resolved response and the body, so it constructs the Stream. Rate-limiting
# acts on the head status only — once frames flow there is nothing to replay.
function stream_request(req::Request, method::String)
    check_validation(req)
    pld = payload(req)
    pldio = format_payload(req.endpoint, pld)
    pldtype = if isnothing(pld); Nothing else typeof(pld) end
    rurl, hdrs = url(req), mimeheaders(req, pldtype)
    res, body, transfer = catch_ratelimit(
        open_stream, req.config.rategate, method, rurl, pldio;
        headers = hdrs, timeout = req.config.timeout)
    res.status ∈ 200:299 || throw(ResponseError(req, res, read(body)))
    dtype = responsetype(req.endpoint)
    fmt = dataformat(req.endpoint, dtype)
    Elt, F = eltype(dtype), framingtype(dtype)
    cancelled = Threads.Atomic{Bool}(false)
    items = decode_stream(body, req.endpoint, fmt, Elt, transfer, cancelled)
    postprocess(res, req, Stream{Elt, F}(items, res, body, cancelled))
end

perform(req::Request{:get, <:StreamEndpoint}) = stream_request(req, "GET")
perform(req::Request{:post, <:StreamEndpoint}) = stream_request(req, "POST")
perform(req::Request{:put, <:StreamEndpoint}) = stream_request(req, "PUT")
perform(req::Request{:patch, <:StreamEndpoint}) = stream_request(req, "PATCH")
perform(req::Request{:delete, <:StreamEndpoint}) = stream_request(req, "DELETE")


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

function Base.iterate(ev::SSEvent, s::Int = 1)
    if s == 1
        (ev.event, 2)
    elseif s == 2
        (ev.data, 3)
    elseif s == 3
        (ev.id, 4)
    end
end

elementpayload(::Type{SSEvent{T}}) where {T} = T

mutable struct SSEState
    const scratch::IOBuffer
    id::Union{String, Nothing}
end

framestate(::StreamFormat{:SSE}) = SSEState(IOBuffer(), nothing)

function readframe(bs::IO, state::SSEState, ::StreamFormat{:SSE})
    eof(bs) && return nothing
    copyuntil(state.scratch, bs, "\n\n"; keep = false)
    parse_sse_event(take!(state.scratch), state)
end

# WHATWG SSE parse over one event block, working on the owned `block` bytes: each
# line's value is its bytes after `field:` and one optional space. `data` values
# join with \n (built once at the end, in place when there is a single one);
# `event` is per-event, `id` is sticky on `state`, `retry` and comments are dropped.
function parse_sse_event(block::Vector{UInt8}, state::SSEState)
    event = nothing
    dataranges = UnitRange{Int}[]
    i, n = firstindex(block), lastindex(block)
    while i <= n
        stop = something(findnext(==(UInt8('\n')), block, i), n + 1)
        lineend = stop - 1                          # inclusive end of this line
        if lineend < i || block[i] == UInt8(':')    # empty line or comment
            i = stop + 1; continue
        end
        colon = findnext(==(UInt8(':')), view(block, i:lineend), 1)
        field = if isnothing(colon); i:lineend else i:(i + colon - 2) end
        vlo = if isnothing(colon); lineend + 1 else i + colon end
        vlo <= lineend && block[vlo] == UInt8(' ') && (vlo += 1)   # strip one leading space
        name = view(block, field)
        if name == b"data"
            push!(dataranges, vlo:lineend)
        elseif name == b"event"
            event = String(block[vlo:lineend])
        elseif name == b"id"
            id = block[vlo:lineend]
            UInt8(0) in id || (state.id = String(id))
        end
        i = stop + 1
    end
    SSEvent(event, join_data!(block, dataranges), state.id)
end

# Combine the `data` value ranges into the payload bytes (joined with \n). A single
# range shrinks `block` in place (no allocation); multiple ranges copy into one
# right-sized vector.
function join_data!(block::Vector{UInt8}, ranges::Vector{UnitRange{Int}})
    isempty(ranges) && return empty!(block)
    length(ranges) == 1 && return keepat!(block, ranges[1])
    out = Vector{UInt8}(undef, sum(length, ranges) + length(ranges) - 1)
    pos = firstindex(out)
    for (k, r) in enumerate(ranges)
        k > 1 && (out[pos] = UInt8('\n'); pos += 1)
        copyto!(out, pos, block, first(r), length(r))
        pos += length(r)
    end
    out
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

function readframe(bs::IO, scratch::IOBuffer, ::StreamFormat{:NDJSON})
    eof(bs) && return nothing
    copyuntil(scratch, bs, UInt8('\n'); keep = false)
    NDJSONFrame(take!(scratch))
end

mimetype(::Type{<:StreamFormat{:NDJSON}}) = "application/x-ndjson"
