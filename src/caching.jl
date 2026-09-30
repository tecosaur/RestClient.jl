# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using Dates
using BaseDirs

"""
    CACHE_STALE_DURATION = 2 weeks (in seconds)

How long an expired cache entry that can be revalidated (it has an `ETag` or
`Last-Modified` validator) is kept for, before being deleted. Entries without a
validator are deleted as soon as they expire.
"""
const CACHE_STALE_DURATION = 14 * 24 * 60 * 60

# Cache files start with `CACHE_MAGIC <expiry> <retain-until>` (Unix times); bump
# the version on any layout change, so older files read as corrupt.
const CACHE_MAGIC = "RestClient-Cache/1"

# Stored headers a 304 must not overwrite, as they describe the stored body (RFC 9111 §3.2).
const CACHE_BODY_HEADERS = ("content-length", "content-encoding", "content-range", "transfer-encoding")



# Request caching and reading

"""
    dumpresponse(io::IO, res::Downloads.Response, body::IO) -> Int

Dump a response to an IO stream, including the response (`res`) URL, headers,
and message `body`.

Returns the number of bytes written to the IO stream.

The `body` is restored to its original position after writing.
"""
function dumpresponse(io::IO, res::Downloads.Response, body::IO)
    println(io, res.url)
    println(io, res.message)
    for (k, v) in res.headers
        println(io, k, ": ", v)
    end
    println(io)
    mark(body)
    write(io, body)
    bend = position(body)
    bend - reset(body)
end

"""
    tryreadresponse(io::IO) -> Downloads.Response

Read a `Downloads.Response` from an IO stream including the url, status, and headers.

The message body is not read, and the IO stream is left positioned at the
beginning of the message body.
"""
function tryreadresponse(io::IO)
    url = readline(io)
    message = readline(io)
    components = split(message, ' ', limit=3)
    length(components) >= 2 || return
    proto = eachsplit(components[1], '/') |> first |> lowercase
    status = tryparse(Int, components[2])
    !isnothing(status) || return
    headers = Pair{String, String}[]
    while !eof(io)
        if peek(io) == UInt8('\n')
            read(io, 1)
            break
        end
        name = readuntil(io, ':')
        !eof(io) && read(io, UInt8) == UInt8(' ') || return
        value = readline(io)
        push!(headers, name => value)
    end
    Downloads.Response(proto, url, status, message, headers)
end


# Caching requests to an endpoint

cachedir() = BaseDirs.User.cache(BaseDirs.Project("RestClient"))

function cachekey(url::String, method::String, headers::Vector{Pair{String, String}},
                  payload::Union{Vector{UInt8}, Nothing})
    h = hash(payload, hash(headers, hash(method, hash(url))))
    string(h, base=10+26)
end

"""
    cachelifetime(req::Request, res::Downloads.Response) -> Union{DateTime, Integer, Nothing}

Determine how long a response may be served from the cache: until a (UTC)
`DateTime`, for a number of seconds, or `nothing` to follow the response's
HTTP caching headers.

# Specialisation

To specialise this function for a specific endpoint, define one of the following methods:

```
cachelifetime(req::Request{kind, <:AbstractEndpoint}, res::Downloads.Response)
cachelifetime([conf::RequestConfig], endpoint::AbstractEndpoint, res::Downloads.Response)
```
"""
function cachelifetime end

cachelifetime(req::Request, res::Downloads.Response) =
    cachelifetime(req.config, req.endpoint, res)

cachelifetime(@nospecialize(::RequestConfig), endpoint::AbstractEndpoint, res::Downloads.Response) =
    cachelifetime(endpoint, res)

cachelifetime(@nospecialize(::AbstractEndpoint), res::Downloads.Response) =
    cachelifetime(res)

cachelifetime(@nospecialize(_::Downloads.Response)) = nothing

"""
    expirytime(headers::Vector{Pair{String, String}}, now::Int = trunc(Int, time())) -> Union{Int, Nothing}

Determine when a response with `headers` stops being fresh, as a Unix time, or
`nothing` if it must not be stored.

Follows RFC 9111 for a private cache: `no-store` and `Vary: *` forbid storage;
`no-cache` or a lack of freshness information makes the response stale at once;
otherwise it is fresh for `max-age` less `Age`, or until `Expires` (relative to `Date`).
"""
function expirytime(headers::Vector{Pair{String, String}}, now::Int = trunc(Int, time()))
    strip(getheader(headers, "vary")) == "*" && return nothing
    directives = [strip(d) for (k, v) in headers if k == "cache-control"
                      for d in eachsplit(lowercase(v), ',')]
    "no-store" ∈ directives && return nothing
    "no-cache" ∈ directives && return now
    for directive in directives
        name, value = if '=' in directive split(directive, '=', limit=2) else (directive, "") end
        name == "max-age" || continue
        maxage = tryparse(Int, strip(value))
        isnothing(maxage) && continue
        return now + maxage - something(tryparse(Int, getheader(headers, "age")), 0)
    end
    expires = tryparse(DateTime, getheader(headers, "expires"), HTTP_DATE_FORMAT)
    isnothing(expires) && return now # An absent or invalid Expires means stale (§5.3)
    date = something(tryparse(DateTime, getheader(headers, "date"), HTTP_DATE_FORMAT),
                     unix2datetime(now))
    now + fld(Dates.value(expires - date), 1000)
end

hasvalidator(headers::Vector{Pair{String, String}}) =
    any(((k, _),) -> k == "etag" || k == "last-modified", headers)



# Cache files

# The `(expiry, retainuntil)` stamp heading a cache file, or `nothing` if it isn't one
function readcachestamp(io::IO)
    fields = split(readline(io), ' ')
    length(fields) == 3 && first(fields) == CACHE_MAGIC || return nothing
    expiry, retain = tryparse(Int, fields[2]), tryparse(Int, fields[3])
    if !isnothing(expiry) && !isnothing(retain)
        expiry, retain
    end
end

# `(expiry, retainuntil, response, body)` for the cache entry `cfile`, or `nothing`
# if there is none (an unreadable entry is removed)
function opencacheentry(cfile::String)
    io = openifexists(cfile)
    isnothing(io) && return nothing
    stamp = readcachestamp(io)
    res = if !isnothing(stamp) tryreadresponse(io) end
    if isnothing(res)
        close(io)
        rm(cfile; force = true)
        return nothing
    end
    (stamp..., res, io)
end

"""
    cachesave(req::Request, cfile::String, res::Downloads.Response, body::IO)

Store `res` and its `body` as the cache entry `cfile`, or remove the entry if the
response may not be stored (see [`cachelifetime`](@ref) and [`expirytime`](@ref)).
Caching is best-effort: a filesystem failure only warns.
"""
function cachesave(req::Request, cfile::String, res::Downloads.Response, body::IO)
    now = trunc(Int, time())
    lifetime = cachelifetime(req, res)
    expiry = if isnothing(lifetime)
        expirytime(res.headers, now)
    elseif lifetime isa DateTime
        round(Int, datetime2unix(lifetime))
    else
        now + lifetime
    end
    retain = if !isnothing(expiry)
        expiry + ifelse(hasvalidator(res.headers), CACHE_STALE_DURATION, 0)
    end
    try
        if isnothing(retain) || retain <= now
            rm(cfile; force = true)
        else
            stored = Downloads.Response(res.proto, redactsecrets(res.url, authsecrets(req)),
                                        res.status, res.message, res.headers)
            cachewrite(cfile, stored, body, expiry, retain)
        end
    catch err
        err isa Union{SystemError, Base.IOError} || rethrow()
        @warn "Unable to update the response cache ($(dirname(cfile)))" exception = err maxlog = 1
    end
    nothing
end

# Written to a temporary file and moved into place, so readers never see a partial entry
function cachewrite(cfile::String, res::Downloads.Response, body::IO, expiry::Integer, retain::Integer)
    mkpath(dirname(cfile))
    tmpfile, io = mktemp(dirname(cfile))
    try
        println(io, CACHE_MAGIC, ' ', expiry, ' ', retain)
        dumpresponse(io, res, body)
        close(io)
        mv(tmpfile, cfile; force = true)
    catch
        close(io)
        rm(tmpfile; force = true)
        rethrow()
    end
end


function openifexists(cfile::String)
    try
        open(cfile)
    catch err
        err isa SystemError && err.errnum == Libc.ENOENT || rethrow()
        nothing
    end
end



# Performing a cached request

"""
    http_cached(cfile::String, req::Request, out::Outgoing) -> Tuple{Incoming, Bool}

Perform the request `out` through the cache entry `cfile`: serve it if fresh,
revalidate it if stale (RFC 9111 §4.3), or else make the request afresh. The
`Bool` is true when the response is the stored entry unchanged.
"""
function http_cached(cfile::String, req::Request, out::Outgoing)
    entry = opencacheentry(cfile)
    isnothing(entry) && return http_request(req, out), false
    expiry, retain, res, io = entry
    now = time()
    if expiry >= now
        @debug debug_request("CACHE", out.url, out.headers; secrets = authsecrets(req)) _file=nothing
        return Incoming(res, io), true
    elseif retain < now
        close(io)
        return http_request(req, out), false
    end
    @debug S"{inverse,magenta,bold: CACHE } revalidating stale entry" _file=nothing
    etag, lastmod = getheader(res.headers, "etag"), getheader(res.headers, "last-modified")
    conditions = Pair{String, String}[]
    isempty(etag) || push!(conditions, "if-none-match" => etag)
    isempty(lastmod) || push!(conditions, "if-modified-since" => lastmod)
    revalidation = http_request(req, out, [out.headers; conditions])
    if revalidation.response.status != 304
        close(io)
        return revalidation, false
    end
    # Not Modified: refresh the stored headers (§4.3.4)
    @debug S"{inverse,magenta,bold: CACHE } stale entry is still valid" _file=nothing
    updates = filter(((k, _),) -> k ∉ CACHE_BODY_HEADERS, revalidation.response.headers)
    kept = filter(((k, _),) -> !any(u -> first(u) == k, updates), res.headers)
    body = IOBuffer(read(io))
    close(io)
    Incoming(Downloads.Response(res.proto, res.url, res.status, res.message, vcat(kept, updates)), body), false
end


# Cleanup

const CLEANUP_BLOCK_MAX_SIZE = 100

"""
    cleancache()

Remove some of the files under `cachedir()` that are past retention (or unreadable).

Statistically, this will eventually remove all such files.
"""
function cleancache()
    cdir = cachedir()
    isdir(cdir) || return
    cachefiles = filter!(endswith(".http"), readdir(cdir, join=true))
    isempty(cachefiles) && return
    for i in eachindex(cachefiles)
        j = rand(i:length(cachefiles))
        cachefiles[i], cachefiles[j] = cachefiles[j], cachefiles[i]
    end
    bsize = clamp(length(cachefiles) ÷ 6, 1, CLEANUP_BLOCK_MAX_SIZE)
    strike = false
    now = trunc(Int, time())
    for block in Iterators.partition(cachefiles, bsize)
        nexpired = 0
        for cfile in block
            io = openifexists(cfile)
            isnothing(io) && continue
            stamp = try readcachestamp(io) finally close(io) end
            if isnothing(stamp) || last(stamp) < now
                rm(cfile; force = true)
                nexpired += 1
            end
        end
        if iszero(nexpired)
            strike && break
            strike = true
        else
            strike = false
        end
    end
end
