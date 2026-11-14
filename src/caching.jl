# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using BaseDirs

"""
    CACHE_STALE_DURATION = 2 weeks (in seconds)

How long an expired cache entry that can be revalidated (it has an `ETag` or
`Last-Modified` validator) is kept for, before being deleted. Entries without a
validator are deleted as soon as they expire.
"""
const CACHE_STALE_DURATION = 14 * 24 * 60 * 60

# Cache files start with `CACHE_MAGIC <expiry> <retain-until> <url>`: Unix times, and
# the (redacted) URL of the request the entry answers. Bump the version on any layout
# change, so older files read as corrupt.
const CACHE_MAGIC = "RestClient-Cache/1"

# Stored headers a 304 must not overwrite, as they describe the stored body (RFC 9111 §3.2).
const CACHE_BODY_HEADERS = ("content-length", "content-encoding", "content-range", "transfer-encoding")



# Request caching and reading

"""
    dumpresponse(io::IO, res::Response, body::IO) -> Int

Dump a response to an IO stream, including the response (`res`) URL, headers,
and message `body`.

Returns the number of bytes written to the IO stream.

The `body` is restored to its original position after writing.
"""
function dumpresponse(io::IO, res::Response, body::IO)
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
    tryreadresponse(io::IO) -> Response

Read a [`Response`](@ref) from an IO stream including the url, status, and headers.

The message body is not read, and the IO stream is left positioned at the
beginning of the message body.
"""
function tryreadresponse(io::IO)
    url = readline(io)
    message = readline(io)
    components = split(message, ' ', limit=3)
    length(components) >= 2 || return
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
    Response(url, status, message, Headers(headers))
end


# Caching requests to an endpoint

cachedir() = BaseDirs.User.cache(BaseDirs.App("RestClient"))

# Every cached variant of `url` together, so a change to the resource drops them all
cachefolder(url::String) = joinpath(cachedir(), string(hash(url), base = 36))

function cachekey(url::String, method::String, headers::Vector{Pair{String, String}},
                  payload::Union{Vector{UInt8}, Nothing})
    h = hash(headers, hash(method, hash(url)))
    # Byte by byte, as `hash(payload)` samples the elements of a large array
    for byte in something(payload, ())
        h = hash(byte, h)
    end
    string(h, base = 36)
end

function cachefile(req::Request{kind}, out::Outgoing) where {kind}
    # RFC 9111 §3, RFC 10008 §2.7: GET, HEAD, and QUERY (keyed on its content) are cacheable
    req.config.cache && responsetype(req.endpoint) !== Nothing &&
        kind ∈ (:get, :head, :query) && !(out.payload isa IO) || return nothing
    joinpath(cachefolder(out.url), cachekey(out.url, out.method, out.headers, out.payload) * ".http")
end

"""
    cachelifetime(req::Request, res::Response) -> Union{DateTime, Integer, Nothing}

Determine how long a response may be served from the cache: until a (UTC)
`DateTime`, for a number of seconds, or `nothing` to follow the response's
HTTP caching headers. With `0`, a response is out of date at once: checked with
the server each time it's wanted if it has an `ETag` or `Last-Modified`, and
otherwise not kept.

# Specialisation

To specialise this function for a specific endpoint, define one of the following methods:

```
cachelifetime(req::Request{kind, <:AbstractEndpoint}, res::Response)
cachelifetime([conf::RequestConfig], endpoint::AbstractEndpoint, res::Response)
```
"""
function cachelifetime end

cachelifetime(req::Request, res::Response) =
    cachelifetime(req.config, req.endpoint, res)

cachelifetime(@nospecialize(::RequestConfig), endpoint::AbstractEndpoint, res::Response) =
    cachelifetime(endpoint, res)

cachelifetime(@nospecialize(::AbstractEndpoint), res::Response) =
    cachelifetime(res)

cachelifetime(@nospecialize(_::Response)) = nothing

"""
    expirytime(headers::Vector{Pair{String, String}}, now::Int = trunc(Int, time())) -> Union{Int, Nothing}

Determine when a response with `headers` stops being fresh, as a Unix time, or
`nothing` if it must not be stored.

Follows RFC 9111 for a private cache: `no-store` and `Vary: *` forbid storage;
`no-cache` or a lack of freshness information makes the response stale at once;
otherwise it is fresh for `max-age` less `Age`, or until `Expires` (relative to `Date`).
"""
function expirytime(headers::Headers, now::Int = trunc(Int, time()))
    strip(get(headers, "vary", "")) == "*" && return nothing
    directives = [strip(d) for (k, v) in headers if k == "cache-control"
                      for d in eachsplit(lowercase(v), ',')]
    "no-store" ∈ directives && return nothing
    "no-cache" ∈ directives && return now
    for directive in directives
        name, value = if '=' in directive split(directive, '=', limit=2) else (directive, "") end
        name == "max-age" || continue
        maxage = tryparse(Int, strip(value))
        isnothing(maxage) && continue
        return now + maxage - something(tryparse(Int, get(headers, "age", "")), 0)
    end
    expires = tryparse(DateTime, get(headers, "expires", ""), HTTP_DATE_FORMAT)
    isnothing(expires) && return now # An absent or invalid Expires means stale (§5.3)
    date = something(tryparse(DateTime, get(headers, "date", ""), HTTP_DATE_FORMAT),
                     unix2datetime(now))
    now + fld(Dates.value(expires - date), 1000)
end

hasvalidator(headers::Headers) =
    any(((k, _),) -> k == "etag" || k == "last-modified", headers)



# Cache files

# The `(; expiry, retain, url)` stamp heading a cache file, or `nothing` if it isn't one
function readcachestamp(io::IO)
    fields = split(readline(io), ' '; limit = 4)
    length(fields) == 4 && first(fields) == CACHE_MAGIC || return nothing
    expiry, retain = tryparse(Int, fields[2]), tryparse(Int, fields[3])
    if !isnothing(expiry) && !isnothing(retain)
        (; expiry, retain, url = String(fields[4]))
    end
end

# `(expiry, retainuntil, response, body)` for `cfile`'s entry for `url`, or `nothing`
# (an unreadable entry is removed; one for another URL is left for its own request)
function opencacheentry(cfile::String, url::String)
    io = openifexists(cfile)
    isnothing(io) && return nothing
    stamp = readcachestamp(io)
    if !isnothing(stamp) && stamp.url != url
        close(io)
        return nothing
    end
    res = if !isnothing(stamp) tryreadresponse(io) end
    if isnothing(res)
        close(io)
        rm(cfile; force = true)
        return nothing
    end
    (stamp.expiry, stamp.retain, res, io)
end

"""
    cachesave(req::Request, cfile::String, url::String, res::Response, body::IO)

Store `res` and its `body` as the cache entry `cfile` for `req`, sent to `url`, or
remove the entry if the response may not be stored (see [`cachelifetime`](@ref) and
[`expirytime`](@ref)).
Caching is best-effort: a filesystem failure only warns.
"""
function cachesave(req::Request, cfile::String, url::String, res::Response, body::IO)
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
            secrets = authsecrets(req)
            cachewrite(cfile, redactsecrets(url, secrets), redactsecrets(res, secrets), body, expiry, retain)
        end
    catch err
        err isa Union{SystemError, Base.IOError} || rethrow()
        @warn "Unable to update the response cache ($(dirname(cfile)))" exception = err maxlog = 1
    end
    nothing
end

# Written to a temporary file and moved into place, so readers never see a partial entry
function cachewrite(cfile::String, url::String, res::Response, body::IO,
                    expiry::Integer, retain::Integer)
    mkpath(dirname(cfile))
    tmpfile, io = mktemp(dirname(cfile))
    try
        println(io, CACHE_MAGIC, ' ', expiry, ' ', retain, ' ', url)
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
    entry = opencacheentry(cfile, redactsecrets(out.url, authsecrets(req)))
    isnothing(entry) && return http_request(req, out), false
    expiry, retain, res, io = entry
    now = time()
    if expiry >= now
        @debug debug_request("CACHE", out.url, out.headers; secrets = authsecrets(req)) _file=nothing
        # Served with its `Age`, the time since it was stored added to any it had (RFC 9111 §5.1)
        stored = something(tryparse(Int, get(res.headers, "age", "")), 0)
        age = "age" => string(stored + max(0, round(Int, now - mtime(cfile))))
        headers = Headers(push!([h for h in res.headers if first(h) != "age"], age))
        return Incoming(Response(res.url, res.status, res.message, headers), io), true
    elseif retain < now
        close(io)
        return http_request(req, out), false
    end
    @debug S"{inverse,magenta,bold: CACHE } revalidating stale entry" _file=nothing
    etag, lastmod = get(res.headers, "etag", ""), get(res.headers, "last-modified", "")
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
    updates = [h for h in revalidation.response.headers if first(h) ∉ CACHE_BODY_HEADERS]
    kept = [h for h in res.headers if !any(u -> first(u) == first(h), updates)]
    body = IOBuffer(read(io))
    close(io)
    Incoming(Response(res.url, res.status, res.message, Headers(vcat(kept, updates))), body), false
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
    cachefiles = [joinpath(dir, file) for (dir, _, files) in walkdir(cdir)
                  for file in files if endswith(file, ".http")]
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
            if isnothing(stamp) || stamp.retain < now
                rm(cfile; force = true)
                emptied = dirname(cfile)
                # Another process may have just stored a response there
                emptied != cdir && isempty(readdir(emptied)) &&
                    try rm(emptied) catch err; err isa Base.IOError || rethrow() end
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
