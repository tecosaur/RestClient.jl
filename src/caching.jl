# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using Dates
using BaseDirs

"""
    CACHE_STALE_DURATION = 2 weeks (in seconds)

How long a cached entry will be kept for after becoming stale,
before being deleted.
"""
const CACHE_STALE_DURATION = 2 * 24 * 60 * 60


# Request caching and reading

"""
    dumpresponse(io::IO,, res::Response, body::IO)

Dump a response to an IO stream, including the response (`res`) URL, headers,
and message `body`.

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
    reset(body)
    nothing
end

"""
    tryreadresponse(io::IO) -> Response

Read a `Response` from an IO stream including the url, status, and headers.

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

function cachekey(url::String, headers::Union{<:AbstractVector, <:AbstractDict}, payload::Union{<:IO, Nothing})
    h = hash(url)
    for (k, v) in headers
        h = hash(v, hash(k, h))
    end
    if !isnothing(payload)
        mark(payload)
        # This sort of hashing is quite sub-optimal, but
        # it's way faster than most network speeds and
        # I don't think it's worth having some adversarial
        # protection here. The only stdlibs available are
        # CRC32c (trivially reversible) and slow SHA implementations.
        # If this proves to be a bottleneck, we could grab
        # `KangarooTwelve` or similar.
        while !eof(payload)
            peek(payload) # Refill buffer
            for b in readavailable(payload)
                h = hash(b, h)
            end
        end
        reset(payload)
    end
    string(h, base=10+26)
end

"""
    cachelifetime(req::Request, res::Response) -> Union{DateTime, Integer, Nothing}

Determine the expiry time of a cached response, based on the request and response.

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

cachelifetime(@nospecialize ::Response) = nothing

"""
    expirytime(headers::Vector{Pair{String, String}}) -> Integer

Determine the expiry time of a response based on the headers.

The time is returned as a Unix timestamp, or `0` if the response should not be cached.

If set, the `Cache-Control` header is used to determine the expiry time.
The directives `no-cache`, `no-store`, and `private` prevent caching, while
`must-revalidate` forces revalidation, and `max-age` and `s-maxage` set the
expiry time.

If `Cache-Control` is not set, the `Expires` header is used to determine the
expiry time.

Should the headers not contain any information about expiry, `0` is returned.
"""
function expirytime(headers::Vector{Pair{String, String}})
    cachecontrol = ""
    expiry = ""
    for (k, v) in headers
        if k == "cache-control"
            cachecontrol = lowercase(v)
        elseif k == "expires"
            expiry = v
        end
    end
    isempty(cachecontrol) && return 0
    directives = map(strip, split(cachecontrol, ','))
    any(d -> d ∈ ("no-cache", "no-store", "private"), directives) && return 0
    "must-revalidate" ∈ directives && return trunc(Int, time())
    for directive in directives
        if '=' in directive
            components = split(directive, '=', limit=2)
            if length(components) == 2 && first(components) ∈ ("max-age", "s-maxage")
                age = tryparse(UInt, strip(last(components)))
                !isnothing(age) && return trunc(Int, time()) + age
            end
        end
    end
    if !isempty(expiry)
        dtime = tryparse(DateTime, expiry, HTTP_DATE_FORMAT)
        !isnothing(dtime) && return trunc(Int, datetime2unix(dtime))
    end
    0
end


# File age checking

"""
    expirytime(path::String) -> Integer

Return the expiry time of a cached response stored at `path`.

On supporting Linux systems, this uses a filesystem extended attribute to store
the expiry time.
"""
function expirytime(path::String)
    @something(expirytime_xattr(path),
               Some(expirytime_read(path)))
end

@static if Sys.islinux()
    function expirytime_xattr(path::String)
        unixtime = Ref{Int64}()
        nb = @ccall getxattr(path::Cstring, "user.expirytime"::Cstring, unixtime::Ptr{Int64}, sizeof(Int64)::Csize_t)::Csize_t
        if nb > 0
            unixtime[]
        end
    end

    function setexpiry(path::String, unixtime::Integer)
        timeref = Ref(Int64(unixtime))
        @ccall setxattr(path::Cstring, "user.expirytime"::Cstring, timeref::Ptr{Int64}, sizeof(Int64)::Csize_t, 0::Cint)::Cint
        nothing
    end
else
    expirytime_xattr(::String) = nothing
    setexpiry(::String) = nothing
end

function expirytime_read(path::String)
    res = open(tryreadresponse, path)
    if !isnothing(res)
        expirytime(res.headers)
    end
end


# Performing a cached request

"""
    http_cached(method::String, url::String, payload::Union{<:IO, Nothing};
                headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                timeout::Float64 = Inf) -> Tuple{Response, IO, Bool}

Perform an HTTP request, using a cached response if available.
"""
function http_cached(method::String, url::String, payload::Union{<:IO, Nothing} = nothing;
                     headers::Union{<:AbstractVector, <:AbstractDict} = Pair{String, String}[],
                     timeout::Float64 = Inf)
    ckey = cachekey(url, headers, payload)
    cfile = joinpath(cachedir(), ckey * ".http")
    isfile(cfile) || @goto freshreq
    etime = expirytime(cfile)
    return if etime >= ceil(Int, time())
        io = open(cfile)
        res = tryreadresponse(io)
        if isnothing(res)
            close(io)
            rm(cfile)
            @goto freshreq
        end
        @debug debug_request("CACHE", url, headers, payload)
        res, io, true
    elseif etime + CACHE_STALE_DURATION >= ceil(Int, time())
        io = open(cfile)
        res = tryreadresponse(io)
        if isnothing(res)
            close(io)
            rm(cfile)
            @goto freshreq
        end
        mheaders = copy(headers)
        @debug S"{inverse,magenta,bold: CACHE } checking validity of stale entry"
        for (k, v) in res.headers
            if k == "etag"
                push!(mheaders, "if-none-match" => v)
                break
            elseif k == "last-modified"
                push!(mheaders, "if-modified-since" => v)
                break
            end
        end
        if length(mheaders) == length(headers)
            mtime = Dates.format(HTTP_DATE_FORMAT, ctime(cfile))
            push!(mheaders, "if-modified-since" => mtime)
        end
        eres, buf = http_request(method, url, payload; headers=mheaders, timeout)
        if eres.status == 304
            close(buf)
            @debug S"{inverse,magenta,bold: CACHE } stale entry is valid"
            fperm = filemode(cfile)
            chmod(cfile, fperm | 0o200)
            setexpiry(cfile, trunc(Int, time()))
            chmod(cfile, fperm)
            res, io, true
        else
            close(io)
            res, buf, false
        end
    else
        @goto freshreq
    end
    @label freshreq
    res, buf = http_request(method, url, payload; headers, timeout)
    res, buf, false
end

function cachesave(req::Request, url::String, headers, payload, res::Response, body::IO)
    cachetime = cachelifetime(req, res)
    etime = if isnothing(cachetime)
        expirytime(res.headers)
    elseif cachetime isa DateTime
        datetime2unix(cachetime)
    else
        trunc(Int, time()) + cachetime
    end
    iszero(etime) && return
    cdir = cachedir()
    isdir(cdir) || mkpath(cdir)
    cfile = joinpath(cdir, cachekey(url, headers, payload) * ".http")
    open(io -> dumpresponse(io, res, body), cfile, "w")
    setexpiry(cfile, etime)
    chmod(cfile, 0o100444 & filemode(cfile)) # Make read-only
    nothing
end


# Cleanup

const CLEANUP_BLOCK_MAX_SIZE = 100

"""
    cleancache()

Remove some of the files under `cachedir()` that have expired.

Statistically, this will eventually remove all expired files.
"""
function cleancache()
    cdir = cachedir()
    isdir(cdir) || return
    cachefiles = readdir(cdir, join=true)
    isempty(cachefiles) && return
    for i in eachindex(cachefiles)
        j = rand(axes(cachefiles, 1))
        cachefiles[i], cachefiles[j] = cachefiles[j], cachefiles[i]
    end
    bsize = clamp(length(cachefiles) ÷ 6, 1, CLEANUP_BLOCK_MAX_SIZE)
    strike = false
    cleantime = trunc(Int, time()) - CACHE_STALE_DURATION
    for block in Iterators.partition(cachefiles, bsize)
        nexpired = 0
        for cfile in block
            if expirytime(cfile) < cleantime
                rm(cfile)
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
