# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# A thin unit layer for behaviour no request can reach: parsing edge cases,
# cache-file robustness, and macro-expansion errors. The happy paths are all
# covered by integration.jl; prefer adding coverage there.

using Dates
using Downloads: Response
using RestClient: retrydelay, expirytime, dumpresponse, tryreadresponse, cachesave,
    opencacheentry, readcachestamp, CACHE_STALE_DURATION, HTTP_DATE_FORMAT

hdr(pairs...) = Pair{String, String}[pairs...]
http_date(t) = Dates.format(t, HTTP_DATE_FORMAT)

sample_response(; status = 200,
                headers = ["content-type" => "application/json",
                           "cache-control" => "max-age=600"]) =
    Response("https", "https://api.example.com/thing", status,
             "HTTP/1.1 $status OK", Pair{String, String}[headers...])

# Fixtures

struct CacheEP <: AbstractEndpoint end
RestClient.urlpath(::CacheEP) = "thing"
struct DatedCacheEP <: AbstractEndpoint end
RestClient.urlpath(::DatedCacheEP) = "thing"
RestClient.cachelifetime(::DatedCacheEP, ::Response) = now(UTC) + Hour(1)
struct KeyedCacheEP <: AbstractEndpoint end
RestClient.urlpath(::KeyedCacheEP) = "thing"
RestClient.authscheme(::KeyedCacheEP) = QueryAuth("api_key")
RestClient.authpolicy(::KeyedCacheEP) = :required
struct PlainEP <: AbstractEndpoint end
RestClient.urlpath(::PlainEP) = "p"
struct PagedListEP <: ListEndpoint end
RestClient.urlpath(::PagedListEP) = "p"
struct ValidatedEP <: AbstractEndpoint
    issues::Any
end
RestClient.urlpath(::ValidatedEP) = "p"
RestClient.validate(ep::ValidatedEP) = ep.issues
RestClient.authpolicy(::ValidatedEP) = :required
struct SharedHeadersEP <: AbstractEndpoint end
const SHARED_HEADERS = ["X-A" => "1"]
const SHARED_PARAMS = ["a" => "1"]
RestClient.urlpath(::SharedHeadersEP) = "p"
RestClient.headers(::SharedHeadersEP) = SHARED_HEADERS
RestClient.parameters(::SharedHeadersEP) = SHARED_PARAMS
RestClient.authscheme(::SharedHeadersEP) = BearerAuth()
RestClient.authpolicy(::SharedHeadersEP) = :optional
struct QueryEP <: AbstractEndpoint end
RestClient.urlpath(::QueryEP) = "p"
RestClient.authscheme(::QueryEP) = QueryAuth("api_key")
RestClient.authpolicy(::QueryEP) = :optional

RestClient.@multipartdef struct OddNames
    a."we\"ird"::String
end

struct OneContent <: SingleResponse{Int}
    note::String
    value::Int
end
struct TwoContents <: SingleResponse{Int}
    a::Int
    b::Int
end
struct ListContent <: ListResponse{String}
    total::Int
    items::Vector{String}
end

# Tests

@testset "rate limits" begin
    @testset "retry delay" begin
        @test isnothing(retrydelay(hdr()))
        @test isnothing(retrydelay(hdr("retry-after" => "soon")))
        @test isnothing(retrydelay(hdr("x-ratelimit-reset" => "garbage")))
        @testset "HTTP date" begin
            @test 115 <= retrydelay(hdr("retry-after" => http_date(now(UTC) + Second(120)))) <= 121
            @test retrydelay(hdr("retry-after" => http_date(now(UTC) - Hour(1)))) == 0
        end
        @testset "remaining gates reset" begin
            @test isnothing(retrydelay(hdr("x-ratelimit-remaining" => "5", "x-ratelimit-reset" => "60")))
            @test retrydelay(hdr("x-ratelimit-remaining" => "0", "x-ratelimit-reset" => "45")) == 45
        end
        @testset "delta or timestamp" begin
            @test retrydelay(hdr("x-ratelimit-reset" => "90")) == 90        # small ⇒ delta
            abstime = trunc(Int, time()) + 100                              # large ⇒ absolute
            @test 95 <= retrydelay(hdr("x-ratelimit-reset" => string(abstime))) <= 101
        end
        @testset "Retry-After first" begin
            @test retrydelay(hdr("retry-after" => "10", "x-ratelimit-remaining" => "0",
                                 "x-ratelimit-reset" => "999")) == 10
        end
    end
    @testset "payload resending" begin
        seekable = IOBuffer("payload")
        read(seekable, 2)                                        # sent from where it stands
        next = RestClient.resendable(seekable)
        @test read(next(), String) == "yload"
        @test read(next(), String) == "yload"                    # rewound for a retry
        @test isnothing(RestClient.resendable(nothing)())
        pipe = Base.BufferStream()
        once = RestClient.resendable(pipe)
        @test once() === pipe
        @test_throws ArgumentError once()
    end
end

@testset "caching" begin
    # RFC 9111, for a private cache
    @testset "freshness" begin
        t = 1_700_000_000
        @testset "not storable" begin
            @test isnothing(expirytime(hdr("cache-control" => "no-store", "etag" => "x"), t))
            @test isnothing(expirytime(hdr("vary" => "*", "cache-control" => "max-age=600"), t))
        end
        @testset "stale at once" begin
            @test expirytime(hdr(), t) == t
            @test expirytime(hdr("cache-control" => "no-cache, max-age=600"), t) == t
            @test expirytime(hdr("cache-control" => "max-age=notanumber"), t) == t
            @test expirytime(hdr("expires" => "whenever"), t) == t
        end
        @testset "lifetime" begin
            @test expirytime(hdr("cache-control" => "private, max-age=600"), t) == t + 600
            @test expirytime(hdr("cache-control" => "max-age=600", "age" => "100"), t) == t + 500
            @test expirytime(hdr("cache-control" => "public, s-maxage=300"), t) == t  # shared caches only
            # Expires counts from the response's Date, not the local clock.
            date = unix2datetime(t - 3600)
            @test expirytime(hdr("date" => http_date(date),
                                 "expires" => http_date(date + Minute(10))), t) == t + 600
            # max-age wins over Expires (§4.2.1).
            @test expirytime(hdr("cache-control" => "max-age=10",
                                 "expires" => http_date(unix2datetime(t) + Hour(1))), t) == t + 10
        end
    end
    @testset "response dump" begin
        res = sample_response()
        io = IOBuffer()
        @test dumpresponse(io, res, IOBuffer(b"{\"ok\":true}")) == 11
        seekstart(io)
        got = tryreadresponse(io)
        @test got.url == res.url
        @test got.status == res.status
        @test got.headers == res.headers
        @test read(io, String) == "{\"ok\":true}"     # IO left at body start
        @testset "malformed" begin
            @test isnothing(tryreadresponse(IOBuffer("")))
            @test isnothing(tryreadresponse(IOBuffer("only-a-url-line\n")))
            @test isnothing(tryreadresponse(IOBuffer("url\nHTTP/1.1 notastatus OK\n\n")))
        end
    end
    @testset "entries" begin
        cfile = joinpath(mktempdir(), "entry.http")
        req = Request{:get}(RequestConfig("https://api.example.com"), CacheEP())
        stamp(path) = open(readcachestamp, path)
        body(path) = (entry = opencacheentry(path); try read(last(entry), String) finally close(last(entry)) end)
        @testset "round-trip" begin
            cachesave(req, cfile, sample_response(), IOBuffer(b"body"))
            @test body(cfile) == "body"
            expiry, retain = stamp(cfile)
            @test 595 <= expiry - trunc(Int, time()) <= 601
            @test retain == expiry            # no validator ⇒ not kept once stale
        end
        @testset "replaced" begin
            cachesave(req, cfile, sample_response(), IOBuffer(b"new body"))
            @test body(cfile) == "new body"
        end
        @testset "kept to revalidate" begin
            res = sample_response(headers = ["cache-control" => "max-age=0", "etag" => "\"v1\""])
            cachesave(req, cfile, res, IOBuffer(b"x"))
            expiry, retain = stamp(cfile)
            @test retain - expiry == CACHE_STALE_DURATION
        end
        @testset "unstorable removed" begin
            cachesave(req, cfile, sample_response(headers = ["cache-control" => "no-store"]), IOBuffer(b"x"))
            @test !isfile(cfile)
            cachesave(req, cfile, sample_response(headers = Pair{String, String}[]), IOBuffer(b"x"))
            @test !isfile(cfile)              # stale at once, and no validator
        end
        @testset "DateTime lifetime" begin
            dreq = Request{:get}(RequestConfig("https://api.example.com"), DatedCacheEP())
            cachesave(dreq, cfile, sample_response(), IOBuffer(b"x"))
            @test 3595 <= first(stamp(cfile)) - trunc(Int, time()) <= 3601
        end
        @testset "auth keys not stored" begin
            kreq = Request{:get}(RequestConfig("https://api.example.com"; key = "s3cret"), KeyedCacheEP())
            res = Response("https", RestClient.url(kreq), 200, "HTTP/1.1 200 OK",
                           Pair{String, String}["cache-control" => "max-age=600"])
            cachesave(kreq, cfile, res, IOBuffer(b"x"))
            @test !occursin("s3cret", read(cfile, String))
            @test occursin("api_key=*****", read(cfile, String))
        end
        @testset "failed write warns" begin
            rodir = mktempdir()
            chmod(rodir, 0o500)
            @test_logs (:warn, r"Unable to update the response cache") cachesave(
                req, joinpath(rodir, "sub", "entry.http"), sample_response(), IOBuffer(b"x"))
            dir, unreadable = mktempdir(), IOBuffer(b"x")
            close(unreadable)
            @test_throws ArgumentError cachesave(req, joinpath(dir, "entry.http"), sample_response(), unreadable)
            @test isempty(readdir(dir))                      # no temporary file left behind
        end
        @testset "unreadable removed" begin
            write(cfile, "https://old-format/\nHTTP/1.1 200 OK\n\nbody")
            @test isnothing(opencacheentry(cfile))
            @test !isfile(cfile)
            @test isnothing(opencacheentry(cfile))
        end
    end
end

@testset "endpoint macro" begin
    # The macros' happy paths run throughout integration.jl; these pin the
    # compile-time errors, which have no request analogue.
    @testset "errors" begin
        @test_throws Exception @macroexpand @endpoint 42               # not a struct/arrow form
        @test_throws Exception @macroexpand @formdef 42                # not a struct
        @test_throws Exception @macroexpand @multipartdef (x = 1)      # not a struct
        # `func -> struct -> in -> url -> out` is at most five forms.
        @test_throws Exception @macroexpand(
            @endpoint f() -> struct S end -> S -> "u" -> Bool -> extra)
        # An expression URL needs an explicit result type.
        @test_throws ArgumentError @macroexpand @endpoint f(x::String) -> "a" * x
        @test_throws ArgumentError @macroexpand @endpoint f(x::String) -> x -> "a" * x
    end
    @testset "URL templates" begin
        interp(str; kw...) = RestClient.interp_curlies(str; varname = :self, localvars = Symbol[],
                                                       knownfields = [:x], mod = @__MODULE__, kw...)
        @test interp("a\\{b}") == "a{b}"                         # escaped brace is literal
        @test interp("a\\\\b") == "a\\b"                         # escaped backslash
        @test interp("a//b///{x}//c"; ispath = true) ==
            Expr(:call, GlobalRef(Base, :string), "a/b/", :($(RestClient.encode_uri_component)(self.x)), "/c")
        @test interp("a//b") == "a//b"                           # only paths are collapsed
    end
    # A trailing result type may be omitted: the body is discarded and the endpoint
    # returns `nothing`. The result type is distinguished from a payload by shape, so
    # `in -> url` (payload, no result) parses as well as a bare `url`.
    @testset "no result type" begin
        @eval module NoResultClient
            using RestClient
            @endpoint ping() -> "ping"                         # bare URL, GET
            @endpoint announce(msg::String) -> msg -> :post("events")   # payload, no result
        end
        @test RestClient.responsetype(NoResultClient.PingEndpoint()) === Nothing
        @test RestClient.responsetype(NoResultClient.AnnounceEndpoint("hi")) === Nothing
        # The payload endpoint still keeps its body and POST method.
        areq = RestClient.Request{:post}(RequestConfig("http://x"), NoResultClient.AnnounceEndpoint("hi"))
        @test RestClient.payload(areq) == "hi"
        @testset "struct form" begin
            @eval NoResultClient @endpoint struct StatusEndpoint
                "status"
            end
            @test RestClient.responsetype(NoResultClient.StatusEndpoint()) === Nothing
            @test RestClient.urlpath(RequestConfig("http://x"), NoResultClient.StatusEndpoint()) == "status"
        end
        @testset "surplus forms" begin
            @test_throws ArgumentError @macroexpand @endpoint f(x) -> x -> y -> "url"
        end
    end
end

@testset "requests" begin
    @testset "validation" begin
        keyed = RequestConfig("http://x"; key = "k")
        check(issues; config = keyed) =
            RestClient.check_validation(Request{:get}(config, ValidatedEP(issues)))
        @test isnothing(check(nothing))
        @test isnothing(check(String[]))
        @test isnothing(check(true))
        @test_throws MalformedRequest check(false)
        @test_throws MalformedRequest check("bad")
        @test_throws ArgumentError check(42)
        @testset "missing key" begin
            issues = ["bad"]
            err = try check(issues; config = RequestConfig("http://x")) catch e; e end
            @test length(err.issues) == 2 && occursin("requires authentication", err.issues[2])
            @test issues == ["bad"]                              # the caller's vector is untouched
            @test_throws MalformedRequest check(true; config = RequestConfig("http://x"))
        end
    end
    @testset "auth leaves endpoint lists" begin
        req = Request{:get}(RequestConfig("http://x"; key = "k"), SharedHeadersEP())
        for _ in 1:2
            @test RestClient.headers(req) == ["X-A" => "1", "Authorization" => "Bearer k"]
            @test RestClient.parameters(req) == ["a" => "1"]
            @test RestClient.mimeheaders(req, Nothing) == ["X-A" => "1", "Authorization" => "Bearer k"]
        end
        @test SHARED_HEADERS == ["X-A" => "1"]
        @test SHARED_PARAMS == ["a" => "1"]
    end
    @testset "secrets redacted" begin
        breq = Request{:get}(RequestConfig("http://x"; key = "s3cr/t"), SharedHeadersEP())
        qreq = Request{:get}(RequestConfig("http://x"; key = "s3cr/t"), QueryEP())
        @test RestClient.authsecrets(breq) == ["Bearer s3cr/t"]
        @test RestClient.authsecrets(qreq) == ["s3cr%2Ft"]
        secrets = [RestClient.authsecrets(breq); RestClient.authsecrets(qreq)]
        shown = string(RestClient.debug_request("GET", RestClient.url(qreq), RestClient.headers(breq),
                                                IOBuffer("body"); secrets))
        @test !occursin("s3cr", shown)
        @test occursin("api_key=*****", shown)
        @test occursin("Authorization: *****", shown)
        # ...and from a ResponseError's display
        res = Response("http", RestClient.url(qreq), 500, "HTTP/1.1 500", Pair{String, String}[])
        shownerror = sprint(showerror, RestClient.ResponseError(qreq, res, UInt8[]))
        @test !occursin("s3cr", shownerror)
        @test occursin("api_key=*****", shownerror)
    end
end

@testset "formats" begin
    @testset "raw" begin
        raw = RestClient.RawFormat()
        interpret(T, str) = RestClient.interpretresponse(codeunits(str), raw, T)
        @test interpret(Vector{UInt8}, "ab") == b"ab"
        @test interpret(Any, "ab") == b"ab"
        @test interpret(String, "ab") == "ab"
        @test interpret(Bool, "true") === true
        @test interpret(Float64, "1.5") === 1.5
        @test_throws ArgumentError interpret(Dict{String, Int}, "{}")
        written(x) = String(take!((io = IOBuffer(); RestClient.writepayload(io, raw, x); io)))
        @test written(b"\x01\x02") == "\x01\x02"
        @test written("text") == "text"
        @test written(7) == "7"
        @test_throws ArgumentError written(:symbol)
        @test RestClient.payloadmime(raw, String) == "text/plain; charset=utf-8"
        @test RestClient.payloadmime(raw, Vector{UInt8}) == "application/octet-stream"
        # A raw response sets no Accept header (libcurl sends `*/*`).
        @test isempty(RestClient.mimeheaders(Request{:get}(RequestConfig("http://x"), PlainEP()), Nothing))
    end
    @testset "multipart names" begin
        body = sprint(RestClient.writepayload, RestClient.MultipartFormat(), OddNames(a = "v"))
        @test occursin("name=\"we%22ird\"", body)
    end
end

@testset "responses" begin
    @testset "contents" begin
        @test @inferred(RestClient.contents(OneContent("n", 3))) === 3
        @test @inferred(RestClient.contents(ListContent(1, ["x"]))) == ["x"]
        @test_throws ArgumentError RestClient.contents(TwoContents(1, 2))
    end
    @testset "List display" begin
        list = RestClient.List{String, PagedListEP}(
            Request{:get}(RequestConfig("http://x"), PagedListEP()), string.(1:100), Dict{Symbol, Any}())
        bullets(rows) = count(startswith("  • "), split(
            sprint(show, MIME("text/plain"), list; context = :displaysize => (rows, 80)), '\n'))
        shown = sprint(show, MIME("text/plain"), list; context = :displaysize => (14, 80))
        @test bullets(14) == 7                                   # 14 rows - 4 margin - 3 marker
        @test occursin("93 items omitted", shown)
        @test occursin("\"1\"", shown) && occursin("\"100\"", shown)
        @test bullets(2) == 2
    end
end
