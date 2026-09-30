# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# A thin unit layer for behaviour no request can reach: parsing edge cases,
# cache-file robustness, and macro-expansion errors. The happy paths are all
# covered by integration.jl; prefer adding coverage there.

using Dates
using Downloads: Response
using RestClient: retrydelay, expirytime, dumpresponse, tryreadresponse,
    setexpiry, expirytime_xattr, expirytime_read, HTTP_DATE_FORMAT

hdr(pairs...) = Pair{String, String}[pairs...]
http_date(t) = Dates.format(t, HTTP_DATE_FORMAT)

@testset "retrydelay edge cases" begin
    @test retrydelay(hdr()) == 0
    @test retrydelay(hdr("retry-after" => "soon")) == 0
    @test retrydelay(hdr("x-ratelimit-reset" => "garbage")) == 0
    @testset "Retry-After as an HTTP date" begin
        @test 115 <= retrydelay(hdr("retry-after" => http_date(now(UTC) + Second(120)))) <= 121
        @test retrydelay(hdr("retry-after" => http_date(now(UTC) - Hour(1)))) == 0
    end
    @testset "X-RateLimit-Remaining gates the reset" begin
        @test retrydelay(hdr("x-ratelimit-remaining" => "5", "x-ratelimit-reset" => "60")) == 0
        @test retrydelay(hdr("x-ratelimit-remaining" => "0", "x-ratelimit-reset" => "45")) == 45
    end
    @testset "reset delta vs absolute-timestamp inference" begin
        @test retrydelay(hdr("x-ratelimit-reset" => "90")) == 90        # small ⇒ delta
        abstime = trunc(Int, time()) + 100                              # large ⇒ absolute
        @test 95 <= retrydelay(hdr("x-ratelimit-reset" => string(abstime))) <= 101
    end
    @testset "Retry-After takes precedence" begin
        @test retrydelay(hdr("retry-after" => "10", "x-ratelimit-remaining" => "0",
                             "x-ratelimit-reset" => "999")) == 10
    end
end

@testset "expirytime(headers) edge cases" begin
    now_unix = trunc(Int, time())
    @test expirytime(hdr()) == 0
    @test expirytime(hdr("cache-control" => "max-age=notanumber")) == 0
    @test expirytime(hdr("expires" => "whenever")) == 0
    @testset "directives that forbid caching" begin
        @test expirytime(hdr("cache-control" => "no-store")) == 0
        @test expirytime(hdr("cache-control" => "no-cache")) == 0
        @test expirytime(hdr("cache-control" => "private, max-age=600")) == 0
        @test expirytime(hdr("vary" => "*", "cache-control" => "max-age=600")) == 0
    end
    @testset "max-age / s-maxage and Expires priority" begin
        @test 595 <= expirytime(hdr("cache-control" => "max-age=600")) - now_unix <= 601
        @test 295 <= expirytime(hdr("cache-control" => "public, s-maxage=300")) - now_unix <= 301
        stamp = http_date(now(UTC) + Hour(1))
        @test expirytime(hdr("expires" => stamp)) > now_unix
        # max-age wins over Expires (RFC 9111 §4.2.1).
        @test expirytime(hdr("cache-control" => "max-age=10", "expires" => stamp)) - now_unix <= 11
    end
end

sample_response(; status = 200,
                headers = ["content-type" => "application/json",
                           "cache-control" => "max-age=600"]) =
    Response("https", "https://api.example.com/thing", status,
             "HTTP/1.1 $status OK", Pair{String, String}[headers...])

@testset "dump/read round-trip and malformed input" begin
    res = sample_response()
    io = IOBuffer()
    @test dumpresponse(io, res, IOBuffer(b"{\"ok\":true}")) == 11
    seekstart(io)
    got = tryreadresponse(io)
    @test got.url == res.url
    @test got.status == res.status
    @test got.headers == res.headers
    @test read(io, String) == "{\"ok\":true}"     # IO left at body start
    @testset "truncated/garbage returns nothing" begin
        @test isnothing(tryreadresponse(IOBuffer("")))
        @test isnothing(tryreadresponse(IOBuffer("only-a-url-line\n")))
        @test isnothing(tryreadresponse(IOBuffer("url\nHTTP/1.1 notastatus OK\n\n")))
    end
end

@testset "filesystem expiry" begin
    path, _ = mktemp()
    open(io -> dumpresponse(io, sample_response(), IOBuffer(b"body")), path, "w")
    @test expirytime_read(path) - trunc(Int, time()) <= 601    # from max-age=600
    if Sys.islinux()
        @testset "xattr round-trip" begin
            setexpiry(path, 1_700_000_000)
            @test expirytime_xattr(path) == 1_700_000_000
        end
        # Regression: a missing attribute once returned -1 wrapped to a huge
        # unsigned value (Csize_t) and read uninitialised memory.
        @testset "absent xattr returns nothing, not garbage" begin
            fresh, _ = mktemp()
            @test isnothing(expirytime_xattr(fresh))
        end
    end
end

# The macros' happy paths run throughout integration.jl; these pin the
# compile-time errors, which have no request analogue.
@testset "macro errors" begin
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
@testset "optional result type" begin
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
    @testset "forms before the URL are not dropped" begin
        @test_throws ArgumentError @macroexpand @endpoint f(x) -> x -> y -> "url"
    end
end
