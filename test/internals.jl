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

sample_response(; status = 200,
                headers = ["content-type" => "application/json",
                           "cache-control" => "max-age=600"]) =
    Response("https", "https://api.example.com/thing", status,
             "HTTP/1.1 $status OK", Pair{String, String}[headers...])
struct QueryPathEP <: AbstractEndpoint end
RestClient.urlpath(::QueryPathEP) = "search?q=1"
RestClient.authscheme(::QueryPathEP) = QueryAuth("api_key")
RestClient.authpolicy(::QueryPathEP) = :optional

# Tests

@testset "rate limits" begin
    @testset "retry delay" begin
        @test retrydelay(hdr()) == 0
        @test retrydelay(hdr("retry-after" => "soon")) == 0
        @test retrydelay(hdr("x-ratelimit-reset" => "garbage")) == 0
        @testset "HTTP date" begin
            @test 115 <= retrydelay(hdr("retry-after" => http_date(now(UTC) + Second(120)))) <= 121
            @test retrydelay(hdr("retry-after" => http_date(now(UTC) - Hour(1)))) == 0
        end
        @testset "remaining gates reset" begin
            @test retrydelay(hdr("x-ratelimit-remaining" => "5", "x-ratelimit-reset" => "60")) == 0
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
end

@testset "caching" begin
    @testset "freshness" begin
        now_unix = trunc(Int, time())
        @test expirytime(hdr()) == 0
        @test expirytime(hdr("cache-control" => "max-age=notanumber")) == 0
        @test expirytime(hdr("expires" => "whenever")) == 0
        @testset "not storable" begin
            @test expirytime(hdr("cache-control" => "no-store")) == 0
            @test expirytime(hdr("cache-control" => "no-cache")) == 0
            @test expirytime(hdr("cache-control" => "private, max-age=600")) == 0
            @test expirytime(hdr("vary" => "*", "cache-control" => "max-age=600")) == 0
        end
        @testset "lifetime" begin
            @test 595 <= expirytime(hdr("cache-control" => "max-age=600")) - now_unix <= 601
            @test 295 <= expirytime(hdr("cache-control" => "public, s-maxage=300")) - now_unix <= 301
            stamp = http_date(now(UTC) + Hour(1))
            @test expirytime(hdr("expires" => stamp)) > now_unix
            # max-age wins over Expires (RFC 9111 §4.2.1).
            @test expirytime(hdr("cache-control" => "max-age=10", "expires" => stamp)) - now_unix <= 11
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
            Expr(:call, GlobalRef(Base, :string), "a/b/",
                 :($(RestClient.encode_uri_component)($string(self.x))), "/c")
        @test interp("a//b") == "a//b"                           # only paths are collapsed
        @eval module URLClient
            using RestClient
            @endpoint struct OpEndpoint
                op(new::Bool, a::Int) ->
                    "page/{self.new ? \"a\" : \"b\"}?x={self.a == 1 && self.new}&{a}" -> String
                new::Bool
                a::Int
            end
            @endpoint escaped() -> "s?q=a%20b&c%3Dd=e%26f" -> String
            const VERSION_TAG = "v2"
            const KEYNAME = "key"
            const N = 3
            @endpoint globals(q::String) -> "s?v={VERSION_TAG}&{KEYNAME}={q}&{N + 0}=n" -> String
            @endpoint literals() -> "s?n={3}&f={true}&c={'c'}&{1}=one" -> String
            @endpoint flag(q::Union{String, Nothing}) -> "s?verbose&{q}" -> String
        end
        urlof(endpoint) = RestClient.url(RestClient.Request{:get}(RequestConfig("http://x"), endpoint))
        @test urlof(URLClient.OpEndpoint(true, 1)) == "http://x/page/a?x=true&a=1"
        @test urlof(URLClient.OpEndpoint(false, 2)) == "http://x/page/b?x=false&a=2"
        @test urlof(URLClient.EscapedEndpoint()) == "http://x/s?q=a%20b&c%3Dd=e%26f"
        @test urlof(URLClient.GlobalsEndpoint("z")) == "http://x/s?v=v2&key=z&3=n"
        @test urlof(URLClient.LiteralsEndpoint()) == "http://x/s?n=3&f=true&c=c&1=one"
        @test urlof(URLClient.FlagEndpoint("a b")) == "http://x/s?verbose&q=a%20b"
        @test urlof(URLClient.FlagEndpoint(nothing)) == "http://x/s?verbose"
        @test RestClient.decode_uri_component("a%C3%A9%2x%") == "aé%2x%"
        @test RestClient.split_template("a?b=c\\", '?') == ["a", "b=c\\"]   # trailing `\` is literal
        @test RestClient.split_template("a\\", '&') == ["a\\"]
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
    @testset "anaphoric self" begin
        @eval module SelfClient
            using RestClient
            @endpoint struct ItemEndpoint
                self -> "items/{self.id}" -> String
                id::Int
            end
        end
        ep = SelfClient.ItemEndpoint(7)
        @test RestClient.urlpath(RequestConfig("http://x"), ep) == "items/7"
        @test RestClient.payload(RequestConfig("http://x"), ep) === ep
    end
end

@testset "requests" begin
    @testset "path with a query" begin
        req = Request{:get}(RequestConfig("http://x"; key = "k"), QueryPathEP())
        @test RestClient.url(req) == "http://x/search?q=1&api_key=k"
    end
end
