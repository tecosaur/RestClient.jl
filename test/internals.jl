# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# A thin unit layer for behaviour no request can reach: parsing edge cases,
# cache-file robustness, and macro-expansion errors. The happy paths are all
# covered by integration.jl; prefer adding coverage there.

using Dates
using RestClient: Response, Headers
using Tables
using RestClient: retryafter, unixtime, expirytime, dumpresponse, tryreadresponse, cachesave,
    opencacheentry, readcachestamp, CACHE_STALE_DURATION, HTTP_DATE_FORMAT

hdr(pairs...) = Headers(pairs)
http_date(t) = Dates.format(t, HTTP_DATE_FORMAT)

sample_response(; status = 200,
                headers = ["content-type" => "application/json",
                           "cache-control" => "max-age=600"]) =
    Response("https://api.example.com/thing", status, "HTTP/1.1 $status OK", Headers(headers))

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
struct PagedListEP <: AbstractEndpoint end
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
struct UnreadyEP <: AbstractEndpoint end
RestClient.urlpath(::UnreadyEP) = "p"
RestClient.validate(::UnreadyEP) = "not ready"
RestClient.payload(::UnreadyEP) = error("payload of a request that failed validation")
struct PayloadEP <: AbstractEndpoint
    value::Any
end
RestClient.urlpath(::PayloadEP) = "p"
RestClient.payload(ep::PayloadEP) = ep.value
struct AgentEP <: AbstractEndpoint end
RestClient.urlpath(::AgentEP) = "a"
RestClient.headers(::AgentEP) = ["user-agent" => "MyClient/1.0"]
struct QueryPathEP <: AbstractEndpoint end
RestClient.urlpath(::QueryPathEP) = "search?q=1"
RestClient.authscheme(::QueryPathEP) = QueryAuth("api_key")
RestClient.authpolicy(::QueryPathEP) = :optional
struct QueryEP <: AbstractEndpoint end
RestClient.urlpath(::QueryEP) = "p"
RestClient.authscheme(::QueryEP) = QueryAuth("api_key")
RestClient.authpolicy(::QueryEP) = :optional
struct TokenAuth <: AuthScheme end
RestClient.authheaders(::TokenAuth, key::String) = "X-Token" => "Token $key"
struct CustomSchemeEP <: AbstractEndpoint end
RestClient.urlpath(::CustomSchemeEP) = "p"
RestClient.authscheme(::CustomSchemeEP) = TokenAuth()
RestClient.authpolicy(::CustomSchemeEP) = :optional

RestClient.@multipartdef struct OddNames
    a."we\"ird"::String
end
RestClient.@multipartdef struct DatedParts
    day::Date
end
RestClient.@formdef struct GenericForm{T}
    value."the-value"::T
end
RestClient.@formdef struct NoFields end
module FieldSyntax
    using RestClient, JSON
    @formdef struct Positive
        n::Int
        Positive(n) = if n > 0 new(n) else throw(ArgumentError("n must be positive")) end
    end
    @jsondef struct Derived
        a::Int
        b::Int = a + 1
    end
    @formdef struct OptionalNote
        note::Union{String, Nothing}
    end
    @jsondef struct Defaulted
        a."ay"::Int
        c."see"::Int = 7
        d::Int = c + 1
    end
    @jsondef struct Boxed{T}
        value::T
        label::String = "box"
    end
    @jsondef struct PositiveJSON
        n::Int
        PositiveJSON(n) = if n > 0 new(n) else throw(ArgumentError("n must be positive")) end
    end
    @endpoint defaulted() -> "defaulted" -> Defaulted
    @multipartdef struct Upload
        report::MultipartFile
        attachments::Vector{MultipartFile} = MultipartFile[]
    end
    @multipartdef struct CollectionParts
        tags::Vector{Union{String, Nothing}} = String[]
        files::Vector{Vector{UInt8}} = Vector{UInt8}[]
        derived::Vector{Derived} = Derived[]
    end
end
RestClient.@formdef struct OptionalForm
    a::Union{Int, Nothing} = nothing
    b::Union{String, Nothing} = nothing
end
RestClient.@formdef struct TaggedForm
    tags::Vector{Union{String, Nothing}} = String[]
    pair::Tuple{Int, Int} = (1, 2)
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
struct BrokenBody <: IO end
Base.mark(::BrokenBody) = 0
Base.eof(::BrokenBody) = false
Base.readavailable(::BrokenBody) = throw(ArgumentError("unreadable"))
struct OptionalListContent <: ListResponse{String}
    total::Int
    items::Union{Vector{String}, Nothing}
end
struct BothListContents <: ListResponse{String}
    items::Vector{String}
    extra::Union{Vector{String}, Nothing}
end
struct NoListContent <: ListResponse{String}
    total::Int
end
struct Row
    a::Int
    b::String
end

# Tests

@testset "rate limits" begin
    @testset "retry after" begin
        inabout(seconds) = deadline -> abs(deadline - (time() + seconds)) < 2
        @test retryafter(hdr()) == -Inf
        @test retryafter(hdr("retry-after" => "soon")) == -Inf
        @test retryafter(hdr("x-ratelimit-reset" => "garbage")) == -Inf
        @testset "HTTP date" begin
            @test retryafter(hdr("retry-after" => http_date(now(UTC) + Second(120)))) |> inabout(120)
            @test retryafter(hdr("retry-after" => http_date(now(UTC) - Hour(1)))) |> inabout(-3600)
        end
        @testset "remaining gates reset" begin
            @test retryafter(hdr("x-ratelimit-remaining" => "5", "x-ratelimit-reset" => "60")) == -Inf
            @test retryafter(hdr("x-ratelimit-remaining" => "0", "x-ratelimit-reset" => "45")) |> inabout(45)
            @test retryafter(hdr("x-ratelimit-remaining" => "599.0", "x-ratelimit-reset" => "45")) == -Inf
            @test retryafter(hdr("x-ratelimit-remaining" => "0.0", "x-ratelimit-reset" => "45")) |> inabout(45)
            @test retryafter(hdr("x-ratelimit-reset" => "45")) == -Inf
            @test retryafter(hdr("x-ratelimit-remaining" => "many", "x-ratelimit-reset" => "45")) == -Inf
        end
        @testset "delta or timestamp" begin
            spent(reset) = hdr("x-ratelimit-remaining" => "0", "x-ratelimit-reset" => reset)
            @test retryafter(spent("90")) |> inabout(90)
            @test retryafter(spent(string(trunc(Int, time()) + 100))) |> inabout(100)
        end
        @testset "Retry-After first" begin
            @test retryafter(hdr("retry-after" => "10", "x-ratelimit-remaining" => "0",
                                     "x-ratelimit-reset" => "999")) |> inabout(10)
        end
    end
    @testset "relative or absolute times" begin
        @test abs(unixtime(60) - (time() + 60)) < 1
        @test unixtime(1.7e9) == 1.7e9
        @test unixtime(Inf) == Inf
        @test unixtime(-Inf) == -Inf
    end
    @testset "waiting out a deadline" begin
        config = RequestConfig("http://x")
        @atomic RestClient.throttler(config).arrival = time() + 1
        ticks = Ref(0)
        ticker = @async while time() < RestClient.throttler(config).arrival
            ticks[] += 1
            sleep(0.05)
        end
        waited = @elapsed RestClient.throttled(Request{:get}(config, PlainEP())) do
            RestClient.Incoming(sample_response(), IOBuffer())
        end
        wait(ticker)
        @test waited >= 0.9
        @test ticks[] > 5                          # the wait yields, rather than spinning
    end
    @testset "derived config" begin
        base = RequestConfig("https://api.example.com"; key = "k", ratelimit = RateLimit(3), timeout = 10)
        derived = RequestConfig(base; key = "other", timeout = 300)
        @test (derived.key, derived.timeout) == ("other", 300)
        @test (derived.baseurl, derived.ratelimit, derived.cache) == (base.baseurl, base.ratelimit, base.cache)
        @test isnothing(RequestConfig(base; key = nothing).key)
        @test RequestConfig(base; baseurl = "https://uploads.example.com/").baseurl == "https://uploads.example.com"
        @test_throws ArgumentError RequestConfig(base; timeout = 0)
    end
    @testset "shared pacing" begin
        throttler = RestClient.throttler
        base = RequestConfig("https://api.example.com/v1"; key = "k", ratelimit = RateLimit(3))
        @test throttler(RequestConfig(base; timeout = 300)) === throttler(base)
        @test throttler(RequestConfig("HTTPS://API.example.com/v2"; key = "k")) === throttler(base)
        @test throttler(RequestConfig(base; key = "other")) !== throttler(base)
        @test throttler(RequestConfig(base; key = nothing)) !== throttler(base)
        @test throttler(RequestConfig(base; baseurl = "https://uploads.example.com")) !== throttler(base)
        @test throttler(RequestConfig("https://api.example.com:8443"; key = "k")) !== throttler(base)
    end
    @testset "RateLimit" begin
        @test RateLimit() == RateLimit(Inf; burst = 1, inflight = typemax(Int))
        @test_throws ArgumentError RateLimit(0)
        @test_throws ArgumentError RateLimit(1; burst = 0)
        @test_throws ArgumentError RateLimit(; inflight = 0)
        @test sprint(show, RateLimit()) == "RateLimit()"
        @test sprint(show, RateLimit(0.5; burst = 3, inflight = 2)) == "RateLimit(0.5; burst = 3, inflight = 2)"
        @test sprint(show, RequestConfig("http://x"; ratelimit = RateLimit(3))) ==
            "RequestConfig(\"http://x\"; ratelimit = RateLimit(3.0), cache = true)"
    end
    @testset "headers" begin
        headers = hdr("Content-Type" => "text/plain", "Link" => "<a>", "link" => "<b>")
        @test headers["content-type"] == headers["CONTENT-TYPE"] == "text/plain"
        @test get(headers, "Link", nothing) == "<a>"
        @test isnothing(get(headers, "ETag", nothing))
        @test_throws KeyError headers["ETag"]
        @test collect(headers) == ["content-type" => "text/plain", "link" => "<a>", "link" => "<b>"]
    end
    @testset "transport errors" begin
        req = Request{:get}(RequestConfig("https://api.example.com"; timeout = 30), PlainEP())
        shown(reason, message = "curl says") =
            sprint(showerror, RestClient.TransportError(req, "https://api.example.com/thing", reason, message))
        @test occursin("could not find the host api.example.com", shown(:resolve))
        @test occursin("could not connect to api.example.com", shown(:connect))
        @test occursin("certificate", shown(:tls))
        @test occursin("longer than its timeout of 30.0 seconds", shown(:timeout))
        @test occursin("raise the `timeout`", shown(:silence))
        @test occursin("closed before the response was complete", shown(:dropped))
        @test occursin("redirecting in a loop", shown(:redirects))
        @test all(r -> endswith(shown(r), "(curl says)"), (:resolve, :connect, :tls, :timeout, :silence, :dropped, :redirects))
        @test endswith(shown(:other, "odd"), "failed: odd")
        D = RestClient.Downloads
        failure(code) = D.RequestError("https://api.example.com/thing", code, "curl says",
                                       D.Response("https", "https://api.example.com/thing", 0, "", []))
        reason(code) = RestClient.transporterror(req, failure(code), RestClient.Watchdog(20)).reason
        @test reason(D.Curl.CURLE_COULDNT_CONNECT) === :connect
        @test reason(D.Curl.CURLE_PEER_FAILED_VERIFICATION) === :tls
        @test reason(D.Curl.CURLE_RECV_ERROR) === :dropped
        @test reason(D.Curl.CURLE_ABORTED_BY_CALLBACK) === :other
    end
    @testset "payload resending" begin
        prepare(value) = RestClient.outgoing(Request{:post}(RequestConfig("http://x"), PayloadEP(value)))
        send(out) = read(RestClient.payloadio(out), String)
        encoded = prepare("text")
        @test encoded.payload == codeunits("text")               # encoded once, up front
        @test send(encoded) == send(encoded) == "text"
        seekable = IOBuffer("payload")
        read(seekable, 2)                                        # sent from where it stands
        out = prepare(seekable)
        @test out.payloadstart == 2
        @test send(out) == "yload"
        @test send(out) == "yload"                               # rewound for a retry
        @test isnothing(prepare(Base.BufferStream()).payloadstart)   # can't be resent
    end
end

@testset "caching" begin
    @testset "key covers the whole payload" begin
        key(payload) = RestClient.cachekey("https://api.example.com/q", "QUERY", Pair{String, String}[], payload)
        payload = rand(UInt8, 32768)
        edited = map(1:7:length(payload)) do i
            changed = copy(payload)
            changed[i] += 0x01
            key(changed)
        end
        @test key(payload) ∉ edited
        @test allunique(edited)
    end
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
        @test collect(got.headers) == collect(res.headers)
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
        body(path) = (entry = opencacheentry(path, RestClient.url(req)); try read(last(entry), String) finally close(last(entry)) end)
        @testset "round-trip" begin
            cachesave(req, cfile, RestClient.url(req), sample_response(), IOBuffer(b"body"))
            @test body(cfile) == "body"
            (; expiry, retain) = stamp(cfile)
            @test 595 <= expiry - trunc(Int, time()) <= 601
            @test retain == expiry            # no validator ⇒ not kept once stale
        end
        @testset "another request's entry" begin
            cachesave(req, cfile, RestClient.url(req), sample_response(), IOBuffer(b"body"))
            @test isnothing(opencacheentry(cfile, "https://api.example.com/other"))
            @test isfile(cfile)                                  # kept for its own request
        end
        @testset "replaced" begin
            cachesave(req, cfile, RestClient.url(req), sample_response(), IOBuffer(b"new body"))
            @test body(cfile) == "new body"
        end
        @testset "kept to revalidate" begin
            res = sample_response(headers = ["cache-control" => "max-age=0", "etag" => "\"v1\""])
            cachesave(req, cfile, RestClient.url(req), res, IOBuffer(b"x"))
            (; expiry, retain) = stamp(cfile)
            @test retain - expiry == CACHE_STALE_DURATION
        end
        @testset "unstorable removed" begin
            cachesave(req, cfile, RestClient.url(req), sample_response(headers = ["cache-control" => "no-store"]), IOBuffer(b"x"))
            @test !isfile(cfile)
            cachesave(req, cfile, RestClient.url(req), sample_response(headers = Pair{String, String}[]), IOBuffer(b"x"))
            @test !isfile(cfile)              # stale at once, and no validator
        end
        @testset "DateTime lifetime" begin
            dreq = Request{:get}(RequestConfig("https://api.example.com"), DatedCacheEP())
            cachesave(dreq, cfile, RestClient.url(dreq), sample_response(), IOBuffer(b"x"))
            @test 3595 <= stamp(cfile).expiry - trunc(Int, time()) <= 3601
        end
        @testset "auth keys not stored" begin
            kreq = Request{:get}(RequestConfig("https://api.example.com"; key = "s3cret"), KeyedCacheEP())
            res = Response(RestClient.url(kreq), 200, "HTTP/1.1 200 OK", hdr("cache-control" => "max-age=600"))
            cachesave(kreq, cfile, RestClient.url(kreq), res, IOBuffer(b"x"))
            @test !occursin("s3cret", read(cfile, String))
            @test occursin("api_key=*****", read(cfile, String))
        end
        @testset "failed write warns" begin
            rodir = mktempdir()
            chmod(rodir, 0o500)
            @test_logs (:warn, r"Unable to update the response cache") cachesave(
                req, joinpath(rodir, "sub", "entry.http"), RestClient.url(req), sample_response(), IOBuffer(b"x"))
            dir = mktempdir()
            @test_throws ArgumentError cachesave(req, joinpath(dir, "entry.http"), RestClient.url(req), sample_response(), BrokenBody())
            @test isempty(readdir(dir))                      # no temporary file left behind
        end
        @testset "unreadable removed" begin
            write(cfile, "https://old-format/\nHTTP/1.1 200 OK\n\nbody")
            @test isnothing(opencacheentry(cfile, RestClient.url(req)))
            @test !isfile(cfile)
            @test isnothing(opencacheentry(cfile, RestClient.url(req)))
        end
    end
end

@testset "endpoint macro" begin
    # The macros' happy paths run throughout integration.jl; these pin the
    # compile-time errors, which have no request analogue.
    @testset "errors" begin
        @test_throws Exception @macroexpand @endpoint 42               # not a struct/arrow form
        @test_throws "Unknown @jsondef option" @macroexpand @jsondef :nosho struct J; x::Int; end
        # Function forms take plain names and arguments
        @test_throws "plain name" @macroexpand @endpoint Base.f(x) -> "u" -> String
        @test_throws "Malformed function argument" @macroexpand @endpoint f(xs...) -> "u" -> String
        @test_throws "Malformed function argument" @macroexpand(@endpoint struct SplatEndpoint
            f(xs...) -> "u" -> String
            xs::Vector{Int}
        end)
        @test_throws Exception @macroexpand @formdef 42                # not a struct
        @test_throws Exception @macroexpand @multipartdef (x = 1)      # not a struct
        # `func -> struct -> in -> url -> out` is at most five forms.
        @test_throws Exception @macroexpand(
            @endpoint f() -> struct S end -> S -> "u" -> Bool -> extra)
        # An expression URL needs an explicit result type.
        @test_throws ArgumentError @macroexpand @endpoint f(x::String) -> "a" * x
        @test_throws ArgumentError @macroexpand @endpoint f(x::String) -> x -> "a" * x
        @test_throws ArgumentError @macroexpand @endpoint f(x::String) -> x -> self.path
        @test_throws ArgumentError @macroexpand @endpoint f(x::String, p::String) -> x -> p
    end
    @testset "field syntax" begin
        D = FieldSyntax.Derived
        @test sprint(show, D(a = 1)) == "$D(a=1)"               # a default may use earlier fields
        @test sprint(show, D(a = 1, b = 5)) == "$D(a=1, b=5)"
        @test FieldSyntax.OptionalNote().note === nothing
        @test sprint(RestClient.writepayload, RestClient.FormFormat(), FieldSyntax.Positive(n = 2)) == "n=2"
        @test_throws "positive" FieldSyntax.Positive(n = 0)     # inner constructors are kept
        expand(ex) = macroexpand(FieldSyntax, ex)
        @test_throws "Malformed field" expand(:(@jsondef struct J; x.y::Int; end))
        @test_throws "Malformed field" expand(:(@formdef struct F; x.y::Int; end))
        @test_throws "Malformed field" expand(:(@multipartdef struct M; x.y::Int; end))
        @test_throws "Malformed field" expand(:(@formdef struct F; f(x)::Int; end))
        @test_throws "cannot rename" expand(:(@endpoint struct KeyedEndpoint
            "u" -> String
            x."k"::Int
        end))
    end
    @testset "JSON defaults" begin
        read(T, body) = RestClient.interpretresponse(codeunits(body), RestClient.dataformat(T), T)
        @test read(FieldSyntax.PositiveJSON, """{"n":2}""").n == 2
        @test_throws "positive" read(FieldSyntax.PositiveJSON, """{"n":0}""")   # via the inner constructor
        @test read(FieldSyntax.Defaulted, """{"ay":1}""") == FieldSyntax.Defaulted(a = 1, c = 7, d = 8)
        @test read(FieldSyntax.Defaulted, """{"see":2,"ay":1}""") == FieldSyntax.Defaulted(a = 1, c = 2, d = 3)
        @test read(FieldSyntax.Boxed{Int}, """{"value":3}""") == FieldSyntax.Boxed(value = 3)
        @test_throws "required key \"ay\"" read(FieldSyntax.Defaulted, """{"see":2}""")
    end
    @testset "results by type" begin
        # An endpoint's supertype is its own business: what a request returns follows
        # from the result type alone
        @eval module ResultClient
            using RestClient
            abstract type API <: AbstractEndpoint end
            @endpoint numbers() -> "n" -> Vector{Int}
            @endpoint struct ListedEndpoint <: API
                "l" -> Main.ListContent
            end
        end
        @test supertype(ResultClient.NumbersEndpoint) === AbstractEndpoint
        req = Request{:get}(RequestConfig("http://x"), ResultClient.ListedEndpoint())
        @test RestClient.postprocess(req, ListContent(1, ["x"])) isa ListContent
        listed = RestClient.restructure(sample_response(), req, ListContent(1, ["x"]))
        @test listed isa RestClient.List{String}
        @test isnothing(RestClient.thispagenumber(listed))              # not paged
        @test listed.data.total == 1                                   # the response it came from
        @test isconcretetype(fieldtype(typeof(listed), :request))   # so a next page dispatches statically
        @test RestClient.restructure(sample_response(), Request{:get}(RequestConfig("http://x"), PlainEP()),
                                     OneContent("n", 3)) === 3       # a single response gives its contents
    end
    @testset "result type named like a field" begin
        @eval module FieldNamedResult
            using RestClient
            struct license end
            @endpoint getlicense(key::String; license::Union{String, Nothing}) ->
                "licenses/{key}?{license}" -> license
        end
        @test RestClient.responsetype(FieldNamedResult.GetlicenseEndpoint("mit", nothing)) ===
            FieldNamedResult.license                                 # after a URL, only a type fits
    end
    @testset "copying an endpoint" begin
        @eval module CopyClient
            using RestClient
            @endpoint search(query::String; page::Int = 1) -> "s?{query}&{page}" -> String
            @endpoint struct NoFieldsEndpoint
                "n" -> String
            end
        end
        first = CopyClient.SearchEndpoint("q", 1)
        second = CopyClient.SearchEndpoint(first; page = 2)
        @test (second.query, second.page) == ("q", 2)
        @test CopyClient.SearchEndpoint(first) == first
        @test_throws MethodError CopyClient.SearchEndpoint(first; other = 1)
        @test CopyClient.NoFieldsEndpoint() isa CopyClient.NoFieldsEndpoint
    end
    @testset "module supertype" begin
        @eval module SupertypeClient
            using RestClient
            abstract type API <: AbstractEndpoint end
            abstract type Other <: AbstractEndpoint end
            @globalconfig RequestConfig("http://x") supertype = API
            @endpoint ping() -> "p" -> String
            @endpoint struct StatusEndpoint
                "s" -> String
            end
            @endpoint struct OwnEndpoint <: Other
                "o" -> String
            end
        end
        @test supertype(SupertypeClient.PingEndpoint) === SupertypeClient.API
        @test supertype(SupertypeClient.StatusEndpoint) === SupertypeClient.API
        @test supertype(SupertypeClient.OwnEndpoint) === SupertypeClient.Other    # declared wins
        @eval module SameBlockClient
            using RestClient
            abstract type API <: AbstractEndpoint end
            begin
                @globalconfig RequestConfig("http://x") supertype = API
                @endpoint ping() -> "p" -> String
            end
        end
        @test supertype(SameBlockClient.PingEndpoint) === SameBlockClient.API
        @test_throws "supertype" @eval module BadSupertypeClient
            using RestClient
            @globalconfig RequestConfig("http://x") supertype = Int
        end
    end
    @testset "generated names" begin
        @eval module NamedClient
            using RestClient
            @endpoint getUserInfo() -> "u" -> String
            @endpoint list_repos() -> "r" -> String
            @endpoint ping() -> "p" -> String
        end
        @test all(n -> isdefined(NamedClient, n), (:GetUserInfoEndpoint, :ListReposEndpoint, :PingEndpoint))
    end
    @testset "template variables" begin
        vf(ex) = RestClient.varform(ex; varname = :self, localvars = [:self, :config],
                                    knownfields = [:id, :item], mod = @__MODULE__)
        @test vf(:id) == :(self.id)
        @test vf(:(item.name.first)) == :(self.item.name.first)
        @test vf(:(id.(x))) == :(self.id.(x))
        @test vf(:(config.baseurl)) == :(config.baseurl)        # locals and globals pass through
        @test vf(:(Base.pi)) == :(Base.pi)
        @test vf(:(length(items))) == :(length(items))          # as do other expressions
        @test_throws ArgumentError vf(:undefinedthing)
        @test_throws ArgumentError vf(:(undefinedthing.x))
        # Within an expression, fields are values, but not call heads or keyword names
        @test vf(:(Pair(id, item.name))) == :(Pair(self.id, self.item.name))
        @test vf(:(f(; id = id, item))) == :(f(; id = self.id, self.item))
        @test vf(:((id = id,))) == :((id = self.id,))
        @test vf(:(x -> id)) == :(x -> id)                        # binds its own names
        @test vf(:(j(x for x in id))) == :(j(x for x in self.id))
        @test vf(:([x * id for x in item])) == :([x * self.id for x in self.item])
        @test vf(:([id for id in item if id > 0])) == :([id for id in self.item if id > 0])
        @test vf(:(id(1))) == :(id(1))
        @eval module ClashingGlobals
            clashing = 1
        end
        cf(ex) = RestClient.varform(ex; varname = :self, localvars = [:self, :config],
                                    knownfields = [:clashing, :count], mod = ClashingGlobals)
        @test_throws "write `self.clashing`" cf(:(f(clashing)))
        @test cf(:(f(count))) == :(f(self.count))                  # `count` is only a function
    end
    @testset "URL templates" begin
        interp(str; kw...) = RestClient.interp_curlies(str; varname = :self, localvars = Symbol[],
                                                       knownfields = [:x], mod = @__MODULE__, kw...)
        @test interp("a\\{b}") == "a{b}"                         # escaped brace is literal
        @test interp("a\\\\b") == "a\\b"                         # escaped backslash
        @test interp("a//b///{x}//c"; ispath = true) ==
            Expr(:call, GlobalRef(Base, :string), "a/b/",
                 :($(RestClient.pathsegment)(self.x, "x")), "/c")
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
            @endpoint tagged(tags::Vector{String}) -> "s?tag={tags}&{tags}" -> String
        end
        urlof(endpoint) = RestClient.url(RestClient.Request{:get}(RequestConfig("http://x"), endpoint))
        @test urlof(URLClient.OpEndpoint(true, 1)) == "http://x/page/a?x=true&a=1"
        @test urlof(URLClient.OpEndpoint(false, 2)) == "http://x/page/b?x=false&a=2"
        @test urlof(URLClient.EscapedEndpoint()) == "http://x/s?q=a%20b&c%3Dd=e%26f"
        @test urlof(URLClient.GlobalsEndpoint("z")) == "http://x/s?v=v2&key=z&3=n"
        @test urlof(URLClient.LiteralsEndpoint()) == "http://x/s?n=3&f=true&c=c&1=one"
        @test urlof(URLClient.FlagEndpoint("a b")) == "http://x/s?verbose&q=a%20b"
        @test urlof(URLClient.FlagEndpoint(nothing)) == "http://x/s?verbose"
        @test urlof(URLClient.TaggedEndpoint(["a", "b c"])) == "http://x/s?tag=a&tag=b%20c&tags=a&tags=b%20c"
        @test urlof(URLClient.TaggedEndpoint(String[])) == "http://x/s"
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
    @testset "generated functions" begin
        # A module that only imports RestClient, whose requests stop at validation.
        @eval module SigClient
            import RestClient
            RestClient.@globalconfig RestClient.RequestConfig("http://unused.test")
            RestClient.@endpoint struct FooEndpoint
                foo(n::Int) -> "f?{n}" -> String
                n::Int = 1
                tag::Union{String, Nothing}
                flags::Vector{Nothing}
            end
            RestClient.validate(::FooEndpoint) = "not sent"
            # Only keywords default to `nothing`: these stay required positional arguments
            RestClient.@endpoint lead(a::Union{Int, Nothing}, b::String) -> "x/{b}" -> Vector{UInt8}
            RestClient.@endpoint single(a::Union{Int, Nothing}) -> "y" -> Vector{UInt8}
            # A field default carries over to a positional argument only if nothing required follows
            RestClient.@endpoint paged(page, id) -> struct PagedEndpoint
                page::Int = 1
                id::Int
            end -> "p/{id}?{page}" -> Vector{UInt8}
            RestClient.@endpoint tail(id, page) -> struct TailEndpoint
                page::Int = 1
                id::Int
            end -> "t/{id}?{page}" -> Vector{UInt8}
        end
        config = RequestConfig("http://unused.test")
        @test hasmethod(SigClient.foo, Tuple{Int})                  # the argument keeps its type
        @test !hasmethod(SigClient.foo, Tuple{String})
        # `tag` defaults to `nothing`; `flags` (not Union{…, Nothing}) is required
        @test_throws MalformedRequest SigClient.foo(config, 2; flags = Nothing[])
        @test_throws MalformedRequest SigClient.foo(2; flags = Nothing[])   # via globalconfig
        @test_throws UndefKeywordError SigClient.foo(config, 2)
        @test hasmethod(SigClient.lead, Tuple{Nothing, String})
        @test !hasmethod(SigClient.single, Tuple{})
        @test hasmethod(SigClient.paged, Tuple{Int, Int}) && !hasmethod(SigClient.paged, Tuple{Int})
        @test hasmethod(SigClient.tail, Tuple{Int})
    end
    @testset ":noauth overrides a supertype" begin
        @eval module NoAuthClient
            using RestClient
            abstract type API <: AbstractEndpoint end
            RestClient.authpolicy(::API) = :required
            @endpoint :noauth struct PublicEndpoint <: API
                "public" -> String
            end
        end
        @test RestClient.authpolicy(NoAuthClient.PublicEndpoint()) === :off
    end
    @testset "parametric struct" begin
        @eval module ParamClient
            using RestClient
            @endpoint struct BoxEndpoint{T}
                "box/{x}" -> String
                x::T
            end
        end
        ep = ParamClient.BoxEndpoint{Int}(3)
        @test ep isa AbstractEndpoint
        @test RestClient.urlpath(RequestConfig("http://x"), ep) == "box/3"
    end
    @testset "field docstrings" begin
        @eval module DocClient
            using RestClient
            @endpoint struct DocEndpoint
                "doc/{id}" -> String
                "The item's id"
                id::Int
                "Its revision"
                rev::Int = 1
            end
        end
        @test fieldnames(DocClient.DocEndpoint) == (:id, :rev)
        @test RestClient.urlpath(RequestConfig("http://x"), DocClient.DocEndpoint(id = 4)) == "doc/4"
    end
    @testset "docstrings" begin
        @test_nowarn @eval module DocsClient
            using RestClient
            """
                dothing(input) -> String

            Does the thing.
            """
            @endpoint struct DoThingEndpoint
                dothing(input) -> "thing/{input}" -> String
                input::Int
            end
            "Fetches a widget."
            @endpoint widget(id::Int) -> "widgets/{id}" -> String
            "A plain endpoint."
            @endpoint struct PlainDocEndpoint
                "plain" -> String
            end
        end
        # The raw docstrings (rendering them needs the REPL, absent under Pkg.test)
        doc(name) = join((join(string.(d.text)) for d in values(
            Base.Docs.meta(DocsClient)[Base.Docs.Binding(DocsClient, name)].docs)), '\n')
        @test occursin("Does the thing.", doc(:dothing))
        @test occursin("The request made by `dothing`", doc(:DoThingEndpoint))
        @test !occursin("Does the thing.", doc(:DoThingEndpoint))
        @test occursin("Fetches a widget.", doc(:widget))
        @test occursin("The request made by `widget`", doc(:WidgetEndpoint))
        @test occursin("A plain endpoint.", doc(:PlainDocEndpoint))   # no function
    end
    @testset "chained struct with a field docstring" begin
        @eval module ChainDocClient
            using RestClient
            @endpoint thing(id) -> struct ThingEndpoint
                "The id"
                id::Int
            end -> "t/{id}" -> String
        end
        @test RestClient.urlpath(RequestConfig("http://x"), ChainDocClient.ThingEndpoint(2)) == "t/2"
    end
    @testset "chained struct with a supertype" begin
        @eval module ChainClient
            using RestClient
            abstract type API <: AbstractEndpoint end
            @endpoint chain(x) -> struct ChainEndpoint <: API
                x::Int
            end -> "u/{x}" -> String
        end
        @test supertype(ChainClient.ChainEndpoint) === ChainClient.API   # kept, not inferred
        @test RestClient.urlpath(RequestConfig("http://x"), ChainClient.ChainEndpoint(1)) == "u/1"
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

@testset "API macros" begin
    @eval module ImportAPIClient
        using RestClient
        RestClient.@importapi
        RestClient.@reexport
        struct ThingEndpoint <: AbstractEndpoint end
        urlpath(::ThingEndpoint) = "thing"            # extends RestClient's, unqualified
        isstreamend(::ThingEndpoint, _) = true
        struct PinAuth <: AuthScheme end
        authheaders(::PinAuth, key::String) = "X-Pin" => key
    end
    @test RestClient.urlpath(ImportAPIClient.ThingEndpoint()) == "thing"
    @test RestClient.isstreamend(ImportAPIClient.ThingEndpoint(), nothing)
    @test RestClient.authheaders(ImportAPIClient.PinAuth(), "k") == ("X-Pin" => "k")
    @test [:List, :nextpage, :eachpage] ⊆ names(ImportAPIClient)
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
        @test MalformedRequest(Request{:get}(keyed, PlainEP()), [SubString("ab", 1, 1)]).issues == ["a"]
        @test sprint(showerror, MalformedRequest(Request{:post}(keyed, PlainEP()), ["bad"])) ==
            "Malformed :post request to PlainEP: bad"
        @testset "missing key" begin
            issues = ["bad"]
            err = try check(issues; config = RequestConfig("http://x")) catch e; e end
            @test length(err.issues) == 2 && occursin("requires authentication", err.issues[2])
            @test issues == ["bad"]                              # the caller's vector is untouched
            @test_throws MalformedRequest check(true; config = RequestConfig("http://x"))
        end
        @testset "signing its own requests" begin
            # A key sent by the endpoint's own `headers`, with no scheme, is a valid setup
            @eval module SigningClient
                using RestClient
                @endpoint :auth signed() -> "s" -> String
                RestClient.headers(config::RequestConfig, ::SignedEndpoint) = ["X-Signature" => "signed:" * config.key]
            end
            req = Request{:get}(RequestConfig("http://x"; key = "k"), SigningClient.SignedEndpoint())
            @test isnothing(RestClient.check_validation(req))
            @test ("X-Signature" => "signed:k") in RestClient.headers(req)
            @test_throws MalformedRequest RestClient.check_validation(
                Request{:get}(RequestConfig("http://x"), SigningClient.SignedEndpoint()))
        end
        @testset "before the payload" begin
            @test_throws MalformedRequest RestClient.outgoing(Request{:post}(RequestConfig("http://x"), UnreadyEP()))
        end
    end
    @testset "QUERY needs a payload" begin
        err = try perform(Request{:query}(RequestConfig("http://x"), PlainEP())) catch e; e end
        @test err isa MalformedRequest
        @test occursin("QUERY", only(err.issues))
    end
    @testset "path with a query" begin
        req = Request{:get}(RequestConfig("http://x"; key = "k"), QueryPathEP())
        @test RestClient.url(req) == "http://x/search?q=1&api_key=k"
    end
    @testset "base URL with a trailing slash" begin
        @test RestClient.url(Request{:get}(RequestConfig("http://x/api/"), PlainEP())) == "http://x/api/p"
    end
    @testset "URL paths" begin
        @eval module PathClient
            using RestClient
            @globalconfig RequestConfig("http://x")
            struct RootEP <: AbstractEndpoint end
            RestClient.urlpath(::RootEP) = ""
            struct SlashEP <: AbstractEndpoint end
            RestClient.urlpath(::SlashEP) = "/a"
            struct SubEP <: AbstractEndpoint end
            RestClient.urlpath(::SubEP) = chopprefix("prefix/s", "prefix/")
            @endpoint item(id::Union{Int, Nothing}) -> "items/{id}" -> String
        end
        urlof(ep; base = "http://x") = RestClient.url(Request{:get}(RequestConfig(base), ep))
        @test urlof(PathClient.RootEP(); base = "http://x/graphql") == "http://x/graphql"
        @test urlof(PathClient.SlashEP()) == "http://x/a"
        @test urlof(PathClient.SubEP()) == "http://x/s"
        @test RestClient.encode_uri_component("a,b c/d") == "a,b%20c%2Fd"                # any string, as a SubString
        err = try urlof(PathClient.ItemEndpoint(nothing)) catch e; e end
        @test err isa MalformedRequest && occursin("{id}", only(err.issues))
    end
    @testset "timeout must be positive" begin
        @test_throws "timeout" RequestConfig("http://x"; timeout = 0)
        @test_throws "timeout" RequestConfig("http://x"; timeout = -1)
        @test RequestConfig("http://x"; timeout = 0.5).timeout == 0.5
    end
    @testset "unsupported method" begin
        @test_throws ArgumentError perform(Request{:trace}(RequestConfig("http://x"), PlainEP()))
    end
    @testset "idle limit" begin
        @test RestClient.idlelimit(Inf) == RestClient.idlelimit(5.0) == RestClient.DEFAULT_IDLE_SECONDS
        @test RestClient.idlelimit(60.0) == 60
        @test RestClient.Watchdog(0.2).limit == 0.2
        unlimited = RestClient.Watchdog(Inf)
        unlimited.lastactive -= 1e6
        @test unlimited(0, 0)
    end
    @testset "watchdog" begin
        dog = RestClient.Watchdog(1.0)
        @test dog(0, 0)
        dog.lastactive -= 2
        @test dog(5, 0)                                          # data moved in time
        @test dog(5, 3)                                          # uploading counts too
        dog.lastactive -= 2
        @test !dog(5, 3) && dog.fired                            # silent past the limit
    end
    @testset "auth leaves endpoint lists" begin
        req = Request{:get}(RequestConfig("http://x"; key = "k"), SharedHeadersEP())
        for _ in 1:2
            @test RestClient.headers(req) == ["X-A" => "1", "Authorization" => "Bearer k"]
            @test RestClient.parameters(req) == ["a" => "1"]
            @test RestClient.mimeheaders(req, RestClient.UnspecifiedFormat(), Nothing) ==
                ["X-A" => "1", "Authorization" => "Bearer k", "User-Agent" => RestClient.USER_AGENT]
        end
        @test SHARED_HEADERS == ["X-A" => "1"]
        @test SHARED_PARAMS == ["a" => "1"]
    end
    @testset "tiered headers and parameters" begin
        @eval module TieredClient
            using RestClient
            abstract type API <: AbstractEndpoint end
            abstract type Search <: API end
            @globalconfig RequestConfig("http://x") auth = (BearerAuth(), :optional) supertype = API
            RestClient.headers(::RequestConfig, ::Type{API}) = ["Accept" => "application/vnd.x+json"]
            RestClient.parameters(::RequestConfig, ::Type{API}) = ("site" => "main",)
            RestClient.parameters(config::RequestConfig, ::Type{Search}) =
                if isnothing(config.key); () else ("tier" => "keyed",) end
            @endpoint ping() -> "p" -> String
            @endpoint struct FindEndpoint <: Search
                find(q) -> "f?{q}" -> String
                q::String
            end
            RestClient.headers(::FindEndpoint) = ["X-Find" => "1"]
            RestClient.headers(::RequestConfig, ::Type{FindEndpoint}) = ["X-Kind" => "find"]
        end
        get(endpoint; key = nothing) = Request{:get}(RequestConfig("http://x"; key), endpoint)
        ping, find = TieredClient.PingEndpoint(), TieredClient.FindEndpoint("a b")
        @test RestClient.headers(get(ping)) == ["Accept" => "application/vnd.x+json"]
        @test RestClient.url(get(ping)) == "http://x/p?site=main"
        @test RestClient.headers(get(find; key = "k")) ==
            ["X-Find" => "1", "Authorization" => "Bearer k", "X-Kind" => "find",
             "Accept" => "application/vnd.x+json"]
        @test RestClient.url(get(find)) == "http://x/f?q=a%20b&site=main"
        @test RestClient.url(get(find; key = "k")) == "http://x/f?q=a%20b&tier=keyed&site=main"
        @test RestClient.parameters(RequestConfig("http://x"), Int) === ()
        @test RestClient.headers(Request{:get}(RequestConfig("http://x"), SharedHeadersEP())) ===
            SHARED_HEADERS                                       # nothing to add, so not copied
    end
    @testset "secrets redacted" begin
        breq = Request{:get}(RequestConfig("http://x"; key = "s3cr/t"), SharedHeadersEP())
        qreq = Request{:get}(RequestConfig("http://x"; key = "s3cr/t"), QueryEP())
        @test RestClient.authsecrets(breq) == (header = "Authorization", param = nothing)
        @test RestClient.authsecrets(qreq) == (header = nothing, param = "api_key")
        @test RestClient.authsecrets(Request{:get}(RequestConfig("http://x"; key = "k"), PlainEP())) ==
            (header = nothing, param = nothing)                  # policy :off sends no key
        secrets = (header = "Authorization", param = "api_key")
        shown = string(RestClient.debug_request("GET", RestClient.url(qreq), RestClient.headers(breq),
                                                IOBuffer("body"); secrets))
        @test !occursin("s3cr", shown)
        @test occursin("api_key=*****", shown)
        @test occursin("Authorization: *****", shown)
        @test occursin("X-A: 1", shown)                          # other headers are left alone
        @test occursin("4 bytes", shown)                         # the payload's size
        # A custom scheme's header is found through its `authheaders` method
        creq = Request{:get}(RequestConfig("http://x"; key = "s3cr/t"), CustomSchemeEP())
        shown = string(RestClient.debug_request("GET", RestClient.url(creq), RestClient.headers(creq),
                                                nothing; secrets = RestClient.authsecrets(creq)))
        @test occursin("X-Token: *****", shown)
        # Only the auth parameter's value is hidden, however short the key
        shortreq = Request{:get}(RequestConfig("http://api.example.com"; key = "e"), QueryEP())
        @test RestClient.redactsecrets("http://api.example.com/p?name=eve&api_key=e&x=1",
                                       RestClient.authsecrets(shortreq)) ==
            "http://api.example.com/p?name=eve&api_key=*****&x=1"
        # ...and from a ResponseError's display
        res = Response(RestClient.url(qreq), 500, "HTTP/1.1 500", hdr())
        shownerror = sprint(showerror, RestClient.ResponseError(qreq, res, UInt8[]))
        @test !occursin("s3cr", shownerror)
        @test occursin("api_key=*****", shownerror)
    end
    @testset "long error bodies" begin
        req = Request{:get}(RequestConfig("http://x"), PlainEP())
        res = Response("http://x/p", 500, "HTTP/1.1 500", hdr())
        body = Vector{UInt8}("x"^511 * "é" * "y"^10_000)          # the cut splits `é`
        shown = sprint(showerror, RestClient.ResponseError(req, res, body))
        @test endswith(shown, "x"^511 * "� …")
        ascii = sprint(showerror, RestClient.ResponseError(req, res, Vector{UInt8}("a"^600)))
        @test endswith(ascii, "\n  " * "a"^512 * " …")                   # no character split
    end
    @testset "status lines" begin
        req = Request{:get}(RequestConfig("http://x"), PlainEP())
        shown(message, body = "") = sprint(showerror, RestClient.ResponseError(
            req, Response("http://x/p", 404, message, hdr()), Vector{UInt8}(body)))
        @test startswith(shown("HTTP/2 404"), "HTTP 404 from http://x/p")              # no reason phrase
        @test startswith(shown("HTTP/1.1 404 Not Found"), "HTTP 404 Not Found from")
        @test endswith(shown("HTTP/2 404", "{\n  \"a\": 1\n}"), "\n  {\n    \"a\": 1\n  }")  # every line indented
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
        # Unlabelled, a raw payload is labelled as the text or bytes it is
        labels(T) = RestClient.mimeheaders(Request{:post}(RequestConfig("http://x"), PlainEP()), RawFormat(), T)
        @test ("Content-Type" => "text/plain; charset=utf-8") in labels(String)
        @test ("Content-Type" => "application/octet-stream") in labels(Vector{UInt8})
        # A raw response sets no Accept header (libcurl sends `*/*`).
        sent(ep) = RestClient.mimeheaders(Request{:get}(RequestConfig("http://x"), ep), RestClient.UnspecifiedFormat(), Nothing)
        @test !any(h -> first(h) == "Accept", sent(PlainEP()))
        @test ("User-Agent" => "RestClient.jl/$(pkgversion(RestClient))") in sent(PlainEP())
        @test count(h -> lowercase(first(h)) == "user-agent", sent(AgentEP())) == 1     # the endpoint's own
    end
    @testset "parametric form" begin
        form = GenericForm(value = 1)
        @test RestClient.dataformat(typeof(form)) === RestClient.FormFormat()
        @test sprint(RestClient.writepayload, RestClient.FormFormat(), form) == "the-value=1"
    end
    @testset "API format" begin
        @eval module FormatClient
            using RestClient, JSON
            @globalconfig RequestConfig("http://x") format = :json
            @formdef struct Login; user::String; end
            @endpoint names() -> "n" -> Vector{String}
            @endpoint name() -> "n" -> String
            @endpoint bytes() -> "b" -> Vector{UInt8}
            @endpoint login(user::String) -> Login(user) -> "l" -> String
            @endpoint tag(tags::Vector{String}) -> tags -> :post("t") -> Nothing
            @endpoint words() -> "w" -> Stream{String}
            struct CsvText; text::String; end
            RestClient.dataformat(::Type{CsvText}) = RawFormat()     # explicitly raw
            @endpoint csv() -> "c" -> CsvText
        end
        FC, json = FormatClient, RestClient.JSONFormat()
        # Values without a format of their own take the API's; bytes stay raw
        @test @inferred(RestClient.dataformat(FC.NamesEndpoint(), Vector{String})) === json
        @test RestClient.dataformat(FC.NameEndpoint(), String) === json
        @test RestClient.dataformat(FC.BytesEndpoint(), Vector{UInt8}) === RawFormat()
        @test RestClient.dataformat(FC.LoginEndpoint("u"), FC.Login) === RestClient.FormFormat()
        @test RestClient.dataformat(FC.WordsEndpoint(), Stream{String}) == RestClient.StreamFormat{:SSE}(json)
        @test RestClient.dataformat(PlainEP(), Vector{String}) === RawFormat()   # no API format
        @test RestClient.dataformat(FC.CsvEndpoint(), FC.CsvText) === RawFormat()  # declared raw stays raw
        @test RestClient.dataformat(Vector{String}) === RestClient.UnspecifiedFormat()
        # Resolved when compiled: the call is just its result
        ci = only(code_typed(RestClient.dataformat, Tuple{FC.NamesEndpoint, Type{Vector{String}}}))[1]
        @test count(s -> !Meta.isexpr(s, :code_coverage_effect), ci.code) == 1
        # A JSON string decodes, and a collection payload is sent as JSON
        @test RestClient.interpretresponse(codeunits("\"bob\""), RestClient.dataformat(FC.NameEndpoint(), String), String) == "bob"
        out = RestClient.outgoing(Request{:post}(RequestConfig("http://x"), FC.TagEndpoint(["a", "b"])))
        @test String(copy(out.payload)) == "[\"a\",\"b\"]"
        @test ("Content-Type" => "application/json") in out.headers
        @test_throws "unknown format" @macroexpand @globalconfig RequestConfig("http://x") format = :yaml
    end
    @testset "media types" begin
        @eval module LabelClient
            using RestClient, JSON
            abstract type GitHubAPI <: AbstractEndpoint end
            RestClient.mimetype(::GitHubAPI, ::JSONFormat, ::Type) = MIME("application/vnd.github+json")
            @jsondef struct Repo; name::String; end
            @jsondef struct RepoPatch; name::Union{String, Nothing} = nothing; end
            @endpoint struct RepoEndpoint <: GitHubAPI
                "repo" -> Repo
            end
            @endpoint struct AssetEndpoint <: GitHubAPI
                "asset" -> Vector{UInt8}
            end
            @endpoint struct PatchEndpoint <: GitHubAPI
                patch -> :patch("repo") -> Repo
                patch::RepoPatch
            end
            RestClient.mimetype(::PatchEndpoint, ::JSONFormat, ::Type{RepoPatch}) = "application/merge-patch+json"
        end
        LC, cfg = LabelClient, RequestConfig("http://x")
        sent(kind, ep) = Dict(RestClient.outgoing(Request{kind}(cfg, ep)).headers)
        @test sent(:get, LC.RepoEndpoint())["Accept"] == "application/vnd.github+json"   # the API's JSON label
        @test !haskey(sent(:get, LC.AssetEndpoint()), "Accept")                          # bytes keep none
        patched = sent(:patch, LC.PatchEndpoint(LC.RepoPatch(name = "x")))
        @test patched["Content-Type"] == "application/merge-patch+json"                 # one type's label
        @test patched["Accept"] == "application/vnd.github+json"
        @test RestClient.mimetype(RawFormat(), String) === nothing
        ci = only(code_typed(RestClient.mimetype, Tuple{LC.RepoEndpoint, JSONFormat, Type{LC.Repo}}))[1]
        @test count(s -> !Meta.isexpr(s, :code_coverage_effect), ci.code) == 1
    end
    @testset "Link header" begin
        next(value) = RestClient.nextlink(RequestConfig("https://x/v1"), ["link" => value])
        @test next("<https://x/v1/a?p=2>; rel=\"next\", <https://x/v1/a?p=9>; rel=\"last\"") == "https://x/v1/a?p=2"
        @test next("<https://x/v1/a?p=1>; rel=\"prev first\", </v1/b>; rel=NEXT") == "https://x/v1/b"
        @test next("<https://x/v1/a?p=2>; title=\"a; rel=next, <x>\", <https://x/v1/a?p=3>; rel=\"next\"") == "https://x/v1/a?p=3"
        @test isnothing(next("<https://x/v1/a>; title=\"next\"; rel=\"prev\""))
        @test next("<https://x/v1/c> ; Rel = \"last next\"") == "https://x/v1/c"
        @test next("<https://x/v1/a;v=2>; rel=next") == "https://x/v1/a;v=2"
        @test isnothing(next("<https://x/v1/a; rel=\"next\""))
        @test isnothing(next("<https://elsewhere/v1/a>; rel=\"next\""))    # not under the base URL
        @test isnothing(RestClient.nextlink(RequestConfig("https://x"), ["etag" => "x"]))
    end
    @testset "documented names are public" begin
        api = read(joinpath(@__DIR__, "..", "docs", "src", "api.org"), String)
        documented = Symbol.(m.captures[1] for m in eachmatch(r"^(?:RestClient\.)?([@A-Za-z_][\w!]*)$"m, api))
        @test isempty(filter(n -> isdefined(RestClient, n) && !Base.ispublic(RestClient, n), documented))
        @test :UnspecifiedFormat in documented
    end
    @testset "format package not loaded" begin
        # In a process with neither JSON.jl nor XML.jl, each format says what to load
        script = """
            using RestClient
            for fmt in (RestClient.JSONFormat(), RestClient.XMLFormat())
                for act in (() -> RestClient.interpretresponse(codeunits("[]"), fmt, Vector{String}),
                            () -> sprint(RestClient.writepayload, fmt, ["a"]))
                    try act() catch err println(sprint(showerror, err)) end
                end
            end"""
        messages = readlines(`$(Base.julia_cmd()) --startup-file=no --project=$(Base.active_project()) -e $script`)
        @test length(messages) == 4
        @test all(contains("using JSON"), messages[1:2]) && all(contains("using XML"), messages[3:4])
    end
    @testset "form bodies" begin
        formbody(form) = sprint(RestClient.writepayload, RestClient.FormFormat(), form)
        @test formbody(NoFields()) == ""
        @test formbody(OptionalForm()) == ""
        @test formbody(OptionalForm(b = "x y&z")) == "b=x%20y%26z"
        @test formbody(OptionalForm(a = 1, b = "")) == "a=1&b="
        @test formbody(TaggedForm(tags = ["a", nothing, "b"])) == "tags=a&tags=b&pair=1&pair=2"
        @test formbody(TaggedForm()) == "pair=1&pair=2"
    end
    @testset "multipart boundaries" begin
        boundary(T) = match(r"boundary=(\S+)", RestClient.mimetype(RestClient.MultipartFormat(), T)).captures[1]
        @test boundary(OddNames) != boundary(DatedParts)     # each payload type its own
        @test boundary(OddNames) == boundary(OddNames)
        body = sprint(RestClient.writepayload, RestClient.MultipartFormat(), OddNames(a = "v"))
        @test startswith(body, "--$(boundary(OddNames))\r\n") && endswith(body, "--$(boundary(OddNames))--\r\n")
        # Speck128/128's published test vector (Beaulieu et al. 2013)
        @test RestClient.speck128((0x0706050403020100, 0x0f0e0d0c0b0a0908), (0x6c61766975716520, 0x7469206564616d20)) ==
            (0xa65d985179783265, 0x7860fedf5c570d18)
    end
    @testset "multipart names" begin
        body = sprint(RestClient.writepayload, RestClient.MultipartFormat(), OddNames(a = "v"))
        @test occursin("name=\"we%22ird\"", body)
    end
    @testset "multipart plain values" begin
        body = sprint(RestClient.writepayload, RestClient.MultipartFormat(), DatedParts(day = Date(2026, 1, 2)))
        @test occursin("name=\"day\"\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n2026-01-02\r\n", body)
    end
    @testset "multipart collections" begin
        parts(form) = sprint(RestClient.writepayload, RestClient.MultipartFormat(), form)
        textpart(name, value) = "name=\"$name\"\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n$value\r\n"
        body = parts(FieldSyntax.CollectionParts(tags = ["a", nothing, "b"]))
        @test occursin(textpart("tags", "a"), body) && occursin(textpart("tags", "b"), body)
        @test count("name=\"tags\"", body) == 2              # one part per element, `nothing` left out
        body = parts(FieldSyntax.CollectionParts(files = [b"x", b"y"]))
        @test count("name=\"files\"; filename=\"files\"", body) == 2
        body = parts(FieldSyntax.CollectionParts(derived = [FieldSyntax.Derived(a = 1)]))
        @test count("name=\"derived\"", body) == 1           # a JSON array is one part
        @test occursin("Content-Type: application/json\r\n\r\n[{", body)
        body = parts(FieldSyntax.CollectionParts())
        @test !occursin("name=\"tags\"", body) && !occursin("name=\"files\"", body)   # empty: no parts
    end
    @testset "multipart files" begin
        parts(form) = sprint(RestClient.writepayload, RestClient.MultipartFormat(), form)
        filepart(name, file, type, data) =
            "name=\"$name\"; filename=\"$file\"\r\nContent-Type: $type\r\n\r\n$data\r\n"
        csv = MultipartFile(b"a,b"; filename = "q3.csv", mimetype = "text/csv")
        @test occursin(filepart("report", "q3.csv", "text/csv", "a,b"), parts(FieldSyntax.Upload(report = csv)))
        # By default, a file is named after its field and typed as bytes
        @test occursin(filepart("report", "report", "application/octet-stream", "notes"),
                       parts(FieldSyntax.Upload(report = MultipartFile("notes"))))
        body = parts(FieldSyntax.Upload(report = csv, attachments = [MultipartFile(IOBuffer("x"); filename = "a\"b.txt"),
                                                                     MultipartFile("y"; filename = "c.txt")]))
        @test occursin(filepart("attachments", "a%22b.txt", "application/octet-stream", "x"), body)
        @test occursin(filepart("attachments", "c.txt", "application/octet-stream", "y"), body)
        @test MultipartFile(b"x"; mimetype = MIME("text/csv")).mimetype == "text/csv"
        # A line break would end the part's headers early
        @test_throws ArgumentError MultipartFile(b"x"; mimetype = "text/csv\r\nX-Injected: 1")
        @test_throws ArgumentError MultipartFile(b"x"; mimetype = MIME("text/csv\nX-Injected: 1"))
    end
end

@testset "responses" begin
    @testset "malformed" begin
        req = Request{:get}(RequestConfig("https://api.example.com"), FieldSyntax.DefaultedEndpoint())
        res = Response("https://api.example.com/defaulted", 200, "HTTP/1.1 200 OK", hdr())
        err = try RestClient.deliver(req, RestClient.Incoming(res, IOBuffer("{\"see\":2}"))) catch e; e end
        @test err isa MalformedResponse && err.error isa ArgumentError
        @test err.type === FieldSyntax.Defaulted && err.body == codeunits("{\"see\":2}")
        msg = sprint(showerror, err)
        @test all(s -> occursin(s, msg), ("DefaultedEndpoint", "https://api.example.com/defaulted",
                                          "required key \"ay\"", "{\"see\":2}"))
    end
    @testset "contents" begin
        @test @inferred(RestClient.contents(OneContent("n", 3))) === 3
        @test @inferred(RestClient.contents(ListContent(1, ["x"]))) == ["x"]
        @test_throws ArgumentError RestClient.contents(TwoContents(1, 2))
        # An optional list counts when there's no plain one, absent as empty
        @test @inferred(RestClient.contents(OptionalListContent(1, ["x"]))) == ["x"]
        @test @inferred(RestClient.contents(OptionalListContent(0, nothing))) == String[]
        @test RestClient.contents(BothListContents(["a"], ["b"])) == ["a"]
        @test_throws "No field of type" RestClient.contents(NoListContent(0))
    end
    @testset "List as a table" begin
        rows = RestClient.List{Row}(
            Request{:get}(RequestConfig("http://x"), PagedListEP()), [Row(1, "x"), Row(2, "y")], nothing, sample_response())
        @test Tables.istable(rows)
        @test Tables.columntable(rows) == (a = [1, 2], b = ["x", "y"])
        @test Tables.schema(rows).names == (:a, :b)
        strings = RestClient.List{String}(
            Request{:get}(RequestConfig("http://x"), PagedListEP()), ["x"], nothing, sample_response())
        @test !Tables.istable(strings)                           # String has no columns
        @test isnothing(Tables.schema(strings))                  # unknown, not empty
    end
    @testset "List display" begin
        list = RestClient.List{String}(
            Request{:get}(RequestConfig("http://x"), PagedListEP()), string.(1:100), nothing, sample_response())
        bullets(rows; limit = true) = count(startswith("  • "), split(
            sprint(show, MIME("text/plain"), list; context = (:displaysize => (rows, 80), :limit => limit)), '\n'))
        shown = sprint(show, MIME("text/plain"), list; context = (:displaysize => (14, 80), :limit => true))
        @test bullets(14) == 7                                   # 14 rows - 4 margin - 3 marker
        @test occursin("93 items omitted", shown)
        @test occursin("\"1\"", shown) && occursin("\"100\"", shown)
        @test bullets(2) == 2
        @test bullets(14; limit = false) == 100                  # e.g. written to a file
    end
end
