# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# The primary suite: exercise the public API end-to-end against the local
# server (test/server.jl), driving the full pipeline a real client uses.

using RestClient
using JSON

# The local HTTP harness (`with_server`/`ServerState`) is included once by
# runtests.jl so the JSON.jl suite can share it.

# Endpoints covering the server's routes, declared as a package user would. The
# base URL is only known at runtime, so requests use the generated
# `func(config, …)` methods rather than `@globalconfig`.
module IntegrationClient
    using RestClient
    using JSON
    @jsondef struct Item
        name::String
        id::Int
    end
    @jsondef struct Paged <: ListResponse{Item}
        items::Vector{Item}
        page::Int
        more::Bool
    end
    @jsondef struct APIError
        message."error"::String
    end
    abstract type AnyAPIError end
    @jsondef struct Refusal <: AnyAPIError
        message."error"::String
    end
    JSON.@choosetype AnyAPIError _ -> Refusal
    RestClient.dataformat(::Type{AnyAPIError}) = RestClient.JSONFormat()
    @jsondef struct Echoed
        method::String
        content_type::String
        body::String
    end
    @jsondef struct Headers
        accept::String
        authorization::String
    end
    @jsondef struct Reflected
        name::String
        id::Int
    end
    @jsondef struct NothingDefaults
        maybe::Union{Int, Nothing}          # defaults to nothing
        flags::Dict{String, Nothing}        # doesn't: Nothing is only a parameter
    end
    @formdef struct LoginForm
        username::String
        remember."remember-me"::Bool = false
    end
    @multipartdef struct UploadForm
        file::Vector{UInt8}
        meta::Item                    # a @jsondef field → JSON part
        note::Union{String, Nothing} = nothing
    end
    @endpoint getitem(id::String) -> "items/{id}" -> Item
    @endpoint getitem_number(id::Int) -> "items/{id}" -> Item          # a non-string path value
    @endpoint search(; q::String, limit::Int) -> "search?{q}&{limit}" -> Vector{Item}
    @endpoint reflect(; q::String) -> "reflect?{q}" -> Reflected
    @endpoint upload(; item::Item) -> item -> "echo" -> Echoed
    @endpoint replace(; item::Item) -> item -> :put("echo") -> Echoed
    @endpoint lookup(; item::Item) -> item -> :get("echo") -> Echoed      # GET with a body
    @endpoint remove(; item::Item) -> item -> :delete("see-other") -> Echoed  # answered with a 303
    @endpoint query(; item::Item) -> item -> :query("query") -> Echoed
    @endpoint login(; form::LoginForm) -> form -> "echo" -> Echoed
    @endpoint multiupload(; form::UploadForm) -> form -> "echo" -> Echoed
    @endpoint exists(path::String) -> :head("{path}") -> Bool
    @endpoint notify(; item::Item) -> item -> :post("echo")   # no result type ⇒ returns nothing
    @endpoint touch_() -> "items/1"                           # bare URL, no result type
    @endpoint ping_cacheable() -> "cacheable"                  # cacheable route, no result type
    @endpoint limited() -> "ratelimited" -> Item
    @endpoint throttled() -> "throttled" -> Item
    @endpoint cached() -> "cacheable" -> Item
    @endpoint misread() -> "cacheable" -> Echoed
    @endpoint cachedage() -> "cacheable" -> Item
    RestClient.postprocess(res::RestClient.Response, ::RestClient.Request{<:Any, CachedageEndpoint}, ::Item) =
        get(res.headers, "age", nothing)
    @endpoint replace_cached(; item::Item) -> item -> :put("cacheable")
    @endpoint nostore() -> "nostore" -> Item
    @endpoint partial() -> "partial" -> Item
    @endpoint errored() -> "error" -> Item
    @endpoint missing_() -> "notfound" -> Item
    @endpoint apierror(status::Int) -> "apierror/{status}" -> Item
    @endpoint misreported() -> "notfound" -> Item
    RestClient.errortype(::Union{ApierrorEndpoint, MisreportedEndpoint}) = APIError
    @endpoint refusal(status::Int) -> "apierror/{status}" -> Item
    RestClient.errortype(::RefusalEndpoint) = AnyAPIError
    @endpoint forbidden() -> "forbidden" -> Item
    @endpoint forbidden_limited() -> "forbidden/limited" -> Item
    @endpoint always429() -> "always429" -> Item
    @endpoint unavailable() -> "unavailable" -> Item
    @endpoint flaky(status::Int) -> "flaky/{status}" -> Item
    @endpoint flaky_post(status::Int; item::Item) -> item -> "flaky/{status}" -> Item
    @endpoint long_limited() -> "ratelimited/long" -> Item
    @endpoint exhausted() -> "exhausted" -> Item
    @endpoint concurrent() -> "concurrent" -> Item
    @endpoint paced() -> "items/1" -> Item
    RestClient.nextallowed(::PacedEndpoint, _) = 1
    @endpoint stalled() -> "items/1" -> Item
    RestClient.nextallowed(::StalledEndpoint, _) = time() + 3600
    @endpoint hasty() -> "ratelimited" -> Item
    RestClient.nextallowed(::HastyEndpoint, _) = time() - 10
    @endpoint teapot() -> "flaky/418" -> Item
    RestClient.nextallowed(::TeapotEndpoint, r) = if r.status == 418 time() end
    @endpoint refused() -> "always429" -> Item
    RestClient.nextallowed(::RefusedEndpoint, r) = if r.status == 429 Inf end
    @endpoint upload_limited(; item::Item) -> item -> "echo/limited" -> Echoed
    @endpoint upload_stream(; data::IO) -> data -> "echo/limited" -> Echoed
    @endpoint upload_bytes(; data::Vector{UInt8}) -> data -> "echo" -> Echoed
    @endpoint text() -> "text" -> String
    @endpoint number() -> "number" -> Int
    @endpoint revalidate() -> "revalidate" -> Item
    @endpoint rotating() -> "rotating" -> Item
    @endpoint struct AuthEndpoint
        showheaders() -> "headers" -> Headers
    end
    RestClient.headers(c::RestClient.RequestConfig, ::AuthEndpoint) =
        if isnothing(c.key) Pair{String, String}[] else ["Authorization" => "Bearer $(c.key)"] end
    # Endpoints declaring each auth scheme (and an :optional policy, so the scheme is
    # applied to config.key when present), to check each reaches the wire.
    @endpoint bearerauth() -> "headers" -> Headers
    RestClient.authscheme(::BearerauthEndpoint) = BearerAuth()
    RestClient.authpolicy(::BearerauthEndpoint) = :optional
    @endpoint basicauth() -> "headers" -> Headers
    RestClient.authscheme(::BasicauthEndpoint) = BasicAuth()
    RestClient.authpolicy(::BasicauthEndpoint) = :optional
    @endpoint headerauth() -> "headers" -> Headers
    RestClient.authscheme(::HeaderauthEndpoint) = HeaderAuth("X-API-Key")
    RestClient.authpolicy(::HeaderauthEndpoint) = :optional
    @endpoint queryauth(; page::Int = 0) -> "paged?{page}" -> Paged
    RestClient.authscheme(::QueryauthEndpoint) = QueryAuth("api_key")
    RestClient.authpolicy(::QueryauthEndpoint) = :optional
    @endpoint linked(; variant::String = "plain") -> "linked?{variant}" -> Paged
    RestClient.authscheme(::LinkedEndpoint) = BearerAuth()
    RestClient.authpolicy(::LinkedEndpoint) = :optional
    @endpoint linkedq(; variant::String = "plain") -> "linked?{variant}" -> Paged
    RestClient.authscheme(::LinkedqEndpoint) = QueryAuth("api_key")
    RestClient.authpolicy(::LinkedqEndpoint) = :optional
    RestClient.pagecount(response, ::LinkedqEndpoint, ::Paged) =
        parse(Int, response.headers["X-Total-Pages"])
    @endpoint struct LinkedOwnEndpoint
        linked_own(; page::Int = 1) -> "linked?{page}" -> Paged
        page::Int = 1
    end
    RestClient.nextpage(e::LinkedOwnEndpoint, ::Paged) = if e.page == 1 LinkedOwnEndpoint(e; page = 3) end
    @endpoint seenlinked() -> "linked" -> Paged
    const SEEN = Int[]
    RestClient.postprocess(response, ::Request{<:Any, SeenlinkedEndpoint}, page::Paged) =
        (push!(SEEN, response.status); page)
    @endpoint checkedlinked() -> "linked" -> Paged
    RestClient.postprocess(::Request{<:Any, CheckedlinkedEndpoint}, page::Paged) =
        if page.more error("unexpected") else page end
    @endpoint gzipped() -> "gzipped" -> Item
    @endpoint struct PagedEndpoint
        listing(; page::Int) -> "paged?{page}" -> Paged
        page::Int = 1
    end
    RestClient.thispagenumber(e::PagedEndpoint) = e.page
    RestClient.nextpage(e::PagedEndpoint, page::Paged) = if page.more PagedEndpoint(e; page = e.page + 1) end
    @endpoint urlpaged() -> "paged" -> Paged
    RestClient.nextpage(::UrlpagedEndpoint, page::Paged) = if page.more "/paged?page=$(page.page + 1)" end
    @endpoint offsitepaged() -> "paged" -> Paged
    RestClient.nextpage(::OffsitepagedEndpoint, ::Paged) = "http://elsewhere.invalid/paged"
end

const IC = IntegrationClient
cfg(baseurl; kw...) = RestClient.RequestConfig(baseurl; kw...)
# Caching writes to the real `RestClient.cachedir()` (not injectable), so each
# test removes what it cached for its URL.
clean_cache(url) = rm(RestClient.cachefolder(url); recursive = true, force = true)

# A wrapper declaring a module-wide auth policy via @globalconfig, to exercise the
# macro-expansion resolution (module default, inheritance, and :auth/:noauth markers).
module AuthClient
    using RestClient
    using JSON
    @jsondef struct Item; name::String; id::Int; end
    @endpoint early(id::String) -> "items/{id}" -> Item            # declared before the default
    @globalconfig RequestConfig("http://unused.test") auth=(BearerAuth(), :required)
    @endpoint inherits(id::String) -> "items/{id}" -> Item          # module default: Bearer, required
    @endpoint :noauth public_(id::String) -> "items/{id}" -> Item    # opt out: off
    @endpoint :authoption optional_(id::String) -> "items/{id}" -> Item  # optional: send key if set
    @endpoint probe(path::String) -> :head("{path}") -> Bool         # HEAD, required
end
const AC = AuthClient

@testset "responses" begin
    @testset "single" begin
        with_server() do baseurl, state
            item = IC.getitem(cfg(baseurl), "42")
            @test item isa IC.Item
            @test item.name == "item-42"
            @test item.id == 42
            @test state.requests == 1
        end
        @testset "numeric path value" begin
            with_server() do baseurl, _
                @test IC.getitem_number(cfg(baseurl), 7).id == 7
            end
        end
    end
    @testset "list" begin
        with_server() do baseurl, _
            items = IC.search(cfg(baseurl), q = "bean", limit = 3)
            @test items isa Vector{IC.Item}
            @test length(items) == 3
            @test items[1].name == "bean-1"
            @test items[3].id == 3
        end
        @testset "empty" begin
            with_server() do baseurl, _
                @test isempty(IC.search(cfg(baseurl), q = "x", limit = 0))
            end
        end
    end
    @testset "raw" begin
        with_server() do baseurl, _
            @test IC.text(cfg(baseurl)) == "plain words"
            @test IC.number(cfg(baseurl)) === 42
            resp = IC.upload_bytes(cfg(baseurl), data = collect(b"\x01\x02abc"))
            @test resp.body == "\x01\x02abc"
            @test resp.content_type == "application/octet-stream"
        end
    end
    @testset "no result type" begin
        @testset "discarded" begin
            # The server replies 200 with a JSON body; the endpoint declares no
            # result type, so the body is dropped and the call returns nothing.
            with_server() do baseurl, _
                @test IC.notify(cfg(baseurl), item = IC.Item("event", 1)) === nothing
                @test IC.touch_(cfg(baseurl)) === nothing
            end
        end
        @testset "never cached" begin
            with_server() do baseurl, state
                IC.ping_cacheable(cfg(baseurl)); IC.ping_cacheable(cfg(baseurl))
                @test state.requests == 2
                @test !isdir(RestClient.cachefolder("$baseurl/cacheable"))
            end
        end
    end
    @testset "`nothing` defaults" begin
        @test IC.NothingDefaults(flags = Dict{String, Nothing}()).maybe === nothing
        @test_throws UndefKeywordError IC.NothingDefaults()
    end
    @testset "error type" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            caught(f, args...) = try f(config, args...) catch err; err end
            err = caught(IC.apierror, 400)
            @test err isa RestClient.ResponseError{IC.APIError}
            @test (err.response.status, err.detail.message) == (400, "bad id")
            @test occursin("bad id", sprint(showerror, err))
            unread = caught(IC.misreported)
            @test unread isa RestClient.ResponseError{Nothing}
            @test isnothing(unread.detail) && unread.body == codeunits("nope")
            @test caught(IC.missing_) isa RestClient.ResponseError{Nothing}
            @test caught(IC.exists, "broken") isa RestClient.ResponseError{Nothing}
            refused = caught(IC.refusal, 400)
            @test refused isa RestClient.ResponseError{IC.AnyAPIError}
            @test refused.detail == IC.Refusal("bad id")
            try
                before = state.requests
                inband = caught(IC.apierror, 200)
                @test inband isa RestClient.ResponseError{IC.APIError}
                @test (inband.response.status, inband.detail.message) == (200, "bad id")
                @test caught(IC.apierror, 200) isa RestClient.ResponseError{IC.APIError}
                @test state.requests == before + 2
            finally
                clean_cache("$baseurl/apierror/200")
            end
        end
    end
    @testset "error status" begin
        with_server() do baseurl, _
            @test_throws RestClient.ResponseError IC.errored(cfg(baseurl))
            @test_throws RestClient.ResponseError IC.missing_(cfg(baseurl))
            # The error carries the server's status and body, not just the fact of failure.
            err = try IC.errored(cfg(baseurl)) catch e; e end
            @test err isa RestClient.ResponseError
            @test err.response.status == 500
            @test err.body == codeunits("boom")
            err404 = try IC.missing_(cfg(baseurl)) catch e; e end
            @test err404.response.status == 404
            @test err404.body == codeunits("nope")
            # showerror surfaces the status and the body text.
            msg = sprint(showerror, err)
            @test occursin("500", msg)
            @test occursin("boom", msg)
            @test err.body == codeunits("boom")      # still intact once shown
        end
    end
    @testset "pagination" begin
        with_server() do baseurl, _
            page1 = IC.listing(cfg(baseurl), page = 1)
            @test page1 isa RestClient.List
            @test RestClient.thispagenumber(page1) == 1
            @test length(page1) == 1
            @test page1[1].name == "p1"
            @test first(page1) isa IC.Item       # List iterates its items
            page2 = nextpage(page1)
            @test RestClient.thispagenumber(page2) == 2
            @test page2[1].name == "p2"
            page3 = nextpage(page2)
            @test page3[1].name == "p3"
            @test isnothing(nextpage(page3))     # more=false at page 3
            byurl = collect(eachpage(IC.urlpaged(cfg(baseurl))))
            @test [page[1].name for page in byurl] == ["p1", "p2", "p3"]
            @test RestClient.thispagenumber.(byurl) == [1, 2, 3]
            @test_throws "not under" nextpage(IC.offsitepaged(cfg(baseurl)))
        end
    end
    @testset "Link header" begin
        with_server() do baseurl, _
            ids(list) = [page[1].id for page in eachpage(list)]
            pages = collect(eachpage(IC.linked(cfg(baseurl; key = "k"))))
            @test [page[1].id for page in pages] == [1, 2, 3]
            @test all(page -> page[1].name == "Bearer k", pages)         # auth still sent
            @test all(page -> page[1].name == "k", eachpage(IC.linkedq(cfg(baseurl; key = "k"))))
            @test RestClient.thispagenumber.(pages) == [1, 2, 3]            # counted along the links
            @test map(RestClient.remainingpages, eachpage(IC.linkedq(cfg(baseurl)))) == [2, 1, 0]
            @test isnothing(nextpage(IC.linked(cfg(baseurl); variant = "self")))  # links to itself
            @test isnothing(nextpage(IC.linkedq(cfg(baseurl; key = "k"); variant = "self")))
            @test ids(IC.linked(cfg(baseurl); variant = "root")) == [1, 2, 3]
            @test isnothing(nextpage(IC.linked(cfg(baseurl); variant = "away")))   # another host
            @test ids(IC.linked_own(cfg(baseurl))) == [1, 3]           # the endpoint's own nextpage first
            @test ids(IC.seenlinked(cfg(baseurl))) == [1, 2, 3]        # postprocess leaves paging intact
            @test IC.SEEN == [200, 200, 200]
            @test ids(IC.checkedlinked(cfg(baseurl))) == [1, 2, 3]
        end
    end
    @testset "lazy iteration" begin
        with_server() do baseurl, state
            page1 = IC.listing(cfg(baseurl), page = 1)
            for page in eachpage(page1), item in page
                @test item.name == "p1"
                break
            end
            @test state.requests == 1            # no page fetched ahead
            @test [item.name for page in eachpage(page1) for item in page] == ["p1", "p2", "p3"]
            @test state.requests == 3            # each later page fetched once
        end
    end
end

@testset "requests" begin
    # Reserved characters must round-trip: the server reflects the decoded value,
    # so a match proves the client percent-encoded them.
    @testset "compressed responses" begin
        with_server() do baseurl, state
            clean_cache("$baseurl/gzipped")
            @test IC.gzipped(cfg(baseurl)).name == "unzipped"
            @test IC.gzipped(cfg(baseurl)).name == "unzipped"
            @test state.requests == 1
        end
    end
    @testset "query encoding" begin
        with_server() do baseurl, _
            for q in ("a b", "x&y=z", "100%", "café", "a/b?c")
                @test IC.reflect(cfg(baseurl), q = q).name == q
            end
        end
    end
    @testset "headers" begin
        @testset "Accept" begin
            with_server() do baseurl, _
                @test IC.showheaders(cfg(baseurl)).accept == "application/json"
            end
        end
        @testset "custom Authorization" begin
            with_server() do baseurl, _
                @test IC.showheaders(cfg(baseurl; key = "s3cr3t")).authorization == "Bearer s3cr3t"
            end
        end
    end
    @testset "user easy_hook" begin
        called = Ref(false)
        previous = RestClient.Downloads.EASY_HOOK[]
        RestClient.Downloads.EASY_HOOK[] = (_easy, _info) -> (called[] = true)
        try
            with_server() do baseurl, _
                @test IC.getitem(cfg(baseurl), "1").id == 1
            end
        finally
            RestClient.Downloads.EASY_HOOK[] = previous
        end
        @test called[]
    end
    # libcurl < 8.13 (e.g. with Julia 1.11) can't be told to obey the status code,
    # and resends the original method without its body.
    @testset "QUERY" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                resp = IC.query(config, item = IC.Item("widgets", 1))
                @test resp.method == "QUERY"
                @test occursin("application/json", resp.content_type)
                @test JSON.parse(resp.body, IC.Item) == IC.Item("widgets", 1)
                @testset "cached per query" begin
                    @test IC.query(config, item = IC.Item("widgets", 1)) == resp
                    @test state.queries == 1                 # the same query, from cache
                    IC.query(config, item = IC.Item("gadgets", 2))
                    @test state.queries == 2                 # a different query is not
                end
            finally
                clean_cache("$baseurl/query")
            end
        end
    end
    @testset "303 See Other" begin
        curl = VersionNumber(match(r"libcurl/([\d.]+)", unsafe_string(RestClient.Downloads.Curl.curl_version()))[1])
        with_server() do baseurl, _
            resp = IC.remove(cfg(baseurl), item = IC.Item("gone", 1))
            if curl >= v"8.13"
                @test resp.method == "GET"          # never a DELETE of the other resource
                @test resp.body == ""
            else
                @test_broken resp.method == "GET"
            end
        end
    end
    @testset "HEAD" begin
        with_server() do baseurl, _
            @test IC.exists(cfg(baseurl), "exists") === true
            @test IC.exists(cfg(baseurl), "missing") === false
            @test_throws RestClient.ResponseError IC.exists(cfg(baseurl), "broken")
        end
    end
    @testset "payloads" begin
        @testset "JSON" begin
            with_server() do baseurl, _
                resp = IC.upload(cfg(baseurl), item = IC.Item("widget", 7))
                @test resp.method == "POST"
                @test occursin("application/json", resp.content_type)
                echoed = JSON.parse(resp.body, IC.Item)
                @test echoed.name == "widget"
                @test echoed.id == 7
            end
        end
        @testset "GET" begin
            with_server() do baseurl, _
                resp = IC.lookup(cfg(baseurl), item = IC.Item("query", 3))
                @test resp.method == "GET"
                @test JSON.parse(resp.body, IC.Item) == IC.Item("query", 3)
            end
        end
        @testset "PUT" begin
            with_server() do baseurl, _
                resp = IC.replace(cfg(baseurl), item = IC.Item("gadget", 9))
                @test resp.method == "PUT"
                @test JSON.parse(resp.body, IC.Item).id == 9
            end
        end
        @testset "form" begin
            with_server() do baseurl, _
                resp = IC.login(cfg(baseurl), form = IC.LoginForm(username = "ada", remember = true))
                @test occursin("application/x-www-form-urlencoded", resp.content_type)
                # The remapped field name appears as `remember-me` in the wire body.
                @test resp.body == "username=ada&remember-me=true"
                @testset "nothing omitted" begin
                    resp = IC.login(cfg(baseurl), form = IC.LoginForm(username = "bob"))
                    @test resp.body == "username=bob&remember-me=false"
                end
            end
        end
        @testset "multipart" begin
            with_server() do baseurl, _
                form = IC.UploadForm(file = b"hello", meta = IC.Item("m", 3), note = "hi")
                resp = IC.multiupload(cfg(baseurl), form = form)
                @test occursin("multipart/form-data; boundary=", resp.content_type)
                body = resp.body
                @test occursin("name=\"file\"; filename=\"file\"", body)
                @test occursin("application/octet-stream", body)   # binary part
                @test occursin("hello", body)
                @test occursin("name=\"meta\"", body)
                @test occursin("application/json", body)            # @jsondef field → JSON part
                @test occursin("name=\"note\"\r\nContent-Type: text/plain; charset=utf-8", body)
                @testset "nothing omitted" begin
                    bare = IC.UploadForm(file = b"x", meta = IC.Item("m", 1))
                    r = IC.multiupload(cfg(baseurl), form = bare)
                    @test !occursin("name=\"note\"", r.body)
                end
            end
        end
    end
end

@testset "auth" begin
    # The scheme is an endpoint fact (authscheme); the secret is the config's `key`.
    @testset "schemes" begin
        @testset "Bearer and Basic" begin
            with_server() do baseurl, _
                @test IC.bearerauth(cfg(baseurl; key = "tok")).authorization == "Bearer tok"
                # Basic: Authorization is "Basic " * base64("ada:lovelace")
                @test IC.basicauth(cfg(baseurl; key = "ada:lovelace")).authorization == "Basic YWRhOmxvdmVsYWNl"
            end
        end
        @testset "no key" begin
            with_server() do baseurl, _
                @test IC.bearerauth(cfg(baseurl)).authorization == ""   # scheme declared, but key is nothing
            end
        end
        # Header/Query routing is wire-independent here (the routes don't echo arbitrary
        # headers, and /paged ignores extra params), so assert through the request funnel.
        @testset "header and query" begin
            hreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), IC.HeaderauthEndpoint())
            @test ("X-API-Key" => "k") in RestClient.headers(hreq)
            qreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), IC.QueryauthEndpoint())
            @test ("api_key" => "k") in RestClient.parameters(qreq)
            # No scheme declared (default NoAuth) ⇒ nothing added, even with a key set.
            plain = RestClient.Request{:get}(cfg("http://x"; key = "k"), IC.PagedEndpoint(1))
            @test isempty(RestClient.headers(plain))
        end
        @testset "key display" begin
            c = cfg("http://x"; key = "topsecret")
            # Plain repr is round-trippable, so it keeps the key verbatim.
            @test occursin("topsecret", repr(c))
            # Limited display (REPL output, error printing) redacts it instead.
            limited = sprint(show, c; context = :limit => true)
            @test !occursin("topsecret", limited)
            @test occursin("*****", limited)
        end
        @testset "transport errors" begin
            # Nothing listens on port 1, so the request fails before any response
            err = try IC.queryauth(cfg("http://127.0.0.1:1"; key = "topsecret")) catch e; e end
            @test err isa TransportError && err.reason === :connect
            @test !occursin("topsecret", sprint(showerror, err))
        end
    end
    @testset "module default" begin
        @testset "inherited" begin
            @test RestClient.authscheme(AC.InheritsEndpoint("1")) == BearerAuth()
            @test RestClient.authpolicy(AC.InheritsEndpoint("1")) === :required
            @test RestClient.globalauthdefault(Val(AuthClient)) == (BearerAuth(), :required)
            @test RestClient.authscheme(AC.EarlyEndpoint("1")) == BearerAuth()
            @test RestClient.authpolicy(AC.EarlyEndpoint("1")) === :required
        end
        @testset "markers" begin
            @test RestClient.authpolicy(AC.PublicEndpoint("1")) === :off      # :noauth turns it off
            @test RestClient.authscheme(AC.OptionalEndpoint("1")) == BearerAuth()  # :authoption keeps scheme
            @test RestClient.authpolicy(AC.OptionalEndpoint("1")) === :optional
        end
        @testset "no policy" begin
            # IntegrationClient declares no @globalconfig auth, so its plain endpoints default.
            @test RestClient.authscheme(IC.PagedEndpoint(1)) == NoAuth()
            @test RestClient.authpolicy(IC.PagedEndpoint(1)) === :off
        end
        @testset "enforcement" begin
            with_server() do baseurl, _
                # :required, no key ⇒ MalformedRequest, no request made.
                @test_throws RestClient.MalformedRequest AC.inherits(cfg(baseurl), "1")
                # With a key it proceeds and the Bearer header is applied automatically.
                @test AC.inherits(cfg(baseurl; key = "tok"), "42").id == 42
                # HEAD requests are validated too.
                @test_throws RestClient.MalformedRequest AC.probe(cfg(baseurl), "exists")
                @test AC.probe(cfg(baseurl; key = "tok"), "exists") === true
                # :noauth endpoint needs no key.
                @test AC.public_(cfg(baseurl), "42").id == 42
                # :optional sends the key when present, but does not require it.
                @test AC.optional_(cfg(baseurl), "42").id == 42                 # no key: fine
                oreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), AC.OptionalEndpoint("1"))
                @test ("Authorization" => "Bearer k") in RestClient.headers(oreq)
                ooff = RestClient.Request{:get}(cfg("http://x"), AC.OptionalEndpoint("1"))
                @test isempty(RestClient.headers(ooff))                         # no key: nothing sent
            end
        end
    end
end

@testset "rate limits" begin
    # The first call gets a 429 + Retry-After; the client must wait and retry.
    @testset "429 retried" begin
        with_server() do baseurl, state
            item = IC.limited(cfg(baseurl))
            @test item.id == 1
            @test state.ratelimit_hits == 1
            @test state.requests == 2            # 429 + successful retry
        end
    end
    @testset "plain 403 not retried" begin
        with_server() do baseurl, state
            @test_throws RestClient.ResponseError IC.forbidden(cfg(baseurl))
            @test state.requests == 1
        end
    end
    @testset "limited 403 retried" begin
        with_server() do baseurl, state
            @test IC.forbidden_limited(cfg(baseurl)).id == 1
            @test state.requests == 2
        end
    end
    # A 503 is retried when the server says when to come back, as with a 429
    @testset "503 with Retry-After retried" begin
        with_server() do baseurl, state
            @test IC.unavailable(cfg(baseurl)).id == 1
            @test state.requests == 2
        end
    end
    # A gateway failure is retried with backoff, but only where repeating the request is safe
    @testset "$status retried when idempotent" for status in (502, 503, 504)
        with_server() do baseurl, state
            @test IC.flaky(cfg(baseurl), status).id == 1
            @test state.requests == 2
        end
    end
    @testset "$status not retried for a POST" for status in (502, 503)
        with_server() do baseurl, state
            @test_throws RestClient.ResponseError IC.flaky_post(cfg(baseurl), status; item = IC.Item("x", 1))
            @test state.requests == 1
        end
    end
    @testset "attempts capped" begin
        with_server() do baseurl, state
            err = try IC.always429(cfg(baseurl)) catch e; e end
            @test err isa RestClient.ResponseError
            @test err.response.status == 429
            @test state.requests == RestClient.MAX_ATTEMPTS
        end
    end
    @testset "wait beyond timeout" begin
        with_server() do baseurl, state
            t = @elapsed err = try IC.long_limited(cfg(baseurl; timeout = 5)) catch e; e end
            @test err isa RestClient.ResponseError && err.response.status == 429
            @test t < 5
            @test state.requests == 1
        end
    end
    @testset "payload resent" begin
        with_server() do baseurl, state
            resp = IC.upload_limited(cfg(baseurl), item = IC.Item("again", 2))
            @test JSON.parse(resp.body, IC.Item) == IC.Item("again", 2)
            @test state.requests == 2
        end
    end
    @testset "unrewindable payload not retried" begin
        with_server() do baseurl, state
            pipe = Base.BufferStream()
            write(pipe, "streamed"); close(pipe)
            err = try IC.upload_stream(cfg(baseurl), data = pipe) catch e; e end
            @test err isa RestClient.ResponseError
            @test err.response.status == 429
            @test state.requests == 1
        end
    end
    # The first 429 closes the shared gate, so staggered callers wait on its
    # deadline instead of sending — fewer 429s than callers. Independent configs
    # would each retrigger the window (429s == callers), so `<` is the real check.
    # `Threads.@spawn` exercises true parallelism when threads exist, tasks otherwise.
    @testset "shared backoff" begin
        ncallers = 8
        with_server() do baseurl, state
            config = cfg(baseurl)
            tasks = Task[]
            for _ in 1:ncallers
                push!(tasks, Threads.@spawn IC.throttled(config))
                sleep(0.3)   # < THROTTLE_RETRY_AFTER: gate still closed when later tasks wake
            end
            items = fetch.(tasks)
            @test all(item -> item.id == 1, items)
            @test 0 < (@atomic state.throttle_429s) < ncallers   # gate suppressed retries
        end
    end
    @testset "simultaneous 429s share one wait" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            start = time()
            items = fetch.([Threads.@spawn IC.throttled(config) for _ in 1:3])
            @test all(item -> item.id == 1, items)
            @test time() - start < 2 * THROTTLE_RETRY_AFTER
        end
    end
    @testset "@globalconfig ratelimit" begin
        @eval module PacedClient
            using RestClient
            @globalconfig RequestConfig("http://paced.test"; ratelimit = RateLimit(3))
        end
        @test RestClient.globalconfig(Val(PacedClient)).ratelimit == RateLimit(3)
    end
    @testset "paced" begin
        with_server() do baseurl, state
            config = cfg(baseurl; ratelimit = RateLimit(5))
            @test @elapsed(fetch.([Threads.@spawn IC.getitem(config, "1") for _ in 1:4])) >= 0.6
            burst = cfg(baseurl; ratelimit = RateLimit(5; burst = 4))
            @test @elapsed(fetch.([Threads.@spawn IC.getitem(burst, "1") for _ in 1:4])) < 0.5
        end
    end
    @testset "cache hits unpaced" begin
        with_server() do baseurl, state
            config = cfg(baseurl; ratelimit = RateLimit(1/5))
            try
                IC.cached(config)
                @test @elapsed(IC.cached(config)) < 1
            finally
                clean_cache("$baseurl/cacheable")
            end
        end
    end
    @testset "in flight" begin
        with_server() do baseurl, state
            single = cfg(baseurl; ratelimit = RateLimit(inflight = 1))
            fetch.([Threads.@spawn IC.concurrent(single) for _ in 1:3])
            @test (@atomic state.peak) == 1
        end
        with_server() do baseurl, state
            config = cfg(baseurl)
            fetch.([Threads.@spawn IC.concurrent(config) for _ in 1:3])
            @test (@atomic state.peak) > 1
        end
    end
    @testset "exhausted budget holds" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            IC.exhausted(config)
            @test @elapsed(IC.getitem(config, "1")) >= 0.9
        end
    end
    @testset "nextallowed" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            IC.paced(config)
            @test @elapsed(IC.paced(config)) >= 0.9
            @test @elapsed(IC.hasty(config)) >= 0.9
            @test state.requests == 4
        end
        with_server() do baseurl, state
            config = cfg(baseurl; timeout = 2)
            IC.stalled(config)
            @test @elapsed(IC.stalled(config)) < 2
        end
        with_server() do baseurl, state
            @test IC.teapot(cfg(baseurl)).id == 1
            @test state.requests == 2
        end
        with_server() do baseurl, state
            @test_throws RestClient.ResponseError IC.flaky(cfg(baseurl), 418)
            @test state.requests == 1
        end
        with_server() do baseurl, state
            err = try IC.refused(cfg(baseurl)) catch e; e end
            @test err isa RestClient.ResponseError && err.response.status == 429
            @test state.requests == 1
        end
    end
    @testset "waits logged" begin
        with_server() do baseurl, state
            @test_logs (:info, r"asked for a pause of") match_mode = :any IC.limited(cfg(baseurl))
        end
    end
    @testset "@globalconfig of a name" begin
        @eval module NamedConfigClient
            using RestClient
            const CONFIG = RequestConfig("http://named.test")
            @globalconfig CONFIG
        end
        @test RestClient.globalconfig(Val(NamedConfigClient)).baseurl == "http://named.test"
    end
end

@testset "caching" begin
    @testset "max-age" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                a = IC.cached(config)
                b = IC.cached(config)
                @test a.id == b.id == 1
                @test state.requests == 1        # second served from disk
            finally
                clean_cache("$baseurl/cacheable")
            end
        end
    end
    @testset "invalidated by a write" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                IC.cached(config); IC.cached(config)
                @test state.requests == 1
                IC.replace_cached(config; item = IC.Item("new", 2))     # RFC 9111 §4.4
                IC.cached(config)
                @test state.requests == 3
            finally
                clean_cache("$baseurl/cacheable")
            end
        end
    end
    @testset "disabled" begin
        with_server() do baseurl, state
            config = cfg(baseurl; cache = false)
            IC.cached(config); IC.cached(config)
            @test state.requests == 2
        end
    end
    @testset "304 revalidation" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                @test IC.revalidate(config).name == "revalidated"   # stored, already stale
                @test IC.revalidate(config).name == "revalidated"   # 304 ⇒ now fresh for 60s
                @test IC.revalidate(config).name == "revalidated"   # served from disk
                @test state.revalidations == 1
                @test state.requests == 2
            finally
                clean_cache("$baseurl/revalidate")
            end
        end
    end
    @testset "entry replaced" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                @test IC.rotating(config).id == 1
                @test IC.rotating(config).id == 2
                @test IC.rotating(config).id == 3
            finally
                clean_cache("$baseurl/rotating")
            end
        end
    end
    @testset "no-store" begin
        with_server() do baseurl, state
            IC.nostore(cfg(baseurl)); IC.nostore(cfg(baseurl))
            @test state.requests == 2
        end
    end
    @testset "partial content" begin
        with_server() do baseurl, state
            try
                IC.partial(cfg(baseurl)); IC.partial(cfg(baseurl))
                @test state.requests == 2
            finally
                clean_cache("$baseurl/partial")
            end
        end
    end
    @testset "age of a stored response" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                @test isnothing(IC.cachedage(config))
                @test parse(Int, IC.cachedage(config)) >= 0
                @test state.requests == 1
            finally
                clean_cache("$baseurl/cacheable")
            end
        end
    end
    @testset "unreadable success" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                @test_throws MalformedResponse IC.misread(config)
                @test_throws MalformedResponse IC.misread(config)
                @test state.requests == 2
                @test IC.cached(config).id == 1
                @test state.requests == 3
            finally
                clean_cache("$baseurl/cacheable")
            end
        end
    end
end
