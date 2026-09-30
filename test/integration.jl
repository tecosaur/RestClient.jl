# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

# The primary suite: exercise the public API end-to-end against the local
# server (test/server.jl), driving the full pipeline a real client uses.

using RestClient
using JSON3

# The local HTTP harness (`with_server`/`ServerState`) is included once by
# runtests.jl so the JSON.jl suite can share it.

# Endpoints covering the server's routes, declared as a package user would. The
# base URL is only known at runtime, so requests use the generated
# `func(config, …)` methods rather than `@globalconfig`.
module IntegrationClient
    using RestClient
    using JSON3
    @jsondef struct Item
        name::String
        id::Int
    end
    @jsondef struct Paged <: ListResponse{Item}
        items::Vector{Item}
        page::Int
        more::Bool
    end
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
    @endpoint search(; q::String, limit::Int) -> "search?{q}&{limit}" -> Vector{Item}
    @endpoint reflect(; q::String) -> "reflect?{q}" -> Reflected
    @endpoint upload(; item::Item) -> item -> "echo" -> Echoed
    @endpoint replace(; item::Item) -> item -> :put("echo") -> Echoed
    @endpoint login(; form::LoginForm) -> form -> "echo" -> Echoed
    @endpoint multiupload(; form::UploadForm) -> form -> "echo" -> Echoed
    @endpoint exists(path::String) -> :head("{path}") -> Bool
    @endpoint notify(; item::Item) -> item -> :post("echo")   # no result type ⇒ returns nothing
    @endpoint touch_() -> "items/1"                           # bare URL, no result type
    @endpoint ping_cacheable() -> "cacheable"                  # cacheable route, no result type
    @endpoint limited() -> "ratelimited" -> Item
    @endpoint throttled() -> "throttled" -> Item
    @endpoint cached() -> "cacheable" -> Item
    @endpoint nostore() -> "nostore" -> Item
    @endpoint errored() -> "error" -> Item
    @endpoint missing_() -> "notfound" -> Item
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
    @endpoint struct PagedEndpoint
        listing(; page::Int) -> "paged?{page}" -> Paged
        page::Int = 1
    end
    RestClient.thispagenumber(e::PagedEndpoint) = e.page
    RestClient.nextpage(e::PagedEndpoint) =
        if e.page < 3 RestClient.setfield(e, :page, e.page + 1) end
end

const IC = IntegrationClient
cfg(baseurl; kw...) = RestClient.RequestConfig(baseurl; kw...)

# A wrapper declaring a module-wide auth policy via @globalconfig, to exercise the
# macro-expansion resolution (module default, inheritance, and :auth/:noauth markers).
module AuthClient
    using RestClient
    using JSON3
    @jsondef struct Item; name::String; id::Int; end
    @globalconfig RequestConfig("http://unused.test") auth=(BearerAuth(), :required)
    @endpoint inherits(id::String) -> "items/{id}" -> Item          # module default: Bearer, required
    @endpoint :noauth public_(id::String) -> "items/{id}" -> Item    # opt out: off
    @endpoint :authoption optional_(id::String) -> "items/{id}" -> Item  # optional: send key if set
end
const AC = AuthClient

@testset "GET → typed single response" begin
    with_server() do baseurl, state
        item = IC.getitem(cfg(baseurl), "42")
        @test item isa IC.Item
        @test item.name == "item-42"
        @test item.id == 42
        @test state.requests == 1
    end
end

@testset "GET list → Vector of typed items" begin
    with_server() do baseurl, _
        items = IC.search(cfg(baseurl), q = "bean", limit = 3)
        @test items isa Vector{IC.Item}
        @test length(items) == 3
        @test items[1].name == "bean-1"
        @test items[3].id == 3
    end
    @testset "empty list" begin
        with_server() do baseurl, _
            @test isempty(IC.search(cfg(baseurl), q = "x", limit = 0))
        end
    end
end

# Reserved characters must round-trip: the server reflects the decoded value,
# so a match proves the client percent-encoded them.
@testset "query parameters are URI-encoded" begin
    with_server() do baseurl, _
        for q in ("a b", "x&y=z", "100%", "café", "a/b?c")
            @test IC.reflect(cfg(baseurl), q = q).name == q
        end
    end
end

@testset "request headers" begin
    @testset "Accept set from response format" begin
        with_server() do baseurl, _
            @test IC.showheaders(cfg(baseurl)).accept == "application/json"
        end
    end
    @testset "custom Authorization header from config key" begin
        with_server() do baseurl, _
            @test IC.showheaders(cfg(baseurl; key = "s3cr3t")).authorization == "Bearer s3cr3t"
        end
    end
end

@testset "authentication schemes" begin
    # The scheme is an endpoint fact (authscheme); the secret is the config's `key`.
    @testset "Bearer/Basic applied to key, reach Authorization" begin
        with_server() do baseurl, _
            @test IC.bearerauth(cfg(baseurl; key = "tok")).authorization == "Bearer tok"
            # Basic: Authorization is "Basic " * base64("ada:lovelace")
            @test IC.basicauth(cfg(baseurl; key = "ada:lovelace")).authorization == "Basic YWRhOmxvdmVsYWNl"
        end
    end
    @testset "no key ⇒ no Authorization sent" begin
        with_server() do baseurl, _
            @test IC.bearerauth(cfg(baseurl)).authorization == ""   # scheme declared, but key is nothing
        end
    end
    # Header/Query routing is wire-independent here (the routes don't echo arbitrary
    # headers, and /paged ignores extra params), so assert through the request funnel.
    @testset "scheme routing through the request funnel" begin
        hreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), IC.HeaderauthEndpoint())
        @test ("X-API-Key" => "k") in RestClient.headers(hreq)
        qreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), IC.QueryauthEndpoint())
        @test ("api_key" => "k") in RestClient.parameters(qreq)
        # No scheme declared (default NoAuth) ⇒ nothing added, even with a key set.
        plain = RestClient.Request{:get}(cfg("http://x"; key = "k"), IC.PagedEndpoint(1))
        @test isempty(RestClient.headers(plain))
    end
    @testset "secret key is redacted in limited display, but repr round-trips" begin
        c = cfg("http://x"; key = "topsecret")
        # Plain repr is round-trippable, so it keeps the key verbatim.
        @test occursin("topsecret", repr(c))
        # Limited display (REPL output, error printing) redacts it instead.
        limited = sprint(show, c; context = :limit => true)
        @test !occursin("topsecret", limited)
        @test occursin("*****", limited)
    end
end

@testset "module-wide auth via @globalconfig" begin
    @testset "endpoints inherit the module scheme and policy" begin
        @test RestClient.authscheme(AC.InheritsEndpoint("1")) == BearerAuth()
        @test RestClient.authpolicy(AC.InheritsEndpoint("1")) === :required
        @test RestClient.globalauthdefault(Val(AuthClient)) == (BearerAuth(), :required)
    end
    @testset "markers override the policy" begin
        @test RestClient.authscheme(AC.Public_Endpoint("1")) == NoAuth()    # :noauth drops scheme
        @test RestClient.authpolicy(AC.Public_Endpoint("1")) === :off
        @test RestClient.authscheme(AC.Optional_Endpoint("1")) == BearerAuth()  # :authoption keeps scheme
        @test RestClient.authpolicy(AC.Optional_Endpoint("1")) === :optional
    end
    @testset "a module with no policy stays off" begin
        # IntegrationClient declares no @globalconfig auth, so its plain endpoints default.
        @test RestClient.authscheme(IC.PagedEndpoint(1)) == NoAuth()
        @test RestClient.authpolicy(IC.PagedEndpoint(1)) === :off
    end
    @testset "policy governs sending and validation" begin
        with_server() do baseurl, _
            # :required, no key ⇒ MalformedRequest, no request made.
            @test_throws RestClient.MalformedRequest AC.inherits(cfg(baseurl), "1")
            # With a key it proceeds and the Bearer header is applied automatically.
            @test AC.inherits(cfg(baseurl; key = "tok"), "42").id == 42
            # :noauth endpoint needs no key.
            @test AC.public_(cfg(baseurl), "42").id == 42
            # :optional sends the key when present, but does not require it.
            @test AC.optional_(cfg(baseurl), "42").id == 42                 # no key: fine
            oreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), AC.Optional_Endpoint("1"))
            @test ("Authorization" => "Bearer k") in RestClient.headers(oreq)
            ooff = RestClient.Request{:get}(cfg("http://x"), AC.Optional_Endpoint("1"))
            @test isempty(RestClient.headers(ooff))                         # no key: nothing sent
        end
    end
end

@testset "POST/PUT payload round-trip" begin
    with_server() do baseurl, _
        @testset "POST sends JSON body with Content-Type" begin
            resp = IC.upload(cfg(baseurl), item = IC.Item("widget", 7))
            @test resp.method == "POST"
            @test occursin("application/json", resp.content_type)
            echoed = JSON3.read(resp.body, IC.Item)
            @test echoed.name == "widget"
            @test echoed.id == 7
        end
        @testset "PUT uses the PUT method" begin
            resp = IC.replace(cfg(baseurl), item = IC.Item("gadget", 9))
            @test resp.method == "PUT"
            @test JSON3.read(resp.body, IC.Item).id == 9
        end
        @testset "no result type ⇒ returns nothing, body discarded" begin
            # The server replies 200 with a JSON body; the endpoint declares no
            # result type, so the body is dropped and the call returns nothing.
            @test IC.notify(cfg(baseurl), item = IC.Item("event", 1)) === nothing
            @test IC.touch_(cfg(baseurl)) === nothing
        end
    end
    @testset "no result type ⇒ never cached" begin
        with_server() do baseurl, state
            IC.ping_cacheable(cfg(baseurl)); IC.ping_cacheable(cfg(baseurl))
            @test state.requests == 2
            @test !isfile(joinpath(RestClient.cachedir(),
                                   RestClient.cachekey("$baseurl/cacheable", "GET",
                                                       Pair{String, String}[], nothing) * ".http"))
        end
    end
end

@testset "@formdef → urlencoded body" begin
    with_server() do baseurl, _
        resp = IC.login(cfg(baseurl), form = IC.LoginForm(username = "ada", remember = true))
        @test occursin("application/x-www-form-urlencoded", resp.content_type)
        # The remapped field name appears as `remember-me` in the wire body.
        @test resp.body == "username=ada&remember-me=true"
    end
    @testset "nothing-valued fields are omitted" begin
        with_server() do baseurl, _
            resp = IC.login(cfg(baseurl), form = IC.LoginForm(username = "bob"))
            @test resp.body == "username=bob&remember-me=false"
        end
    end
end

@testset "@multipartdef → multipart body" begin
    with_server() do baseurl, _
        form = IC.UploadForm(file = b"hello", meta = IC.Item("m", 3), note = "hi")
        resp = IC.multiupload(cfg(baseurl), form = form)
        @test occursin("multipart/form-data; boundary=", resp.content_type)
        body = resp.body
        @test occursin("name=\"file\"", body)
        @test occursin("application/octet-stream", body)   # binary part
        @test occursin("hello", body)
        @test occursin("name=\"meta\"", body)
        @test occursin("application/json", body)            # @jsondef field → JSON part
        @test occursin("name=\"note\"", body)
        @testset "nothing parts omitted" begin
            bare = IC.UploadForm(file = b"x", meta = IC.Item("m", 1))
            r = IC.multiupload(cfg(baseurl), form = bare)
            @test !occursin("name=\"note\"", r.body)
        end
    end
end

@testset "HEAD existence probe" begin
    with_server() do baseurl, _
        @test IC.exists(cfg(baseurl), "exists") === true
        @test IC.exists(cfg(baseurl), "missing") === false
    end
end

@testset "non-2xx responses raise" begin
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
    end
end

@testset "pagination via nextpage" begin
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
    end
end

# The first call gets a 429 + Retry-After; the client must wait and retry.
@testset "rate-limit backoff retries" begin
    with_server() do baseurl, state
        item = IC.limited(cfg(baseurl))
        @test item.id == 1
        @test state.ratelimit_hits == 1
        @test state.requests == 2            # 429 + successful retry
    end
end

# The first 429 closes the shared gate, so staggered callers wait on its
# deadline instead of sending — fewer 429s than callers. Independent configs
# would each retrigger the window (429s == callers), so `<` is the real check.
# `Threads.@spawn` exercises true parallelism when threads exist, tasks otherwise.
@testset "rate-limit backoff is shared across concurrent requests" begin
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

# Caching writes to the real `RestClient.cachedir()` (not injectable), so each
# test cleans up the exact file it creates via the reconstructed key.
function clean_cachekey(url, method, headers)
    cfile = joinpath(RestClient.cachedir(),
                     RestClient.cachekey(url, method, headers, nothing) * ".http")
    isfile(cfile) && rm(cfile)
end

@testset "HTTP caching" begin
    @testset "max-age serves the second request from cache" begin
        with_server() do baseurl, state
            config = cfg(baseurl)
            try
                a = IC.cached(config)
                b = IC.cached(config)
                @test a.id == b.id == 1
                @test state.requests == 1        # second served from disk
            finally
                clean_cachekey("$baseurl/cacheable", "GET",
                               ["Accept" => "application/json"])
            end
        end
    end
    @testset "cache=false bypasses the cache" begin
        with_server() do baseurl, state
            config = cfg(baseurl; cache = false)
            IC.cached(config); IC.cached(config)
            @test state.requests == 2
        end
    end
    @testset "Cache-Control: no-store is never cached" begin
        with_server() do baseurl, state
            IC.nostore(cfg(baseurl)); IC.nostore(cfg(baseurl))
            @test state.requests == 2
        end
    end
end
