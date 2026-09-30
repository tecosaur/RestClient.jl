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
    @endpoint nostore() -> "nostore" -> Item
    @endpoint errored() -> "error" -> Item
    @endpoint missing_() -> "notfound" -> Item
    @endpoint forbidden() -> "forbidden" -> Item
    @endpoint forbidden_limited() -> "forbidden/limited" -> Item
    @endpoint always429() -> "always429" -> Item
    @endpoint long_limited() -> "ratelimited/long" -> Item
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
                @test !isfile(joinpath(RestClient.cachedir(),
                                       RestClient.cachekey("$baseurl/cacheable", "GET",
                                                           Pair{String, String}[], nothing) * ".http"))
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
        end
    end
end

@testset "requests" begin
    # Reserved characters must round-trip: the server reflects the decoded value,
    # so a match proves the client percent-encoded them.
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
            outgoing(item) = RestClient.outgoing(RestClient.Request{:query}(config, IC.QueryEndpoint(item)))
            try
                resp = IC.query(config, item = IC.Item("widgets", 1))
                @test resp.method == "QUERY"
                @test occursin("application/json", resp.content_type)
                @test JSON3.read(resp.body, IC.Item) == IC.Item("widgets", 1)
                @testset "cached per query" begin
                    @test IC.query(config, item = IC.Item("widgets", 1)) == resp
                    @test state.queries == 1                 # the same query, from cache
                    IC.query(config, item = IC.Item("gadgets", 2))
                    @test state.queries == 2                 # a different query is not
                end
            finally
                for item in (IC.Item("widgets", 1), IC.Item("gadgets", 2))
                    out = outgoing(item)
                    rm(joinpath(RestClient.cachedir(),
                                RestClient.cachekey(out.url, out.method, out.headers, out.payload) * ".http");
                       force = true)
                end
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
                echoed = JSON3.read(resp.body, IC.Item)
                @test echoed.name == "widget"
                @test echoed.id == 7
            end
        end
        @testset "GET" begin
            with_server() do baseurl, _
                resp = IC.lookup(cfg(baseurl), item = IC.Item("query", 3))
                @test resp.method == "GET"
                @test JSON3.read(resp.body, IC.Item) == IC.Item("query", 3)
            end
        end
        @testset "PUT" begin
            with_server() do baseurl, _
                resp = IC.replace(cfg(baseurl), item = IC.Item("gadget", 9))
                @test resp.method == "PUT"
                @test JSON3.read(resp.body, IC.Item).id == 9
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
    end
    @testset "module default" begin
        @testset "inherited" begin
            @test RestClient.authscheme(AC.InheritsEndpoint("1")) == BearerAuth()
            @test RestClient.authpolicy(AC.InheritsEndpoint("1")) === :required
            @test RestClient.globalauthdefault(Val(AuthClient)) == (BearerAuth(), :required)
        end
        @testset "markers" begin
            @test RestClient.authscheme(AC.Public_Endpoint("1")) == NoAuth()    # :noauth drops scheme
            @test RestClient.authpolicy(AC.Public_Endpoint("1")) === :off
            @test RestClient.authscheme(AC.Optional_Endpoint("1")) == BearerAuth()  # :authoption keeps scheme
            @test RestClient.authpolicy(AC.Optional_Endpoint("1")) === :optional
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
                oreq = RestClient.Request{:get}(cfg("http://x"; key = "k"), AC.Optional_Endpoint("1"))
                @test ("Authorization" => "Bearer k") in RestClient.headers(oreq)
                ooff = RestClient.Request{:get}(cfg("http://x"), AC.Optional_Endpoint("1"))
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
    @testset "attempts capped" begin
        with_server() do baseurl, state
            err = try IC.always429(cfg(baseurl)) catch e; e end
            @test err isa RestClient.ResponseError
            @test err.response.status == 429
            @test state.requests == RestClient.RATELIMIT_MAX_ATTEMPTS
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
            @test JSON3.read(resp.body, IC.Item) == IC.Item("again", 2)
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
    @testset "@globalconfig gate" begin
        @test RestClient.globalconfig(Val(AuthClient)).rategate ===
            RestClient.globalconfig(Val(AuthClient)).rategate
    end
end

# Caching writes to the real `RestClient.cachedir()` (not injectable), so each
# test cleans up the exact file it creates via the reconstructed key.
function clean_cachekey(url, method, headers)
    cfile = joinpath(RestClient.cachedir(),
                     RestClient.cachekey(url, method, headers, nothing) * ".http")
    isfile(cfile) && rm(cfile)
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
                clean_cachekey("$baseurl/cacheable", "GET",
                               ["Accept" => "application/json"])
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
                clean_cachekey("$baseurl/revalidate", "GET", ["Accept" => "application/json"])
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
                clean_cachekey("$baseurl/rotating", "GET", ["Accept" => "application/json"])
            end
        end
    end
    @testset "no-store" begin
        with_server() do baseurl, state
            IC.nostore(cfg(baseurl)); IC.nostore(cfg(baseurl))
            @test state.requests == 2
        end
    end
end
