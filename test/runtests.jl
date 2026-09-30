# SPDX-FileCopyrightText: © 2026 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using Test
using RestClient

include("server.jl")            # local HTTP harness shared by the live-request suites

@testset "RestClient" begin
    @testset "integration" begin
        include("integration.jl")   # public API, end-to-end against a local server
    end
    @testset "internals" begin
        include("internals.jl")     # edge cases no live request can reach
    end
    @testset "JSON.jl backend" begin
        include("json.jl")          # the JSON.jl @jsondef backend (integration uses JSON3)
    end
    @testset "XML extension" begin
        include("xml.jl")           # @xmldef, the xpath subset, parsexml dispatch
    end
    @testset "streaming" begin
        @testset "framing" begin
            include("streaming.jl")             # readframe: SSE/NDJSON over adversarial chunks
        end
        @testset "decode" begin
            include("streaming_decode.jl")      # decode_stream + cancellation, no network
            StreamDecodeTest.run()
        end
        @testset "transport" begin
            include("streaming_transport.jl")   # open_stream over the streaming server
            StreamTransportTest.run(with_stream_server, SLOW_INTERVAL)
        end
        @testset "endpoint" begin
            include("streaming_endpoint.jl")    # @endpoint Stream arm, end-to-end
            with_stream_server() do base
                StreamEndpointTest.run(base)
            end
        end
    end
end
