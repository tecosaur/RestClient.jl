# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module RestClient

using Downloads
using Base.ScopedValues: ScopedValue, with
using StyledStrings: @styled_str as @S_str

export @endpoint, @globalconfig, @jsondef, @xmldef, @formdef, @multipartdef
export RequestConfig, Request, perform, AbstractEndpoint, SingleResponse, ListResponse
export List, nextpage, eachpage, Stream, SSEvent
export RawFormat, JSONFormat, XMLFormat, FormFormat, MultipartFormat, MultipartFile
export AuthScheme, NoAuth, BearerAuth, BasicAuth, HeaderAuth, QueryAuth
export MalformedRequest, ResponseError, MalformedResponse
public urlpath, headers, parameters, payload, validate, responsetype, postprocess,
    dataformat, mimetype, interpretresponse, writepayload, AbstractFormat,
    authscheme, authpolicy, isstreamend, thispagenumber, remainingpages,
    globalconfig, setfield
public StreamFormat, UnspecifiedFormat, readframe, framestate, framedata, remake,
    elementpayload, authheaders, authparams, contents, metadata, cachelifetime

include("types.jl")
include("interface.jl")
include("caching.jl")
include("requests.jl")
include("streaming.jl")
include("forms.jl")
include("utilities.jl")
include("endpoint.jl")

macro importapi()
    api = (:urlpath, :headers, :parameters, :payload, :validate, :responsetype,
           :dataformat, :mimetype, :interpretresponse, :writepayload, :postprocess,
           :contents, :metadata, :thispagenumber, :nextpage, :remainingpages,
           :authscheme, :authpolicy, :authheaders, :authparams, :isstreamend, :cachelifetime)
    Expr(:import, Expr(:(:), Expr(:., fullname(@__MODULE__)...), (Expr(:., name) for name in api)...)) |> esc
end

macro reexport()
    :(export List, nextpage, eachpage)
end

function __init__()
    global MULTIPART_KEY = (rand(UInt64), rand(UInt64))
    DOWNLOADER[] = Downloads.Downloader()
    DOWNLOADER[].easy_hook = transferhook
    ccall(:jl_generating_output, Cint, ()) == 1 || atexit(cleancache)
end

end
