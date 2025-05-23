# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module RestClient

using Downloads
using StyledStrings: @styled_str as @S_str

export AbstractEndpoint, SingleEndpoint, ListEndpoint, Request, RequestConfig,
    @endpoint, @globalconfig, globalconfig
export SingleResponse, ListResponse, Single, List, perform, urlpath,
    parameters, headers, payload, responsetype, dataformat, mimetype,
    interpretresponse, validate, postprocess, thispagenumber, nextpage,
    remainingpages
export AbstractFormat, RawFormat, JSONFormat, @jsondef, XMLFormat, @xmldef
export setfield

include("types.jl")
include("interface.jl")
include("caching.jl")
include("requests.jl")
include("utilities.jl")
include("endpoint.jl")

macro importapi()
    :(import $(__module__): urlpath, parameters, responsetype,
      validate, postprocess, thispagenumber, nextpage, remainingpages,
      contents, metadata) |> esc
end

macro reexport()
    :(export Single, List, nextpage)
end

__init__() = atexit(cleancache)

end
