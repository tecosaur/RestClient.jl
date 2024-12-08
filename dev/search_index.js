var documenterSearchIndex = {"docs":
[{"location":"api/#API","page":"API","title":"API","text":"","category":"section"},{"location":"api/#Macros","page":"API","title":"Macros","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"@globalconfig\n@endpoint\n@jsondef","category":"page"},{"location":"api/#RestClient.@globalconfig","page":"API","title":"RestClient.@globalconfig","text":"@globalconfig value\n\nStore value as the global request configuration, and define globalconfig to return it.\n\nExamples\n\n@globalconfig RequestConfig(\"https://api.example.com\")\n\n\n\n\n\n","category":"macro"},{"location":"api/#RestClient.@endpoint","page":"API","title":"RestClient.@endpoint","text":"@endpoint struct ... end\n@endpoint [func(...)] -> [struct ... end] -> [payload] -> url -> resulttype\n\nGenerate an endpoint implementation from a concise shorthand form.\n\nThis macro can be applied to either an endpoint struct definition or a chain of arrow forms joined by ->. These arrow forms represent the flow of information:\n\nThe creation of a request, from a function call (optional)\nThe struct used to represent the request data (optional, autogenerated from the function call)\nAny payload provided with the request (if applicable)\nThe page URL the request is directed to (relative to the base URL)\nThe type that the response is expected to correspond to\n\nFor a basic API, this can be as simple as:\n\n@endpoint israining(city::String) -> \"checkrain/{city}\" -> Bool\n\nUsing this macro is equivalent to implementing a plain struct, along with these endpoint methods:\n\npagename\nparameters (if needed)\npayload (optionally)\nresponsetype\n\nIn our @endpoint israining() ... example, in addition to defining the israining function and an (implicit) IsrainigEndpoint struct, the macro will generate the implementations pagename(rain::IsrainingEndpoint) = \"checkrain/$(rain.city)\" and responsetype(::IsrainingEndpoint) = Bool.\n\nIn addition to applying @endpoint to a sequence of arrow forms you can also apply it to a struct that starts with a special line of non-struct arrows.\n\nFor instance, our israining() example could be alternatively written as:\n\n@endpoint struct IsrainingEndpoint\n    israining(city) -> \"checkrain/{city}\" -> Bool\n    city::String\nend\n\nThis generates the exact same code as the initial example form.\n\nFor more information on how each component of the shorthand form is interpreted, as well as more complex examples, see the extended help section.\n\nSee also: pagename, parameters, payload, responsetype, Request, globalconfig, @globalconfig.\n\nExtended help\n\nFunction forms\n\nShould you chose to include a function call as the first arrow form, a function definition with a matching signature will be generated.\n\nWithout a struct form, the arguments of the function will be used to generate a struct with a derived name ($(titlecase(funcname))Endpoint), and populate its fields.\n\nWith a struct form, all arguments that match a field name will inherit the field's type and default value . All the fields of the struct not present in the signature will be added as keyword arguments. This makes function forms an easy way to prescribe positional, mandatory arguments.\n\nStruct forms\n\nThe endpoint struct can be given explicitly, or generated from the function call. For instance, the function call api(target::String; count::Int=1) will generate this struct form:\n\n@kwdef struct ApiEndpoint\n    target::String\n    count::Int = 1\nend\n\nWhen autogenerated, or not specified, the endpoint supertype is selected from AbstractEndpoint, SingleEndpoint, and ListEndpoint based on the supertype of the output form.\n\nIf @endpoint is directly applied to a struct, the first line of the struct is removed and parsed as a series of arrow forms.\n\n@endpoint ApiEndpoint\n    api(target; count) -> ...\n    target::String\n    count::Int = 1\nend\n\nThis makes no difference to the generated code, and is purely a matter of personal preference (I prefer explicit structs for more complex forms).\n\nPayload forms\n\nHTTP methods like :post, :put, and :patch include a content payload (also referred to as the request body). The payload can be specified with a payload form that names a field of the endpoint struct, names a global variable, or is an expression is evaluated to generate the payload value (with the current endpoint available through the anaphoric variable self).\n\nUnless otherwise specified, an endpoint that includes a payload is assumed to be a :post request (:get without).\n\n@endpoint upload(content::String) -> content -> \"create\" -> Status\n\nIn this example content is a String, and so a :post request is made to \"$BASEURL/create\" with content as the payload/body. More complex types are encoded according with writepayload according to dataformat.\n\nURL forms\n\nThe endpoint URL is represent by a string, relative to the base URL of the API (a leading / is stripped). Such paths usually consist of a path component, and optionally a query component.\n\n\"some/path/for/my/api?query=param&other=value\"\n\nWithin this endpoint URL, you can include references to fields of the struct (or global variables) by surrounding them with curly braces.\n\n\"page/{somefield}?param={another}&{globalvar}=7\"\n\nFor convenience, a parameter by the same name as the field can be referred to by the field name alone (e.g. ?{var} instead of ?var={var}).\n\nIn more complex cases, arbitrary Julia code can be included in the curly braces. This code will be evaluated with the endpoint value bound the the anaphoric variable self.\n\n\"page/{if self.new \\\"new\\\" else \\\"fetch\\\" end}/{id}\"\n\nIf that's not enough, you can also use an arbitrary expression in place of the endpoint string:\n\nif self.create \"pages/create/\" else \"pages/fetch/id/\" * self.id end\n\nWhen using expression instead of a string though, curly braces are not interpreted as field references.\n\nSpecifying the HTTP method\n\nBy default, it is guessed whether the API expects a :get or :post request based on the presence or absence of an input form. This can be explicitly specified by wrapping the URL form in an HTTP method symbol, for example:\n\n:head(\"existence/{entity}\")\n\nResponse type forms\n\nIt is sensible to parse the raw response into a more informative Julia representation. The response type specifies what type the response can be parsed to, which is performed by interpretresponse according to dataformat.\n\nOften the result type will name a struct defined with [@jsondef].\n\nExamples\n\n@endpoint struct ShuffleEndpoint <: SingleEndpoint\n    \"deck/{deck}/shuffle?remaining={ifelse(self.remaining, '1', '0')}\" -> Deck\n    deck::String\n    remaining::Bool\nend\n\nThis is equivalent to defining the struct by itself, and then separately defining the three basic endpoint methods.\n\nstruct ShuffleEndpoint <: SingleEndpoint\n    deck::String\n    remaining::Bool\nend\n\nRestClient.pagename(shuf::ShuffleEndpoint) =\n    \"deck/$(shuf.deck)/shuffle\"\nRestClient.parameters(shuf::ShuffleEndpoint) =\n    [\"remaining\" => string(shuf.remaining)]\nRestClient.responsetype(shuf::ShuffleEndpoint) = Deck\n\n\n\n\n\n","category":"macro"},{"location":"api/#RestClient.@jsondef","page":"API","title":"RestClient.@jsondef","text":"@jsondef [kind] struct ... end\n\nDefine a struct that can be used with JSON3.\n\nThis macro conveniently combines the following pieces:\n\n@kwdef to define keyword constructors for the struct.\nCustom Base.show method to show the struct with keyword arguments, omitting default values.\nStructTypes.StructType to define the struct type, and StructTypes.names (if needed) to define the JSON field mapping.\nRestClient.dataformat to declare that this struct is JSON-formatted.\n\nOptionally the JSON representation kind can be specified. It defaults to Struct, but can any of: Struct, Dict, Array, Vector, String, Number, Bool, Nothing.\n\nwarning: Soft JSON3 dependency\nThis macro is implemented in a package extension, and so requires JSON3 to be loaded before it can be used.\n\nExamples\n\n@jsondef struct DocumentStatus\n    exists::Bool  # Required, goes by 'exists' in JSON too\n    status:\"document_status\"::Union{String, Nothing} = nothing\n    url:\"document_url\"::Union{String, Nothing} = nothing\n    age::Int = 0  # Known as 'age' in JSON too, defaults to 0\nend\n\n\n\n\n\n","category":"macro"},{"location":"api/#Request-types","page":"API","title":"Request types","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"Request\nRequestConfig\nAbstractEndpoint\nSingleEndpoint\nListEndpoint","category":"page"},{"location":"api/#RestClient.Request","page":"API","title":"RestClient.Request","text":"Request{E<:AbstractEndpoint}\n\nA request to an API endpoint, with a specific configuration.\n\nThis is the complete set of information required to make a request to an API.\n\nSee also: AbstractEndpoint, RequestConfig.\n\nData flow\n\n         ╭─╴config╶────────────────────────────╮\n         │     ╎                               │\n         │     ╎        ╭─▶ responsetype ╾─────┼────────────────┬──▶ dataformat ╾───╮\nRequest╶─┤     ╰╶╶╶╶╶╶╶╶│                      │                ╰─────────╮         │\n         │              ├─▶ pagename ╾───╮     │      ┌╶╶╶ debug╴╴╴┐      │  ╭──────╯\n         │              │                ├──▶ url ╾─┬─━─▶ request ╾━┬─▶ interpret ╾──▶ data\n         ├─╴endpoint╶───┼─▶ parameters ╾─╯          │               │                   │\n         │              │                           │               │             ╭─────╯\n         │              ├─▶ parameters ╾────────────┤               ╰─────────╮   │\n         │              │                           │                      postprocess ╾──▶ result\n         │             *╰─▶ payload ─▶ writepayload╶╯                           │\n         │                    ╰─▶ dataformat ╾╯                                 │\n         ╰─────────┬────────────────────────────────────────────────────────────╯\n                   ╰────▶ validate (before initiating the request)\n\n * Only for POST requests   ╶╶ Optional first argument\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.RequestConfig","page":"API","title":"RestClient.RequestConfig","text":"RequestConfig\n\nThe general configuration for a request to the API, not tied to any specific endpoint.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.AbstractEndpoint","page":"API","title":"RestClient.AbstractEndpoint","text":"AbstractEndpoint\n\nAbstract supertype for API endpoints.\n\nUsually you will want to subtype either SingleEndpoint or ListEndpoint, which share the same interface as AbstractEndpoint but have additional semantics.\n\nInterface\n\npagename([config::RequestConfig], endpoint::AbstractEndpoint) -> String\nheaders([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\nparameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\nresponsetype(endpoint::AbstractEndpoint) -> Union{Type, Nothing}\nvalidate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool\npostprocess([response::Downloads.Response], request::Request, data) -> Any\n\nAll of these functions but pagename have default implementations.\n\nSee also: Request, dataformat, interpretresponse.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.SingleEndpoint","page":"API","title":"RestClient.SingleEndpoint","text":"SingleEndpoint <: AbstractEndpoint\n\nAbstract supertype for API endpoints that return a single value.\n\nSee also: AbstractEndpoint, SingleResponse, Single.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.ListEndpoint","page":"API","title":"RestClient.ListEndpoint","text":"ListEndpoint <: AbstractEndpoint\n\nAbstract supertype for API endpoints that return a list of values.\n\nSee also: AbstractEndpoint, ListResponse, List.\n\n\n\n\n\n","category":"type"},{"location":"api/#Response-types","page":"API","title":"Response types","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"SingleResponse\nSingle\nListResponse\nList","category":"page"},{"location":"api/#RestClient.SingleResponse","page":"API","title":"RestClient.SingleResponse","text":"SingleResponse{T}\n\nAbstract supertype for responses that contain a single T item and (optionally) metadata.\n\nInterface\n\nSubtypes of SingleResponse may need to define these two methods:\n\ncontents(single::SingleResponse{T}) -> T\nmetadata(single::SingleResponse{T}) -> Dict{Symbol, Any}\n\nBoth have generic implementations that are sufficient for simple cases.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.Single","page":"API","title":"RestClient.Single","text":"Single{T, E<:SingleEndpoint}\n\nHolds a single value of type T returned from an API endpoint, along with request information and metadata.\n\nSee also: SingleEndpoint, SingleResponse.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.ListResponse","page":"API","title":"RestClient.ListResponse","text":"ListResponse{T}\n\nAbstract supertype for responses that contain a list of T items and (optionally) metadata.\n\nInterface\n\nSubtypes of ListResponse may need to define these two methods:\n\ncontents(list::ListResponse{T}) -> Vector{T}\nmetadata(list::ListResponse{T}) -> Dict{Symbol, Any}\n\nBoth have generic implementations that are sufficient for simple cases.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.List","page":"API","title":"RestClient.List","text":"List{T, E<:ListEndpoint}\n\nHolds a list of values of type T returned from an API endpoint, along with request information and metadata.\n\nSee also: ListEndpoint, ListResponse.\n\n\n\n\n\n","category":"type"},{"location":"api/#Request-interface","page":"API","title":"Request interface","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"RestClient.globalconfig\nRestClient.pagename\nRestClient.headers\nRestClient.parameters\nRestClient.payload","category":"page"},{"location":"api/#RestClient.globalconfig","page":"API","title":"RestClient.globalconfig","text":"globalconfig(::Val{::Module}) -> RequestConfig\n\nReturn the global configuration for the given module.\n\nThis is used in @endpoint generated API functions.\n\nwarning: Warning\nBe careful not to accidentally define this function in a way that generates a new RequestConfig every time it is called, as this will cause state information (like rate limits) to be lost between requests.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.pagename","page":"API","title":"RestClient.pagename","text":"pagename([config::RequestConfig], endpoint::AbstractEndpoint) -> String\n\nReturn the name of the page for the given endpoint.\n\nThis is combined with the base URL and parameters to form the full URL for the request.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.headers","page":"API","title":"RestClient.headers","text":"headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\n\nReturn headers for the given endpoint.\n\nThe default implementation returns an empty list.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.parameters","page":"API","title":"RestClient.parameters","text":"parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\n\nReturn URI parameters for the given endpoint.\n\nThis are combined with the endpoint URL to form the full query URL.\n\nThe default implementation returns an empty list.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.payload","page":"API","title":"RestClient.payload","text":"payload([config::RequestConfig], endpoint::AbstractEndpoint) -> Any\n\nReturn the payload for the given endpoint.\n\nThis is used for POST requests, and is sent as the body of the request.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#Response-interface","page":"API","title":"Response interface","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"RestClient.validate\nRestClient.responsetype\nRestClient.postprocess\nRestClient.contents\nRestClient.metadata\nRestClient.nextpage\nRestClient.thispagenumber\nRestClient.remainingpages","category":"page"},{"location":"api/#RestClient.validate","page":"API","title":"RestClient.validate","text":"validate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool\n\nCheck if the request to endpoint according to config is valid.\n\nThis is called before the request is made, and can be used to check if the request is well-formed. This is the appropriate place to emit warnings about potential issues with the request.\n\nReturn true if the request should proceed, false otherwise.\n\nThe default implementation always returns true.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.responsetype","page":"API","title":"RestClient.responsetype","text":"responsetype(endpoint::AbstractEndpoint) -> Type\n\nReturn the type of the response for the given endpoint.\n\nTogether with dataformat, this is used to parse the response.\n\nIf IO (the default implementation), the response is not parsed at all.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.postprocess","page":"API","title":"RestClient.postprocess","text":"postprocess([response::Downloads.Response], request::Request, data) -> Any\n\nPost-process the data returned by the request.\n\nThere are three generic implementations provided:\n\nFor SingleEndpoint requests that return a SingleResponse, the data is wrapped in a Single object.\nFor ListEndpoint requests that return a ListResponse, the data are wrapped in a List object.\nFor all other endpoints, the data is returned as-is.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.contents","page":"API","title":"RestClient.contents","text":"content(response::SingleResponse{T}) -> T\n\nReturn the content of the response.\n\n\n\n\n\ncontent(response::ListResponse{T}) -> Vector{T}\n\nReturn the items of the response.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.metadata","page":"API","title":"RestClient.metadata","text":"metadata(response::SingleResponse) -> Dict{Symbol, Any}\nmetadata(response::ListResponse) -> Dict{Symbol, Any}\n\nReturn metadata for the given response.\n\nThe default implementation returns an empty dictionary.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.nextpage","page":"API","title":"RestClient.nextpage","text":"nextpage(response::List) -> Union{List, Nothing}\n\nFetch the next page of results after response.\n\nIf there are no more pages, or this method is not available for the given endpoint, return nothing.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.thispagenumber","page":"API","title":"RestClient.thispagenumber","text":"thispagenumber(response::List) -> Union{Int, Nothing}\n\nReturn the current page number of response, if known.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.remainingpages","page":"API","title":"RestClient.remainingpages","text":"remainingpages(response::List) -> Union{Int, Nothing}\n\nReturn the number of remaining pages after response, if known.\n\n\n\n\n\n","category":"function"},{"location":"api/#Content-formatting","page":"API","title":"Content formatting","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"RestClient.AbstractFormat\nRestClient.RawFormat\nRestClient.JSONFormat\nRestClient.dataformat\nRestClient.interpretresponse\nRestClient.writepayload","category":"page"},{"location":"api/#RestClient.AbstractFormat","page":"API","title":"RestClient.AbstractFormat","text":"AbstractFormat\n\nAbstract supertype for response formats.\n\nSee also: RawFormat, JSONFormat.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.RawFormat","page":"API","title":"RestClient.RawFormat","text":"RawFormat <: AbstractFormat\n\nSingleton type for raw response formats.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.JSONFormat","page":"API","title":"RestClient.JSONFormat","text":"JSONFormat <: AbstractFormat\n\nSingleton type for JSON response formats.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClient.dataformat","page":"API","title":"RestClient.dataformat","text":"dataformat([endpoint::AbstractEndpoint], ::Type{T}) -> AbstractFormat\n\nReturn the expected format that T is represented by in the response from endpoint.\n\nUsing the default dataformat(::Type) method, the format is RawFormat.\n\nA dataformat(::Type) method is automatically defined when invoking @jsondef.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.interpretresponse","page":"API","title":"RestClient.interpretresponse","text":"interpretresponse(data::IO, fmt::AbstractFormat, ::Type{T}) -> value::T\n\nInterpret data as a response of type T according to fmt.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClient.writepayload","page":"API","title":"RestClient.writepayload","text":"writepayload(dest::IO, fmt::AbstractFormat, data)\n\nWrite data to dest according to fmt.\n\n\n\n\n\n","category":"function"},{"location":"#RestClient","page":"Introduction","title":"RestClient","text":"","category":"section"},{"location":"","page":"Introduction","title":"Introduction","text":"Interacting with a RESTful API is fairly simple, but there's often a fair bit of boilerplate involved in making well-formed requests, handling serialisation and deserialisation, request limiting, and result pagination.","category":"page"},{"location":"","page":"Introduction","title":"Introduction","text":"RestClient aims to take care of as much of this boilerplate as possible. See the tutorial for a sense of what this looks like in practice.","category":"page"},{"location":"tutorial/#Tutorial","page":"Tutorial","title":"Tutorial","text":"","category":"section"},{"location":"tutorial/#Basic-setup","page":"Tutorial","title":"Basic setup","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"To demonstrate how this can simplify API wrapping, in this tutorial we'll implement a client for the Deck of Cards API. For demonstration purposes, we'll just wrap part of the API.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Looking at the API documentation, we can see that","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"The base URL is https://deckofcardsapi.com/api/\nDecks can be created at the deck/new API (optionally shuffled)\nOperations on a deck occur at deck/<id>/operation endpoints","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"That's it! To get started we'll create an blank package and call it \"DeckAPI\" (I did this in my temp directory).","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"pkg> generate DeckAPI\n  Generating  project DeckAPI:\n    DeckAPI/Project.toml\n    DeckAPI/src/DeckAPI.jl","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Then, we activate the project and add JSON3 and RestClient as dependencies.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"pkg> activate DeckAPI\n\n(DeckAPI) pkg> add RestClient JSON3\n    Updating `/tmp/DeckAPI/Project.toml`\n  [d1389577] + RestClient v0.1.0\n  [0f8b85d8] + JSON3 v1.14.1\n    Updating `/tmp/DeckAPI/Manifest.toml`\n  [...] ...","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Then we can navigate to DeckAPI/src/DeckAPI.jl, get rid of the default greet() = print(\"Hello World!\"), and add","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"using RestClient, JSON3","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Now we're ready to get started implementing the API! To start with, we'll want to create a RequestConfig to hold the context in which we're calling the API. This just holds the base URL, a request lock, API access key (optional), and timeout value (optional). The Deck of Cards API is simple enough that we can set a single global RequestConfig, but in a more complex case we might define a utility function to create a RequestConfig based on user-provided parameters.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"const DECK_CONFIG = RequestConfig(\"https://deckofcardsapi.com/api\")","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"That's all the setup needed, next we'll define types for the JSON structures that Desk of Cards can return.","category":"page"},{"location":"tutorial/#Defining-API-types","page":"Tutorial","title":"Defining API types","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Reading the documentation in order, we come across a few types we'll want to implement. First, there's the new deck object","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"{\n    \"success\": true,\n    \"deck_id\": \"3p40paa87x90\",\n    \"shuffled\": true,\n    \"remaining\": 52\n}","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Thanks to the @jsondef macro, this is merely a matter of","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@jsondef struct Deck\n    # success::Bool # I don't think we care about this?\n    id.\"deck_id\"::String\n    # shuffled::Bool # We should know this anyway?\n    remaining::Int\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"We can also draw cards, which look like this","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@jsondef struct Card\n    code::String\n    # image # Not needed by us (at the moment)\n    value::String\n    suit::String\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Cards are given as a list in a certain response form, which gives us an opportunity to define a ListResponse subtype.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@jsondef struct CardsResponse <: ListResponse{Card}\n    deck.\"deck_id\"::String\n    remaining::Int\n    cards::Vector{Card}\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Since we've subtyped ListResponse, this will automagically be turned into a List holding cards.","category":"page"},{"location":"tutorial/#Adding-endpoints","page":"Tutorial","title":"Adding endpoints","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"In RestClient, each endpoint is represented with a dedicated subtype of AbstractEndpoint. Endpoints that provide a single value should subtype SingleEndpoint, while endpoints that provide multiple values should subtype ListEndpoint.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"While we could define the structs and the interface methods separately, we can conveniently combine these steps with the @endpoint macro.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Let's start off with deck creation.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct NewDeckEndpoint <: SingleEndpoint\n    \"deck/new{ifelse(self.shuffle, \\\"/shuffle\\\", \\\"\\\")}?deck_count={count}\" -> Deck\n    count::Int\n    shuffle::Bool\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"We can also shuffle decks.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct ShuffleEndpoint <: SingleEndpoint\n    \"deck/{deck}/shuffle?{remaining}\" -> Deck\n    deck::String\n    remaining::Bool\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Now let's draw some cards.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct DrawEndpoint <: ListEndpoint\n    \"deck/{deck}/draw?{count}\" -> CardsResponse\n    deck::String\n    count::Int\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Cards can also be returned to the deck.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct CardReturnEndpoint <: SingleEndpoint\n    \"deck/{deck}/return\" -> Deck\n    deck::String\n    cards::Union{Nothing, Vector{Card}}\nend\n\nfunction RestClient.parameters(ret::CardReturnEndpoint)\n    if isnothing(ret.cards)\n        Pair{String, String}[]\n    else\n        [\"cards\" => join(map(c -> c.code, ret.cards), \",\")]\n    end\nend","category":"page"},{"location":"tutorial/#Defining-our-API","page":"Tutorial","title":"Defining our API","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Now we've gone to the effort of defining all our types and endpoints, we just need to create the API for our package.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"new(count::Int = 1; shuffled::Bool=false) =\n    api_get(Request(DECK_CONFIG, NewDeckEndpoint(count, shuffled)))","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"shuffle(deck::Deck, remaining::Bool=false) =\n    api_get(Request(DECK_CONFIG, ShuffleEndpoint(deck.id, remaining)))","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"draw(deck::Deck, count::Int=1) =\n    api_get(Request(DECK_CONFIG, DrawEndpoint(deck.id, count)))","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"putback(deck::Deck, cards::Union{Nothing, Vector{Card}} = nothing) =\n    api_get(Request(DECK_CONFIG, CardReturnEndpoint(deck.id, cards)))\n\nputback(cardlist::List{Cards}) = putback(cardlist.request.endpoint.deck, cardlist.items)","category":"page"},{"location":"tutorial/#Demonstration","page":"Tutorial","title":"Demonstration","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"By starting Julia with the environment variable JULIA_DEBUG=RestClient set, we will see information on the requests sent and responses received. This helps us verify that everything is behaving as expected, and debug any failures or unexpected results.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> deck = DeckAPI.new()\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/new?deck_count=1\n└ @ RestClient\n┌ Debug:  200  80 bytes (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/new?deck_count=1\n└ @ RestClient\nDeckAPI.Deck(id=\"01n3ezer3rly\", remaining=52)\n\njulia> cards = DeckAPI.draw(deck, 5)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClient\n┌ Debug:  200  1.181 KiB (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClient\nRestClient.List{DeckAPI.Card} holding 5 items:\n  • Card(code=\"AS\", value=\"ACE\", suit=\"SPADES\")\n  • Card(code=\"2S\", value=\"2\", suit=\"SPADES\")\n  • Card(code=\"3S\", value=\"3\", suit=\"SPADES\")\n  • Card(code=\"4S\", value=\"4\", suit=\"SPADES\")\n  • Card(code=\"5S\", value=\"5\", suit=\"SPADES\")\n\njulia> DeckAPI.putback(cards)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/return?cards=AS%2C2S%2C3S%2C4S%2C5S\n└ @ RestClient\n┌ Debug:  200  61 bytes (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/return?cards=AS%2C2S%2C3S%2C4S%2C5S\n└ @ RestClient\nDeckAPI.Deck(id=\"01n3ezer3rly\", remaining=52)\n\njulia> DeckAPI.shuffle(deck)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/shuffle?remaining=false\n└ @ RestClient\n┌ Debug:  200  79 bytes (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/shuffle?remaining=false\n└ @ RestClient\nDeckAPI.Deck(id=\"01n3ezer3rly\", remaining=52)\n\njulia> cards = DeckAPI.draw(deck, 5)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClient\n┌ Debug:  200  1.183 KiB (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClient\nRestClient.List{DeckAPI.Card} holding 5 items:\n  • Card(code=\"3C\", value=\"3\", suit=\"CLUBS\")\n  • Card(code=\"QC\", value=\"QUEEN\", suit=\"CLUBS\")\n  • Card(code=\"4S\", value=\"4\", suit=\"SPADES\")\n  • Card(code=\"2D\", value=\"2\", suit=\"DIAMONDS\")\n  • Card(code=\"3S\", value=\"3\", suit=\"SPADES\")","category":"page"}]
}
