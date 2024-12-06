var documenterSearchIndex = {"docs":
[{"location":"api/#API","page":"API","title":"API","text":"","category":"section"},{"location":"api/#Macros","page":"API","title":"Macros","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"@endpoint\n@jsondef","category":"page"},{"location":"api/#RestClientScaffold.@endpoint","page":"API","title":"RestClientScaffold.@endpoint","text":"@endpoint struct ... end\n\nDefine a struct that serves as an API endpoint.\n\nThis macro serves as a shorthand way of defining the basic endpoint methods:\n\npagename\nparameters (if needed)\nresponsetype\npayload (optionally)\n\nDefinitions for these methods are generated based on a non-standard endpoint declaration line at the top of the struct definition. This line takes the forms\n\n\"page/path?params...\" -> ResultType           # Get request\ninput -> \"page/path?params...\" -> ResultType  # Post request\n\nBoth the page path and the parameters can contain references to global variables or fields of the struct, surrounded by curly braces. For convenience, a parameter by the same name as the field can be referred to by the field name alone (e.g. ?{var} instead of ?var={var}). The ResultType can be any type, but is typically a struct defined with @jsondef.\n\n\"page/{somefield}?param={another}&{globalvar}=7\" -> ResultType\n\nIn more complex cases, arbitrary Julia code can be included in the curly braces. This code will be evaluated with the endpoint value bound the the anaphoric variable self. You can also reference type parameters of the struct.\n\n\"page/{if self.new \"new\" else \"fetch\" end}/{id}\" -> ResultType\n\nIf that's not enough, you can also use an arbitrary expression in place of the endpoint string:\n\nif self.create \"pages/create/\" else \"pages/fetch/id/\" * self.id end -> ResultType\n\nWhen using an input -> page -> ResultType form for a post request, input should follow the form of a curly brace interpolation — be either a field name, global variable, or expression. It is used to define payload for the endpoint.\n\nExamples\n\n@endpoint struct ShuffleEndpoint <: SingleEndpoint\n    \"deck/{deck}/shuffle?remaining={ifelse(self.remaining, '1', '0')}\" -> Deck\n    deck::String\n    remaining::Bool\nend\n\nThis is equivalent to defining the struct by itself, and then separately defining the three basic endpoint methods.\n\nstruct ShuffleEndpoint <: SingleEndpoint\n    deck::String\n    remaining::Bool\nend\n\nRestClientScaffold.pagename(shuf::ShuffleEndpoint) =\n    \"deck/$(shuf.deck)/shuffle\"\nRestClientScaffold.parameters(shuf::ShuffleEndpoint) =\n    [\"remaining\" => string(shuf.remaining)]\nRestClientScaffold.responsetype(shuf::ShuffleEndpoint) = Deck\n\n\n\n\n\n","category":"macro"},{"location":"api/#RestClientScaffold.@jsondef","page":"API","title":"RestClientScaffold.@jsondef","text":"@jsondef [kind] struct ... end\n\nDefine a struct that can be used with JSON3.\n\nThis macro conveniently combines the following pieces:\n\n@kwdef to define keyword constructors for the struct.\nCustom Base.show method to show the struct with keyword arguments, omitting default values.\nStructTypes.StructType to define the struct type, and StructTypes.names (if needed) to define the JSON field mapping.\nRestClientScaffold.dataformat to declare that this struct is JSON-formatted.\n\nOptionally the JSON representation kind can be specified. It defaults to Struct, but can any of: Struct, Dict, Array, Vector, String, Number, Bool, Nothing.\n\nwarning: Soft JSON3 dependency\nThis macro is implemented in a package extension, and so requires JSON3 to be loaded before it can be used.\n\nExamples\n\n@jsondef struct DocumentStatus\n    exists::Bool  # Required, goes by 'exists' in JSON too\n    status:\"document_status\"::Union{String, Nothing} = nothing\n    url:\"document_url\"::Union{String, Nothing} = nothing\n    age::Int = 0  # Known as 'age' in JSON too, defaults to 0\nend\n\n\n\n\n\n","category":"macro"},{"location":"api/#Request-types","page":"API","title":"Request types","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"Request\nRequestConfig\nAbstractEndpoint\nSingleEndpoint\nListEndpoint","category":"page"},{"location":"api/#RestClientScaffold.Request","page":"API","title":"RestClientScaffold.Request","text":"Request{E<:AbstractEndpoint}\n\nA request to an API endpoint, with a specific configuration.\n\nThis is the complete set of information required to make a request to an API.\n\nSee also: AbstractEndpoint, RequestConfig.\n\nData flow\n\n         ╭─╴config╶────────────────────────────╮\n         │     ╎                               │\n         │     ╎        ╭─▶ responsetype ╾─────┼────────────────┬──▶ dataformat ╾───╮\nRequest╶─┤     ╰╶╶╶╶╶╶╶╶│                      │                ╰─────────╮         │\n         │              ├─▶ pagename ╾───╮     │      ┌╶╶╶ debug╴╴╴┐      │  ╭──────╯\n         │              │                ├──▶ url ╾─┬─━─▶ request ╾━┬─▶ interpret ╾──▶ data\n         ├─╴endpoint╶───┼─▶ parameters ╾─╯          │               │                   │\n         │              │                           │               │             ╭─────╯\n         │              ├─▶ parameters ╾────────────┤               ╰─────────╮   │\n         │              │                           │                      postprocess ╾──▶ result\n         │             *╰─▶ payload ─▶ writepayload╶╯                           │\n         │                    ╰─▶ dataformat ╾╯                                 │\n         ╰─────────┬────────────────────────────────────────────────────────────╯\n                   ╰────▶ validate (before initiating the request)\n\n * Only for POST requests   ╶╶ Optional first argument\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.RequestConfig","page":"API","title":"RestClientScaffold.RequestConfig","text":"RequestConfig\n\nThe general configuration for a request to the API, not tied to any specific endpoint.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.AbstractEndpoint","page":"API","title":"RestClientScaffold.AbstractEndpoint","text":"AbstractEndpoint\n\nAbstract supertype for API endpoints.\n\nUsually you will want to subtype either SingleEndpoint or ListEndpoint, which share the same interface as AbstractEndpoint but have additional semantics.\n\nInterface\n\npagename([config::RequestConfig], endpoint::AbstractEndpoint) -> String\nheaders([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\nparameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\nresponsetype(endpoint::AbstractEndpoint) -> Union{Type, Nothing}\nvalidate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool\npostprocess([response::Downloads.Response], request::Request, data) -> Any\n\nAll of these functions but pagename have default implementations.\n\nSee also: Request, dataformat, interpretresponse.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.SingleEndpoint","page":"API","title":"RestClientScaffold.SingleEndpoint","text":"SingleEndpoint <: AbstractEndpoint\n\nAbstract supertype for API endpoints that return a single value.\n\nSee also: AbstractEndpoint, SingleResponse, Single.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.ListEndpoint","page":"API","title":"RestClientScaffold.ListEndpoint","text":"ListEndpoint <: AbstractEndpoint\n\nAbstract supertype for API endpoints that return a list of values.\n\nSee also: AbstractEndpoint, ListResponse, List.\n\n\n\n\n\n","category":"type"},{"location":"api/#Response-types","page":"API","title":"Response types","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"SingleResponse\nSingle\nListResponse\nList","category":"page"},{"location":"api/#RestClientScaffold.SingleResponse","page":"API","title":"RestClientScaffold.SingleResponse","text":"SingleResponse{T}\n\nAbstract supertype for responses that contain a single T item and (optionally) metadata.\n\nInterface\n\nSubtypes of SingleResponse may need to define these two methods:\n\ncontents(single::SingleResponse{T}) -> T\nmetadata(single::SingleResponse{T}) -> Dict{Symbol, Any}\n\nBoth have generic implementations that are sufficient for simple cases.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.Single","page":"API","title":"RestClientScaffold.Single","text":"Single{T, E<:SingleEndpoint}\n\nHolds a single value of type T returned from an API endpoint, along with request information and metadata.\n\nSee also: SingleEndpoint, SingleResponse.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.ListResponse","page":"API","title":"RestClientScaffold.ListResponse","text":"ListResponse{T}\n\nAbstract supertype for responses that contain a list of T items and (optionally) metadata.\n\nInterface\n\nSubtypes of ListResponse may need to define these two methods:\n\ncontents(list::ListResponse{T}) -> Vector{T}\nmetadata(list::ListResponse{T}) -> Dict{Symbol, Any}\n\nBoth have generic implementations that are sufficient for simple cases.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.List","page":"API","title":"RestClientScaffold.List","text":"List{T, E<:ListEndpoint}\n\nHolds a list of values of type T returned from an API endpoint, along with request information and metadata.\n\nSee also: ListEndpoint, ListResponse.\n\n\n\n\n\n","category":"type"},{"location":"api/#Request-interface","page":"API","title":"Request interface","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"RestClientScaffold.pagename\nRestClientScaffold.headers\nRestClientScaffold.parameters\nRestClientScaffold.payload","category":"page"},{"location":"api/#RestClientScaffold.pagename","page":"API","title":"RestClientScaffold.pagename","text":"pagename([config::RequestConfig], endpoint::AbstractEndpoint) -> String\n\nReturn the name of the page for the given endpoint.\n\nThis is combined with the base URL and parameters to form the full URL for the request.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.headers","page":"API","title":"RestClientScaffold.headers","text":"headers([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\n\nReturn headers for the given endpoint.\n\nThe default implementation returns an empty list.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.parameters","page":"API","title":"RestClientScaffold.parameters","text":"parameters([config::RequestConfig], endpoint::AbstractEndpoint) -> Vector{Pair{String, String}}\n\nReturn URI parameters for the given endpoint.\n\nThis are combined with the endpoint URL to form the full query URL.\n\nThe default implementation returns an empty list.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.payload","page":"API","title":"RestClientScaffold.payload","text":"payload([config::RequestConfig], endpoint::AbstractEndpoint) -> Any\n\nReturn the payload for the given endpoint.\n\nThis is used for POST requests, and is sent as the body of the request.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#Response-interface","page":"API","title":"Response interface","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"RestClientScaffold.validate\nRestClientScaffold.responsetype\nRestClientScaffold.postprocess\nRestClientScaffold.contents\nRestClientScaffold.metadata\nRestClientScaffold.nextpage\nRestClientScaffold.thispagenumber\nRestClientScaffold.remainingpages","category":"page"},{"location":"api/#RestClientScaffold.validate","page":"API","title":"RestClientScaffold.validate","text":"validate([config::RequestConfig], endpoint::AbstractEndpoint) -> Bool\n\nCheck if the request to endpoint according to config is valid.\n\nThis is called before the request is made, and can be used to check if the request is well-formed. This is the appropriate place to emit warnings about potential issues with the request.\n\nReturn true if the request should proceed, false otherwise.\n\nThe default implementation always returns true.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.responsetype","page":"API","title":"RestClientScaffold.responsetype","text":"responsetype(endpoint::AbstractEndpoint) -> Type\n\nReturn the type of the response for the given endpoint.\n\nTogether with dataformat, this is used to parse the response.\n\nIf IO (the default implementation), the response is not parsed at all.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.postprocess","page":"API","title":"RestClientScaffold.postprocess","text":"postprocess([response::Downloads.Response], request::Request, data) -> Any\n\nPost-process the data returned by the request.\n\nThere are three generic implementations provided:\n\nFor SingleEndpoint requests that return a SingleResponse, the data is wrapped in a Single object.\nFor ListEndpoint requests that return a ListResponse, the data are wrapped in a List object.\nFor all other endpoints, the data is returned as-is.\n\nnote: Note\nPart of the AbstractEndpoint interface.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.contents","page":"API","title":"RestClientScaffold.contents","text":"content(response::SingleResponse{T}) -> T\n\nReturn the content of the response.\n\n\n\n\n\ncontent(response::ListResponse{T}) -> Vector{T}\n\nReturn the items of the response.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.metadata","page":"API","title":"RestClientScaffold.metadata","text":"metadata(response::SingleResponse) -> Dict{Symbol, Any}\nmetadata(response::ListResponse) -> Dict{Symbol, Any}\n\nReturn metadata for the given response.\n\nThe default implementation returns an empty dictionary.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.nextpage","page":"API","title":"RestClientScaffold.nextpage","text":"nextpage(response::List) -> Union{List, Nothing}\n\nFetch the next page of results after response.\n\nIf there are no more pages, or this method is not available for the given endpoint, return nothing.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.thispagenumber","page":"API","title":"RestClientScaffold.thispagenumber","text":"thispagenumber(response::List) -> Union{Int, Nothing}\n\nReturn the current page number of response, if known.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.remainingpages","page":"API","title":"RestClientScaffold.remainingpages","text":"remainingpages(response::List) -> Union{Int, Nothing}\n\nReturn the number of remaining pages after response, if known.\n\n\n\n\n\n","category":"function"},{"location":"api/#Content-formatting","page":"API","title":"Content formatting","text":"","category":"section"},{"location":"api/","page":"API","title":"API","text":"RestClientScaffold.AbstractFormat\nRestClientScaffold.RawFormat\nRestClientScaffold.JSONFormat\nRestClientScaffold.dataformat\nRestClientScaffold.interpretresponse\nRestClientScaffold.writepayload","category":"page"},{"location":"api/#RestClientScaffold.AbstractFormat","page":"API","title":"RestClientScaffold.AbstractFormat","text":"AbstractFormat\n\nAbstract supertype for response formats.\n\nSee also: RawFormat, JSONFormat.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.RawFormat","page":"API","title":"RestClientScaffold.RawFormat","text":"RawFormat <: AbstractFormat\n\nSingleton type for raw response formats.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.JSONFormat","page":"API","title":"RestClientScaffold.JSONFormat","text":"JSONFormat <: AbstractFormat\n\nSingleton type for JSON response formats.\n\n\n\n\n\n","category":"type"},{"location":"api/#RestClientScaffold.dataformat","page":"API","title":"RestClientScaffold.dataformat","text":"dataformat([endpoint::AbstractEndpoint], ::Type{T}) -> AbstractFormat\n\nReturn the expected format that T is represented by in the response from endpoint.\n\nUsing the default dataformat(::Type) method, the format is RawFormat.\n\nA dataformat(::Type) method is automatically defined when invoking @jsondef.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.interpretresponse","page":"API","title":"RestClientScaffold.interpretresponse","text":"interpretresponse(data::IO, fmt::AbstractFormat, ::Type{T}) -> value::T\n\nInterpret data as a response of type T according to fmt.\n\n\n\n\n\n","category":"function"},{"location":"api/#RestClientScaffold.writepayload","page":"API","title":"RestClientScaffold.writepayload","text":"writepayload(dest::IO, fmt::AbstractFormat, data)\n\nWrite data to dest according to fmt.\n\n\n\n\n\n","category":"function"},{"location":"#RestClientScaffold","page":"Introduction","title":"RestClientScaffold","text":"","category":"section"},{"location":"","page":"Introduction","title":"Introduction","text":"Interacting with a RESTful API is fairly simple, but there's often a fair bit of boilerplate involved in making well-formed requests, handling serialisation and deserialisation, request limiting, and result pagination.","category":"page"},{"location":"","page":"Introduction","title":"Introduction","text":"RestClientScaffold aims to take care of as much of this boilerplate as possible. See the tutorial for a sense of what this looks like in practice.","category":"page"},{"location":"tutorial/#Tutorial","page":"Tutorial","title":"Tutorial","text":"","category":"section"},{"location":"tutorial/#Basic-setup","page":"Tutorial","title":"Basic setup","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"To demonstrate how this can simplify API wrapping, in this tutorial we'll implement a client for the Deck of Cards API. For demonstration purposes, we'll just wrap part of the API.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Looking at the API documentation, we can see that","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"The base URL is https://deckofcardsapi.com/api/\nDecks can be created at the deck/new API (optionally shuffled)\nOperations on a deck occur at deck/<id>/operation endpoints","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"That's it! To get started we'll create an blank package and call it \"DeckAPI\" (I did this in my temp directory).","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"pkg> generate DeckAPI\n  Generating  project DeckAPI:\n    DeckAPI/Project.toml\n    DeckAPI/src/DeckAPI.jl","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Then, we activate the project and add JSON3 and RestClientScaffold as dependencies.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"pkg> activate DeckAPI\n\n(DeckAPI) pkg> add RestClientScaffold JSON3\n    Updating `/tmp/DeckAPI/Project.toml`\n  [d1389577] + RestClientScaffold v0.1.0\n  [0f8b85d8] + JSON3 v1.14.1\n    Updating `/tmp/DeckAPI/Manifest.toml`\n  [...] ...","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Then we can navigate to DeckAPI/src/DeckAPI.jl, get rid of the default greet() = print(\"Hello World!\"), and add","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"using RestClientScaffold, JSON3","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Now we're ready to get started implementing the API! To start with, we'll want to create a RequestConfig to hold the context in which we're calling the API. This just holds the base URL, a request lock, API access key (optional), and timeout value (optional). The Deck of Cards API is simple enough that we can set a single global RequestConfig, but in a more complex case we might define a utility function to create a RequestConfig based on user-provided parameters.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"const DECK_CONFIG = RequestConfig(\"https://deckofcardsapi.com/api\")","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"That's all the setup needed, next we'll define types for the JSON structures that Desk of Cards can return.","category":"page"},{"location":"tutorial/#Defining-API-types","page":"Tutorial","title":"Defining API types","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Reading the documentation in order, we come across a few types we'll want to implement. First, there's the new deck object","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"{\n    \"success\": true,\n    \"deck_id\": \"3p40paa87x90\",\n    \"shuffled\": true,\n    \"remaining\": 52\n}","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Thanks to the @jsondef macro, this is merely a matter of","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@jsondef struct Deck\n    # success::Bool # I don't think we care about this?\n    id.\"deck_id\"::String\n    # shuffled::Bool # We should know this anyway?\n    remaining::Int\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"We can also draw cards, which look like this","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@jsondef struct Card\n    code::String\n    # image # Not needed by us (at the moment)\n    value::String\n    suit::String\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Cards are given as a list in a certain response form, which gives us an opportunity to define a ListResponse subtype.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@jsondef struct CardsResponse <: ListResponse{Card}\n    deck.\"deck_id\"::String\n    remaining::Int\n    cards::Vector{Card}\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Since we've subtyped ListResponse, this will automagically be turned into a List holding cards.","category":"page"},{"location":"tutorial/#Adding-endpoints","page":"Tutorial","title":"Adding endpoints","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"In RestClientScaffold, each endpoint is represented with a dedicated subtype of AbstractEndpoint. Endpoints that provide a single value should subtype SingleEndpoint, while endpoints that provide multiple values should subtype ListEndpoint.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"While we could define the structs and the interface methods separately, we can conveniently combine these steps with the @endpoint macro.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Let's start off with deck creation.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct NewDeckEndpoint <: SingleEndpoint\n    \"deck/new{ifelse(self.shuffle, \\\"/shuffle\\\", \\\"\\\")}?deck_count={count}\" -> Deck\n    count::Int\n    shuffle::Bool\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"We can also shuffle decks.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct ShuffleEndpoint <: SingleEndpoint\n    \"deck/{deck}/shuffle?{remaining}\" -> Deck\n    deck::String\n    remaining::Bool\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Now let's draw some cards.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct DrawEndpoint <: ListEndpoint\n    \"deck/{deck}/draw?{count}\" -> CardsResponse\n    deck::String\n    count::Int\nend","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Cards can also be returned to the deck.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"@endpoint struct CardReturnEndpoint <: SingleEndpoint\n    \"deck/{deck}/return\" -> Deck\n    deck::String\n    cards::Union{Nothing, Vector{Card}}\nend\n\nfunction RestClientScaffold.parameters(ret::CardReturnEndpoint)\n    if isnothing(ret.cards)\n        Pair{String, String}[]\n    else\n        [\"cards\" => join(map(c -> c.code, ret.cards), \",\")]\n    end\nend","category":"page"},{"location":"tutorial/#Defining-our-API","page":"Tutorial","title":"Defining our API","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Now we've gone to the effort of defining all our types and endpoints, we just need to create the API for our package.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"new(count::Int = 1; shuffled::Bool=false) =\n    api_get(Request(DECK_CONFIG, NewDeckEndpoint(count, shuffled)))","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"shuffle(deck::Deck, remaining::Bool=false) =\n    api_get(Request(DECK_CONFIG, ShuffleEndpoint(deck.id, remaining)))","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"draw(deck::Deck, count::Int=1) =\n    api_get(Request(DECK_CONFIG, DrawEndpoint(deck.id, count)))","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"putback(deck::Deck, cards::Union{Nothing, Vector{Card}} = nothing) =\n    api_get(Request(DECK_CONFIG, CardReturnEndpoint(deck.id, cards)))\n\nputback(cardlist::List{Cards}) = putback(cardlist.request.endpoint.deck, cardlist.items)","category":"page"},{"location":"tutorial/#Demonstration","page":"Tutorial","title":"Demonstration","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"By starting Julia with the environment variable JULIA_DEBUG=RestClientScaffold set, we will see information on the requests sent and responses received. This helps us verify that everything is behaving as expected, and debug any failures or unexpected results.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> deck = DeckAPI.new()\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/new?deck_count=1\n└ @ RestClientScaffold\n┌ Debug:  200  80 bytes (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/new?deck_count=1\n└ @ RestClientScaffold\nDeckAPI.Deck(id=\"01n3ezer3rly\", remaining=52)\n\njulia> cards = DeckAPI.draw(deck, 5)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClientScaffold\n┌ Debug:  200  1.181 KiB (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClientScaffold\nRestClientScaffold.List{DeckAPI.Card} holding 5 items:\n  • Card(code=\"AS\", value=\"ACE\", suit=\"SPADES\")\n  • Card(code=\"2S\", value=\"2\", suit=\"SPADES\")\n  • Card(code=\"3S\", value=\"3\", suit=\"SPADES\")\n  • Card(code=\"4S\", value=\"4\", suit=\"SPADES\")\n  • Card(code=\"5S\", value=\"5\", suit=\"SPADES\")\n\njulia> DeckAPI.putback(cards)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/return?cards=AS%2C2S%2C3S%2C4S%2C5S\n└ @ RestClientScaffold\n┌ Debug:  200  61 bytes (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/return?cards=AS%2C2S%2C3S%2C4S%2C5S\n└ @ RestClientScaffold\nDeckAPI.Deck(id=\"01n3ezer3rly\", remaining=52)\n\njulia> DeckAPI.shuffle(deck)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/shuffle?remaining=false\n└ @ RestClientScaffold\n┌ Debug:  200  79 bytes (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/shuffle?remaining=false\n└ @ RestClientScaffold\nDeckAPI.Deck(id=\"01n3ezer3rly\", remaining=52)\n\njulia> cards = DeckAPI.draw(deck, 5)\n┌ Debug:  GET  https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClientScaffold\n┌ Debug:  200  1.183 KiB (saved to /tmp/api-get.dump) from https://deckofcardsapi.com/api/deck/01n3ezer3rly/draw?count=5\n└ @ RestClientScaffold\nRestClientScaffold.List{DeckAPI.Card} holding 5 items:\n  • Card(code=\"3C\", value=\"3\", suit=\"CLUBS\")\n  • Card(code=\"QC\", value=\"QUEEN\", suit=\"CLUBS\")\n  • Card(code=\"4S\", value=\"4\", suit=\"SPADES\")\n  • Card(code=\"2D\", value=\"2\", suit=\"DIAMONDS\")\n  • Card(code=\"3S\", value=\"3\", suit=\"SPADES\")","category":"page"}]
}
