# The endpoint macro, in all its glory

const ANAPHORIC_VAR = :self

const SUPPORTED_HTTP_METHODS = (:get, :post)

"""
    @endpoint struct ... end
    @endpoint [func(...)] -> [struct ... end] -> [payload] -> url -> resulttype

Generate an endpoint implementation from a concise shorthand form.

This macro can be applied to either an endpoint struct definition or a chain of
_arrow forms_ joined by `->`. These arrow forms represent the flow of
information:
1. The creation of a request, from a function call (optional)
2. The `struct` used to represent the request data (optional, autogenerated from
   the function call)
3. Any payload provided with the request (if applicable)
4. The page URL the request is directed to (relative to the base URL)
5. The type that the response is expected to correspond to

For a basic API, this can be as simple as:

```julia
@endpoint israining(city::String) -> "checkrain/{city}" -> Bool
```

Using this macro is equivalent to implementing a plain struct, along with these
endpoint methods:
- [`pagename`](@ref)
- [`parameters`](@ref) (if needed)
- [`payload`](@ref) (optionally)
- [`responsetype`](@ref)

In our `@endpoint israining() ...` example, in addition to defining the
`israining` function and an (implicit) `IsrainigEndpoint` struct, the macro will
generate the implementations `pagename(rain::IsrainingEndpoint) = "checkrain/\$(rain.city)"` and
`responsetype(::IsrainingEndpoint) = Bool`.

In addition to applying `@endpoint` to a sequence of arrow forms you can also
apply it to a `struct` that starts with a special line of non-`struct` arrows.

For instance, our `israining()` example could be alternatively written as:

```julia
@endpoint struct IsrainingEndpoint
    israining(city) -> "checkrain/{city}" -> Bool
    city::String
end
```

This generates the exact same code as the initial example form.

For more information on how each component of the shorthand form is interpreted,
as well as more complex examples, see the extended help section.

See also: [`pagename`](@ref), [`parameters`](@ref), [`payload`](@ref),
[`responsetype`](@ref), [`Request`](@ref), [`globalconfig`](@ref), [`@globalconfig`](@ref).

# Extended help

## Function forms

Should you chose to include a function call as the first arrow form, a function
definition with a matching signature will be generated. This requires
[`globalconfig(::Val{@__Module__})`](@ref globalconfig) to be defined, as is
done by [`@globalconfig`](@ref).

Without a `struct` form, the arguments of the function will be used to generate
a `struct` with a derived name (`\$(titlecase(funcname))Endpoint`), and populate
its fields.

With a `struct` form, all arguments that match a field name will inherit the
field's type and default value . All the fields of the struct not present in the
signature will be added as keyword arguments. This makes function forms an easy
way to prescribe positional, mandatory arguments.

## Struct forms

The endpoint struct can be given explicitly, or generated from the function
call. For instance, the function call `api(target::String; count::Int=1)` will
generate this struct form:

```
@kwdef struct ApiEndpoint
    target::String
    count::Int = 1
end
```

When autogenerated, or not specified, the endpoint supertype is selected from
`AbstractEndpoint`, `SingleEndpoint`, and `ListEndpoint` based on the supertype
of the output form.

If `@endpoint` is directly applied to a `struct`, the first line of the struct
is removed and parsed as a series of arrow forms.

```
@endpoint ApiEndpoint
    api(target; count) -> ...
    target::String
    count::Int = 1
end
```

This makes no difference to the generated code, and is purely a matter of
personal preference (I prefer explicit structs for more complex forms).

## Payload forms

HTTP methods like `:post`, `:put`, and `:patch` include a content payload (also
referred to as the request body). The payload can be specified with a _payload
form_ that names a field of the endpoint struct, names a global variable, or is
an expression is evaluated to generate the payload value (with the current
endpoint available through the anaphoric variable `$ANAPHORIC_VAR`).

Unless otherwise specified, an endpoint that includes a payload is assumed to be
a `:post` request (`:get` without).

```
@endpoint upload(content::String) -> content -> "create" -> Status
```

In this example `content` is a `String`, and so a `:post` request is made to
`"\$BASEURL/create"` with `content` as the payload/body. More complex types are
encoded according with [`writepayload`](@ref) according to [`dataformat`](@ref).

## URL forms

The endpoint URL is represent by a string, relative to the base URL of the API
(a leading `/` is stripped). Such paths usually consist of a path component, and
optionally a query component.

```julia
"some/path/for/my/api?query=param&other=value"
```

Within this endpoint URL, you can include references to fields of the struct (or
global variables) by surrounding them with curly braces.

```julia
"page/{somefield}?param={another}&{globalvar}=7"
```

For convenience, a parameter by the same name as the field can be referred to by
the field name alone (e.g. `?{var}` instead of `?var={var}`).

In more complex cases, arbitrary Julia code can be included in the curly braces.
This code will be evaluated with the endpoint value bound the the anaphoric
variable `$ANAPHORIC_VAR`.

```julia
"page/{if self.new \\"new\\" else \\"fetch\\" end}/{id}"
```

If that's not enough, you can also use an arbitrary expression in place of the
endpoint string:

```julia
if self.create "pages/create/" else "pages/fetch/id/" * self.id end
```

When using expression instead of a string though, curly braces are not
interpreted as field references.

### Specifying the HTTP method

By default, it is guessed whether the API expects a `:get` or `:post` request
based on the presence or absence of an input form. This can be explicitly
specified by wrapping the URL form in an HTTP method symbol, for example:

```julia
:head("existence/{entity}")
```

## Response type forms

It is sensible to parse the raw response into a more informative Julia
representation. The response type specifies what type the response can be parsed
to, which is performed by [`interpretresponse`](@ref) according to
[`dataformat`](@ref).

Often the result type will name a struct defined with [`@jsondef`].

The parsed result can be reinterpreted into another form with
[`postprocess`](@ref). By default, [`ListResponse`](@ref)-type responses from a
[`ListEndpoint`](@ref) are restructured into a [`List`](@ref) to facilitate
paging.

# Examples

```julia
@endpoint struct ShuffleEndpoint <: SingleEndpoint
    "deck/{deck}/shuffle?remaining={ifelse(self.remaining, '1', '0')}" -> Deck
    deck::String
    remaining::Bool
end
```

This is equivalent to defining the struct by itself, and then separately
defining the three basic endpoint methods.

```julia
struct ShuffleEndpoint <: SingleEndpoint
    deck::String
    remaining::Bool
end

$(@__MODULE__).pagename(shuf::ShuffleEndpoint) =
    "deck/\$(shuf.deck)/shuffle"
$(@__MODULE__).parameters(shuf::ShuffleEndpoint) =
    ["remaining" => string(shuf.remaining)]
$(@__MODULE__).responsetype(shuf::ShuffleEndpoint) = Deck
```
"""
macro endpoint(expr::Expr)
    einfo = extract_forms(expr; mod=__module__)
    efields = Symbol[f.name for f in einfo.fields]
    body = Expr[]
    # Standard components: struct, page, parameters, responsetype
    push!(body, Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")), __source__, einfo.structdef))
    defform(func::Symbol, ::Nothing, body) = :($(@__MODULE__).$func($ANAPHORIC_VAR::$(einfo.structname)) = $body)
    defform(func::Symbol, params::Vector{Symbol}, body) =
        :($(@__MODULE__).$func($ANAPHORIC_VAR::$(einfo.structname){$(params...)}) where {$(params...)} = $body)
    if !isnothing(einfo.in)
        inval = varform(einfo.in, varname = ANAPHORIC_VAR, knownfields = efields, mod=__module__)
        push!(body, :($(@__MODULE__).payload($ANAPHORIC_VAR::$(einfo.structname)) = $inval))
    end
    path, params = if einfo.url isa String
        parse_endpoint_url(einfo.url, varname = ANAPHORIC_VAR, knownfields = efields, mod=__module__, filename = string(__source__.file))
    else
        varform(einfo.url, varname = ANAPHORIC_VAR, knownfields = efields, mod=__module__), nothing
    end
    push!(body, defform(:pagename, einfo.structparams, path))
    !isnothing(params) &&
        push!(body, defform(:parameters, einfo.structparams, Expr(:vect, params...)))
    push!(body, defform(:responsetype, einfo.structparams, einfo.out))
    # Function (optional)
    if !isnothing(einfo.func)
        for fn in generate_funcalls(einfo; mod=__module__)
            push!(body, fn)
        end
    end
    esc(Expr(:block, body...))
end

function generate_funcalls(einfo; mod::Module)
    includedargs = Symbol[]
    fspecs = Dict(f.name => (; type=f.type, default=f.default) for f in einfo.fields)
    fname = first(einfo.func.args)
    args, kwargs = if length(einfo.func.args) >= 2 && Meta.isexpr(einfo.func.args[2], :parameters)
        einfo.func.args[3:end], einfo.func.args[2].args
    else
        einfo.func.args[2:end], Expr[]
    end
    for set in (args, kwargs), (i, arg) in enumerate(set)
        name, type, default = nothing, nothing, nothing
        if Meta.isexpr(arg, :kw, 2)
            arg, default = arg.args
        end
        if Meta.isexpr(arg, :(::), 2)
            name, type = arg.args
        elseif arg isa Symbol
            name = arg
        end
        push!(includedargs, name)
        fspec = get(fspecs, name, nothing)
        isnothing(fspec) && throw(ArgumentError("Argument $name not found in @endpoint struct"))
        if (isnothing(default) && !isnothing(fspec.default)) || (isnothing(type) && !isnothing(fspec.type))
            newarg = name
            newtype = something(fspec.type, type, Some(nothing))
            newdefault = something(fspec.default, default, Some(nothing))
            if isnothing(type)
                newarg = Expr(:(::), newarg, newtype)
            end
            if isnothing(default)
                newarg = Expr(:kw, newarg, newdefault)
            end
            set[i] = newarg
        end
    end
    for field in einfo.fields
        if field.name ∉ includedargs
            farg = field.name
            if !isnothing(field.type)
                farg = Expr(:(::), farg, field.type)
            end
            if !isnothing(field.default)
                farg = Expr(:kw, farg, field.default)
            end
            push!(kwargs, farg)
        end
    end
    fcall = if isempty(kwargs)
        Expr(:call, fname, args...)
    else
        Expr(:call, fname, Expr(:parameters, kwargs...), args...)
    end
    if !isnothing(einfo.structparams)
        fcall = Expr(:where, fcall, einfo.structparams...)
    end
    estrucform = Expr(:call, einfo.structname, map(f -> f.name, einfo.fields)...)
    reqmethod = QuoteNode(einfo.method)
    f1 = :($fcall = perform(Request{$reqmethod}(globalconfig(Val{$mod}()), $estrucform)))
    confarg = gensym("request-config")
    pushfirst!(args, Expr(:(::), confarg, GlobalRef(@__MODULE__, :RequestConfig)))
    fcall = if isempty(kwargs)
        Expr(:call, fname, args...)
    else
        Expr(:call, fname, Expr(:parameters, kwargs...), args...)
    end
    if !isnothing(einfo.structparams)
        fcall = Expr(:where, fcall, einfo.structparams...)
    end
    f2 = :($fcall = perform(Request{$reqmethod}($confarg, $estrucform)))
    f1, f2
end

function extract_forms(expr::Expr; mod::Module)
    efunc, esuper, estruct, estructname, estructparams, efields, ein, emethod, eurl, eout = ntuple(_ -> nothing, 10)
    components = if Meta.isexpr(expr, :struct)
        sinfo = decompose_struct(expr)
        !isnothing(sinfo.special) ||
            throw(ArgumentError("Expected special form with @endpoint, not a plain struct"))
        estruct = sinfo.desugar
        estructname = sinfo.name
        estructparams = sinfo.params
        efields = sinfo.fields
        esuper = sinfo.super
        linearize_arrowforms(sinfo.special)
    elseif Meta.isexpr(expr, :->, 2)
        linearize_arrowforms(expr)
    else
        throw(ArgumentError("@endpoint may be applied to either a struct or a series of -> forms, not a $(expr.head)"))
    end
    # 1: Check for `func(...) <: Type`
    if !isempty(components) && Meta.isexpr(first(components), :<:, 2)
        if !isnothing(estruct)
            throw(ArgumentError("When a struct is defined, the subtype must be declared by the struct form"))
        elseif !isnothing(esuper)
            throw(ArgumentError("Multiple subtype declarations in @endpoint"))
        end
        subject, esuper = popfirst!(components).args
        if Meta.isexpr(subject, :call)
            efunc = subject
        else
            throw(ArgumentError("Supertype declarations may be applied the function or stuct"))
        end
    end
    # 2: Check for `func(...)`
    if !isempty(components) && Meta.isexpr(first(components), :call)
        efunc = popfirst!(components)
    end
    # 3: Extract fields from function arguments
    if !isnothing(efunc) && isnothing(estruct)
        efields = @NamedTuple{name::Symbol, type::Any, default::Any}[]
        fargs = if isempty(efunc.args)
            Expr[]
        elseif Meta.isexpr(first(efunc.args), :parameters)
            vcat(efunc.args[3:end], efunc.args[1].args)
        else
            efunc.args[2:end]
        end
        for arg in fargs
            name, type, default = nothing, Any, nothing
            if Meta.isexpr(arg, :kw, 2)
                arg, default = arg.args
            end
            if arg isa Symbol
                name = arg
            elseif Meta.isexpr(arg, :(::), 2)
                name, type = arg.args
            else
                throw(ArgumentError("Malformed function argument $arg in @endpoint function"))
            end
            push!(efields, (name, type, default))
        end
    end
    # 4: Check for `struct`
    if isnothing(estruct) && !isempty(components) && Meta.isexpr(first(components), :struct)
        sinfo = decompose_struct(popfirst!(components))
        isnothing(sinfo.special) ||
            throw(ArgumentError("Struct definition in @endpoint within a `->` chain must not contain a special line"))
        estruct = sinfo.desugar
        estructname = sinfo.name
        estructparams = sinfo.params
        efields = sinfo.fields
        (!isnothing(esuper) && !isnothing(sinfo.super)) &&
            throw(ArgumentError("Multiple subtype declarations in @endpoint"))
        if isnothing(sinfo.super) && !isnothing(esuper)
            estruct.args[2] = Expr(:<:, sinfo.name, esuper)
        end
    end
    # Check that remaining components are: `[in] -> url -> out`
    if length(components) < 2
        throw(ArgumentError("Expected @endpoint to include a URL and response type"))
    elseif length(components) > 3
        throw(ArgumentError("Too many components in @endpoint, at most could contain `func -> struct -> in -> url -> out`"))
    end
    ein, urlform, eout = if length(components) == 2
        nothing, components...
    else
        components
    end
    emethod, eurl = if Meta.isexpr(urlform, :call, 2) && urlform.args[1] isa QuoteNode
        urlform.args[1].value, urlform.args[2]
    else
        ifelse(isnothing(ein), :get, :post), urlform
    end
    emethod ∈ SUPPORTED_HTTP_METHODS || throw(ArgumentError("Unsupported HTTP method $emethod"))
    # Attempt to infer missing parameters
    if isnothing(esuper)
        outtype = Core.eval(mod, eout)
        esuper = if outtype <: SingleResponse
            :SingleEndpoint
        elseif outtype <: ListResponse
            :ListEndpoint
        else
            :AbstractEndpoint
        end
        if !isnothing(estruct) && !isnothing(estructname)
            estruct.args[2] = Expr(:<:, estructname, esuper)
        end
    end
    if isnothing(estruct) && !isnothing(efunc)
        fieldlines = Any[]
        for (; name, type, default) in efields
            fline = name
            if !isnothing(type)
                fline = Expr(:(::), name, type)
            end
            if !isnothing(default)
                fline = Expr(:(=), fline, default)
            end
            push!(fieldlines, fline)
        end
        estructname = Symbol(titlecase(String(efunc.args[1])) * "Endpoint")
        estructparams = nothing
        estruct = Expr(:struct, false, Expr(:<:, estructname, esuper), Expr(:block, fieldlines...))
    end
    (; func=efunc, structdef=estruct, structname=estructname, structparams=estructparams,
     fields=efields, in=ein, method=emethod, url=eurl, out=eout)
end

function decompose_struct(strux::Expr)
    Meta.isexpr(strux, :struct, 3) ||
        throw(ArgumentError("@endpoint expects a struct definition"))
    Meta.isexpr(strux.args[3], :block) ||
        throw(ArgumentError("@endpoint expects a block definition within the struct"))
    structmutable, structdecl, structdef::Expr = strux.args
    structlabel, structsuper = if Meta.isexpr(structdecl, :(<:), 2)
        structdecl.args
    elseif structdecl isa Symbol
        structdecl, nothing
    else
        throw(ArgumentError("Struct name/supertype declaration is malformed"))
    end
    structname, structparams = if Meta.isexpr(structlabel, :curly)
        first(structlabel.args)::Symbol,
        [if Meta.isexpr(a, :(<:), 2) first(a.args) else a end::Symbol
             for a in structlabel.args[2:end]]
    else
        structlabel::Symbol, nothing
    end
    specialline = nothing
    fields = @NamedTuple{name::Symbol, type::Any, default::Any}[]
    for line in structdef.args
        if line isa LineNumberNode
            continue
        elseif isempty(fields) && isnothing(specialline) && Meta.isexpr(line, :(->), 2)
            specialline = line
            continue
        end
        type, default = :Any, nothing
        if Meta.isexpr(line, :(=), 2)
            line, default = line.args
        end
        if Meta.isexpr(line, :(::), 2)
            line, type = line.args
        end
        name::Symbol = line
        push!(fields, (name, type, default))
    end
    desugar = if isnothing(specialline)
        strux
    else
        Expr(:struct, structmutable, structdecl, Expr(:block, structdef.args[3:end]...))
    end
    (; name=structname, super=structsuper, params=structparams, fields=fields, desugar, special=specialline)
end

"""
    linearize_arrowforms(expr::Expr) -> Vector{Any}

Linearize an expression of the form `a -> b -> c` into a vector of component
expressions.
"""
function linearize_arrowforms(expr::Expr)
    components = Any[]
    while Meta.isexpr(expr, :->, 2)
        push!(components, expr.args[1])
        expr = if Meta.isexpr(expr.args[2], :block, 2)
            expr.args[2].args[2]
        end
    end
    push!(components, expr)
    components
end

"""
    parse_endpoint_url(url::String; kwargs...)

Split an endpoint URL into its path and query components, and parse each using
`interp_curlies` (passing `kwargs`).
"""
function parse_endpoint_url(url::String; kwargs...)
    path, query = if '?' in url
        split(url, '?', limit=2)
    else
        url, nothing
    end
    path = chopprefix(path, "/")
    isnothing(query) && return interp_curlies(String(path); percentescape=true, kwargs...), nothing
    components = split(query, '&')
    parameters = Expr[]
    for comp in components
        if '=' in comp
            key, value = split(comp, '=', limit=2)
            keyex, valex = interp_curlies(String(key); kwargs...), interp_curlies(String(value); kwargs...)
            push!(parameters, :($keyex => $valex))
        elseif startswith(comp, '{') && endswith(comp, '}')
            compparsed = Meta.parse(comp)
            Meta.isexpr(compparsed, :braces, 1) || throw(ArgumentError("Invalid query component $comp"))
            compvar = first(compparsed.args)
            compvar isa Symbol || throw(ArgumentError("Invalid query component $comp"))
            kwargs2 = ((; filename, k...) -> k)(; kwargs...)
            compex = Expr(:call, GlobalRef(Base, :string), varform(compvar; kwargs2...))
            push!(parameters, :($(String(compvar)) => $compex))
        else
            throw(ArgumentError("Invalid query component $comp"))
        end
    end
    interp_curlies(String(path); percentescape=true, kwargs...), parameters
end

"""
    interp_curlies(str::String; filename::String = "unknown", kwargs...)

Interpret `{curly}` expressions in `str` with `varform` (passing through `kwargs`).
"""
function interp_curlies(str::String; filename::String = "unknown", percentescape::Bool=false, kwargs...)
    components = Union{String, Expr}[]
    lastidx = idx = firstindex(str)
    escaped = false
    while idx < ncodeunits(str)
        if escaped
            escaped = false
            idx += 1
        elseif str[idx] == '\\'
            escaped = true
            idx += 1
        elseif str[idx] == '{'
            lastidx < idx &&
                push!(components, str[lastidx:prevind(str, idx)])
            expr, idx = Meta.parseatom(str, idx; filename)
            Meta.isexpr(expr, :braces, 1) ||
                throw(ArgumentError("Expected single {curly} form in URL, instead saw $expr"))
            exval = first(expr.args)
            exstr = Expr(:call, GlobalRef(Base, :string), varform(exval; kwargs...))
            push!(components, if percentescape; :($encode_uri_component($exstr)) else exstr end)
            lastidx = idx
        else
            idx = nextind(str, idx)
        end
    end
    if lastidx <= lastindex(str)
        push!(components, str[lastidx:end])
    end
    if length(components) == 1
        first(components)
    else
        Expr(:call, GlobalRef(Base, :string), components...)
    end
end

"""
    varform(exval; varname::Symbol, knownfields::Vector{Symbol}, mod::Module)

Convert implicit references to a field of a variable `varname` into explicit references.

A field reference is recognised when `exval` is one of `knownfields`. If `exval` is a variable
but not a member of `knownfields` or a global variable, an `ArgumentError` is thrown.
"""
function varform(exval; varname::Symbol, knownfields::Vector{Symbol}, mod::Module)
    if exval isa Symbol
        if exval ∈ knownfields
            :($varname.$exval)
        elseif isdefined(mod, exval)
            exval
        else
            throw(ArgumentError("$exval is not a known field or global variable"))
        end
    elseif Meta.isexpr(exval, :., 2)
        dotpath = Any[exval]
        while Meta.isexpr(dotpath[end], :., 2)
            elt = dotpath[end]
            dotpath[end] = elt.args[2]
            push!(dotpath, elt.args[1])
        end
        last(dotpath) isa Symbol || return exval
        if last(dotpath) ∈ knownfields
            dotpath[end] = QuoteNode(last(dotpath))
            foldl((d, x) -> Expr(:., d, x), reverse(dotpath), init=varname)
        elseif isdefined(mod, last(dotpath))
            exval
        else
            throw(ArgumentError("$exval is not a known field or global variable"))
        end
    else
        exval
    end
end
