using Documenter
using RestClient
using Org

orgfiles = filter(f -> endswith(f, ".org"),
                  readdir(joinpath(@__DIR__, "src"), join=true))

for orgfile in orgfiles
    mdfile = replace(orgfile, r"\.org$" => ".md")
    read(orgfile, String) |>
        c -> Org.parse(OrgDoc, c) |>
        o -> sprint(markdown, o) |>
        s -> replace(s, r"\.org]" => ".md]") |>
        m -> write(mdfile, m)
end

makedocs(;
    modules=[RestClient],
    pages=[
        "Introduction" => "index.md",
        "Tutorial" => "tutorial.md",
        "Streaming" => "streaming.md",
        "API" => "api.md",
    ],
    format=Documenter.HTML(assets=["assets/favicon.ico"]),
    sitename="RestClient.jl",
    authors = "tecosaur and contributors: https://github.com/tecosaur/RestClient.jl/graphs/contributors",
    warnonly = [:missing_docs],
)

deploydocs(repo="github.com/tecosaur/RestClient.jl")
