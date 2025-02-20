module TableExt

using Tables
using RestClient

Tables.isrowtable(::Type{List{T}}) where {T} = isstructtype(T)

function Tables.schema(list::List{T}) where {T}
    isstructtype(T) || return Tables.Schema((), ())
    Tables.Schema(fieldnames(T), fieldtypes(T))
end

end
