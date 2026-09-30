# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module TableExt

using Tables
using RestClient

# Rows are records: structs with fields (so not e.g. `String`)
isrecordtype(T::Type) = isstructtype(T) && fieldcount(T) > 0

Tables.isrowtable(::Type{<:List{T}}) where {T} = isrecordtype(T)

function Tables.schema(list::List{T}) where {T}
    isrecordtype(T) || return nothing
    Tables.Schema(fieldnames(T), fieldtypes(T))
end

end
