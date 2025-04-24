# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module TableExt

using Tables
using RestClient

Tables.isrowtable(::Type{List{T}}) where {T} = isstructtype(T)

function Tables.schema(list::List{T}) where {T}
    isstructtype(T) || return Tables.Schema((), ())
    Tables.Schema(fieldnames(T), fieldtypes(T))
end

end
