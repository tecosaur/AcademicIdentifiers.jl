# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module JsonExt

using AcademicIdentifiers
using JSON

JSON.lower(id::AcademicIdentifier) = string(id)
JSON.lift(::Type{T}, id::String) where {T<:AcademicIdentifier} = parse(T, id)

end
