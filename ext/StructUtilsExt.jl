# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module StructUtilsExt

using AcademicIdentifiers
using StructUtils

StructUtils.structlike(::Type{<:AcademicIdentifier}) = false
StructUtils.lower(id::AcademicIdentifier) = string(id)
StructUtils.lift(::Type{T}, id::String) where {T<:AcademicIdentifier} = parse(T, id)

end
