# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module StructTypesExt

using AcademicIdentifiers
using StructTypes

StructTypes.StructType(::Type{<:AcademicIdentifier}) = StructTypes.StringType()
StructTypes.construct(::Type{T}, id::String; _kw...) where {T<:AcademicIdentifier} = parse(T, id)

end
