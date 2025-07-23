# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module StructTypesExt

using AcademicIdentifiers
using StructTypes

for idtype in (ArXiv, DOI, ORCID, ROR, PMID, PMCID, ISSN, ISBN, Wikidata)
    @eval StructTypes.StructType(::Type{$idtype}) = StructTypes.StringType()
    @eval StructTypes.construct(::Type{$idtype}, id::String; _kw...) = $idtype(id)
end

end
