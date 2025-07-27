# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module StructTypesExt

using AcademicIdentifiers
using StructTypes

for idtype in (ArXiv, DOI, ISSN, ISBN, OCN, ORCID, OpenAlexID, ROR, PMID, PMCID, Wikidata)
    @eval StructTypes.StructType(::Type{$idtype}) = StructTypes.StringType()
    @eval StructTypes.construct(::Type{$idtype}, id::String; _kw...) = parse($idtype, id)
end

end
