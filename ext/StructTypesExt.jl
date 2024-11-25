module StructTypesExt

using AcademicIdentifiers
using StructTypes

StructTypes.StructType(::Type{DOI}) = StructTypes.StringType()
StructTypes.construct(::Type{DOI}, doi::String; _kw...) = DOI(doi)

StructTypes.StructType(::Type{ORCID}) = StructTypes.StringType()
StructTypes.construct(::Type{ORCID}, orcid::String; _kw...) = ORCID(orcid)

StructTypes.StructType(::Type{ROR}) = StructTypes.StringType()
StructTypes.construct(::Type{ROR}, ror::String; _kw...) = ROR(ror)

StructTypes.StructType(::Type{PMID}) = StructTypes.StringType()
StructTypes.construct(::Type{PMID}, pmid::String; _kw...) = PMID(pmid)

StructTypes.StructType(::Type{PMCID}) = StructTypes.StringType()
StructTypes.construct(::Type{PMCID}, pmcid::String; _kw...) = PMCID(pmcid)

StructTypes.StructType(::Type{ISSN}) = StructTypes.StringType()
StructTypes.construct(::Type{ISSN}, issn::String; _kw...) = ISSN(issn)

StructTypes.StructType(::Type{ISBN}) = StructTypes.StringType()
StructTypes.construct(::Type{ISBN}, isbn::String; _kw...) = ISBN(isbn)

StructTypes.StructType(::Type{Wikidata}) = StructTypes.StringType()
StructTypes.construct(::Type{Wikidata}, wikidata::String; _kw...) = Wikidata(wikidata)

end
