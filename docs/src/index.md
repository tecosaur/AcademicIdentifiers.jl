# AcademicIdentifiers.jl

Structured and validated types for common identifiers used in and around academic work.

Built on [FastIdentifiers.jl](https://code.tecosaur.net/tec/FastIdentifiers), each
type provides parsing (from shortcodes, prefixed forms, and URLs), canonical string
output, persistent URL generation, and round-trip guarantees.

## Index

```@index
```

## Identifier Types

### Bibliographic

```@docs
ArXiv
DOI
EAN13
ISBN
ISSN
OCN
PMCID
PMID
```

### People & Organisations

```@docs
ISNI
OpenAlexID
ORCID
ROR
VIAF
```

### Research Activities & Entities

```@docs
RAiD
Wikidata
```

## Abstract Types

```@docs
AcademicIdentifier
```
