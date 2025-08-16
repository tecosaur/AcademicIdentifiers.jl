# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

"""
    AcademicIdentifiers

Structured and validated types for academic identifiers.

## Implemented Identifiers

- `ArXiv` - arXiv preprint identifiers
- `DOI` - Digital Object Identifiers
- `ISNI` - International Standard Name Identifiers
- `ISSN` - International Standard Serial Numbers
- `ISBN` - International Standard Book Numbers
- `OCN` - OCLC Control Numbers
- `ORCID` - Open Researcher and Contributor Identifiers
- `OpenAlexID` - OpenAlex identifiers
- `RAiD` - Research Activity Identifiers
- `ROR` - Research Organization Registry identifiers
- `PMID` - PubMed Identifiers
- `PMCID` - PubMed Central Identifiers
- `VIAF` - Virtual International Authority File identifiers
- `Wikidata` - Wikidata entity identifiers

## Examples

```julia
julia> using AcademicIdentifiers

julia> doi = parse(DOI, "10.1371/journal.pone.0068810")
DOI:10.1371/journal.pone.0068810

julia> string(doi)
"doi:10.1371/journal.pone.0068810"

julia> purl(doi)
"https://doi.org/10.1371/journal.pone.0068810"

julia> orcid = parse(ORCID, "https://orcid.org/0000-0002-1825-0097")
ORCID:0000-0002-1825-0097

julia> shortcode(orcid)
"0000-0002-1825-0097"
```
"""
module AcademicIdentifiers

using DigitalIdentifiersBase
import DigitalIdentifiersBase: idcode, idchecksum, shortcode, purl, purlprefix, parseid, parsefor, lchopfolded, unsafe_substr

export AcademicIdentifier, ArXiv, DOI, ISNI, ISSN, ISBN, OCN, ORCID, OpenAlexID, RAiD, ROR, PMID, PMCID, VIAF, Wikidata
DigitalIdentifiersBase.@reexport

include("isbn-hyphenation.jl")

"""
    AcademicIdentifier <: AbstractIdentifier

An abstract type representing an academic identifier.

Academic identifiers are unique identifiers referring to resources used in
academic and scholarly contexts. They can refer to a wide variety of resources,
including publications, researchers, and organisations.

See also: `AbstractIdentifier`.
"""
abstract type AcademicIdentifier <: AbstractIdentifier end

digitstart(s::AbstractString) = !isempty(s) && codeunit(s, 1) ∈ 0x30:0x39

function iso7064mod11m2checksum(num::Integer)
    digsum = 0
    for dig in Iterators.reverse(digits(num))
        digsum = (digsum + dig) * 2
    end
    (12 - digsum % 11) % 11
end

function parsedashcode(::Type{I}, str::AbstractString, skip::NTuple{N, Char}) where {I <: Integer, N}
    isempty(str) && return nothing
    sbytes = map(UInt8, skip)
    i, ndigits, num = 1, 0, zero(UInt64)
    @inbounds while i < ncodeunits(str)
        b = codeunit(str, i)
        if b ∈ 0x30:0x39
            num = muladd(num, 10, b - 0x30)
            ndigits += 1
        elseif b ∈ sbytes
        else
            return nothing
        end
        i += 1
    end
    lastdigit = codeunit(str, ncodeunits(str))
    check = if lastdigit | 0x20 == UInt8('x')
        0x0a
    else
        lastdigit - 0x30
    end
    (; num, ndigits, check)
end

parsedashcode(T::Type{<:Integer}, num::AbstractString, skip::Char) =
    parsedashcode(T, num, (skip,))

 
# ArXiv

"""
    ArXiv <: AcademicIdentifier

An ArXiv identifier is a unique identifier for preprints in the arXiv repository. It consists of a
`YYMM.NNNNN` format, where `YY` is the year, `MM` is the month, and `NNNNN` is the number of the preprint.
Preprints prior to 2015 have a 4-digit number, while those from 2015 onwards have a 5-digit number.

Prior to 2007, ArXiv identifiers were in the form `archive.class/YYMMNNN`, where `archive` is the
name of the archive (e.g., `math`, `hep-th`) and `class` is a two-letter class code (e.g., `CO`, `EP`).

A version number can be appended to the identifier, separated by a 'v'.

As of 2022, all ArXiv identifiers have a corresponding DOI (which has been
backdated to include all ArXiv entries).

# Examples

```julia-repl
julia> parse(ArXiv, "1411.1607v4")
ArXiv:2001.12345v4

julia> parse(ArXiv, "https://arxiv.org/abs/1411.1607")
ArXiv:2001.12345

julia> purl(parse(ArXiv, "arXiv:1411.1607"))
"https://arxiv.org/abs/2001.12345"

julia> convert(DOI, parse(ArXiv, "1411.1607v4"))
DOI:10.48550/arXiv.1411.1607

julia> parse(ArXiv, "math.GT/0309136") # pre-2007 form
ArXiv:math.GT/0309136

julia> parse(ArXiv, "arxiv:hep-th/9901001v1")
ArXiv:hep-th/9901001v1
```
"""
struct ArXiv <: AcademicIdentifier
    meta::UInt32 # It's this or we go over 8 bytes
    number::UInt32
end

function parseid(::Type{ArXiv}, id::SubString)
    _, id = lchopfolded(id, "https://", "http://")
    isweb, id = lchopfolded(id, "arxiv.org/")
    if isweb
        prefixend = findfirst('/', id)
        isnothing(prefixend) && return MalformedIdentifier{ArXiv}(id, "incomplete ArXiv URL")
        id = unsafe_substr(id, prefixend)
    else
        _, id = lchopfolded(id, "arxiv:")
    end
    if occursin('/', id)
        arxiv_old(id)
    else
        arxiv_new(id)
    end
end

arxiv_meta(archive::UInt8, class::UInt8, year::UInt8, month::UInt8, version::UInt16) =
    UInt32(archive) << (32 - 5) +
    UInt32(class) << (32 - 11) +
    UInt32(year) << (32 - 18) +
    UInt32(month) << (32 - 22) +
    version

arxiv_archive(arxiv::ArXiv) = (arxiv.meta >> (32 - 5)) % UInt8
arxiv_class(arxiv::ArXiv) = 0x3f & (arxiv.meta >> (32 - 11)) % UInt8
arxiv_year(arxiv::ArXiv) = 0x7f & (arxiv.meta >> (32 - 18)) % UInt8
arxiv_month(arxiv::ArXiv) = 0x0f & (arxiv.meta >> (32 - 22)) % UInt8
arxiv_version(arxiv::ArXiv) = arxiv.meta % UInt16 & 0x03ff

function arxiv_new(id::AbstractString)
    ncodeunits(id) >= 6 || return MalformedIdentifier{ArXiv}(id, "is too short to be a valid ArXiv identifier")
    bytes = codeunits(id)
    bdigit(b::UInt8) = b ∈ 0x30:0x39
    local year, month
    y1, y2, m1, m2 = @inbounds bytes[1], bytes[2], bytes[3], bytes[4]
    all(bdigit, (y1, y2)) || return MalformedIdentifier{ArXiv}(id, "year component (YYmm.nnnnn) must be an integer")
    all(bdigit, (m1, m2)) || return MalformedIdentifier{ArXiv}(id, "month component (yyMM.nnnnn) must be an integer")
    year = 0xa * (y1 - 0x30) + (y2 - 0x30)
    month = 0xa * (m1 - 0x30) + (m2 - 0x30)
    month ∈ 1:12 || return MalformedIdentifier{ArXiv}(id, "month component (yyMM.nnnnn) must be between 01 and 12")
    (@inbounds bytes[5]) == UInt8('.') || return MalformedIdentifier{ArXiv}(id, "must contain a period separating the date and number component (yymm.nnnnn)")
    i, number, version = 6, zero(UInt32), zero(UInt16)
    @inbounds while i <= ncodeunits(id)
        b = bytes[i]
        i += 1
        if (b | 0x20) == UInt8('v')
            i > ncodeunits(id) && return MalformedIdentifier{ArXiv}(id, "version component must be non-empty")
            break
        elseif bdigit(b)
            number = muladd(number, UInt32(10), b - 0x30)
        else
            return MalformedIdentifier{ArXiv}(id, "number component (yymm.NNNNN) must be an integer")
        end
    end
    number <= UInt32(99999) || return MalformedIdentifier{ArXiv}(id, "number component (yymm.NNNNN) must no more than 5 digits")
    @inbounds while i <= ncodeunits(id)
        b = bytes[i]
        if bdigit(b)
            version = muladd(version, UInt8(10), b - 0x30)
            iszero(version & ~0x03ff) || return MalformedIdentifier{ArXiv}(id, "version is larger than the maximum supported value (1023)")
        else
            return MalformedIdentifier{ArXiv}(id, "version component must be an integer")
        end
        i += 1
    end
    ArXiv(arxiv_meta(0x00, 0x00, year, month, version), number)
end

const ARXIV_OLD_ARCHIVES, ARXIV_OLD_CLASSES = let
    arxiv_catsubs = (
        "astro-ph" => ["CO", "EP", "GA", "HE", "IM", "SR"],
        "cond-mat" => ["dis-nn", "mes-hall", "mtrl-sci", "other", "quant-gas", "soft", "stat-mech", "str-el", "supr-con"],
        "cs" => ["AI", "AR", "CC", "CE", "CG", "CL", "CR", "CV", "CY", "DB", "DC", "DL", "DM", "DS", "ET",
                 "FL", "GL", "GR", "GT", "HC", "IR", "IT", "LG", "LO", "MA", "MM", "MS", "NA", "NI", "OH",
                 "OS", "PF", "PL", "RO", "SC", "SD", "SE", "SI", "SY"],
        "econ" => ["EM", "GN", "TH"],
        "eess" => ["AS", "IV", "SP", "SY"],
        "gr-qc" => String[],
        "hep-ex" => String[],
        "hep-lat" => String[],
        "hep-ph" => String[],
        "hep-th" => String[],
        "math-ph" => String[],
        "math" => ["AC", "AG", "AP", "AT", "CA", "CO", "CT", "CV", "DG", "DS", "FA", "GM", "GN", "GR", "GT",
                   "HO", "IT", "KT", "LO", "MG", "MP", "NA", "NT", "OA", "OC", "PR", "QA", "RA", "RT", "SG",
                   "SP", "ST",],
        "nlin" => ["AO", "CD", "CG", "PS", "SI"],
        "nucl-ex" => String[],
        "nucl-th" => String[],
        "physics" => ["acc-ph", "ao-ph", "app-ph", "atm-clus", "atom-ph", "bio-ph", "chem-ph", "class-ph",
                      "comp-ph", "data-an", "ed-pn", "flu-dyn", "gen-ph", "geo-ph", "hist-ph", "ins-det",
                      "med-ph", "optics", "plasm-ph", "pop-ph", "soc-ph", "space-ph"],
        "q-bio" => ["BM", "CB", "GN", "MN", "NC", "OT", "PE", "QM", "SC", "TO"],
        "q-fin" => ["CP", "EC", "GN", "MF", "PM", "PR", "RM", "ST", "SR"],
        "quant-ph" => String[],
        "stat" => ["AP", "CO", "ME", "ML", "OT", "TH"])
    map(first, arxiv_catsubs), map(last, arxiv_catsubs)
end

function arxiv_old(id::AbstractString)
    bytes = codeunits(id)
    slashpos = @something(findfirst(==(UInt8('/')), bytes),
                          return MalformedIdentifier{ArXiv}(id, "must contain a slash separating the components (archive.class/YYMMNNN)"))
    archclass = unsafe_substr(id, 0, slashpos - 1)
    numverstr = unsafe_substr(id, slashpos)
    dotpos = something(findfirst(==(UInt8('.')), view(bytes, 1:slashpos)), slashpos)
    archive = unsafe_substr(archclass, 0, dotpos - 1)
    class = unsafe_substr(archclass, dotpos, max(0, slashpos - dotpos - 1))
    archiveidx = findfirst(==(archive), ARXIV_OLD_ARCHIVES)
    isnothing(archiveidx) && return MalformedIdentifier{ArXiv}(id, "does not use a recognised ArXiv archive name")
    classidx = if isempty(class)
        0
    else
        findfirst(==(class), ARXIV_OLD_CLASSES[archiveidx])
    end
    isnothing(classidx) && return MalformedIdentifier{ArXiv}(id, "does not use a recognised ArXiv archive class")
    length(class) ∈ (0, 2) || return MalformedIdentifier{ArXiv}(id, "class component must be 2 characters")
    #--
    ncodeunits(numverstr) >= 5 || return MalformedIdentifier{ArXiv}(id, "is too short to be a valid ArXiv identifier")
    bytes = codeunits(numverstr)
    bdigit(b::UInt8) = b ∈ 0x30:0x39
    local year, month
    y1, y2, m1, m2 = @inbounds bytes[1], bytes[2], bytes[3], bytes[4]
    all(bdigit, (y1, y2)) || return MalformedIdentifier{ArXiv}(id, "year component (YYmmnnnn) must be an integer")
    all(bdigit, (m1, m2)) || return MalformedIdentifier{ArXiv}(id, "month component (yyMMnnnn) must be an integer")
    year = 0xa * (y1 - 0x30) + (y2 - 0x30)
    month = 0xa * (m1 - 0x30) + (m2 - 0x30)
    (year >= 91 || year <= 7) || return MalformedIdentifier{ArXiv}(id, "year component (YYmmnnn) must be between 91 and 07")
    month ∈ 1:12 || return MalformedIdentifier{ArXiv}(id, "month component (yyMMnnnn) must be between 01 and 12")
    i, number, version = 5, zero(UInt32), zero(UInt16)
    @inbounds while i <= ncodeunits(numverstr)
        b = bytes[i]
        i += 1
        if (b | 0x20) == UInt8('v')
            i > ncodeunits(id) && return MalformedIdentifier{ArXiv}(id, "version component must be non-empty")
            break
        elseif bdigit(b)
            number = muladd(number, UInt32(10), b - 0x30)
        else
            return MalformedIdentifier{ArXiv}(id, "number component (yymmNNNN) must be an integer")
        end
    end
    number <= UInt32(9999) || return MalformedIdentifier{ArXiv}(id, "number component (yymmNNNN) must no more than 4 digits")
    @inbounds while i <= ncodeunits(numverstr)
        b = bytes[i]
        if bdigit(b)
            version = muladd(version, UInt8(10), b - 0x30)
            iszero(version & ~0x03ff) || return MalformedIdentifier{ArXiv}(id, "version is larger than the maximum supported value (1023)")
        else
            return MalformedIdentifier{ArXiv}(id, "version component must be an integer")
        end
        i += 1
    end
    #--
    ArXiv(arxiv_meta(archiveidx % UInt8, classidx % UInt8, year, month, version), number)
end

function shortcode(io::IO, arxiv::ArXiv)
    archid, classid = arxiv_archive(arxiv), arxiv_class(arxiv)
    if !iszero(archid) # Old form
        print(io, ARXIV_OLD_ARCHIVES[archid])
        if !iszero(classid)
            print(io, '.', ARXIV_OLD_CLASSES[archid][classid])
        end
        print(io, '/')
    end
    year, month, ver = arxiv_year(arxiv), arxiv_month(arxiv), arxiv_version(arxiv)
    print(io, lpad(year, 2, '0'), lpad(month, 2, '0'))
    if iszero(archid) # New form
        print(io, '.', lpad(arxiv.number, ifelse(year >= 15, 5, 4), '0'))
    else # Old form
        print(io, lpad(arxiv.number, 3, '0'))
    end
    if ver > 0
        print(io, 'v', ver)
    end
end

idcode(arxiv::ArXiv) =
    UInt64(arxiv.meta & 0xffffff00) << 32 +
    UInt64(arxiv.number) << 8 +
    arxiv_version(arxiv)

purlprefix(::Type{ArXiv}) = "https://arxiv.org/abs/"

function Base.print(io::IO, arxiv::ArXiv)
    get(io, :limit, false) === true && get(io, :compact, false) === true ||
        print(io, "arXiv:")
    shortcode(io, arxiv)
end


# DOI

"""
    DOI <: AcademicIdentifier

A Digital Object Identifier (DOI) is a unique identifier for a broad range of digital objects.

A DOI it consists of a registrant prefix and an object suffix separated by a
slash. The registrant prefix consists solely of digits and periods, while
the object suffix is unrestricted, but treated as ASCII case-insensitive.

# Examples

```julia-repl
julia> parse(DOI, "10.1145/3276490")
doi:10.1145/3276490

julia> parse(DOI, "https://doi.org/10.1137/141000671")
DOI:10.1137/141000671

julia> println(parse(DOI, "10.1145/3276490"))
doi:10.1145/3276490

julia> parse(DOI, "10.123/abc") == parse(DOI, "10.123/AbC")
true
```
"""
struct DOI <: AcademicIdentifier
    registrant::SubString{String}
    object::SubString{String}
end

function Base.convert(::Type{DOI}, arxiv::ArXiv)
    arxiv_nover = ArXiv(arxiv.meta & 0xfffffe00, arxiv.number)
    doistr = string("10.48550/", "arXiv.", shortcode(arxiv_nover))
    DOI(SubString(doistr, 1, 8), SubString(doistr, 10))
end

function parseid(::Type{DOI}, doi::SubString{String})
    if !digitstart(doi)
        _, doi = lchopfolded(doi, "doi:", "https://", "http://", "dx.doi.org/", "doi.org/")
    end
    registrant, object = if '/' in doi
        split(doi, '/', limit=2)
    else
        doi, ""
    end
    isempty(object) && return MalformedIdentifier{DOI}(doi, "must contain an object component after the slash")
    lastdecimal = 0
    for (i, char) in enumerate(codeunits(registrant))
        if char == UInt8('.')
            i - 1 == lastdecimal && return MalformedIdentifier{DOI}(doi, "registrant cannot initial or consecutive periods")
            lastdecimal = i
        elseif char ∉ UInt8('0'):UInt8('9')
            return MalformedIdentifier{DOI}(doi, "registrant must be a sequence of digits and periods")
        end
    end
    lastdecimal == 0 && return MalformedIdentifier{DOI}(doi, "registrant must contain at least one period")
    DOI(registrant, object)
end

parseid(::Type{DOI}, doi::SubString) = parseid(DOI, SubString(String(doi)))

purlprefix(::Type{DOI}) = "https://doi.org/"
shortcode(io::IO, doi::DOI) = print(io, doi.registrant, '/', doi.object)

Base.print(io::IO, doi::DOI) = (print(io, "doi:"); shortcode(io, doi))
Base.show(io::IO, doi::DOI) = (show(io, DOI); show(io, (doi.registrant, doi.object)))

function Base.:(==)(a::DOI, b::DOI)
    a.registrant == b.registrant || return false
    ncodeunits(a.object) == ncodeunits(b.object) || return false
    for (ca, cb) in zip(codeunits(a.object), codeunits(b.object))
        xasciia = ifelse(ca ∈ 0x41:0x5a, 0x20, 0x00)
        xasciib = ifelse(cb ∈ 0x41:0x5a, 0x20, 0x00)
        (ca | xasciia) == (cb | xasciib) || return false
    end
    true
end

function Base.hash(doi::DOI, h::UInt)
    h = hash(doi.registrant, h)
    for c in codeunits(doi.object)
        xascii = ifelse(c ∈ 0x41:0x5a, 0x20, 0x00)
        h = hash(c | xascii, h)
    end
    h
end


# OCN

"""
    OCN <: AcademicIdentifier

An **OCLC control number** (OCN) is the primary key that identifies a
bibliographic master record in WorldCat, the global union catalogue maintained
by the Online Computer Library Center (OCLC).

Depending on the number of digits, OCNs may be prefixed with `ocm`, `ocn`, or `on`.

### Examples

```julia-repl
julia> parse(OCN, "ocm00045678")
OCN:45678

julia> parse(OCN, "(OCoLC)ocn148290923")
OCN:148290923

julia> print(parse(OCN, "https://www.worldcat.org/oclc/12345678901"))
on12345678901
```
"""
struct OCN <: AcademicIdentifier
    id::UInt64
end

function parseid(::Type{OCN}, id::SubString)
    isempty(id) && return MalformedIdentifier{OCN}(id, "cannot be empty")
    if first(id) == '('
        _, id = lchopfolded(id, "(ocolc)")
    end
    if !digitstart(id)
        _, id = lchopfolded(id, "ocn:", "oclc:", "ocolc", "oclc", "ocm", "ocn", "on", "https://", "www.", "worldcat.org/oclc/")
    end
    id = lstrip(id)
    all(isdigit, id) || return MalformedIdentifier{OCN}(id, "must only consist of digits")
    num = parsefor(OCN, UInt64, id)
    num isa UInt64 || return num
    OCN(num)
end

purlprefix(::Type{OCN}) = "https://worldcat.org/oclc/"

function Base.print(io::IO, ocn::OCN)
    print(io, if ocn.id < 10^8
              "ocm"
          elseif ocn.id < 10^9
              "ocn"
          else
              "on"
          end, ocn.id)
end


# ORCID

"""
    ORCID <: AcademicIdentifier

An Open Researcher and Contributor ID (ORCID) is a unique identifier for researchers and contributors
to academic works. It consists of a 16-digit integer with a checksum digit.

Invalid ORCID identifiers will throw a `MalformedIdentifier` exception, and identifiers with an
incorrect checksum will throw a `ChecksumViolation` exception.

# Examples

```julia-repl
julia> parse(ORCID, "https://orcid.org/0000-0001-5109-3700")
ORCID:https://orcid.org/0000-0001-5109-3700

julia> println(parse(ORCID, "0000-0001-5109-3700"))
https://orcid.org/0000-0001-5109-3700

julia> parse(ORCID, "0000-0001-5109-3701")
ERROR: Checksum violation: the correct checksum for ORCID identifier 15109370 is 0 but got 1
```
"""
struct ORCID <: AcademicIdentifier
    id::UInt64
    function ORCID(id::Union{Int64, UInt64})
        id >= 0 || throw(MalformedIdentifier{ORCID}(id, "must be a non-negative integer"))
        ndigits(id) <= 15 || throw(MalformedIdentifier{ORCID}(id, "must be a 16-digit integer"))
        i7064check = iso7064mod11m2checksum(id)
        oid = UInt64(id) + UInt64(i7064check) << 60
        new(oid)
    end
end

function parseid(::Type{ORCID}, id::SubString)
    if !digitstart(id)
        _, id = lchopfolded(id, "orcid:", "orcid ", "https://", "http://", "orcid.org/")
    end
    code = parsedashcode(UInt64, id, '-')
    isnothing(code) && return MalformedIdentifier{ORCID}(id, "must only consist of digits and hyphens")
    1 <= code.ndigits <= 15 ||
        return MalformedIdentifier{ORCID}(id, "must be a 2-16 digit identifier")
    try ORCID(code.num, code.check) catch e; e end
end

idcode(orcid::ORCID) = Int(orcid.id & 0x003fffffffffffff)
idchecksum(orcid::ORCID) = (orcid.id >> 60) % UInt8

function shortcode(io::IO, orcid::ORCID)
    idstr, check = string(idcode(orcid)), idchecksum(orcid)
    join(io, Iterators.partition(lpad(idstr, 15, '0'), 4), '-')
    print(io, '0' + ifelse(check == 0xa, 0x28, check))
end

purlprefix(::Type{ORCID}) = "https://orcid.org/"
Base.show(io::IO, orcid::ORCID) = (show(io, ORCID); show(io, (idcode(orcid), idchecksum(orcid))))


# OpenAlex

"""
    OpenAlexID{kind} <: AcademicIdentifier

An OpenAlex identifier is a primary key for resources in the OpenAlex database.

It consists of a category prefix (a single letter) followed by positive integer.
Currently, OpenAlex defines seven entity categories:
- **W** for works
- **A** for authors
- **S** for sources
- **I** for institutions
- **C** for concepts
- **P** for publishers
- **F** for funders

Invalid OpenAlex identifiers will throw a `MalformedIdentifier` exception.

# Examples

```julia-repl
julia> parse(OpenAlexID{:W}, "W2741809807")

julia> parse(OpenAlexID, "A5092938886")
OpenAlexID:A2741809807

julia> purl(parse(OpenAlexID, "W2741809807"))
"https://openalex.org/W2741809807"
```
"""
struct OpenAlexID{kind} <: AcademicIdentifier
    num::UInt64
end

function parseid(::Type{OpenAlexID{kind}}, id::SubString) where {kind}
    isempty(id) && return MalformedIdentifier{OpenAlexID{kind}}(id, "cannot be empty")
    klower, kupper = codeunit(String(kind), 1) | 0x20, codeunit(String(kind), 1)
    if codeunit(id, 1) ∉ (klower, kupper)
        _, id = lchopfolded(id, "openalex:", "https://", "http://", "openalex.org/")
        prefixend = findfirst('/', id)
        if !isnothing(prefixend)
            id = unsafe_substr(id, prefixend)
        end
        isempty(id) && return MalformedIdentifier{OpenAlexID{kind}}(id, "must include a kind prefix")
        codeunit(id, 1) | 0x20 == klower ||
            return MalformedIdentifier{OpenAlexID{kind}}(id, "kind does not match")
    end
    id = unsafe_substr(id, 1)
    num = parsefor(OpenAlexID{kind}, UInt64, id)
    num isa UInt64 || return num
    OpenAlexID{kind}(num)
end

function parseid(::Type{OpenAlexID}, id::SubString)
    isempty(id) && return MalformedIdentifier{OpenAlexID}(id, "cannot be empty")
    kindchar = codeunit(id.string, 1) & ~0x20
    if kindchar ∉ map(UInt8, ('W', 'A', 'S', 'I', 'C', 'P', 'F'))
        _, id = lchopfolded(id, "openalex:", "https://", "http://", "openalex.org/")
        prefixend = findfirst('/', id)
        if !isnothing(prefixend)
            id = unsafe_substr(id, prefixend)
        end
        isempty(id) && return MalformedIdentifier{OpenAlexID}(id, "must include a kind prefix")
        ncodeunits(id) == 1 && return MalformedIdentifier{OpenAlexID}(id, "must include a number after the kind prefix")
        kindchar = codeunit(id, 1) & ~0x20
        kindchar ∈ map(UInt8, ('W', 'A', 'S', 'I', 'C', 'P', 'F')) ||
            return MalformedIdentifier{OpenAlexID}(id, "unrecognised kind prefix")
    end
    num = parsefor(OpenAlexID, UInt64, unsafe_substr(id, 1))
    num isa UInt64 || return num
    OpenAlexID{Symbol(Char(kindchar & ~0x20))}(num)
end

shortcode(io::IO, id::OpenAlexID{kind}) where {kind} = print(io, kind, id.num)
purlprefix(@nospecialize(::OpenAlexID)) = "https://openalex.org/"


# RAiD

"""
    RAiD <: AcademicIdentifier

A Research Activity Identifier (RAiD) is a unique identifier for research activities.

All RAiDs have a corresponding DOI, which can be obtained by converting to a `DOI`.

Standardised in [ISO 23527](https://www.iso.org/standard/75931.html).

# Examples

```julia-repl
julia> parse(RAiD, "10.25.10.1234/a1b2c")
RAiD:10.25.10.1234/a1b2c

julia> parse(RAiD, "https://raid.org/10.25.10.1234/a1b2c")
RAiD:10.25.10.1234/a1b2c

julia> print(parse(RAiD, "10.25.10.1234/a1b2c"))
https://raid.org/10.25.10.1234/a1b2c
```
"""
struct RAiD <: AcademicIdentifier
    id::DOI
end

Base.convert(::Type{DOI}, raid::RAiD) = raid.id

function parseid(::Type{RAiD}, id::SubString)
    chopped, id = lchopfolded(id, "raid:")
    if !chopped
        _, id = lchopfolded(id, "https://", "http://", "raid.org/")
    end
    doi = parseid(DOI, id)
    doi isa DOI || return doi
    RAiD(doi)
end

purlprefix(::Type{RAiD}) = "https://raid.org/"

Base.:(==)(a::RAiD, b::RAiD) = a.id == b.id
Base.hash(raid::RAiD, h::UInt) = hash(raid.id, h)


# ROR

Base.@assume_effects :foldable function croc32decode(::Type{T}, str::AbstractString) where {T <: Integer}
    svec = codeunits(str) .| 0x20 # Assume letters, convert to lowercase
    skipchars = UInt8.(('i', 'l', 'o', 'u'))
    svec .= svec .- (UInt8(count(s .> skipchars)) for s in svec)
    parse(T, String(svec), base=32)
end

Base.@assume_effects :foldable function croc32encode(num::Integer)
    svec = collect(codeunits(string(num, base=32)))
    skipchars = UInt8.(('i', 'l'-1, 'o'-2, 'u'-3))
    svec .= svec .+ (UInt8(count(s .>= skipchars)) for s in svec)
    String(svec)
end

"""
    ROR <: AcademicIdentifier

A Research Organization Registry (ROR) identifier is a unique identifier for research organizations.
It consists of a 6-digit integer with a checksum digit.

Invalid ROR identifiers will throw a `MalformedIdentifier` exception, and identifiers with an
incorrect checksum will throw a `ChecksumViolation` exception.

# Examples

```julia-repl
julia> parse(ROR, "https://ror.org/05cy4wa09")
ROR:05cy4wa09

julia> print(parse(ROR, "05cy4wa09"))
https://ror.org/05cy4wa09

julia> parse(ROR, "05cy4wa08")
ERROR: Checksum violation: the correct checksum for ROR identifier 05cy4wa is 9 but got 8
```
"""
struct ROR <: AcademicIdentifier
    num::Int32
    function ROR(num::Integer)
        num >= 0 || throw(MalformedIdentifier{ROR}(num, "must be a non-negative value"))
        num <= croc32decode(Int, "zzzzzz") || throw(MalformedIdentifier{ROR}(croc32encode(num), "must be no more than 6-digits"))
        new(Int32(num))
    end
end

function ROR(num::Integer, check::Integer)
    ror = ROR(num)
    check == idchecksum(ror) ||
        throw(ChecksumViolation{ROR}('0' * croc32encode(num), idchecksum(ror), check))
    ror
end

function parseid(::Type{ROR}, num::SubString)
    chopped, num = lchopfolded(num, "ror:")
    if !chopped
        _, num = lchopfolded(num, "ror.org/", "https://ror.org/")
    end
    length(num) == 9 || return MalformedIdentifier{ROR}(num, "must be 9 characters long")
    char0, rest... = num
    char0 == '0' || return MalformedIdentifier{ROR}(num, "must start with '0'")
    all(c -> c ∈ 'a':'z' || c ∈ 'A':'Z' || c ∈ '0':'9', rest) || return MalformedIdentifier{ROR}(num, "must only contain alphanumeric characters")
    check = parsefor(ROR, UInt, view(rest, 7:8))
    check isa UInt || return check
    try ROR(croc32decode(Int, view(rest, 1:6)), check) catch e; e end
end

idcode(ror::ROR) = ror.num
idchecksum(ror::ROR) = 98 - ((ror.num * 100) % 97)
shortcode(io::IO, ror::ROR) = print(io, '0', lpad(croc32encode(ror.num), 6, '0'), lpad(string(idchecksum(ror)), 2, '0'))
purlprefix(::Type{ROR}) = "https://ror.org/"


# PMID

"""
    PMID <: AcademicIdentifier

A PubMed Identifier (PMID) is a unique identifier for a publication in the
PubMed database. It consists of an 8-digit integer. Invalid PMIDs will throw a
`MalformedIdentifier` exception.

PMID identifiers should not be confused with PubMed Central identifiers ([`PMCID`](@ref)s),
which are specifically used by PubMed Central but are distinct from PMIDs.

# Examples

```julia-repl
julia> PMID(28984872)
PMID:28984872

julia> parse(PMID, "https://pubmed.ncbi.nlm.nih.gov/28984872")
PMID:28984872

julia> PMID(123456789)
ERROR: Malformed identifier: PMID identifier 123456789 must be no more than 8 digits
```
"""
struct PMID <: AcademicIdentifier
    id::UInt
    function PMID(id::Union{Int, UInt})
        id >= 0 || throw(MalformedIdentifier{PMID}(id, "must be a non-negative value"))
        id <= 10^8 || throw(MalformedIdentifier{PMID}(id, "must be no more than 8 digits"))
        new(UInt(id))
    end
end

function parseid(::Type{PMID}, id::SubString)
    _, id = lchopfolded(id, "pmid:", " ", "https://", "http://", "www.", "pubmed.ncbi.nlm.nih.gov/")
    pint = parsefor(PMID, UInt, id)
    pint isa UInt || return pint
    try PMID(pint) catch e; e end
end

purlprefix(::Type{PMID}) = "https://pubmed.ncbi.nlm.nih.gov/"

function Base.print(io::IO, pmid::PMID)
    get(io, :limit, false) === true && get(io, :compact, false) === true ||
        print(io, "PMID:")
    print(io, pmid.id)
end


# PMCID

"""
    PMCID <: AcademicIdentifier

A PubMed Central Identifier (PMCID) is a unique identifier for a publication in the
PubMed Central database. It consists of an 8-digit integer. Invalid PMCIDs will throw a
`MalformedIdentifier` exception.

PMCID identifiers should not be confused with PubMed identifiers ([`PMID`](@ref)s),
which are specifically used by PubMed but are distinct from PMCIDs.

# Examples

```julia-repl
julia> parse(PMCID, "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC012345678")
PMCID:12345678

julia> println(PMCID(12345678))
PMC12345678

julia> PMCID(123456789)
ERROR: Malformed identifier: PMCID identifier 123456789 must be no more than 8 digits
```
"""
struct PMCID <: AcademicIdentifier
    id::UInt
    function PMCID(id::Union{Int, UInt})
        id >= 0 || throw(MalformedIdentifier{PMCID}(id, "must be a non-negative value"))
        id <= 10^8 || throw(MalformedIdentifier{PMCID}(id, "must be no more than 8 digits"))
        new(UInt(id))
    end
end

function parseid(::Type{PMCID}, id::SubString)
    _, id = lchopfolded(id, "pmcid:", " ", "https://", "http://", "www.", "pmc.", "ncbi.nlm.nih.gov/pmc/articles/", "pmc")
    pint = parsefor(PMCID, UInt, id)
    pint isa UInt || return pint
    try PMCID(pint) catch e; e end
end

shortcode(io::IO, pmcid::PMCID) = print(io, "PMC", pmcid.id)
purlprefix(::Type{PMCID}) = "https://www.ncbi.nlm.nih.gov/pmc/articles/"

Base.print(io::IO, pmcid::PMCID) = print(io, "PMC", pmcid.id)


# ISNI

"""
    ISNI <: AcademicIdentifier

An **International Standard Name Identifier** (ISNI, ISO 27729) uniquely
identifies natural persons and organisations engaged in creative activities.

It is a **16-character string** formed by a 15-digit base number followed by
a single checksum digit calculated with the ISO 7064 *MOD 11-2* algorithm.
The check digit can be **0–9** or **`X`** (representing the value 10).

Standardised in [ISO 27729](https://www.iso.org/standard/87177.html)

## Examples
```julia-repl
julia> parse(ISNI, "0000 0001 2103 2683")
ISNI:0000 0001 2103 2683

julia> parse(ISNI, "https://isni.org/isni/0000000121032683")
ISNI:0000 0001 2103 2683
```
"""
struct ISNI <: AcademicIdentifier
    code::UInt64
    function ISNI(id::Union{Int64, UInt64})
        ndigits(id) <= 15 || throw(MalformedIdentifier{ISNI}(id, "must be a 15-digit integer"))
        check = iso7064mod11m2checksum(id)
        new(UInt64(id) + UInt64(check) << 60)
    end
end

function parseid(::Type{ISNI}, id::SubString)
    if !digitstart(id)
        chopped, id = lchopfolded(id, "isni:", "isni ")
        if chopped
            id = lstrip(id)
        else
            _, id = lchopfolded(id, "https://", "http://", "www.",
                                "isni.org/isni/", "isni.org/", "isni",
                                "viaf.org/viaf/sourceID/ISNI%7C", "viaf.org/processed/ISNI%7C")
        end
    end
    code = parsedashcode(UInt64, id, ' ')
    isnothing(code) && return MalformedIdentifier{ISNI}(id, "must only consist of digits and spaces")
    1 <= code.ndigits <= 15 ||
        return MalformedIdentifier{ISNI}(id, "must be a 2-16 character string")
    code.check <= 0xa ||
        return MalformedIdentifier{ISNI}(id, "check digit must be 0-9 or X")
    try ISNI(code.num, code.check) catch e; e end
end

idcode(isni::ISNI) = Int(isni.code & 0x003fffffffffffff)
idchecksum(isni::ISNI) = (isni.code >> 60) % UInt8

function shortcode(io::IO, isni::ISNI)
    idstr, check = string(idcode(isni)), idchecksum(isni)
    join(io, Iterators.partition(lpad(idstr, 15, '0'), 4), ' ')
    print(io, '0' + ifelse(check == 0xa, 0x28, check))
end

purl(isni::ISNI) = string(
    "https://isni.org/isni/",
    '0' ^ (15 - ndigits(idcode(isni))),
    idcode(isni),
    if idchecksum(isni) == 10 "X" else
        string(idchecksum(isni)) end)


# ISSN

"""
    ISSN <: AcademicIdentifier

An International Standard Serial Number (ISSN) is a unique identifier for serial
publications.

It consists of a 7-digit integer with a checksum digit, and is standerdised in [ISO 3297](https://www.iso.org/standard/84536.html).

Invalid ISSNs will throw a `MalformedIdentifier` exception, and identifiers with
an incorrect checksum will throw a `ChecksumViolation` exception.

# Examples

```julia-repl
julia> parse(ISSN, "1095-5054")
ISSN:1095-5054

julia> parse(ISSN, "1095-5053")
ERROR: Checksum violation: the correct checksum for ISSN identifier 1095505 is 4 but got 3
```
"""
struct ISSN <: AcademicIdentifier
    code::UInt32
    function ISSN(id::Union{UInt32, Int32, UInt64, Int64})
        ndigits(id) <= 7 || throw(MalformedIdentifier{ISSN}(id, "must be a 7-digit integer"))
        digsum = 0
        for (i, dig) in enumerate(digits(id, pad=7))
            digsum += (i + 1) * dig
        end
        checkcalc = (11 - digsum % 11) % 11
        new(UInt32(id) + UInt32(checkcalc) << 24)
    end
end

function parseid(::Type{ISSN}, id::SubString)
    chopped, id = lchopfolded(id, "issn:", "issn")
    if chopped
        id = lstrip(id)
    else
        _, id = lchopfolded(id, "https://", "http://", "portal.issn.org/resource/ISSN/")
    end
    code = parsedashcode(UInt32, id, '-')
    isnothing(code) && return MalformedIdentifier{ISSN}(id, "must only consist of digits and hyphens")
    1 <= code.ndigits <= 7 ||
        return MalformedIdentifier{ISSN}(code, "must be a 2-8 digit integer")
    code.check <= 0xa ||
        return MalformedIdentifier{ISSN}(id, "check digit must be 0-9 or X")
    try ISSN(code.num, code.check) catch e; e end
end

idcode(issn::ISSN) = Int(issn.code & 0x00ffffff)
idchecksum(issn::ISSN) = Int8((issn.code & 0xff000000) >> 24)
function shortcode(io::IO, issn::ISSN)
    join(io, Iterators.partition(lpad(string(idcode(issn)), 7, '0'), 4), '-')
    csum = idchecksum(issn)
    print(io, '0' + ifelse(csum == 0xa, 0x28, csum))
end
purlprefix(::Type{ISSN}) = "https://portal.issn.org/resource/ISSN/"

function Base.print(io::IO, issn::ISSN)
    get(io, :limit, false) === true && get(io, :compact, false) === true ||
        print(io, "ISSN ")
    shortcode(io, issn)
end


# EAN13

"""
    EAN13 <: AcademicIdentifier

A European Article Number (EAN-13) is a 13-digit barcode standard which is a superset of the
original 12-digit Universal Product Code (UPC) system. It consists of a 12-digit code with
a checksum digit calculated using a weighted modulo-10 algorithm.

Invalid EAN-13 identifiers will throw a `MalformedIdentifier` exception, and identifiers with an
incorrect checksum will throw a `ChecksumViolation` exception.

# Examples

```julia-repl
julia> parse(EAN13, "9780596520687")
EAN13:9780596520687

julia> parse(EAN13, "978-0-596-52068-7")
EAN13:9780596520687

julia> println(parse(EAN13, "9780596520687"))
9780596520687

julia> parse(EAN13, "9780596520688")
ERROR: Checksum violation: the correct checksum for EAN13 identifier 978059652068 is 7 but got 8
```
"""
struct EAN13 <: AcademicIdentifier
    code::UInt64
    function EAN13(code::Integer)
        ndigits(code) <= 12 || throw(MalformedIdentifier{EAN13}(code, "must be a 12-digit integer"))
        digsum = 0
        for (i, dig) in enumerate(digits(code, pad=12))
            digsum += if i % 2 == 0 dig else dig * 3 end
        end
        checkcalc = (10 - digsum % 10) % 10
        new(10 * code + checkcalc)
    end
    function EAN13((code, flag)::Tuple{<:Integer, UInt16})
        ean = EAN13(code)
        new(ean.code | UInt64(flag) << 48)
    end
end

function parseid(::Type{EAN13}, id::SubString)
    code = parsedashcode(UInt64, id, '-')
    isnothing(code) && return MalformedIdentifier{EAN13}(id, "must only consist of digits and hyphens")
    code.ndigits > 13 &&
        return MalformedIdentifier{EAN13}(code, "must be a 13-digit integer")
    code.check <= 0x9 ||
        return MalformedIdentifier{EAN13}(id, "check digit must be 0-9")
    try EAN13(code.num, code.check) catch e; e end
end

idcode(ean::EAN13) = Int((0x00001fffffffffff & ean.code) ÷ 10)
idchecksum(ean::EAN13) = Int8(ean.code % 10)
shortcode(io::IO, ean::EAN13) = print(io, lpad(idcode(ean), 12, '0'), idchecksum(ean))

const IAN = EAN13


# ISBN

"""
    ISBN <: AcademicIdentifier

An International Standard Book Number (ISBN) is a unique identifier for books and book-like
publications.

Standerdised in [ISO 2108](https://www.iso.org/standard/65483.html), ISBNs
either consist of a 10-digit integer including a checksum digit, or an
EAN-13 code starting with 978 or 979.

Invalid ISBNs will throw a `MalformedIdentifier` exception, and identifiers with an incorrect
checksum will throw a `ChecksumViolation` exception.

# Examples

```julia-repl
julia> parse(ISBN, "0141439564")
ISBN("0-14-143956-4")

julia> parse(ISBN, "9781718502765")
ISBN:978-1-7185-0276-5
```
"""
struct ISBN <: AcademicIdentifier
    code::EAN13
end

Base.convert(::Type{EAN13}, isbn::ISBN) = isbn.code

function Base.convert(::Type{ISBN}, ean::EAN13)
    idcode(ean) ÷ 10^9 ∈ (978, 979) ||
        throw(MalformedIdentifier{ISBN}(idcode(ean), "must start with 978 or 979"))
    ISBN(ean)
end

function ISBN(id::Integer, check::Integer)
    ndigits(id) == 12 || throw(MalformedIdentifier{ISBN}(id, "must be a 12-digit integer"))
    id ÷ 10^9 ∈ (978, 979) || throw(MalformedIdentifier{ISBN}(id, "must start with 978 or 979"))
    ISBN(EAN13(id, check))
end

function ISBN(id::Integer)
    ndigits(id) == 12 || throw(MalformedIdentifier{ISBN}(id, "must be a 12-digit integer"))
    id ÷ 10^9 ∈ (978, 979) || throw(MalformedIdentifier{ISBN}(id, "must start with 978 or 979"))
    ISBN(EAN13(id))
end

function parseid(::Type{ISBN}, id::SubString)
    _, id = lchopfolded(id, "isbn:", "isbn")
    ncode = sum(c -> c ∉ (UInt8(' '), UInt8('-')), codeunits(id), init=0)
    if ncode == 13
        code = parsedashcode(UInt64, id, ('-', ' '))
        isnothing(code) && return MalformedIdentifier{ISBN}(id, "must only consist of digits and hyphens")
        code.check <= 0x9 || return MalformedIdentifier{ISBN}(id, "check digit must be 0-9")
        try ISBN(code.num, code.check) catch e; e end
    elseif ncode == 10
        code = parsedashcode(UInt64, id, ('-', ' '))
        isnothing(code) && return MalformedIdentifier{ISBN}(id, "must only consist of digits and hyphens")
        code.check <= 0xa || return MalformedIdentifier{ISBN}(id, "check digit must be 0-9 or X")
        csum = 0
        for (i, digit) in enumerate(digits(code.num * 10, pad=10))
            csum += i * digit
        end
        cdigit = 0xb - mod1(csum, 0xb)
        cdigit == code.check ||
            return ChecksumViolation{ISBN}(id, cdigit, code.check)
        try ISBN(EAN13((code.num, 0x1000 | UInt8(cdigit)))) catch e; e end
    else
        return MalformedIdentifier{ISBN}(id, "must be a 10 or 13-digit integer")
    end
end

function Base.print(io::IO, isbn::ISBN)
    get(io, :limit, false) === true && get(io, :compact, false) === true ||
        print(io, "ISBN ")
    shortcode(io, isbn)
end

function shortcode(io::IO, isbn::ISBN)
    flag = UInt16((isbn.code.code & UInt64(0xffff) << 48) >> 48)
    code3, code10 = if iszero(flag)
        divrem(isbn.code.code, 10^10)
    else
        978, idcode(isbn.code) * 10 + idchecksum(isbn.code)
    end
    iszero(flag) && print(io, lpad(code3, 3, '0'), '-')
    # Find group length by checking which range the first 7 digits fall into
    first7 = code10 ÷ 1000
    group_length = 0
    for (prefix, class) in ISBN_GROUP_HYPHENATION
        prefix == code3 || continue
        for (len, range) in class
            len > 0 && first(range) ≤ first7 ≤ last(range) || continue
            group_length = len
            break
        end
        group_length > 0 && break
    end
    # Extract group and remaining digits using arithmetic
    codegroup, remaining_after_group = if group_length > 0
        divrem(code10, 10^(10 - group_length))
    else
        0, code10
    end
    group_length > 0 && print(io, lpad(codegroup, group_length, '0'), '-')
    # Find publisher length by checking which range the remaining digits fall into
    remaining_digits = 10 - group_length
    remaining_padded = if remaining_digits < 7
        remaining_after_group * 10^(7 - remaining_digits)
    else
        remaining_after_group ÷ 10^(remaining_digits - 7)
    end
    publisher_length = 0
    for (prefix, classes) in ISBN_PUB_HYPHENATION
        prefix == code3 || continue
        for (group_code, class) in classes
            group_code == codegroup || continue
            for (len, range) in class
                len > 0 && first(range) ≤ remaining_padded ≤ last(range) || continue
                publisher_length = len
                break
            end
            publisher_length > 0 && break
        end
        publisher_length > 0 && break
    end
    # Extract publisher, title, and check digit using mathematical operations
    publisher, title_and_check = if publisher_length > 0
        divrem(remaining_after_group, 10^(remaining_digits - publisher_length))
    else
        0, remaining_after_group
    end
    publisher_length > 0 && print(io, lpad(publisher, publisher_length, '0'), '-')
    # Split title and check digit
    if title_and_check >= 10
        title, check_digit = divrem(title_and_check, 10)
    else
        title, check_digit = 0, title_and_check
    end
    title_length = remaining_digits - publisher_length - 1
    title_length ∈ 1:9 && print(io, lpad(title, title_length, '0'), '-')
    check = if iszero(flag)
        Char(0x30 + check_digit)
     else
        check_char = Int8(flag & 0x00ff)
        '0' + ifelse(check_char == 0xa, 0x28, check_char)
    end
    print(io, check)
end


# VIAF

"""
    VIAF <: AcademicIdentifier

A **V**irtual **I**nternational **A**uthority **F**ile identifier is a numeric
key used by libraries to disambiguate persons, corporate bodies, and works.

Invalid inputs throw `MalformedIdentifier{VIAF}`.

# Examples

```julia-repl
julia> parse(VIAF, "113230702")
VIAF:113230702

julia> parse(VIAF, "https://viaf.org/viaf/113230702/")
VIAF:113230702

julia> parse(VIAF, "VIAF:abc")
ERROR: Malformed identifier: VIAF identifier abc must contain only digits
```
"""
struct VIAF <: AcademicIdentifier
    id::UInt32
end

function parseid(::Type{VIAF}, id::AbstractString)
    chopped, id = lchopfolded(id, "viaf:")
    if !chopped
        _, id = lchopfolded(id, "https://", "http://", "viaf.org/viaf/")
    end
    all(isdigit, id) ||
        return MalformedIdentifier{VIAF}(id, "must contain only digits")
    vid = parsefor(VIAF, UInt32, id)
    vid isa UInt32 || return vid
    VIAF(vid)
end

purlprefix(::Type{VIAF}) = "https://viaf.org/viaf/"


# Wikidata

"""
    Wikidata <: AcademicIdentifier

A Wikidata identifier is a unique identifier for entities in the Wikidata knowledge base. It consists
of a 'Q' followed by an integer. Invalid Wikidata identifiers will throw a `MalformedIdentifier`
exception.

# Examples

```julia-repl
julia> parse(Wikidata, "Q42")
Wikidata:Q42

julia> print(parse(Wikidata, "Q42"))
Q42
```
"""
struct Wikidata <: AcademicIdentifier
    id::UInt64
end

function parseid(::Type{Wikidata}, id::SubString)
    chopped, id = lchopfolded(id, "wikidata:", "wd:")
    if !chopped
        _, id = lchopfolded(id, "https://", "http://", "www.", "wikidata.org/wiki/")
    end
    if startswith(id, 'Q')
        wint = parsefor(Wikidata, UInt64, id[2:end])
        wint isa UInt64 || return wint
        Wikidata(wint)
    else
        MalformedIdentifier{Wikidata}(id, "must start with 'Q'")
    end
end

shortcode(io::IO, wd::Wikidata) = print(io, 'Q', wd.id)
purlprefix(::Type{Wikidata}) = "https://www.wikidata.org/wiki/"

end
