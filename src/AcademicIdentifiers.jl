# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module AcademicIdentifiers

using StyledStrings: @styled_str as @S_str

export AcademicIdentifier, ArXiv, DOI, EAN13, ISSN, ISBN, ORCID, ROR, PMID, PMCID, Wikidata
export shortcode, purl

include("isbn-hyphenation.jl")

"""
    AcademicIdentifier

An abstract type representing an academic identifier.

Academic identifiers are unique identifiers referring to resources used in
academic and scholarly contexts. They can refer to a wide variety of resources,
including publications, researchers, and organisations. This type is used to
represent a common interface for working with these identifiers.

It is expected that all identifiers have a plain text canonical form, and
optionally a PURL (Persistent Uniform Resource Locator) that can be used to link
to the resource. These may be one and the same.

# API

## Mandatory components

Your academic identifier, named `AId` for example, must be able to be constructed
from its canonical form as well as the plain form (these may be the same).

```julia
AId("canonical string form") -> AId
AId("minimal plain form") -> AId
shortcode(::AId) -> String
```

Invariants:
- `AId(shortcode(x::AId)) == x`
- `x::AId == y::AId` *iff* `shortcode(x) == shortcode(y)`

If the constructor is passed a string that doesn't match the expected format, is
is reasonable to throw a [`MalformedIdentifier`](@ref) error.

When there is a checksum component to the identifier, it is usual for an inner
constructor to be defined that verifies the checksum matches or throw a
[`ChecksumViolation`](@ref) error. This makes invalid identifiers
unconstructable.

## Standard components

Most identifiers can be represented in a numerical form, possibly with a
checksum value. Should that be the case, it is recommended that you define the
`idcode` and `idchecksum` accessors.

```
idcode(::AId) -> Integer
idchecksum(::AId) -> Integer
```

Invariants:
- `idcode(x::AId) == idcode(y::AId) && idchecksum(x) == idchecksum(y)` *iff* `x == y`

## Optional components

When a standard persistent URL exists for the resource, you should define either
`purlprefix` when the URL is of the form `\$prefix\$(shortcode(x::AId))` or
`purl(x::AId)` when the URL scheme is more complicated.

```
purlprefix(::Type{AId}) -> String
purl(::Type{AId}) -> String
```

Invariants:
- `AId(purl(x::AId)) == x`
- `purl(x::AId) == purl(y::AId)` *iff* `x == y`
"""
abstract type AcademicIdentifier end

"""
    MalformedIdentifier{T<:AcademicIdentifier}(input, problem::String) -> MalformedIdentifier{T}

The provided `input` is not a recognised form of a `T` identifier,
due to the specified `problem`.
"""
struct MalformedIdentifier{T <: AcademicIdentifier, I} <: Exception
    input::I
    problem::String
end

MalformedIdentifier{T}(input::I, problem::String) where {T, I} =
    MalformedIdentifier{T, I}(input, problem)

"""
    ChecksumViolation{T<:AcademicIdentifier}(id, checksum, expected) -> ChecksumViolation{T}

The checksum `checksum` for the `T` identifier `id` is incorrect; the correct
checksum is `expected`.
"""
struct ChecksumViolation{T <: AcademicIdentifier, I} <: Exception
    id::I
    checksum::Integer
    expected::Integer
end

ChecksumViolation{T}(id::I, checksum::Integer, expected::Integer) where {T, I} =
    ChecksumViolation{T, I}(id, checksum, expected)

function Base.showerror(io::IO, @nospecialize(ex::MalformedIdentifier{T})) where {T}
    print(io, S"Malformed identifier: {bold:$T} identifier {emphasis:$(ex.input)} $(ex.problem)")
end

function Base.showerror(io::IO, @nospecialize(ex::ChecksumViolation{T})) where {T}
    print(io, S"Checksum violation: the correct checksum for {bold:$T} identifier {emphasis:$(ex.id)} \
                is {success:$(ex.expected)} but got {error:$(ex.checksum)}")
end

"""
    idcode(id::AcademicIdentifier) -> Union{Integer, Nothing}

If applicable, return the base identifier of an `AcademicIdentifier`.
"""
function idcode(::AcademicIdentifier) end

"""
    idchecksum(id::AcademicIdentifier) -> Union{Integer, Nothing}

If applicable, return the check digit of an `AcademicIdentifier`.
"""
function idchecksum(::AcademicIdentifier) end

"""
    shortcode(id::AcademicIdentifier) -> String

Return a plain string representation of an `AcademicIdentifier`.

This should be the minimal complete representation of the identifier,
with no additional formatting.

The canonical form for the identifier should contain the plain identifier,
but may include additional information such as a standard prefix and/or suffix.
"""
function shortcode end

"""
    purlprefix(::Type{<:AcademicIdentifier}) -> Union{String, Nothing}

Return the standard prefix of a PURL for an `AcademicIdentifier`, if applicable.

If defined, this implies that a PURL can be constructed by appending the `shortcode`
representation of the identifier to this prefix. As such, you should take care to
include any necessary trailing slashes or other separators in this prefix.
"""
function purlprefix(::Type{T}) where {T <: AcademicIdentifier} end

purlprefix(::T) where {T <: AcademicIdentifier} = purlprefix(T)

"""
    purl(id::AcademicIdentifier) -> Union{String, Nothing}

If applicable, return the PURL of an `AcademicIdentifier`.

PURLs are Persistent Uniform Resource Locators that provide a permanent link to
a resource.
"""
function purl(id::AcademicIdentifier)
    prefix = purlprefix(id)
    if !isnothing(prefix)
        prefix * shortcode(id)
    end
end

function Base.print(io::IO, id::AcademicIdentifier)
    print(io, something(purl(id), shortcode(id)))
end

function Base.show(io::IO, id::AcademicIdentifier)
    show(io, typeof(id))
    print(io, '(')
    show(io, shortcode(id))
    print(io, ')')
end

function Base.show(io::IO, ::MIME"text/plain", id::AcademicIdentifier)
    label = nameof(typeof(id))
    url = purl(id)
    idstr = shortcode(id)
    if isnothing(url)
        print(io, S"{bold:$label:}$idstr")
    else
        print(io, S"{bold:$label:}{link=$url:$idstr}")
    end
end


# ArXiv

"""
    ArXiv <: AcademicIdentifier

An ArXiv identifier is a unique identifier for preprints in the arXiv repository. It consists of a
`YYMM.NNNNN` format, where `YY` is the year, `MM` is the month, and `NNNNN` is the number of the preprint.
Preprints prior to 2015 have a 4-digit number, while those from 2015 onwards have a 5-digit number.

A version number can be appended to the identifier, separated by a 'v'.

# Examples

```julia
julia> ArXiv("2001.12345")
ArXiv:2001.12345

julia> ArXiv("https://arxiv.org/abs/2001.12345")
ArXiv:2001.12345

julia> print(ArXiv("arXiv:2001.12345"))
https://arxiv.org/abs/2001.12345

julia> ArXiv("math.GT/0309136") # pre-2007 form
ArXiv:math.GT/0309136

julia> ArXiv("arxiv:hep-th/9901001v1")
ArXiv:hep-th/9901001v1
"""
struct ArXiv <: AcademicIdentifier
    meta::UInt32 # It's this or we go over 8 bytes
    number::UInt32
end

function ArXiv(id::AbstractString)
    if startswith(id, "https://arxiv.org/")
        prefixend = findnext('/', id, ncodeunits("https://arxiv.org/")+1)
        return ArXiv(@view id[something(prefixend, ncodeunits("https://arxiv.org/"))+1:end])
    elseif startswith(lowercase(id), "arxiv:")
        return ArXiv(@view id[ncodeunits("arxiv:")+1:end])
    end
    if occursin('/', id)
        arxiv_old(id)
    else
        arxiv_new(id)
    end
end

arxiv_meta(archive::UInt8, class::UInt8, year::UInt8, month::UInt8, version::UInt8) =
    UInt32(archive) << (32 - 5) +
    UInt32(class) << (32 - 11) +
    UInt32(year) << (32 - 18) +
    UInt32(month) << (32 - 22) +
    version

arxiv_archive(arxiv::ArXiv) = (arxiv.meta >> (32 - 5)) % UInt8
arxiv_class(arxiv::ArXiv) = 0x3f & (arxiv.meta >> (32 - 11)) % UInt8
arxiv_year(arxiv::ArXiv) = 0x7f & (arxiv.meta >> (32 - 18)) % UInt8
arxiv_month(arxiv::ArXiv) = 0x0f & (arxiv.meta >> (32 - 22)) % UInt8
arxiv_version(arxiv::ArXiv) = arxiv.meta % UInt8

function arxiv_new(id::AbstractString)
    code, verstr = if 'v' in id
        split(id, 'v', limit=2)
    else
        id, "0"
    end
    version = tryparse(UInt8, verstr)
    isnothing(version) && throw(MalformedIdentifier{ArXiv}(id, "version must be an integer"))
    '.' in code || throw(MalformedIdentifier{ArXiv}(id, "must contain a period separating the date and number component (YYMM.NNNNN)"))
    datestr, numstr = split(code, '.', limit=2)
    all(isdigit, datestr) && ncodeunits(datestr) == 4 || throw(MalformedIdentifier{ArXiv}(id, "date component must be 4 digits (YYMM.nnnnn)"))
    year = tryparse(UInt8, @view datestr[1:2])
    isnothing(year) && throw(MalformedIdentifier{ArXiv}(id, "year component (YYmm.nnnnn) must be an integer"))
    month = tryparse(UInt8, @view datestr[3:4])
    isnothing(month) && throw(MalformedIdentifier{ArXiv}(id, "month component (yyMM.nnnnn) must be an integer"))
    1 <= month <= 12 || throw(MalformedIdentifier{ArXiv}(id, "month component (yyMM.nnnnn) must be between 01 and 12"))
    number = tryparse(UInt32, numstr)
    isnothing(number) && throw(MalformedIdentifier{ArXiv}(id, "number component (yymm.NNNNN) must be an integer"))
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
    '/' in id || throw(MalformedIdentifier{ArXiv}(id, "must contain a slash separating the components (archive.class/YYMMNNN)"))
    archclass, numverstr = split(id, '/', limit=2)
    archive, class = if '.' in archclass
        archive, class = split(archclass, '.', limit=2)
    else
        archclass, ""
    end
    archiveidx = findfirst(==(archive), ARXIV_OLD_ARCHIVES)
    isnothing(archiveidx) && throw(MalformedIdentifier{ArXiv}(id, "does not use a recognised ArXiv archive name"))
    classidx = if isempty(class)
        0
    else
        findfirst(==(class), ARXIV_OLD_CLASSES[archiveidx])
    end
    isnothing(classidx) && throw(MalformedIdentifier{ArXiv}(id, "does not use a recognised ArXiv archive class"))
    length(class) ∈ (0, 2) || throw(MalformedIdentifier{ArXiv}(id, "class component must be 2 characters"))
    numstr, verstr = if 'v' in numverstr
        split(numverstr, 'v', limit=2)
    else
        numverstr, "0"
    end
    version = tryparse(UInt8, verstr)
    isnothing(version) && throw(MalformedIdentifier{ArXiv}(id, "version must be an integer"))
    length(numstr) == 7 || throw(MalformedIdentifier{ArXiv}(id, "number component must be 7 characters (YYMMNNN)"))
    year = tryparse(UInt8, @view numstr[1:nextind(numstr, 1)])
    isnothing(year) && throw(MalformedIdentifier{ArXiv}(id, "year component (YYmmnnn) must be an integer"))
    (year >= 91 || year <= 7) || throw(MalformedIdentifier{ArXiv}(id, "year component (YYmmnnn) must be between 91 and 07"))
    month = tryparse(UInt8, @view numstr[3:nextind(numstr, 3)])
    isnothing(month) && throw(MalformedIdentifier{ArXiv}(id, "month component (yyMMnnn) must be an integer"))
    1 <= month <= 12 || throw(MalformedIdentifier{ArXiv}(id, "month component (yyMMnnn) must be between 01 and 12"))
    num = tryparse(UInt16, @view numstr[5:nextind(numstr, 5, 2)])
    isnothing(num) && throw(MalformedIdentifier{ArXiv}(id, "number component (yymmNNN) must be an integer"))
    ArXiv(arxiv_meta(archiveidx % UInt8, classidx % UInt8, year, month, version), num)
end

function shortcode(arxiv::ArXiv)
    archid, classid = arxiv_archive(arxiv), arxiv_class(arxiv)
    year, month, ver = arxiv_year(arxiv), arxiv_month(arxiv), arxiv_version(arxiv)
    verstr = if ver > 0 string('v', ver) else "" end
    if iszero(archid) # New form
        string(lpad(year, 2, '0'), lpad(month, 2, '0'),
               '.', lpad(arxiv.number, ifelse(year >= 15, 5, 4), '0'),
               verstr)
    else # Old form
        archive = ARXIV_OLD_ARCHIVES[archid]
        class = if iszero(classid) "" else ARXIV_OLD_CLASSES[archid][classid] end
        string(archive, ifelse(iszero(classid), "", "."), class, '/',
               lpad(year, 2, '0'), lpad(month, 2, '0'), lpad(arxiv.number, 3, '0'),
               verstr)
    end
end

purlprefix(::Type{ArXiv}) = "https://arxiv.org/abs/"


# DOI

"""
    DOI <: AcademicIdentifier

A Digital Object Identifier (DOI) is a unique identifier for a digital object such as a document or
a dataset. It consists of a registrant prefix and an object suffix separated by a slash.

# Examples

```julia
julia> DOI("10.1145/3276490")
doi:10.1145/3276490

julia> DOI("https://doi.org/10.1137/141000671")
doi:10.1137/141000671

julia> println(DOI("10.1145/3276490"))
10.1145/3276490
```
"""
struct DOI <: AcademicIdentifier
    registrant::String
    object::String
end

function DOI(doi::AbstractString)
    ldoi = lowercase(doi)
    for prefix in ("doi:", "doi.org/", "http://doi.org/", "https://doi.org/")
        if startswith(ldoi, prefix)
            return DOI(@view doi[ncodeunits(prefix)+1:end])
        end
    end
    if '/' in doi
        registrant, object = split(doi, '/', limit=2)
        DOI(String(registrant), String(object))
    else
        DOI(String(doi), "")
    end
end

purlprefix(::Type{DOI}) = "https://doi.org/"
shortcode(doi::DOI) = doi.registrant * '/' * doi.object

function Base.print(io::IO, doi::DOI)
    print(io, "doi:", shortcode(doi))
end


# ORCID

"""
    ORCID <: AcademicIdentifier

An Open Researcher and Contributor ID (ORCID) is a unique identifier for researchers and contributors
to academic works. It consists of a 16-digit integer with a checksum digit.

Invalid ORCID identifiers will throw a `MalformedIdentifier` exception, and identifiers with an
incorrect checksum will throw a `ChecksumViolation` exception.

# Examples

```julia
julia> ORCID("https://orcid.org/0000-0001-5109-3700")
ORCID:https://orcid.org/0000-0001-5109-3700

julia> println(ORCID("0000-0001-5109-3700"))
https://orcid.org/0000-0001-5109-3700

julia> ORCID("0000-0001-5109-3701")
ERROR: Checksum violation: the correct checksum for ORCID identifier 15109370 is 0 but got 1
````
"""
struct ORCID <: AcademicIdentifier
    id::UInt64
    function ORCID(id::Union{Int64, UInt64}, checksum::Integer)
        ndigits(id) <= 16 || throw(MalformedIdentifier{ORCID}(id, "must be a 16-digit integer"))
        digsum = 0
        for dig in Iterators.reverse(digits(id))
            digsum = (digsum + dig) * 2
        end
        checkcalc = (12 - digsum % 11) % 11
        if checkcalc != checksum
            throw(ChecksumViolation{ORCID}(id, checksum, checkcalc))
        end
        oid = UInt64(id) + UInt64(checksum) << 60
        new(oid)
    end
end

function ORCID(id::AbstractString)
    lid = lowercase(id)
    for prefix in ("orcid:", "orcid.org/", "https://orcid.org/")
        if startswith(lid, prefix)
            return ORCID(@view id[ncodeunits(prefix)+1:end])
        end
    end
    orcdigits = replace(id, '-' => "")
    if length(orcdigits) > 16
        throw(MalformedIdentifier{ORCID}(id, "must be a 16-digit integer"))
    end
    iddigits..., checksum = orcdigits
    id = parse(Int64, iddigits)
    check = if uppercase(checksum) == 'X' 10 else parse(Int, checksum) end
    ORCID(id, check)
end

idcode(orcid::ORCID) = Int(orcid.id & 0x003fffffffffffff)
idchecksum(orcid::ORCID) = Int8((orcid.id & 0xff00000000000000) >> 60)

function shortcode(orcid::ORCID)
    idstr, check = string(idcode(orcid)), idchecksum(orcid)
    join(Iterators.partition(lpad(idstr, 15, '0'), 4), '-') *
    if check == 10 "X" else string(check) end
end

purlprefix(::Type{ORCID}) = "https://orcid.org/"


# ROR

Base.@assume_effects :foldable function croc32decode(::Type{T}, str::AbstractString) where {T <: Integer}
    svec = collect(codeunits(lowercase(str)))
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

```julia
julia> ROR("https://ror.org/05cy4wa09")
ROR:05cy4wa09

julia> print(ROR("05cy4wa09"))
https://ror.org/05cy4wa09

julia> ROR("05cy4wa08")
ERROR: Checksum violation: the correct checksum for ROR identifier 05cy4wa is 9 but got 8
````
"""
struct ROR <: AcademicIdentifier
    num::Int32
    function ROR(num::Integer, check::Integer)
        num >= 0 || throw(MalformedIdentifier{ROR}(num, "must be a non-negative value"))
        num <= croc32decode(Int, "zzzzzz") || throw(MalformedIdentifier{ROR}(croc32encode(num), "must be no more than 6-digits"))
        shouldcheck = 98 - ((num * 100) % 97)
        if check != shouldcheck
            throw(ChecksumViolation{ROR}('0' * croc32encode(num), check, shouldcheck))
        end
        new(Int32(num))
    end
end

function ROR(num::AbstractString)
    for prefix in ("ror:", "ror.org/", "https://ror.org/")
        if startswith(lowercase(num), prefix)
            return ROR(@view num[ncodeunits(prefix)+1:end])
        end
    end
    length(num) == 9 || throw(MalformedIdentifier{ROR}(num, "must be 9 characters long"))
    char0, rest... = num
    char0 == '0' || throw(MalformedIdentifier{ROR}(num, "must start with '0'"))
    all(c -> c ∈ 'a':'z' || c ∈ 'A':'Z' || c ∈ '0':'9', rest) || throw(MalformedIdentifier{ROR}(num, "must only contain alphanumeric characters"))
    ROR(croc32decode(Int, view(rest, 1:6)), parse(UInt, view(rest, 7:8)))
end

idcode(ror::ROR) = ror.num
idchecksum(ror::ROR) = 98 - ((ror.num * 100) % 97)
shortcode(ror::ROR) = '0' * croc32encode(ror.num) * string(idchecksum(ror))
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

```julia
julia> PMID(28984872)
PMID:28984872

julia> PMID("https://pubmed.ncbi.nlm.nih.gov/28984872")
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

function PMID(id::AbstractString)
    lid = lowercase(id)
    for prefix in ("pmid:", "pubmed.ncbi.nlm.nih.gov/", "https://pubmed.ncbi.nlm.nih.gov/")
        if startswith(lid, prefix)
            return PMID(@view id[ncodeunits(prefix)+1:end])
        end
    end
    PMID(parse(UInt, id))
end

idcode(pmid::PMID) = pmid.id
shortcode(pmid::PMID) = string(pmid.id)
purlprefix(::Type{PMID}) = "https://pubmed.ncbi.nlm.nih.gov/"


# PMCID

"""
    PMCID <: AcademicIdentifier

A PubMed Central Identifier (PMCID) is a unique identifier for a publication in the
PubMed Central database. It consists of an 8-digit integer. Invalid PMCIDs will throw a
`MalformedIdentifier` exception.

PMCID identifiers should not be confused with PubMed identifiers ([`PMID`](@ref)s),
which are specifically used by PubMed but are distinct from PMCIDs.

# Examples

```julia
julia> PMCID("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC012345678")
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

function PMCID(id::AbstractString)
    lid = lowercase(id)
    for prefix in ("pmc", "pmcid:", "https://www.ncbi.nlm.nih.gov/pmc/articles/")
        if startswith(lid, prefix)
            return PMCID(@view id[ncodeunits(prefix)+1:end])
        end
    end
    PMCID(parse(UInt, id))
end

idcode(pmcid::PMCID) = pmcid.id
shortcode(pmcid::PMCID) = lpad(string(pmcid.id), 8, '0')
purlprefix(::Type{PMCID}) = "https://www.ncbi.nlm.nih.gov/pmc/articles/"


# ISSN

"""
    ISSN <: AcademicIdentifier

An International Standard Serial Number (ISSN) is a unique identifier for serial
publications.  It consists of a 7-digit integer with a checksum digit. Invalid
ISSNs will throw a `MalformedIdentifier` exception, and identifiers with an
incorrect checksum will throw a `ChecksumViolation` exception.

# Examples

```julia
julia> ISSN("1095-5054")
ISSN:1095-5054

julia> ISSN("1095-5053")
ERROR: Checksum violation: the correct checksum for ISSN identifier 1095505 is 4 but got 3
```
"""
struct ISSN <: AcademicIdentifier
    code::UInt32
    function ISSN(id::Union{UInt32, Int32, UInt64, Int64}, checksum::Integer)
        ndigits(id) <= 7 || throw(MalformedIdentifier{ISSN}(id, "must be a 7-digit integer"))
        digsum = 0
        for (i, dig) in enumerate(digits(id, pad=7))
            digsum += (i + 1) * dig
        end
        checkcalc = (11 - digsum % 11) % 11
        if checkcalc != checksum
            throw(ChecksumViolation{ISSN}(id, checksum, checkcalc))
        end
        code = UInt32(id) + UInt32(checksum) << 24
        new(code)
    end
end

function ISSN(code::AbstractString)
    lcode = lowercase(code)
    for prefix in ("issn:", "issn", "https://portal.issn.org/resource/ISSN/")
        if startswith(lcode, prefix)
            return ISSN(@view code[ncodeunits(prefix)+1:end])
        end
    end
    issndigits = replace(code, '-' => "")
    if length(issndigits) > 8
        throw(MalformedIdentifier{ISSN}(code, "must be an 8-digit integer"))
    end
    iddigits..., checksum = issndigits
    id = parse(Int32, iddigits)
    check = if uppercase(checksum) == 'X' 10 else parse(Int, checksum) end
    ISSN(id, check)
end

idcode(issn::ISSN) = Int(issn.code & 0x00ffffff)
idchecksum(issn::ISSN) = Int8((issn.code & 0xff000000) >> 24)
function shortcode(issn::ISSN)
    code = join(Iterators.partition(lpad(string(idcode(issn)), 7, '0'), 4), '-')
    csum = idchecksum(issn)
    code * if csum == 10 "X" else string(csum) end
end
purlprefix(::Type{ISSN}) = "https://portal.issn.org/resource/ISSN/"


# EAN13

struct EAN13 <: AcademicIdentifier
    code::UInt64
    function EAN13(code::Integer, checksum::Integer)
        ndigits(code) <= 12 || throw(MalformedIdentifier{EAN13}(code, "must be a 12-digit integer"))
        digsum = 0
        for (i, dig) in enumerate(digits(code, pad=12))
            digsum += if i % 2 == 0 dig else dig * 3 end
        end
        checkcalc = (10 - digsum % 10) % 10
        if checkcalc != checksum
            throw(ChecksumViolation{EAN13}(code, checksum, checkcalc))
        end
        new(code * 10 + checksum)
    end
    function EAN13(code::Integer, checksum::Integer, flag::UInt16)
        ean = EAN13(code, checksum)
        new(ean.code | UInt64(flag) << 48)
    end
end

function EAN13(code::Integer)
    idcode, checksum = divrem(code, 10)
    EAN13(idcode, checksum)
end

function EAN13(code::AbstractString)
    digits = replace(code, '-' => "")
    if length(digits) > 13
        throw(MalformedIdentifier{EAN13}(code, "must be a 13-digit integer"))
    end
    EAN13(parse(Int64, digits))
end

idcode(ean::EAN13) = Int((0x00001fffffffffff & ean.code) ÷ 10)
idchecksum(ean::EAN13) = Int8(ean.code % 10)
shortcode(ean::EAN13) = string(lpad(idcode(ean), 12, '0'), idchecksum(ean))

const IAN = EAN13

struct EAN8 <: AcademicIdentifier
    code::UInt
end

struct ISBN <: AcademicIdentifier
    code::EAN13
end

Base.convert(::Type{EAN13}, isbn::ISBN) = isbn.code


# ISBN

"""
    ISBN <: AcademicIdentifier

An International Standard Book Number (ISBN) is a unique identifier for books and book-like
publications. It consists of a 10-digit integer with a checksum digit. ISBNs can also be 13-digit
EAN-13 codes starting with 978.

Invalid ISBNs will throw a `MalformedIdentifier` exception, and identifiers with an incorrect
checksum will throw a `ChecksumViolation` exception.

# Examples

```julia
julia> ISBN("0141439564")
ISBN("0-14-143956-4")
```
"""
function ISBN(code::Integer)
    ndigits(code) == 13 || throw(MalformedIdentifier{ISBN}(code, "must be a 13-digit integer"))
    code ÷ 10^10 == 978 || throw(MalformedIdentifier{ISBN}(code, "must start with 978"))
    ISBN(EAN13(code))
end

function ISBN(code::AbstractString)
    if startswith(lowercase(code), "isbn:")
        return ISBN(@view code[ncodeunits("isbn:")+1:end])
    end
    plaincode = replace(code, '-' => "", ' ' => "")
    if length(plaincode) == 13
        ISBN(parse(Int64, plaincode))
    elseif length(plaincode) == 10
        cdigits..., check = plaincode
        dcode = parse(Int, cdigits)
        csum = 0
        for (i, digit) in enumerate(digits(dcode * 10, pad=10))
            csum += i * digit
        end
        cdigit = 11 - mod1(csum, 11)
        checknum = if check == 'X' 10 else parse(Int, check) end
        if cdigit != checknum
            throw(ChecksumViolation{ISBN}(code, checknum, cdigit))
        end
        eansum = 0
        for (i, dig) in enumerate(digits(dcode, pad=12))
            eansum += if i % 2 == 0 dig else dig * 3 end
        end
        eancheck = (10 - eansum % 10) % 10
        ISBN(EAN13(dcode, eancheck, 0x1000 | UInt8(cdigit)))
    else
        throw(MalformedIdentifier{ISBN}(code, "must be a 10 or 13-digit integer"))
    end
end

idcode(isbn::ISBN) = idcode(isbn.code)
idchecksum(isbn::ISBN) = idchecksum(isbn.code)
shortcode(isbn::ISBN) = string(isbn)

function Base.convert(::Type{ISBN}, ean::EAN13)
    if idcode(ean) ÷ 10^10 != 978
        throw(MalformedIdentifier{ISBN}(idcode(ean), "must start with 978"))
    end
    ISBN(ean)
end

function Base.print(io::IO, isbn::ISBN)
    flag = UInt16((isbn.code.code & UInt64(0xffff) << 48) >> 48)
    code3, code10 = if iszero(flag)
        divrem(isbn.code.code, 10^10)
    else
        978, idcode(isbn) * 10 + idchecksum(isbn)
    end
    unhyphenated, last3 = divrem(code10, 1000)
    codegroup, groupstr = 0, ""
    for (prefix3, class) in ISBN_GROUP_HYPHENATION
        prefix3 == code3 || continue
        for (len, range) in class
            first(range) <= unhyphenated <= last(range) || continue
            if len > 0
                codegroup, unhyphenated = divrem(unhyphenated, 10^(7 - len))
                groupstr = lpad(codegroup, len, '0')
            end
            break
        end
        break
    end
    pubstr = ""
    for (prefix3, classes) in ISBN_PUB_HYPHENATION
        prefix3 == code3 || continue
        for (prefixgroup, class) in classes
            prefixgroup == codegroup || continue
            for (len, range) in class
                first(range) <= unhyphenated <= last(range) || continue
                if len > 0
                    codepub, unhyphenated = divrem(unhyphenated, 10^(7 - ncodeunits(groupstr) - len))
                    pubstr = lpad(codepub, len, '0')
                end
                break
            end
        end
        break
    end
    unhyphenated = 1000 * unhyphenated + last3
    unhyphenated, checkdigit = divrem(unhyphenated, 10)
    iszero(flag) && print(io, code3, '-')
    !isempty(groupstr) && print(io, groupstr, '-')
    !isempty(pubstr) && print(io, pubstr, '-')
    print(io, unhyphenated, '-')
    if !iszero(flag)
        cdigit = Int8(flag & 0x00ff)
        print(io, if cdigit == 10 'X' else cdigit end)
    else
        print(io, checkdigit)
    end
end


# Wikidata

"""
    Wikidata <: AcademicIdentifier

A Wikidata identifier is a unique identifier for entities in the Wikidata knowledge base. It consists
of a 'Q' followed by an integer. Invalid Wikidata identifiers will throw a `MalformedIdentifier`
exception.

# Examples

```julia
julia> Wikidata("Q42")
Wikidata:Q42

julia> print(Wikidata("Q42"))
Q42
"""
struct Wikidata <: AcademicIdentifier
    id::UInt64
end

function Wikidata(id::AbstractString)
    if startswith(id, 'Q')
        Wikidata(parse(UInt64, id[2:end]))
    else
        throw(MalformedIdentifier{Wikidata}(id, "must start with 'Q'"))
    end
end

shortcode(wd::Wikidata) = string('Q', wd.id)
purlprefix(::Type{Wikidata}) = "https://www.wikidata.org/wiki/"

end
