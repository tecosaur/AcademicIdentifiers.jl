# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

module AcademicIdentifiers

using StyledStrings: @styled_str as @S_str

export AcademicIdentifier, ArXiv, DOI, ISSN, ISBN, OCN, ORCID, OpenAlexID, ROR, PMID, PMCID, Wikidata
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

# Extended help

**Interface and guidelines**

## Mandatory components

Your academic identifier, named `AId` for example, must be able to be constructed
from its canonical form as well as the plain form (these may be the same).

```julia
parseid(AId, "canonical string form or purl") -> AId or Exception
parseid(AId, "minimal plain form") -> AId or Exception
shortcode(::AId) -> String
```

The `parseid` function is used in generic `parse` and `tryparse` implementations.
You can either implement `parseid` or define both `parse` and `tryparse` methods.

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

When `idcode` is defined, the generic `shortcode` function will use it to
construct the plain string representation of the identifier.

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
    parseid(::Type{T}, input::SubString) -> Union{T, MalformedIdentifier{T}, ChecksumViolation{T}}

Attempt to parse the `input` string as an identifier of type `T`.

This is used by the generic `parse` and `tryparse` functions to interpret a string as a `T`, and
should be implemented by  `AcademicIdentifier` subtypes.
"""
function parseid end

function Base.parse(::Type{T}, input::AbstractString) where {T <: AcademicIdentifier}
    id = parseid(T, SubString(input))
    id isa T || throw(id)
    id
end

function Base.tryparse(::Type{T}, input::AbstractString) where {T <: AcademicIdentifier}
    id = parseid(T, SubString(input))
    if id isa T id end
end

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
shortcode(id::AcademicIdentifier) = string(idcode(id))

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
    show(io, parse)
    show(io, (typeof(id), shortcode(id)))
end

function Base.show(io::IO, ::MIME"text/plain", id::AcademicIdentifier)
    label = String(nameof(typeof(id)))
    url = purl(id)
    idstr = SubString(shortcode(id))
    idstr = if ':' in idstr
        chopprefix(idstr, label * ':')
    else
        chopprefix(idstr, label)
    end
    if endswith(label, "ID")
        idstr = chopprefix(idstr, chopsuffix(label, "ID"))
    end
    if get(io, :typeinfo, Nothing) != typeof(id)
        print(io, S"{bold:$label:}")
    end
    if isnothing(url)
        print(io, S"$idstr")
    else
        print(io, S"{link=$url:$idstr}")
    end
end

function Base.isless(a::T, b::T) where {T <: AcademicIdentifier}
    ca = idcode(a)
    cb = idcode(b)
    (isnothing(ca) || isnothing(cb)) && return isless(shortcode(a), shortcode(b))
    ca < cb
end


# General utilities

"""
    parsefor(::Type{T<:AcademicIdentifier}, ::Type{I<:Integer}, num::Union{<:AbstractString, <:AbstractChar})

Attempt to parse the `num` string as an integer of type `I`, returning it if successful.

If the string cannot be parsed as an integer, a `MalformedIdentifier{T}` exception is returned.
"""
function parsefor(::Type{T}, ::Type{I}, num::Union{<:AbstractString, <:AbstractChar}) where {T <: AcademicIdentifier, I <: Integer}
    int = if num isa Char
        try parse(I, num) catch end # See: <https://github.com/JuliaLang/julia/issues/45640>
    else
        tryparse(I, num)
    end
    if isnothing(int)
        (@noinline function(iT, inum)
             nonint = if inum isa AbstractChar inum else filter(c -> c ∉ '0':'9', inum) end
             MalformedIdentifier{T}(inum, "includes invalid base 10 digit$(ifelse(length(nonint)==1, "", "s")) '$(nonint)'")
         end)(T, num)
    else
        int
    end
end

"""
    chopprefix(s::SubString, prefix::AbstractString) -> Tuple{Bool, SubString}

Remove an ASCII `prefix` from the start of `s`, ignoring case.

The `prefix` argument must be lowercase.
"""
function choplowerprefix(s::SubString, prefix::AbstractString)
    k = firstindex(s)
    i, j = iterate(s), iterate(prefix)
    while true
        isnothing(j) && isnothing(i) && return true, SubString(s, 1, 0)
        isnothing(j) && return true, @inbounds SubString(s, k)
        isnothing(i) && return false, s
        UInt32(first(i)) | 0x20 == UInt32(first(j)) || return false, s
        k = last(i)
        i, j = iterate(s, k), iterate(prefix, last(j))
    end
end

function chopprefixes(s::SubString, prefixes::String...)
    chopped = false
    for prefix in prefixes
        did, s = choplowerprefix(s, prefix)
        chopped |= did
    end
    chopped, s
end

digitstart(s::AbstractString) = !isempty(s) && isdigit(first(s))

function iso7064mod11m2checksum(num::Integer)
    digsum = 0
    for dig in Iterators.reverse(digits(num))
        digsum = (digsum + dig) * 2
    end
    (12 - digsum % 11) % 11
end

 
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
    _, id = chopprefixes(id, "https://", "http://")
    isweb, id = chopprefixes(id, "arxiv.org/")
    if isweb
        prefixend = findfirst('/', id)
        isnothing(prefixend) && return MalformedIdentifier{ArXiv}(id, "incomplete ArXiv URL")
        id = @view id[prefixend+1:end]
    else
        _, id = chopprefixes(id, "arxiv:")
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
    isnothing(version) && return MalformedIdentifier{ArXiv}(id, "version must be an integer")
    '.' in code || return MalformedIdentifier{ArXiv}(id, "must contain a period separating the date and number component (YYMM.NNNNN)")
    datestr, numstr = split(code, '.', limit=2)
    all(isdigit, datestr) && ncodeunits(datestr) == 4 || return MalformedIdentifier{ArXiv}(id, "date component must be 4 digits (YYMM.nnnnn)")
    year = tryparse(UInt8, @view datestr[1:2])
    isnothing(year) && return MalformedIdentifier{ArXiv}(id, "year component (YYmm.nnnnn) must be an integer")
    month = tryparse(UInt8, @view datestr[3:4])
    isnothing(month) && return MalformedIdentifier{ArXiv}(id, "month component (yyMM.nnnnn) must be an integer")
    1 <= month <= 12 || return MalformedIdentifier{ArXiv}(id, "month component (yyMM.nnnnn) must be between 01 and 12")
    number = tryparse(UInt32, numstr)
    isnothing(number) && return MalformedIdentifier{ArXiv}(id, "number component (yymm.NNNNN) must be an integer")
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
    '/' in id || return MalformedIdentifier{ArXiv}(id, "must contain a slash separating the components (archive.class/YYMMNNN)")
    archclass, numverstr = split(id, '/', limit=2)
    archive, class = if '.' in archclass
        archive, class = split(archclass, '.', limit=2)
    else
        archclass, ""
    end
    archiveidx = findfirst(==(archive), ARXIV_OLD_ARCHIVES)
    isnothing(archiveidx) && return MalformedIdentifier{ArXiv}(id, "does not use a recognised ArXiv archive name")
    classidx = if isempty(class)
        0
    else
        findfirst(==(class), ARXIV_OLD_CLASSES[archiveidx])
    end
    isnothing(classidx) && return MalformedIdentifier{ArXiv}(id, "does not use a recognised ArXiv archive class")
    length(class) ∈ (0, 2) || return MalformedIdentifier{ArXiv}(id, "class component must be 2 characters")
    numstr, verstr = if 'v' in numverstr
        split(numverstr, 'v', limit=2)
    else
        numverstr, "0"
    end
    version = tryparse(UInt8, verstr)
    isnothing(version) && return MalformedIdentifier{ArXiv}(id, "version must be an integer")
    length(numstr) == 7 || return MalformedIdentifier{ArXiv}(id, "number component must be 7 characters (YYMMNNN)")
    year = tryparse(UInt8, @view numstr[1:nextind(numstr, 1)])
    isnothing(year) && return MalformedIdentifier{ArXiv}(id, "year component (YYmmnnn) must be an integer")
    (year >= 91 || year <= 7) || return MalformedIdentifier{ArXiv}(id, "year component (YYmmnnn) must be between 91 and 07")
    month = tryparse(UInt8, @view numstr[3:nextind(numstr, 3)])
    isnothing(month) && return MalformedIdentifier{ArXiv}(id, "month component (yyMMnnn) must be an integer")
    1 <= month <= 12 || return MalformedIdentifier{ArXiv}(id, "month component (yyMMnnn) must be between 01 and 12")
    num = tryparse(UInt16, @view numstr[5:nextind(numstr, 5, 2)])
    isnothing(num) && return MalformedIdentifier{ArXiv}(id, "number component (yymmNNN) must be an integer")
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

idcode(arxiv::ArXiv) =
    UInt64(arxiv.meta & 0xffffff00) << 32 +
    UInt64(arxiv.number) << 8 +
    arxiv_version(arxiv)

purlprefix(::Type{ArXiv}) = "https://arxiv.org/abs/"

Base.print(io::IO, arxiv::ArXiv) = print(io, "arXiv:", shortcode(arxiv))


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
        chopped, doi = chopprefixes(doi, "doi:")
        if !chopped
            _, doi = chopprefixes(doi, "https://", "http://", "dx.doi.org/", "doi.org/")
        end
    end
    registrant, object = if '/' in doi
        split(doi, '/', limit=2)
    else
        doi, ""
    end
    '.' in registrant && all(c -> c == '.' || c ∈ '0':'9', registrant) ||
        return MalformedIdentifier{DOI}(doi, "registrant must be a string of digits and periods")
    DOI(registrant, object)
end

parseid(::Type{DOI}, doi::SubString) = parseid(DOI, SubString(String(doi)))

purlprefix(::Type{DOI}) = "https://doi.org/"
shortcode(doi::DOI) = doi.registrant * '/' * doi.object

Base.print(io::IO, doi::DOI) = print(io, "doi:", shortcode(doi))
Base.show(io::IO, doi::DOI) = (show(io, DOI); show(io, (doi.registrant, doi.object)))

function Base.:(==)(a::DOI, b::DOI)
    a.registrant == b.registrant || return false
    ncodeunits(a.object) == ncodeunits(b.object) || return false
    for (ca, cb) in zip(codeunits(a.object), codeunits(b.object))
        xascii = ifelse(ca < 0x80, 0x20, 0x00)
        (ca | xascii) == (cb | xascii) || return false
    end
    true
end

function Base.hash(doi::DOI, h::UInt)
    h = hash(doi.registrant, h)
    for c in codeunits(doi.object)
        xascii = ifelse(c < 0x80, 0x20, 0x00)
        h = hash(c | xascii, h)
    end
    h
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
````
"""
struct ORCID <: AcademicIdentifier
    id::UInt64
    function ORCID(id::Union{Int64, UInt64}, checksum::Integer)
        ndigits(id) <= 15 || throw(MalformedIdentifier{ORCID}(id, "must be a 16-digit integer"))
        i7064check = iso7064mod11m2checksum(id)
        i7064check == checksum ||
            throw(ChecksumViolation{ORCID}(id, checksum, i7064check))
        oid = UInt64(id) + UInt64(checksum) << 60
        new(oid)
    end
end

function parseid(::Type{ORCID}, id::SubString)
    if !digitstart(id)
        chopped, id = chopprefixes(id, "orcid:", "orcid ")
        if !chopped
            _, id = chopprefixes(id, "https://", "http://", "orcid.org/")
        end
    end
    orcdigits = replace(id, '-' => "")
    2 <= length(orcdigits) <= 16 ||
        return MalformedIdentifier{ORCID}(id, "must be a 2-16 digit integer")
    iddigits..., checksum = orcdigits
    id = parsefor(ORCID, UInt64, iddigits)
    id isa UInt64 || return id
    check = if uppercase(checksum) == 'X' 0x0a else parsefor(ORCID, UInt8, checksum) end
    check isa UInt8 || return check
    try ORCID(id, check) catch e; e end
end

idcode(orcid::ORCID) = Int(orcid.id & 0x003fffffffffffff)
idchecksum(orcid::ORCID) = (orcid.id >> 60) % UInt8

function shortcode(orcid::ORCID)
    idstr, check = string(idcode(orcid)), idchecksum(orcid)
    join(Iterators.partition(lpad(idstr, 15, '0'), 4), '-') *
        if check == 10 "X" else string(check) end
end

purlprefix(::Type{ORCID}) = "https://orcid.org/"
Base.show(io::IO, orcid::ORCID) = (show(io, ORCID); show(io, (idcode(orcid), idchecksum(orcid))))
Base.print(io::IO, orcid::ORCID) = print(io, "ORCID:", shortcode(orcid))


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
    if first(id) ∈ (first(String(kind)), lowercase(first(String(kind))))
    else
        chopped, id = chopprefixes(id, "openalex:")
        if !chopped
            _, id = chopprefixes(id, "https://", "http://", "openalex.org/")
        end
        prefixend = findfirst('/', id)
        if !isnothing(prefixend)
            id = @view id[prefixend+1:end]
        end
        uppercase(first(id)) == first(String(kind)) ||
            return MalformedIdentifier{OpenAlexID{kind}}(id, "kind does not match")
    end
    num = parsefor(OpenAlexID, UInt64, @view id[2:end])
    num isa UInt64 || return num
    OpenAlexID{kind}(num)
end

function parseid(::Type{OpenAlexID}, id::SubString)
    isempty(id) && return MalformedIdentifier{OpenAlexID}(id, "cannot be empty")
    kindchar = uppercase(first(id))
    if kindchar ∉ ('W', 'A', 'S', 'I', 'C', 'P', 'F')
        chopped, id = chopprefixes(id, "openalex:")
        if !chopped
            _, id = chopprefixes(id, "https://", "http://", "openalex.org/")
        end
        prefixend = findfirst('/', id)
        if !isnothing(prefixend)
            id = @view id[prefixend+1:end]
        end
        kindchar = uppercase(first(id))
        first(id) ∈ ('W', 'A', 'S', 'I', 'C', 'P', 'F') ||
            return MalformedIdentifier{OpenAlexID}(id, "unrecognised kind prefix")
        ncodeunits(id) == 1 &&
            return MalformedIdentifier{OpenAlexID}(id, "must include a number after the kind prefix")
    end
    num = parsefor(OpenAlexID, UInt64, @view id[2:end])
    num isa UInt64 || return num
    OpenAlexID{Symbol(kindchar)}(num)
end

shortcode(id::OpenAlexID{kind}) where {kind} = string(kind, id.num)
purlprefix(@nospecialize(::OpenAlexID)) = "https://openalex.org/"

Base.show(io::IO, id::OpenAlexID) = (show(io, typeof(id)); print(io, '(', id.num, ')'))
Base.print(io::IO, id::OpenAlexID) = print(io, shortcode(id))


# ROR

Base.@assume_effects :foldable function croc32decode(::Type{T}, str::AbstractString) where {T <: Integer}
    svec = codeunits(str) .| 0x20 # Convert to lowercase
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
````
"""
struct ROR <: AcademicIdentifier
    num::Int32
    function ROR(num::Integer, check::Integer)
        num >= 0 || throw(MalformedIdentifier{ROR}(num, "must be a non-negative value"))
        num <= croc32decode(Int, "zzzzzz") || throw(MalformedIdentifier{ROR}(croc32encode(num), "must be no more than 6-digits"))
        checkexpected = 98 - ((num * 100) % 97)
        if check != checkexpected
            throw(ChecksumViolation{ROR}('0' * croc32encode(num), check, checkexpected))
        end
        new(Int32(num))
    end
end

function parseid(::Type{ROR}, num::SubString)
    chopped, num = chopprefixes(num, "ror:")
    if !chopped
        _, num = chopprefixes(num, "ror.org/", "https://ror.org/")
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
shortcode(ror::ROR) = '0' * lpad(croc32encode(ror.num), 6, '0') * lpad(string(idchecksum(ror)), 2, '0')
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
    chopped, id = chopprefixes(id, "pmid:")
    if !chopped
        _, id = chopprefixes(id, "https://", "http://", "pubmed.ncbi.nlm.nih.gov/")
    end
    pint = parsefor(PMID, UInt, id)
    pint isa UInt || return pint
    try PMID(pint) catch e; e end
end

idcode(pmid::PMID) = pmid.id
purlprefix(::Type{PMID}) = "https://pubmed.ncbi.nlm.nih.gov/"

Base.show(io::IO, pmid::PMID) = (show(io, PMID); print(io, '(', pmid.id, ')'))
Base.print(io::IO, pmid::PMID) = print(io, "PMID:", pmid.id)


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
    chopped, id = chopprefixes(id, "pmcid:", "pmc")
    if !chopped
        _, id = chopprefixes(id, "https://", "http://", "www.ncbi.nlm.nih.gov/pmc/articles/")
    end
    pint = parsefor(PMCID, UInt, id)
    pint isa UInt || return pint
    try PMCID(pint) catch e; e end
end

idcode(pmcid::PMCID) = pmcid.id
shortcode(pmcid::PMCID) = string("PMC", pmcid.id)
purlprefix(::Type{PMCID}) = "https://www.ncbi.nlm.nih.gov/pmc/articles/"

Base.show(io::IO, pmcid::PMCID) = (show(io, PMCID); print(io, '(', pmcid.id, ')'))
Base.print(io::IO, pmcid::PMCID) = print(io, "PMC", pmcid.id)


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

function parseid(::Type{ISSN}, code::SubString)
    chopped, code = chopprefixes(code, "issn:", "issn")
    if chopped
        code = lstrip(code)
    else
        _, code = chopprefixes(code, "https://", "http://", "portal.issn.org/resource/ISSN/")
    end
    issndigits = replace(code, '-' => "")
    2 <= length(issndigits) <= 8 ||
        return MalformedIdentifier{ISSN}(code, "must be a 2-8 digit integer")
    iddigits..., checksum = issndigits
    id = parsefor(ISSN, UInt32, iddigits)
    id isa UInt32 || return id
    check = if uppercase(checksum) == 'X' 0x0a else parsefor(ISSN, UInt8, checksum) end
    check isa UInt8 || return check
    try ISSN(id, check) catch e; e end
end

idcode(issn::ISSN) = Int(issn.code & 0x00ffffff)
idchecksum(issn::ISSN) = Int8((issn.code & 0xff000000) >> 24)
function shortcode(issn::ISSN)
    code = join(Iterators.partition(lpad(string(idcode(issn)), 7, '0'), 4), '-')
    csum = idchecksum(issn)
    code * if csum == 10 "X" else string(csum) end
end
purlprefix(::Type{ISSN}) = "https://portal.issn.org/resource/ISSN/"

Base.print(io::IO, issn::ISSN) = print(io, "ISSN ", shortcode(issn))


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

function parseid(::Type{EAN13}, code::SubString)
    digits = replace(code, '-' => "")
    if length(digits) > 13
        return MalformedIdentifier{EAN13}(code, "must be a 13-digit integer")
    end
    eint = parsefor(EAN13, UInt64, digits)
    eint isa UInt64 || return eint
    try EAN13(eint) catch e; e end
end

idcode(ean::EAN13) = Int((0x00001fffffffffff & ean.code) ÷ 10)
idchecksum(ean::EAN13) = Int8(ean.code % 10)
shortcode(ean::EAN13) = string(lpad(idcode(ean), 12, '0'), idchecksum(ean))

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

function ISBN(code::Integer)
    ndigits(code) == 13 || throw(MalformedIdentifier{ISBN}(code, "must be a 13-digit integer"))
    code ÷ 10^10 ∈ (978, 979) || throw(MalformedIdentifier{ISBN}(code, "must start with 978 or 979"))
    ISBN(EAN13(code))
end

function parseid(::Type{ISBN}, code::SubString)
    _, code = chopprefixes(code, "isbn:", "isbn")
    code = lstrip(code)
    plaincode = replace(code, '-' => "", ' ' => "")
    if length(plaincode) == 13
        iint = parsefor(ISBN, UInt64, plaincode)
        iint isa UInt64 || return iint
        ISBN(iint)
    elseif length(plaincode) == 10
        cdigits..., check = plaincode
        dcode = parsefor(ISBN, UInt, cdigits)
        dcode isa UInt || return dcode
        csum = 0
        for (i, digit) in enumerate(digits(dcode * 10, pad=10))
            csum += i * digit
        end
        cdigit = 11 - mod1(csum, 11)
        checknum = if check == 'X' 0x0a else parsefor(ISBN, UInt8, check) end
        checknum isa UInt8 || return checknum
        if cdigit != checknum
            return ChecksumViolation{ISBN}(code, checknum, cdigit)
        end
        eansum = 0
        for (i, dig) in enumerate(digits(dcode, pad=12))
            eansum += if i % 2 == 0 dig else dig * 3 end
        end
        eancheck = (10 - eansum % 10) % 10
        ISBN(EAN13(dcode, eancheck, 0x1000 | UInt8(cdigit)))
    else
        return MalformedIdentifier{ISBN}(code, "must be a 10 or 13-digit integer")
    end
end

idcode(isbn::ISBN) = idcode(isbn.code)
idchecksum(isbn::ISBN) = idchecksum(isbn.code)
shortcode(isbn::ISBN) = string(isbn)

function Base.convert(::Type{ISBN}, ean::EAN13)
    if idcode(ean) ÷ 10^9 ∉ (978, 979)
        return MalformedIdentifier{ISBN}(idcode(ean), "must start with 978 or 979")
    end
    ISBN(ean)
end

function Base.print(io::IO, isbn::ISBN)
    (; prefix, group, publisher, title, check) = isbn_hyphenate(isbn)
    print(io, "ISBN ")
    isempty(prefix) || print(io, prefix, '-')
    isempty(group) || print(io, group, '-')
    isempty(publisher) || print(io, publisher, '-')
    isempty(title) || print(io, title, '-')
    print(io, check)
end

function isbn_hyphenate(isbn::ISBN)
    flag = UInt16((isbn.code.code & UInt64(0xffff) << 48) >> 48)
    code3, code10 = if iszero(flag)
        divrem(isbn.code.code, 10^10)
    else
        978, idcode(isbn.code) * 10 + idchecksum(isbn.code)
    end
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
    if publisher_length > 0
        publisher, title_and_check = divrem(remaining_after_group, 10^(remaining_digits - publisher_length))
    else
        publisher, title_and_check = 0, remaining_after_group
    end
    # Split title and check digit
    if title_and_check >= 10
        title, check_digit = divrem(title_and_check, 10)
    else
        title, check_digit = 0, title_and_check
    end
    title_length = remaining_digits - publisher_length - 1
    (prefix = if iszero(flag) lpad(code3, 3, '0') else "" end,
     group = lpad(codegroup, group_length, '0'),
     publisher = lpad(publisher, publisher_length, '0'),
     title = if title_length ∈ 1:9
         lpad(title, title_length, '0')
     else
         ""
     end,
     check = if iszero(flag)
         Char(0x30 + check_digit)
     else
         check_char = Int8(flag & 0x00ff)
         if check_char == 10
             'X'
         else
             Char(0x30 + check_char)
         end
     end)
end


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
"""
struct Wikidata <: AcademicIdentifier
    id::UInt64
end

function parseid(::Type{Wikidata}, id::SubString)
    chopped, id = chopprefixes(id, "wikidata:", "wd:")
    if !chopped
        _, id = chopprefixes(id, "https://", "http://", "www.", "wikidata.org/wiki/")
    end
    if startswith(id, 'Q')
        wint = parsefor(Wikidata, UInt64, id[2:end])
        wint isa UInt64 || return wint
        Wikidata(wint)
    else
        MalformedIdentifier{Wikidata}(id, "must start with 'Q'")
    end
end

idcode(wd::Wikidata) = wd.id
shortcode(wd::Wikidata) = string('Q', wd.id)
purlprefix(::Type{Wikidata}) = "https://www.wikidata.org/wiki/"

Base.show(io::IO, wd::Wikidata) = (show(io, Wikidata); print(io, '(', wd.id, ')'))

end
