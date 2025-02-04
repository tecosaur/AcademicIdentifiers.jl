module AcademicIdentifiers

using StyledStrings: @styled_str as @S_str

export AcademicIdentifier, DOI, ORCID, ROR, PMID, PMCID, ISSN, ISBN, Wikidata
export shortcode, purl

include("isbn-hyphenation.jl")

abstract type AcademicIdentifier end

struct MalformedIdentifier{T <: AcademicIdentifier, I} <: Exception
    input::I
    problem::String
end

MalformedIdentifier{T}(input::I, problem::String) where {T, I} =
    MalformedIdentifier{T, I}(input, problem)

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

function shortcode end

function purlprefix(::Type{T}) where {T <: AcademicIdentifier} end

purlprefix(::T) where {T <: AcademicIdentifier} = purlprefix(T)

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
    if length(num) != 9
        throw(MalformedIdentifier{ROR}(num, "must be 9 characters long"))
    end
    char0, rest... = num
    if char0 != '0'
        throw(MalformedIdentifier{ROR}(num, "must start with '0'"))
    end
    ROR(croc32decode(Int, view(rest, 1:6)), parse(Int, view(rest, 7:8)))
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
