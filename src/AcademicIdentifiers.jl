# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

"""
    AcademicIdentifiers

Structured and validated types for academic identifiers.

## Implemented Identifiers

- [`ArXiv`](@ref): arXiv preprint identifiers
- [`DOI`](@ref): Digital Object Identifiers
- [`EAN13`](@ref): European Article Numbers (13-digit barcodes)
- [`ISBN`](@ref): International Standard Book Numbers
- [`ISNI`](@ref): International Standard Name Identifiers
- [`ISSN`](@ref): International Standard Serial Numbers
- [`OCN`](@ref): OCLC Control Numbers
- [`OpenAlexID`](@ref): OpenAlex entity identifiers
- [`ORCID`](@ref): Open Researcher and Contributor Identifiers
- [`PMCID`](@ref): PubMed Central Identifiers
- [`PMID`](@ref): PubMed Identifiers
- [`RAiD`](@ref): Research Activity Identifiers
- [`ROR`](@ref): Research Organization Registry identifiers
- [`VIAF`](@ref): Virtual International Authority File identifiers
- [`Wikidata`](@ref): Wikidata entity identifiers

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

using FastIdentifiers
import FastIdentifiers: idcode, idchecksum, purlprefix, shortcode
using FastIdentifiers: @defid
using FastIdentifiers.Checksums: Checksums, mod10, mod11_2

FastIdentifiers.@reexport

export AcademicIdentifier, ArXiv, DOI, ISNI, ISSN, ISBN, OCN, ORCID,
    OpenAlexID, RAiD, ROR, PMID, PMCID, VIAF, Wikidata, EAN13

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


## Local helpers

digitstart(s::AbstractString) = !isempty(s) && codeunit(s, 1) ∈ 0x30:0x39

"""
    chopprefixes(str, prefixes...) -> (matched::Bool, rest::SubString)

Case-insensitive prefix stripping.  All prefixes must be lowercase ASCII.
"""
function chopprefixes(str::AbstractString, prefixes::AbstractString...)
    s = SubString(string(str))
    matched = false
    for prefix in prefixes
        ncodeunits(s) >= ncodeunits(prefix) || continue
        ok = true
        for i in 1:ncodeunits(prefix)
            (codeunit(s, i) | 0x20) == codeunit(prefix, i) || (ok = false; break)
        end
        ok || continue
        s = SubString(s, ncodeunits(prefix) + 1)
        matched = true
    end
    matched, s
end

# ISSN check digit: weighted sum with weights 8,7,...,2 (right to left), mod 11.
# Check value 10 maps to 'X'.
function issn_mod11(code::Integer)
    s, w = 0, 2
    while code > 0
        code, d = divrem(code, 10)
        s += d * w
        w += 1
    end
    (11 - s % 11) % 11
end
Checksums.parse_byte(::typeof(issn_mod11), bytevar::Symbol, nctx::Checksums.NodeCtx) =
    Checksums.parse_byte(mod11_2, bytevar, nctx)
Checksums.valid_bytes(::typeof(issn_mod11), nctx::Checksums.NodeCtx) =
    Checksums.valid_bytes(mod11_2, nctx)
Checksums.print_byte(::typeof(issn_mod11), valexpr, nctx::Checksums.NodeCtx) =
    Checksums.print_byte(mod11_2, valexpr, nctx)

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


## ArXiv

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
ArXiv:1411.1607v4

julia> parse(ArXiv, "https://arxiv.org/abs/1411.1607")
ArXiv:1411.1607

julia> purl(parse(ArXiv, "arXiv:1411.1607"))
"https://arxiv.org/abs/1411.1607"

julia> parse(ArXiv, "math.GT/0309136") # pre-2007 form
ArXiv:math.GT/0309136

julia> parse(ArXiv, "arxiv:hep-th/9901001v1")
ArXiv:hep-th/9901001v1
```
"""
@defid(ArXiv <: AcademicIdentifier,
       (choice(:format,
            :new => seq(:year(digits(2, pad=2)),
                        :month(digits(2, min=1, max=12, pad=2)),
                        ".", :num(digits(4:5, pad=4)),
                        optional("v", :ver(digits(max=1023)))),
            :old => seq(choice(:archive,
                :astro_ph => seq("astro-ph.", :class(choice("CO", "EP", "GA", "HE", "IM", "SR"))),
                :cond_mat => seq("cond-mat.", :class(choice("dis-nn", "mes-hall", "mtrl-sci", "other", "quant-gas", "soft", "stat-mech", "str-el", "supr-con"))),
                :cs       => seq("cs.", :class(choice("AI", "AR", "CC", "CE", "CG", "CL", "CR", "CV", "CY", "DB", "DC", "DL", "DM", "DS", "ET", "FL", "GL", "GR", "GT", "HC", "IR", "IT", "LG", "LO", "MA", "MM", "MS", "NA", "NI", "OH", "OS", "PF", "PL", "RO", "SC", "SD", "SE", "SI", "SY"))),
                :econ     => seq("econ.", :class(choice("EM", "GN", "TH"))),
                :eess     => seq("eess.", :class(choice("AS", "IV", "SP", "SY"))),
                :gr_qc    => "gr-qc",
                :hep_ex   => "hep-ex",
                :hep_lat  => "hep-lat",
                :hep_ph   => "hep-ph",
                :hep_th   => "hep-th",
                :math_ph  => "math-ph",
                :math     => seq("math.", :class(choice("AC", "AG", "AP", "AT", "CA", "CO", "CT", "CV", "DG", "DS", "FA", "GM", "GN", "GR", "GT", "HO", "IT", "KT", "LO", "MG", "MP", "NA", "NT", "OA", "OC", "PR", "QA", "RA", "RT", "SG", "SP", "ST"))),
                :nlin     => seq("nlin.", :class(choice("AO", "CD", "CG", "PS", "SI"))),
                :nucl_ex  => "nucl-ex",
                :nucl_th  => "nucl-th",
                :physics  => seq("physics.", :class(choice("acc-ph", "ao-ph", "app-ph", "atm-clus", "atom-ph", "bio-ph", "chem-ph", "class-ph", "comp-ph", "data-an", "ed-pn", "flu-dyn", "gen-ph", "geo-ph", "hist-ph", "ins-det", "med-ph", "optics", "plasm-ph", "pop-ph", "soc-ph", "space-ph"))),
                :q_bio    => seq("q-bio.", :class(choice("BM", "CB", "GN", "MN", "NC", "OT", "PE", "QM", "SC", "TO"))),
                :q_fin    => seq("q-fin.", :class(choice("CP", "EC", "GN", "MF", "PM", "PR", "RM", "ST", "SR"))),
                :quant_ph => "quant-ph",
                :stat     => seq("stat.", :class(choice("AP", "CO", "ME", "ML", "OT", "TH")))),
              "/", :year(digits(2, pad=2, exclude=8:90)),
              :month(digits(2, min=1, max=12, pad=2)),
              :num(digits(3:4, pad=3)),
              optional("v", :ver(digits(max=1023)))))),
       prefix="arXiv:", purlprefix="https://arxiv.org/abs/")


## DOI

"""
    DOI <: AcademicIdentifier

A Digital Object Identifier (DOI) is a persistent identifier for digital objects.

Standardised in [ISO 26324](https://www.iso.org/standard/43506.html) and built
on the [Handle System](https://www.dona.net/handle-system), DOIs are managed by
the [International DOI Foundation](https://doi.org/) and assigned by registration
agencies such as Crossref and DataCite. A DOI consists of a registrant prefix
(digits and periods, e.g. `10.1038`) and an object suffix separated by a slash.
The object suffix is unrestricted but treated as ASCII case-insensitive.

# Examples

```julia-repl
julia> parse(DOI, "10.1145/3276490")
DOI:10.1145/3276490

julia> parse(DOI, "https://doi.org/10.1137/141000671")
DOI:10.1137/141000671

julia> string(parse(DOI, "10.1145/3276490"))
"doi:10.1145/3276490"

julia> purl(parse(DOI, "10.1145/3276490"))
"https://doi.org/10.1145/3276490"

julia> parse(DOI, "10.123/abc") == parse(DOI, "10.123/AbC")
true
```
"""
struct DOI <: AcademicIdentifier
    registrant::SubString{String}
    object::SubString{String}
end

function Base.convert(::Type{DOI}, arxiv::ArXiv)
    sc = shortcode(arxiv)
    # Strip version suffix (vN) for DOI
    vidx = findlast('v', sc)
    base = if !isnothing(vidx) && vidx > 1 sc[1:prevind(sc, vidx)] else sc end
    doistr = string("10.48550/arXiv.", base)
    DOI(SubString(doistr, 1, 8), SubString(doistr, 10))
end

function Base.parse(::Type{DOI}, id::AbstractString)
    doi = SubString(string(id))
    if !digitstart(doi)
        _, doi = chopprefixes(doi, "doi:", "https://", "http://", "dx.doi.org/", "doi.org/")
    end
    registrant, object = if '/' in doi
        split(doi, '/', limit=2)
    else
        doi, ""
    end
    isempty(object) && throw(MalformedIdentifier{DOI}(doi, ncodeunits(doi), "must contain an object component after the slash"))
    lastdecimal = 0
    for (i, char) in enumerate(codeunits(registrant))
        if char == UInt8('.')
            i - 1 == lastdecimal && throw(MalformedIdentifier{DOI}(doi, i, "registrant cannot initial or consecutive periods"))
            lastdecimal = i
        elseif char ∉ UInt8('0'):UInt8('9')
            throw(MalformedIdentifier{DOI}(doi, i, "registrant must be a sequence of digits and periods"))
        end
    end
    lastdecimal == 0 && throw(MalformedIdentifier{DOI}(doi, 1, "registrant must contain at least one period"))
    DOI(SubString(string(registrant)), SubString(string(object)))
end

function Base.tryparse(::Type{DOI}, id::AbstractString)
    try parse(DOI, id) catch e; e isa MalformedIdentifier || rethrow(); nothing end
end

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


## EAN13

"""
    EAN13 <: AcademicIdentifier

A European Article Number (EAN-13) is a 13-digit barcode used globally for product identification.

Defined by [GS1](https://www.gs1.org/) and standardised in
[ISO/IEC 15420](https://www.iso.org/standard/46143.html), EAN-13 is a superset
of the original 12-digit Universal Product Code (UPC) system. It consists of a
12-digit code followed by a check digit calculated with a weighted mod-10
algorithm. [`ISBN`](@ref)-13 codes are a subset of EAN-13 (prefixes 978/979).
Hyphens in the input are accepted and stripped during parsing.

# Examples

```julia-repl
julia> parse(EAN13, "9780596520687")
EAN13:9780596520687

julia> parse(EAN13, "978-0-596-52068-7")
EAN13:9780596520687

julia> println(parse(EAN13, "9780596520687"))
9780596520687

julia> parse(EAN13, "9780596520688")
ERROR: ChecksumViolation{EAN13}: expected 7, got 8
```
"""
@defid(EAN13 <: AcademicIdentifier,
       (:code(digits(12, pad=12, skip="-")),
        skip("-"), checkdigit(:code, mod10)))

const IAN = EAN13


## ISBN

"""
    ISBN <: AcademicIdentifier

An International Standard Book Number (ISBN) is a unique identifier for books and book-like publications.

Standardised in [ISO 2108](https://www.iso.org/standard/65483.html) and
managed by the [International ISBN Agency](https://www.isbn-international.org/),
ISBNs come in two forms: 10-digit (ISBN-10, with a mod-11 check digit that may
be `X`) and 13-digit (ISBN-13, an [`EAN13`](@ref) code with prefix 978 or 979).
ISBN-10 inputs are automatically converted to ISBN-13 for storage. Output is
hyphenated according to the official range data.

# Examples

```julia-repl
julia> parse(ISBN, "0141439564")
ISBN:0-14-143956-4

julia> parse(ISBN, "978-1-7185-0276-5")
ISBN:978-1-7185-0276-5

julia> string(parse(ISBN, "9781718502765"))
"ISBN 978-1-7185-0276-5"
```
"""
struct ISBN <: AcademicIdentifier
    code::EAN13
    is10::Bool  # true when parsed from ISBN-10 format
end

ISBN(ean::EAN13) = ISBN(ean, false)

Base.convert(::Type{EAN13}, isbn::ISBN) = isbn.code

function Base.convert(::Type{ISBN}, ean::EAN13)
    idcode(ean) ÷ 10^9 ∈ (978, 979) ||
        throw(MalformedIdentifier{ISBN}(idcode(ean), "must start with 978 or 979"))
    ISBN(ean)
end

function ISBN(id::Integer, check::Integer)
    ndigits(id) <= 12 || throw(MalformedIdentifier{ISBN}(id, "must be at most a 12-digit integer"))
    id ÷ 10^9 ∈ (978, 979) || throw(MalformedIdentifier{ISBN}(id, "must start with 978 or 979"))
    ISBN(EAN13(id, check))
end

function ISBN(id::Integer)
    ndigits(id) <= 12 || throw(MalformedIdentifier{ISBN}(id, "must be at most a 12-digit integer"))
    id ÷ 10^9 ∈ (978, 979) || throw(MalformedIdentifier{ISBN}(id, "must start with 978 or 979"))
    ISBN(EAN13(id))
end

# ISBN-10 checksum: weighted sum with weights 10,9,...,2, mod 11
function isbn10_checkdigit(code9::Integer)
    csum = 0
    for (i, digit) in enumerate(digits(code9 * 10, pad=10))
        csum += i * digit
    end
    0xb - mod1(csum, 0xb)
end

function Base.parse(::Type{ISBN}, id::AbstractString)
    id = SubString(string(id))
    _, id = chopprefixes(id, "isbn:", "isbn")
    ncode = sum(c -> c ∉ (UInt8(' '), UInt8('-')), codeunits(id), init=0)
    if ncode == 13
        code = parsedashcode(UInt64, id, ('-', ' '))
        isnothing(code) && throw(MalformedIdentifier{ISBN}(id, "must only consist of digits and hyphens"))
        code.check <= 0x9 || throw(MalformedIdentifier{ISBN}(id, "check digit must be 0-9"))
        ISBN(code.num, code.check)
    elseif ncode == 10
        code = parsedashcode(UInt64, id, ('-', ' '))
        isnothing(code) && throw(MalformedIdentifier{ISBN}(id, "must only consist of digits and hyphens"))
        code.check <= 0xa || throw(MalformedIdentifier{ISBN}(id, "check digit must be 0-9 or X"))
        cdigit = isbn10_checkdigit(code.num)
        cdigit == code.check ||
            throw(ChecksumViolation{ISBN}(id, cdigit, code.check))
        # Convert ISBN-10 to ISBN-13 (978 prefix)
        ean12 = 978 * 10^9 + code.num
        ISBN(EAN13(ean12), true)
    else
        throw(MalformedIdentifier{ISBN}(id, "must be a 10 or 13-digit integer"))
    end
end

function Base.tryparse(::Type{ISBN}, id::AbstractString)
    try parse(ISBN, id) catch e; e isa Union{MalformedIdentifier, ChecksumViolation} || rethrow(); nothing end
end

Base.:(==)(a::ISBN, b::ISBN) = a.code == b.code
Base.hash(isbn::ISBN, h::UInt) = hash(isbn.code, h)

function Base.print(io::IO, isbn::ISBN)
    get(io, :limit, false) === true && get(io, :compact, false) === true ||
        print(io, "ISBN ")
    shortcode(io, isbn)
end

function shortcode(io::IO, isbn::ISBN)
    ean12 = idcode(isbn.code)
    code3, code9 = divrem(ean12, 10^9)
    # ISBN-13 gets the 3-digit prefix; ISBN-10 omits it
    !isbn.is10 && print(io, lpad(code3, 3, '0'), '-')
    # code10 = 9-digit body + check digit (10 digits total for hyphenation lookup)
    code10 = code9 * 10 + idchecksum(isbn.code)
    # Find group length
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
    # Extract group and remaining digits
    codegroup, remaining_after_group = if group_length > 0
        divrem(code10, 10^(10 - group_length))
    else
        0, code10
    end
    group_length > 0 && print(io, lpad(codegroup, group_length, '0'), '-')
    # Find publisher length
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
    # Extract publisher, title, and check digit
    publisher, title_and_check = if publisher_length > 0
        divrem(remaining_after_group, 10^(remaining_digits - publisher_length))
    else
        0, remaining_after_group
    end
    publisher_length > 0 && print(io, lpad(publisher, publisher_length, '0'), '-')
    title, check_digit = if title_and_check >= 10
        divrem(title_and_check, 10)
    else
        0, title_and_check
    end
    title_length = remaining_digits - publisher_length - 1
    title_length ∈ 1:9 && print(io, lpad(title, title_length, '0'), '-')
    # ISBN-10 uses its own check digit (may be X), ISBN-13 uses EAN check
    if isbn.is10
        c = isbn10_checkdigit(code9)
        print(io, if c == 10 'X' else Char('0' + c) end)
    else
        print(io, Char('0' + check_digit))
    end
end


## ISNI

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
@defid(ISNI <: AcademicIdentifier,
       (skip(choice("isni:", "isni ")),
        :code(digits(15, pad=15, skip=" ", groups=4)),
        checkdigit(:code, mod11_2)),
       prefix="ISNI ", purlprefix="https://isni.org/isni/")

# ISNI PURLs use the compact (unspaced) form
function FastIdentifiers.purl(isni::ISNI)
    check = if idchecksum(isni) == 10 "X" else string(idchecksum(isni)) end
    string(purlprefix(ISNI), lpad(idcode(isni), 15, '0'), check)
end


## ISSN

"""
    ISSN <: AcademicIdentifier

An International Standard Serial Number (ISSN) is a unique identifier for serial publications.

Standardised in [ISO 3297](https://www.iso.org/standard/84536.html) and
managed by the [ISSN International Centre](https://www.issn.org/), an ISSN
consists of seven digits followed by a check digit (0–9 or `X`) computed with
a weighted mod-11 algorithm. The canonical display form groups the digits as
`NNNN-NNNC`. ISSNs are used to identify journals, newspapers, and other
periodicals regardless of medium.

# Examples

```julia-repl
julia> parse(ISSN, "1095-5054")
ISSN:1095-5054

julia> string(parse(ISSN, "10955054"))
"ISSN 1095-5054"

julia> purl(parse(ISSN, "1095-5054"))
"https://portal.issn.org/resource/ISSN/1095-5054"
```
"""
@defid(ISSN <: AcademicIdentifier,
       (skip(choice("issn:", "issn ", "issn")),
        :code(digits(7, pad=7, skip="-", groups=4)),
        checkdigit(:code, issn_mod11)),
       prefix="ISSN ", purlprefix="https://portal.issn.org/resource/ISSN/")


## OCN

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

function Base.parse(::Type{OCN}, id::AbstractString)
    id = SubString(string(id))
    isempty(id) && throw(MalformedIdentifier{OCN}(id, "cannot be empty"))
    if first(id) == '('
        _, id = chopprefixes(id, "(ocolc)")
    end
    if !digitstart(id)
        _, id = chopprefixes(id, "ocn:", "oclc:", "ocolc", "oclc", "ocm", "ocn", "on", "https://", "www.", "worldcat.org/oclc/")
    end
    id = lstrip(id)
    all(isdigit, id) || throw(MalformedIdentifier{OCN}(id, "must only consist of digits"))
    num = tryparse(UInt64, id)
    isnothing(num) && throw(MalformedIdentifier{OCN}(id, "must contain only digits"))
    OCN(num)
end

function Base.tryparse(::Type{OCN}, id::AbstractString)
    try parse(OCN, id) catch e; e isa MalformedIdentifier || rethrow(); nothing end
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


## OpenAlexID

@defid(GenericOpenAlexID <: AcademicIdentifier,
       (skip(choice("openalex:", "OpenAlex:"),
             choice("https://", "http://"),
             choice("www.openalex.org/", "openalex.org/"),
             choice("works/", "authors/", "sources/", "institutions/",
                    "concepts/", "publishers/", "funders/")),
        :category(choice("W", "A", "S", "I", "C", "P", "F")),
        :num(digits(1:18))))

purlprefix(::Type{GenericOpenAlexID}) = "https://openalex.org/"

"""
    OpenAlexID{kind} <: AcademicIdentifier

An [OpenAlex](https://openalex.org/) identifier for entities in the OpenAlex scholarly metadata database.

Operated by [OurResearch](https://ourresearch.org/), OpenAlex provides free
and open bibliographic data. Each identifier consists of a single-letter
category prefix followed by a positive integer. The seven entity categories
are: **W** (works), **A** (authors), **S** (sources), **I** (institutions),
**C** (concepts), **P** (publishers), and **F** (funders).

# Examples

```julia-repl
julia> parse(OpenAlexID, "W2741809807")
OpenAlexID:W2741809807

julia> parse(OpenAlexID, "A5092938886")
OpenAlexID:A5092938886

julia> purl(parse(OpenAlexID, "W2741809807"))
"https://openalex.org/W2741809807"
```
"""
struct OpenAlexID{kind} <: AcademicIdentifier
    inner::GenericOpenAlexID
end

OpenAlexID{kind}(num::Integer) where {kind} =
    OpenAlexID{kind}(GenericOpenAlexID(kind, num))

function Base.parse(::Type{OpenAlexID{kind}}, id::AbstractString) where {kind}
    inner = parse(GenericOpenAlexID, id)
    Symbol(inner.category) == kind ||
        throw(MalformedIdentifier{OpenAlexID{kind}}(id, 1, "kind does not match"))
    OpenAlexID{kind}(inner)
end

function Base.parse(::Type{OpenAlexID}, id::AbstractString)
    inner = parse(GenericOpenAlexID, id)
    OpenAlexID{Symbol(inner.category)}(inner)
end

function Base.tryparse(::Type{OpenAlexID{kind}}, id::AbstractString) where {kind}
    inner = tryparse(GenericOpenAlexID, id)
    isnothing(inner) && return nothing
    Symbol(inner.category) == kind || return nothing
    OpenAlexID{kind}(inner)
end

function Base.tryparse(::Type{OpenAlexID}, id::AbstractString)
    inner = tryparse(GenericOpenAlexID, id)
    isnothing(inner) && return nothing
    OpenAlexID{Symbol(inner.category)}(inner)
end

function Base.show(io::IO, @nospecialize(id::OpenAlexID))
    if get(io, :limit, false) === true
        if get(io, :typeinfo, Nothing) != typeof(id)
            print(io, "OpenAlexID:")
        end
        shortcode(io, id)
    else
        print(io, "OpenAlexID{:", id.inner.category, "}(", id.inner.num, ')')
    end
end

Base.isless(@nospecialize(a::OpenAlexID), @nospecialize(b::OpenAlexID)) = isless(a.inner, b.inner)
Base.write(io::IO, @nospecialize(id::OpenAlexID)) = write(io, id.inner)
Base.string(@nospecialize(id::OpenAlexID)) = shortcode(id)
shortcode(io::IO, @nospecialize(id::OpenAlexID)) = shortcode(io, id.inner)
shortcode(@nospecialize(id::OpenAlexID)) = shortcode(id.inner)
purlprefix(@nospecialize(_::Type{<:OpenAlexID})) = purlprefix(GenericOpenAlexID)
idcode(@nospecialize(id::OpenAlexID)) = id.num


## ORCID

"""
    ORCID <: AcademicIdentifier

An Open Researcher and Contributor ID (ORCID) is a persistent identifier for researchers and scholarly contributors.

Operated by [ORCID, Inc.](https://orcid.org/) and conforming to
[ISO 27729](https://www.iso.org/standard/44292.html) (ISNI), an ORCID
consists of 15 base digits followed by an ISO 7064 MOD 11-2 check character
(0–9 or `X`), displayed in four groups of four separated by hyphens
(e.g. `0000-0001-5109-3700`). The ORCID consortium specifies the full
`https://orcid.org/` URL as the canonical representation.

# Examples

```julia-repl
julia> parse(ORCID, "https://orcid.org/0000-0001-5109-3700")
ORCID:0000-0001-5109-3700

julia> shortcode(parse(ORCID, "0000-0001-5109-3700"))
"0000-0001-5109-3700"

julia> println(parse(ORCID, "0000-0001-5109-3700"))
https://orcid.org/0000-0001-5109-3700
```
"""
@defid(ORCID <: AcademicIdentifier,
       (skip("orcid:"),
        :code(digits(15, pad=15, skip="-", groups=4)),
        checkdigit(:code, mod11_2)),
       purlprefix="https://orcid.org/")


## PMCID

"""
    PMCID <: AcademicIdentifier

A PubMed Central Identifier (PMCID) is a unique identifier for a publication in the
PubMed Central database. It consists of an 8-digit integer. Invalid PMCIDs will throw a
`MalformedIdentifier` exception.

PMCID identifiers should not be confused with PubMed identifiers ([`PMID`](@ref)s),
which are specifically used by PubMed but are distinct from PMCIDs.

# Examples

```julia-repl
julia> parse(PMCID, "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12345678")
PMCID:12345678

julia> println(PMCID(12345678))
PMC12345678

julia> PMCID(123456789)
ERROR: ArgumentError: Value 123456789 is above maximum 99999999
```
"""
@defid(PMCID <: AcademicIdentifier,
       (skip(choice("https://", "http://"), choice("www.", "pmc."),
             "ncbi.nlm.nih.gov/pmc/articles/"),
        skip("pmcid:", " "), skip(print="PMC"), :id(digits(1:8))))

FastIdentifiers.idprefix(::Type{PMCID}) = nothing
purlprefix(::Type{PMCID}) = "https://www.ncbi.nlm.nih.gov/pmc/articles/"


## PMID

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
ERROR: ArgumentError: Value 123456789 is above maximum 99999999
```
"""
@defid(PMID <: AcademicIdentifier,
       (skip("pmid:", " "), :id(digits(1:8))),
       prefix="", purlprefix="https://pubmed.ncbi.nlm.nih.gov/")


## RAiD

"""
    RAiD <: AcademicIdentifier

A Research Activity Identifier (RAiD) is a persistent identifier for research projects and activities.

Standardised in [ISO 23527](https://www.iso.org/standard/75931.html) and
operated by the [Australian Research Data Commons](https://www.raid.org.au/),
RAiDs link together the people, organisations, outputs, and funding associated
with a research activity. Each RAiD is implemented as a [`DOI`](@ref) and can
be converted to one via `convert(DOI, raid)`.

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

function Base.parse(::Type{RAiD}, id::AbstractString)
    id = SubString(string(id))
    chopped, id = chopprefixes(id, "raid:")
    if !chopped
        _, id = chopprefixes(id, "https://", "http://", "raid.org/")
    end
    RAiD(parse(DOI, id))
end

function Base.tryparse(::Type{RAiD}, id::AbstractString)
    try parse(RAiD, id) catch e; e isa MalformedIdentifier || rethrow(); nothing end
end

purlprefix(::Type{RAiD}) = "https://raid.org/"

Base.:(==)(a::RAiD, b::RAiD) = a.id == b.id
Base.hash(raid::RAiD, h::UInt) = hash(raid.id, h)


## ROR

"""
    ROR <: AcademicIdentifier

A Research Organization Registry (ROR) identifier is a persistent identifier for research organisations.

Operated by [ROR](https://ror.org/) as an open community-led registry, ROR
IDs provide a stable reference for affiliations in scholarly metadata. Each
identifier consists of a leading `0`, six Crockford base-32 characters, and a
two-digit ISO 7064 MOD 97-10 checksum. The full `https://ror.org/` URL is the
canonical representation.

# Examples

```julia-repl
julia> parse(ROR, "https://ror.org/05cy4wa09")
ROR:05cy4wa09

julia> print(parse(ROR, "05cy4wa09"))
https://ror.org/05cy4wa09

julia> parse(ROR, "05cy4wa08")
ERROR: ChecksumViolation{ROR}: expected 09, got 08
```
"""
@defid(ROR <: AcademicIdentifier,
       (skip("ror:"), "0",
        :id(charset(6, '0':'9', 'a':'h', 'j':'k', 'm':'n', 'p':'t', 'v':'z',
                    numeric=true, lower=true)),
        checkdigit(:id, mod97)),
       purlprefix="https://ror.org/")


## VIAF

"""
    VIAF <: AcademicIdentifier

A Virtual International Authority File (VIAF) identifier is a numeric key for authority records in the library domain.

Hosted by [OCLC](https://www.oclc.org/) at [viaf.org](https://viaf.org/),
VIAF links the national authority files of libraries worldwide into a single
virtual authority file. Each VIAF cluster merges records for the same person,
corporate body, or work from participating institutions (Library of Congress,
BnF, DNB, etc.), providing a shared identifier for bibliographic entities.

# Examples

```julia-repl
julia> parse(VIAF, "113230702")
VIAF:113230702

julia> parse(VIAF, "https://viaf.org/viaf/113230702/")
VIAF:113230702

julia> purl(parse(VIAF, "113230702"))
"https://viaf.org/viaf/113230702"
```
"""
@defid(VIAF <: AcademicIdentifier,
       (skip("viaf:"), :id(digits(UInt32)), skip("/")),
       prefix="", purlprefix="https://viaf.org/viaf/")


## Wikidata

"""
    Wikidata <: AcademicIdentifier

A Wikidata identifier (QID) is a unique key for entities in the [Wikidata](https://www.wikidata.org/) knowledge base.

Operated by the [Wikimedia Foundation](https://wikimediafoundation.org/),
Wikidata is a free, collaborative, multilingual knowledge graph. Each entity
(item, property, or lexeme) receives a stable QID consisting of `Q` followed
by a positive integer. QIDs are widely used for authority control, linked
open data, and scholarly knowledge graphs. Common prefixes `wd:` and
`wikidata:` are accepted during parsing.

# Examples

```julia-repl
julia> parse(Wikidata, "Q42")
Wikidata:Q42

julia> purl(parse(Wikidata, "Q42"))
"https://www.wikidata.org/wiki/Q42"

julia> parse(Wikidata, "wd:Q42")
Wikidata:Q42
```
"""
@defid(Wikidata <: AcademicIdentifier,
       (skip(choice("wikidata:", "wd:")), "Q", :id(digits(1:18))),
       prefix="", purlprefix="https://www.wikidata.org/wiki/")

end
