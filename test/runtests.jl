# SPDX-FileCopyrightText: © 2025 TEC <contact@tecosaur.net>
# SPDX-License-Identifier: MPL-2.0

using AcademicIdentifiers
using AcademicIdentifiers: MalformedIdentifier, ChecksumViolation, idcode, idchecksum, shortcode, purl
using AcademicIdentifiers: EAN13

using Test

@testset "ArXiv" begin
    valid_examples = [
        # New format examples
        ("2301.12345", ArXiv(0x5c400, 12345)),
        ("0704.0001", ArXiv(0x1d000, 1)),
        ("1412.7878", ArXiv(0x3b000, 7878)),
        ("1501.00001", ArXiv(0x3c400, 1)),
        ("2312.12345", ArXiv(0x5f000, 12345)),
        # Versioned new format
        ("0704.0001v1", ArXiv(0x1d001, 1)),
        ("2301.12345v9", ArXiv(0x5c409, 12345)),
        # Old format examples
        ("hep-th/9901001", ArXiv(0x5018c400, 1)),
        ("astro-ph/0001001", ArXiv(0x8000400, 1)),
        ("math.CA/0611800", ArXiv(0x60a1ac00, 800)),
        ("cs.AI/0501001", ArXiv(0x18214400, 1)),
        ("cond-mat/0001001", ArXiv(0x10000400, 1)),
        ("gr-qc/0001001", ArXiv(0x30000400, 1)),
        ("nucl-th/0001001", ArXiv(0x78000400, 1)),
        ("physics/0001001", ArXiv(0x80000400, 1)),
        ("quant-ph/0001001", ArXiv(0x98000400, 1)),
        # Versioned old format
        ("hep-th/9901001v2", ArXiv(0x5018c402, 1)),
        ("math.CA/0611800v3", ArXiv(0x60a1ac03, 800)),
        ("astro-ph/0001001v10", ArXiv(0x800040a, 1)),
        # With prefixes
        ("arXiv:2301.12345", ArXiv(0x5c400, 12345)),
        ("ArXiv:hep-th/9901001", ArXiv(0x5018c400, 1)),
        ("ARXIV:0704.0001v1", ArXiv(0x1d001, 1)),
        # URL format
        ("https://arxiv.org/abs/2301.12345", ArXiv(0x5c400, 12345)),
        ("https://arxiv.org/abs/hep-th/9901001v2", ArXiv(0x5018c402, 1))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(ArXiv, input_str) == tryparse(ArXiv, input_str) == expected
            @test parse(ArXiv, shortcode(expected)) == expected
            @test parse(ArXiv, string(expected)) == expected
            @test parse(ArXiv, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("2301.12345", "arXiv:2301.12345", "https://arxiv.org/abs/2301.12345"),
            ("0704.0001", "arXiv:0704.0001", "https://arxiv.org/abs/0704.0001"),
            ("hep-th/9901001", "arXiv:hep-th/9901001", "https://arxiv.org/abs/hep-th/9901001"),
            ("astro-ph/0001001", "arXiv:astro-ph/0001001", "https://arxiv.org/abs/astro-ph/0001001"),
            ("math.CA/0611800", "arXiv:math.CA/0611800", "https://arxiv.org/abs/math.CA/0611800"),
            ("0704.0001v1", "arXiv:0704.0001v1", "https://arxiv.org/abs/0704.0001v1"),
            ("hep-th/9901001v2", "arXiv:hep-th/9901001v2", "https://arxiv.org/abs/hep-th/9901001v2"),
            ("math.CA/0611800v3", "arXiv:math.CA/0611800v3", "https://arxiv.org/abs/math.CA/0611800v3")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(ArXiv, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # New scheme malformed
            "0704",              # Missing paper number
            "070.0001",          # Wrong date format
            "0704.000a",         # Non-numeric paper number
            "13xx.12345",        # Non-numeric date
            "0013.12345",        # Invalid month (13)
            "1399.12345",        # Invalid month (99)
            "2301.",             # Missing paper number
            ".12345",            # Missing year/month
            # Old scheme malformed
            "astro-ph/001001",   # Wrong number length
            "bad-cat/0001001",   # Invalid category
            "astro-ph/000100a",  # Non-numeric number
            "astro-ph0001001",   # Missing slash
            "astro-ph/",         # Missing number
            "/0001001",          # Missing category
            "hep-th/8901001",    # Year too early (pre-1991)
            # Version malformed
            "2301.12345v",       # Missing version number
            "2301.12345v999",    # Invalid version (too large)
            "2301.12345va",      # Non-numeric version
            # General malformed
            "",                  # Empty string
            "invalid",           # Completely invalid
            "   ",               # Whitespace only
            # Unicode edge cases
            "2301.12345🔢",      # emoji in new scheme
            "2301．12345",       # full-width period
            "０704.0001",        # full-width zero
            "0704.０001",        # full-width zero in ID
            "２301.12345",       # full-width 2
            "2301.１2345",       # full-width 1
            "astro‑ph/0001001",  # figure dash in old scheme
            "astro−ph/0001001",  # minus sign
            "astro-ph／0001001", # full-width slash
            "astro-ph/０001001"  # full-width zero in old scheme
        ]

        for bad_arxiv in malformed_examples
            @test_throws MalformedIdentifier{ArXiv} parse(ArXiv, bad_arxiv)
            @test tryparse(ArXiv, bad_arxiv) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test ArXiv to DOI conversion
        arxiv_new = parse(ArXiv, "2301.12345")
        arxiv_versioned = parse(ArXiv, "2301.12345v2")
        arxiv_old = parse(ArXiv, "hep-th/9901001")

        doi_new = convert(DOI, arxiv_new)
        @test shortcode(doi_new) == "10.48550/arXiv.2301.12345"

        # Version should be stripped for DOI conversion
        doi_versioned = convert(DOI, arxiv_versioned)
        @test shortcode(doi_versioned) == "10.48550/arXiv.2301.12345"

        doi_old = convert(DOI, arxiv_old)
        @test shortcode(doi_old) == "10.48550/arXiv.hep-th/9901001"

        # Test version handling
        arxiv_base = parse(ArXiv, "2301.12345")
        arxiv_v1 = parse(ArXiv, "2301.12345v1")
        arxiv_v9 = parse(ArXiv, "2301.12345v9")

        @test shortcode(arxiv_base) == "2301.12345"
        @test shortcode(arxiv_v1) == "2301.12345v1"
        @test shortcode(arxiv_v9) == "2301.12345v9"

        @test parse(ArXiv, "1501.00001v1") < parse(ArXiv, "1501.00001v2")
        @test parse(ArXiv, "1501.00001") < parse(ArXiv, "1501.00002")
        @test parse(ArXiv, "1501.00002") < parse(ArXiv, "1601.00001")
    end
end

@testset "DOI" begin
    valid_examples = [
        # Basic DOI examples
        ("10.1000/182", DOI(SubString("10.1000"), SubString("182"))),
        ("10.1038/nature12373", DOI(SubString("10.1038"), SubString("nature12373"))),
        ("10.1016/j.cell.2020.01.001", DOI(SubString("10.1016"), SubString("j.cell.2020.01.001"))),
        ("10.1093/nar/gkaa1100", DOI(SubString("10.1093"), SubString("nar/gkaa1100"))),
        ("10.5555/12345678", DOI(SubString("10.5555"), SubString("12345678"))),
        ("10.1234/example.doi", DOI(SubString("10.1234"), SubString("example.doi"))),
        # With lowercase prefix
        ("doi:10.1000/182", DOI(SubString("10.1000"), SubString("182"))),
        ("doi:10.1038/nature12373", DOI(SubString("10.1038"), SubString("nature12373"))),
        ("doi:10.1016/j.cell.2020.01.001", DOI(SubString("10.1016"), SubString("j.cell.2020.01.001"))),
        # With uppercase prefix
        ("DOI:10.1000/182", DOI(SubString("10.1000"), SubString("182"))),
        ("DOI:10.1038/nature12373", DOI(SubString("10.1038"), SubString("nature12373"))),
        ("DOI:10.1093/nar/gkaa1100", DOI(SubString("10.1093"), SubString("nar/gkaa1100"))),
        # URL format
        ("https://doi.org/10.1000/182", DOI(SubString("10.1000"), SubString("182"))),
        ("https://doi.org/10.1038/nature12373", DOI(SubString("10.1038"), SubString("nature12373"))),
        ("http://dx.doi.org/10.1016/j.cell.2020.01.001", DOI(SubString("10.1016"), SubString("j.cell.2020.01.001")))
    ]

    @testset "Parsing/formatting invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(DOI, input_str) == tryparse(DOI, input_str) == expected
            @test parse(DOI, shortcode(expected)) == expected
            @test parse(DOI, string(expected)) == expected
            @test parse(DOI, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("10.1000/182", "doi:10.1000/182", "https://doi.org/10.1000/182"),
            ("10.1038/nature12373", "doi:10.1038/nature12373", "https://doi.org/10.1038/nature12373"),
            ("10.1016/j.cell.2020.01.001", "doi:10.1016/j.cell.2020.01.001", "https://doi.org/10.1016/j.cell.2020.01.001"),
            ("10.1093/nar/gkaa1100", "doi:10.1093/nar/gkaa1100", "https://doi.org/10.1093/nar/gkaa1100"),
            ("10.5555/12345678", "doi:10.5555/12345678", "https://doi.org/10.5555/12345678"),
            ("10.1234/example.doi", "doi:10.1234/example.doi", "https://doi.org/10.1234/example.doi")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(DOI, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Structural problems
            "invalid",           # No DOI structure
            "",                  # Empty string
            "   ",               # Whitespace only
            "1000/182",         # Missing prefix
            "10.1000/",         # Missing suffix
            "10.1000",          # Missing suffix entirely
            "10.1000182",       # Missing slash
            "doi:",             # Prefix only
            "DOI:",             # Prefix only uppercase
            # Invalid registrant format
            "10/182",           # Missing period in registrant
            "abc.1000/182",     # Non-numeric in registrant
            "10.abc/182",       # Non-numeric in registrant
            "10..1/abc",        # Double period
            ".10.1/abc",        # Leading period
            # Unicode edge cases that should fail
            "10．1000/182",     # full-width period
            "10.1000／182",     # full-width slash
            "１0.1000/182",      # full-width 1
            "10.１000/182",     # full-width 1
            "１０.1000/182",    # full-width 10
        ]

        for bad_doi in malformed_examples
            @test_throws MalformedIdentifier{DOI} parse(DOI, bad_doi)
            @test tryparse(DOI, bad_doi) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test hash and equality with case-folding
        doi1 = parse(DOI, "10.1000/ABC")
        doi2 = parse(DOI, "10.1000/abc")
        doi3 = parse(DOI, "10.1000/AbC")
        doi4 = parse(DOI, "10.1000/xyz")

        @test doi1 == doi1
        @test doi1 == doi2
        @test doi1 == doi3
        @test doi2 == doi3
        @test doi1 != doi4

        @test hash(doi1) == hash(doi1)
        @test hash(doi1) == hash(doi2)
        @test hash(doi1) == hash(doi3)
        @test hash(doi2) == hash(doi3)
        @test hash(doi1) != hash(doi4)

        # Test registrant and object extraction
        doi = parse(DOI, "10.1038/nature12373")
        @test doi.registrant == "10.1038"
        @test doi.object == "nature12373"
        # SubString parsing
        @test doi == parse(DOI, @view lazy"x10.1038/nature12373x"[2:end-1])

        # Test complex object handling
        complex_doi = parse(DOI, "10.1016/j.cell.2020.01.001")
        @test complex_doi.registrant == "10.1016"
        @test complex_doi.object == "j.cell.2020.01.001"
    end
end

@testset "EAN13" begin
    valid_examples = [
        # ISBN-13 format (978 prefix)
        ("9780439023481", EAN13(978043902348, 1)),
        ("9781402894626", EAN13(978140289462, 6)),
        ("9780123456786", EAN13(978012345678, 6)),
        ("9780486419503", EAN13(978048641950, 3)),
        # ISBN-13 format (979 prefix)
        ("9798886451740", EAN13(979888645174, 0)),
        ("9791234567896", EAN13(979123456789, 6)),
        ("9799876543216", EAN13(979987654321, 6)),
        # Other product codes
        ("1234567890128", EAN13(123456789012, 8)),
        ("5901234123457", EAN13(590123412345, 7)),
        ("6291041500213", EAN13(629104150021, 3))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(EAN13, input_str) == tryparse(EAN13, input_str) == expected
            @test parse(EAN13, shortcode(expected)) == expected
            @test parse(EAN13, string(expected)) == expected
            # EAN13 doesn't have purl support
            @test purl(expected) === nothing
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("9780439023481", "9780439023481", nothing),
            ("9798886451740", "9798886451740", nothing),
            ("9781402894626", "9781402894626", nothing),
            ("9780123456786", "9780123456786", nothing),
            ("1234567890128", "1234567890128", nothing),
            ("5901234123457", "5901234123457", nothing)
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(EAN13, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) === purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Wrong length
            "97804390234812",   # Too many digits (14)

            # Non-numeric content
            "978043902348a1",   # Non-numeric character
            "97804390234a12",   # Non-numeric in middle
            "a780439023481",    # Non-numeric at start
            "978 043 902 348 1", # Spaces
            # Empty or invalid
            "",                 # Empty string
            "   ",              # Whitespace only
            "invalid",          # Completely invalid
            "abcdefghijklm",    # All letters
            # Unicode edge cases
            "9780439023481🔢",  # emoji
            "９780439023481",    # full-width 9
            "９７80439023481",   # full-width 97
            "978043902348１",    # full-width 1
            "９７８0439023481",   # full-width 978
            "９７８０４３９０２３４８１" # all full-width
        ]

        for bad_ean in malformed_examples
            @test_throws MalformedIdentifier{EAN13} parse(EAN13, bad_ean)
            @test tryparse(EAN13, bad_ean) === nothing
        end
    end

    @testset "Checksum errors" begin
        checksum_examples = [
            "9780439023480",  # Wrong checksum (should be 1)
            "9780439023482",  # Wrong checksum (should be 1)
            "9780439023483",  # Wrong checksum (should be 1)
            "9798886451741",  # Wrong checksum (should be 0)
            "9798886451742",  # Wrong checksum (should be 0)
            "9781402894627",  # Wrong checksum (should be 6)
            "9781402894625",  # Wrong checksum (should be 6)
            "9780123456780",  # Wrong checksum (should be 6)
            "1234567890120",  # Wrong checksum (should be 8)
            "5901234123450",  # Wrong checksum (should be 7)
            # Short lengths that are interpreted as having wrong checksums
            "978043902",      # 9 digits - parsed as having wrong checksum
            "97804390234",    # 11 digits - parsed as having wrong checksum
            "978043902348",   # 12 digits - parsed as having wrong checksum
            "978043902348-"   # Hyphen interpreted as wrong checksum
        ]

        for bad_ean in checksum_examples
            @test_throws ChecksumViolation{EAN13} parse(EAN13, bad_ean)
            @test tryparse(EAN13, bad_ean) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor with separate code and checksum
        ean13_int = EAN13(978043902348, 1)
        ean13_str = parse(EAN13, "9780439023481")
        @test idcode(ean13_int) == idcode(ean13_str)
        @test idchecksum(ean13_int) == idchecksum(ean13_str)
        @test shortcode(ean13_int) == shortcode(ean13_str)

        # Test integer constructor with full 13-digit code
        ean13_full = EAN13(9780439023481)
        @test shortcode(ean13_full) == "9780439023481"

        # Test another integer constructor
        ean13_int2 = EAN13(979888645174, 0)
        ean13_str2 = parse(EAN13, "9798886451740")
        @test shortcode(ean13_int2) == shortcode(ean13_str2)

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{EAN13} EAN13(12345678901234, 5)  # Too many digits
        @test_throws ChecksumViolation{EAN13} EAN13(978043902348, 2)  # Wrong checksum

        # Test code and checksum extraction
        ean13_cases = [
            ("9780439023481", 978043902348, 1),
            ("9798886451740", 979888645174, 0),
            ("9781402894626", 978140289462, 6),
            ("9780123456786", 978012345678, 6),
            ("1234567890128", 123456789012, 8),
            ("5901234123457", 590123412345, 7)
        ]
        for (idstr, code, csum) in ean13_cases
            ean13 = parse(EAN13, idstr)
            @test idcode(ean13) == code
            @test idchecksum(ean13) == csum
        end

        # Test checksum calculation through idchecksum function
        @test idchecksum(EAN13(978043902348, 1)) == 1
        @test idchecksum(EAN13(979888645174, 0)) == 0
        @test idchecksum(EAN13(978140289462, 6)) == 6

        # Test equality and hashing
        ean1 = parse(EAN13, "9780439023481")
        ean2 = EAN13(978043902348, 1)
        ean3 = parse(EAN13, "9798886451740")

        @test ean1 == ean2
        @test ean1 != ean3
        @test hash(ean1) == hash(ean2)
        @test hash(ean1) != hash(ean3)
    end
end

@testset "ISNI" begin
    valid_examples = [
        # Basic ISNI format
        ("0000 0001 2281 955X", ISNI(12281955)),
        ("0000 0000 8389 1195", ISNI(8389119)),
        ("0000 0004 0600 5291", ISNI(40600529)),
        ("0000 0000 0000 0001", ISNI(0)),
        ("0000 0001 0000 0009", ISNI(10000000)),
        # With lowercase prefix
        ("isni:0000 0001 2281 955X", ISNI(12281955)),
        ("isni:0000 0000 8389 1195", ISNI(8389119)),
        ("isni:0000 0004 0600 5291", ISNI(40600529)),
        # With uppercase prefix
        ("ISNI:0000 0001 2281 955X", ISNI(12281955)),
        ("ISNI:0000 0000 8389 1195", ISNI(8389119)),
        # URL format
        ("https://isni.org/isni/000000012281955X", ISNI(12281955)),
        ("https://isni.org/isni/0000000083891195", ISNI(8389119)),
        ("https://isni.org/isni/0000000406005291", ISNI(40600529)),
        # Without spaces (should be parsed correctly)
        ("000000012281955X", ISNI(12281955)),
        ("0000000083891195", ISNI(8389119)),
        ("0000000406005291", ISNI(40600529))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(ISNI, input_str) == tryparse(ISNI, input_str) == expected
            @test parse(ISNI, shortcode(expected)) == expected
            @test parse(ISNI, string(expected)) == expected
            @test parse(ISNI, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("0000 0001 2281 955X", "https://isni.org/isni/000000012281955X", "https://isni.org/isni/000000012281955X"),
            ("0000 0000 8389 1195", "https://isni.org/isni/0000000083891195", "https://isni.org/isni/0000000083891195"),
            ("0000 0004 0600 5291", "https://isni.org/isni/0000000406005291", "https://isni.org/isni/0000000406005291"),
            ("0000 0000 0000 0001", "https://isni.org/isni/0000000000000001", "https://isni.org/isni/0000000000000001"),
            ("0000 0001 0000 0009", "https://isni.org/isni/0000000100000009", "https://isni.org/isni/0000000100000009")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(ISNI, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Empty or invalid
            "",                      # Empty string
            "   ",                   # Whitespace only
            "invalid",               # Completely invalid
            "abcd efgh ijkl mnop",   # Non-numeric
            # Wrong length
            "0000 0001 2281 955XX",  # Too long
            # Invalid format structure
            "0000-0001-2281-955X",   # Wrong separators
            "0000.0001.2281.955X",   # Wrong separators
            "0000_0001_2281_955X",   # Wrong separators
            # Invalid characters
            "000G 0001 2281 955X",   # Invalid hex character (G)
            "0000 000H 2281 955X",   # Invalid hex character (H)
            "0000 0001 228I 955X",   # Invalid hex character (I)
            "0000 0001 2281 955Y",   # Invalid check character (Y)
            "0000 0001 2281 955Z",   # Invalid check character (Z)
            # Unicode edge cases
            "0000 0001 2281 955X🔢", # emoji
            "０000 0001 2281 955X",   # full-width zero
            "0000 ０001 2281 955X",   # full-width zero in middle
            "0000 0001 2281 955Ｘ"    # full-width X
        ]

        for bad_isni in malformed_examples
            @test_throws MalformedIdentifier{ISNI} parse(ISNI, bad_isni)
            @test tryparse(ISNI, bad_isni) === nothing
        end
    end

    @testset "Checksum errors" begin
        checksum_examples = [
            "0000 0001 2281 9550",  # Wrong checksum (should be X/10)
            "0000 0001 2281 9551",  # Wrong checksum (should be X/10)
            "0000 0001 2281 9552",  # Wrong checksum (should be X/10)
            "0000 0000 8389 1194",  # Wrong checksum (should be 5)
            "0000 0000 8389 1196",  # Wrong checksum (should be 5)
            "0000 0004 0600 5290",  # Wrong checksum (should be 1)
            "0000 0004 0600 5292",  # Wrong checksum (should be 1)
            "0000 0000 0000 0002",  # Wrong checksum (should be 1)
            "0000 0001 0000 0000",  # Wrong checksum (should be 9)
            "0000 0001 0000 0001",  # Wrong checksum (should be 9)
            "0000 0000 0000 0000",  # Wrong checksum (should be 1)
            # Short lengths that are interpreted as having wrong checksums
            "0000 0001 2281 95",    # Too short - parsed as having wrong checksum
            "0000 0001 2281",       # Missing last group - parsed as having wrong checksum
            "0000",                 # Missing last three groups - parsed as having wrong checksum
            "00000001228195X"       # Missing spaces - parsed as having wrong checksum
        ]

        for bad_isni in checksum_examples
            @test_throws ChecksumViolation{ISNI} parse(ISNI, bad_isni)
            @test tryparse(ISNI, bad_isni) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor with correct checksum
        isni_int = ISNI(12281955, 10)  # X = 10
        isni_str = parse(ISNI, "0000 0001 2281 955X")
        @test AcademicIdentifiers.idcode(isni_int) == AcademicIdentifiers.idcode(isni_str)
        @test AcademicIdentifiers.idchecksum(isni_int) == AcademicIdentifiers.idchecksum(isni_str)
        @test shortcode(isni_int) == shortcode(isni_str)

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ISNI} ISNI(12345678901234567, 1)  # Too many digits
        @test_throws ChecksumViolation{ISNI} ISNI(12281955, 2)  # Wrong checksum

        # Test code and checksum extraction
        isni_cases = [
            ("0000 0001 2281 955X", 12281955, 10),
            ("0000 0000 8389 1195", 8389119, 5),
            ("0000 0004 0600 5291", 40600529, 1),
            ("0000 0000 0000 0001", 0, 1),
            ("0000 0001 0000 0009", 10000000, 9)
        ]
        for (idstr, code, csum) in isni_cases
            isni = parse(ISNI, idstr)
            @test AcademicIdentifiers.idcode(isni) == code
            @test AcademicIdentifiers.idchecksum(isni) == csum
        end

        # Test X checksum handling
        isni_with_x = parse(ISNI, "0000 0001 2281 955X")
        @test AcademicIdentifiers.idchecksum(isni_with_x) == 10
        @test endswith(shortcode(isni_with_x), "X")

        # Test equality and hashing
        isni1 = parse(ISNI, "0000 0001 2281 955X")
        isni2 = ISNI(12281955, 10)
        isni3 = parse(ISNI, "0000 0000 8389 1195")

        @test isni1 == isni2
        @test isni1 != isni3
        @test hash(isni1) == hash(isni2)
        @test hash(isni1) != hash(isni3)
    end
end

@testset "ISSN" begin
    valid_examples = [
        # Basic ISSN format
        ("0317-8471", ISSN(317847)),
        ("1050-124X", ISSN(1050124)),
        ("2049-3630", ISSN(2049363)),
        ("0000-0000", ISSN(0)),
        ("1234-5679", ISSN(1234567)),
        ("9999-9994", ISSN(9999999)),
        # With lowercase prefix
        ("issn:0317-8471", ISSN(317847)),
        ("issn:1050-124X", ISSN(1050124)),
        ("issn:2049-3630", ISSN(2049363)),
        # With uppercase prefix
        ("ISSN:0317-8471", ISSN(317847)),
        ("ISSN:1050-124X", ISSN(1050124)),
        ("ISSN:2049-3630", ISSN(2049363)),
        # Without hyphens (should be parsed correctly)
        ("03178471", ISSN(317847)),
        ("1050124X", ISSN(1050124)),
        ("20493630", ISSN(2049363))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(ISSN, input_str) == tryparse(ISSN, input_str) == expected
            @test parse(ISSN, shortcode(expected)) == expected
            @test parse(ISSN, string(expected)) == expected
            # ISSN doesn't support purl parsing from URLs properly
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("0317-8471", "ISSN 0317-8471", "https://portal.issn.org/resource/ISSN/0317-8471"),
            ("1050-124X", "ISSN 1050-124X", "https://portal.issn.org/resource/ISSN/1050-124X"),
            ("2049-3630", "ISSN 2049-3630", "https://portal.issn.org/resource/ISSN/2049-3630"),
            ("0000-0000", "ISSN 0000-0000", "https://portal.issn.org/resource/ISSN/0000-0000"),
            ("1234-5679", "ISSN 1234-5679", "https://portal.issn.org/resource/ISSN/1234-5679"),
            ("9999-9994", "ISSN 9999-9994", "https://portal.issn.org/resource/ISSN/9999-9994")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(ISSN, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Wrong length
            "0317-84712",       # Too many digits (9 total)
            "12345678901",      # Too many digits (11 total)
            "1",                # Too few digits (1 total)

            # Non-numeric content
            "abcd-efgh",        # Non-numeric
            "031a-8471",        # Non-numeric in first part
            "0317-847a",        # Non-numeric in check position
            "0317-847Y",        # Invalid check character (Y)
            "0317-847Z",        # Invalid check character (Z)
            # Invalid format structure
            "0317_8471",        # Wrong separator
            "0317.8471",        # Wrong separator
            "0317 8471",        # Space separator
            # Empty or invalid
            "",                 # Empty string
            "   ",              # Whitespace only
            "invalid",          # Completely invalid
            # Unicode edge cases
            "0317‑8471",        # figure dash
            "0317−8471",        # minus sign
            "０317-8471",       # full-width zero
            "０３17-8471",      # full-width 03
            "0317-８471",       # full-width 8
            "0317-8471　"       # full-width space
        ]

        for bad_issn in malformed_examples
            @test_throws MalformedIdentifier{ISSN} parse(ISSN, bad_issn)
            @test tryparse(ISSN, bad_issn) === nothing
        end
    end

    @testset "Checksum errors" begin
        checksum_examples = [
            "0317-8470",  # Wrong checksum (should be 1)
            "0317-8472",  # Wrong checksum (should be 1)
            "0317-8473",  # Wrong checksum (should be 1)
            "0317-847",   # Missing check digit - interpreted as wrong checksum
            "031-8471",   # Too few digits but parsed as checksum violation
            "0317-",      # Missing last part but parsed as checksum violation
            "-8471",      # Missing first part but parsed as checksum violation
            "0317",       # Missing hyphen and last part but parsed as checksum violation
            "1050-1241",  # Wrong checksum (should be X/10)
            "1050-1242",  # Wrong checksum (should be X/10)
            "1050-1243",  # Wrong checksum (should be X/10)
            "2049-3631",  # Wrong checksum (should be 0)
            "2049-3632",  # Wrong checksum (should be 0)
            "0000-0001",  # Wrong checksum (should be 0)
            "1234-5670",  # Wrong checksum (should be 9)
            "9999-9995"   # Wrong checksum (should be 4)
        ]

        for bad_issn in checksum_examples
            @test_throws ChecksumViolation{ISSN} parse(ISSN, bad_issn)
            @test tryparse(ISSN, bad_issn) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor with correct checksum
        issn_int = ISSN(317847, 1)
        issn_str = parse(ISSN, "0317-8471")
        @test AcademicIdentifiers.idcode(issn_int) == AcademicIdentifiers.idcode(issn_str)
        @test AcademicIdentifiers.idchecksum(issn_int) == AcademicIdentifiers.idchecksum(issn_str)
        @test shortcode(issn_int) == shortcode(issn_str)

        # Test X checksum with integer constructor
        issn_with_x = ISSN(1050124, 10)
        @test AcademicIdentifiers.idchecksum(issn_with_x) == 10
        @test endswith(shortcode(issn_with_x), "X")

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ISSN} ISSN(12345678, 1)  # Too many digits
        @test_throws ChecksumViolation{ISSN} ISSN(317847, 2)  # Wrong checksum

        # Test code and checksum extraction
        issn_cases = [
            ("0317-8471", 317847, 1),
            ("1050-124X", 1050124, 10),
            ("2049-3630", 2049363, 0),
            ("0000-0000", 0, 0),
            ("1234-5679", 1234567, 9),
            ("9999-9994", 9999999, 4)
        ]
        for (idstr, code, csum) in issn_cases
            issn = parse(ISSN, idstr)
            @test AcademicIdentifiers.idcode(issn) == code
            @test AcademicIdentifiers.idchecksum(issn) == csum
        end

        # Test equality and hashing
        issn1 = parse(ISSN, "0317-8471")
        issn2 = ISSN(317847, 1)
        issn3 = parse(ISSN, "1050-124X")

        @test issn1 == issn2
        @test issn1 != issn3
        @test hash(issn1) == hash(issn2)
        @test hash(issn1) != hash(issn3)
    end
end

@testset "ISBN" begin
    valid_examples = [
        # Basic ISBN-13 format (978 prefix)
        ("978-0-439-02348-1", ISBN(9780439023481)),
        ("978-1-4028-9462-6", ISBN(9781402894626)),
        ("978-0-123-45678-6", ISBN(9780123456786)),
        ("978-0-486-41950-3", ISBN(9780486419503)),
        # ISBN-13 format (979 prefix)
        ("979-8-88645-174-0", ISBN(9798886451740)),
        ("979-1-234-56789-6", ISBN(9791234567896)),
        # With lowercase prefix
        ("isbn:978-0-439-02348-1", ISBN(9780439023481)),
        ("isbn:979-8-88645-174-0", ISBN(9798886451740)),
        ("isbn:978-1-4028-9462-6", ISBN(9781402894626)),
        # With uppercase prefix
        ("ISBN:978-0-439-02348-1", ISBN(9780439023481)),
        ("ISBN:979-8-88645-174-0", ISBN(9798886451740)),
        ("ISBN:978-1-4028-9462-6", ISBN(9781402894626)),
        # Without hyphens (should be parsed correctly)
        ("9780439023481", ISBN(9780439023481)),
        ("9798886451740", ISBN(9798886451740)),
        ("9781402894626", ISBN(9781402894626))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(ISBN, input_str) == tryparse(ISBN, input_str) == expected
            @test parse(ISBN, shortcode(expected)) == expected
            @test parse(ISBN, string(expected)) == expected
            # ISBN doesn't have purl support
            @test purl(expected) === nothing
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    hyphenation_examples = [
        "99921-58-10-7"
        "9971-5-0210-0"
        "960-425-059-0"
        "80-902734-1-6"
        "85-359-0277-5"
        "1-84356-028-3"
        "0-684-84328-5"
        "0-8044-2957-X"
        "0-85131-041-9"
        "93-86954-21-4"
        "0-943396-04-2"
        "0-9752298-0-X"
        "0-439-02348-3"
        "978-0-439-02348-1"
        "979-8-88645-174-0"
        "978-1-4028-9462-6"
        "978-1-56619-909-4"
        "978-0-321-53496-5"
        "978-3-16-148410-0"
        "1-4028-9462-7"
        "978-99953-838-2-4"
        "978-99930-75-89-9"
        "978-1-59059-356-1"
    ]

    @testset "Hyphenation" begin
        for isbn in hyphenation_examples
            @test shortcode(parse(ISBN, isbn)) == isbn
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("978-0-439-02348-1", "ISBN 978-0-439-02348-1", nothing),
            ("979-8-88645-174-0", "ISBN 979-8-88645-174-0", nothing),
            ("978-1-4028-9462-6", "ISBN 978-1-4028-9462-6", nothing),
            ("978-0-12-345678-6", "ISBN 978-0-12-345678-6", nothing),
            ("978-0-486-41950-3", "ISBN 978-0-486-41950-3", nothing),
            ("979-12-3456789-6", "ISBN 979-12-3456789-6", nothing)
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(ISBN, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) === purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Wrong length (non-hyphenated)
            "12345",                # Too short
            "123456789",            # Wrong length (9 digits)
            "12345678901",          # Wrong length (11 digits)
            "123456789012",         # Wrong length (12 digits)
            "12345678901234",       # Too long (14 digits)
            "97804390234812",       # Too long with prefix
            "97804390234",          # Too short with prefix
            # Wrong prefix
            "9771234567890",        # Invalid prefix (977)
            "9801234567890",        # Invalid prefix (980)
            "9751234567890",        # Invalid prefix (975)
            "1234567890123",        # Doesn't start with 978/979
            # Non-numeric content
            "fghdf hdfjhfgj",       # Non-numeric
            "085131ffff",           # Non-numeric
            "9971☃02100",           # Invalid character
            "X604250590",           # X in wrong position
            "960X250590",           # X in wrong position
            "96042X0590",           # X in wrong position
            "978-0-439-02348-a",    # Non-numeric check digit
            "978-a-439-02348-1",    # Non-numeric publisher
            "978-0-43a-02348-1",    # Non-numeric title
            # Invalid format structure
            "978_0_439_02348_1",    # Wrong separators
            "978.0.439.02348.1",    # Wrong separators
            # Note: Space and double separators are actually normalized by the parser
            # Empty or invalid
            "",                     # Empty string
            "   ",                  # Whitespace only
            "invalid",              # Completely invalid
            # Unicode edge cases
            "978-0-439-02348-🔢",   # emoji digit
            "978-0-439-02348-①",    # circled digit
            "978‑0‑439‑02348‑1",     # figure dash instead of hyphen
            "978−0−439−02348−1",     # minus sign instead of hyphen
            "978-０-439-02348-1",    # full-width zero
            "９78-0-439-02348-1",    # full-width 9
            "978-0-４39-02348-1",    # full-width 4
            "978-0-439-０2348-1",    # full-width 0
            "９７８-０-４３９-０２３４８-１" # all full-width
        ]

        for bad_isbn in malformed_examples
            @test_throws MalformedIdentifier{ISBN} parse(ISBN, bad_isbn)
            @test tryparse(ISBN, bad_isbn) === nothing
        end
    end

    @testset "Checksum errors" begin
        checksum_examples = [
            "978-0-439-02348-0",  # Wrong checksum (should be 1)
            "978-0-439-02348-2",  # Wrong checksum (should be 1)
            "978-0-439-02348-3",  # Wrong checksum (should be 1)
            "978-1-4028-9462-5",  # Wrong checksum (should be 6)
            "978-1-4028-9462-7",  # Wrong checksum (should be 6)
            "978-1-4028-9462-8",  # Wrong checksum (should be 6)
            "979-8-88645-174-1",  # Wrong checksum (should be 0)
            "979-8-88645-174-2",  # Wrong checksum (should be 0)
            "978-0-123-45678-0",  # Wrong checksum (should be 9)
            "978-0-486-41950-4",  # Wrong checksum (should be 3)
            "979-1-234-56789-0"   # Wrong checksum (should be 1)
        ]

        for bad_isbn in checksum_examples
            @test_throws ChecksumViolation{EAN13} parse(ISBN, bad_isbn)
            @test tryparse(ISBN, bad_isbn) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        isbn_int = ISBN(9780439023481)
        isbn_str = parse(ISBN, "978-0-439-02348-1")
        @test shortcode(isbn_int) == shortcode(isbn_str)
        @test string(isbn_int) == "ISBN 978-0-439-02348-1"

        # Test another valid 13-digit ISBN
        isbn_int2 = ISBN(9781402894626)
        isbn_str2 = parse(ISBN, "978-1-4028-9462-6")
        @test shortcode(isbn_int2) == shortcode(isbn_str2)

        # Test 979 prefix ISBN
        isbn_979 = ISBN(9798886451740)
        @test shortcode(isbn_979) == "979-8-88645-174-0"
        @test string(isbn_979) == "ISBN 979-8-88645-174-0"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ISBN} ISBN(123456789012)  # 12 digits
        @test_throws MalformedIdentifier{ISBN} ISBN(12345678901234)  # 14 digits
        @test_throws MalformedIdentifier{ISBN} ISBN(9771234567890)  # doesn't start with 978/979
        @test_throws MalformedIdentifier{ISBN} ISBN(9751234567890)  # invalid prefix
        @test_throws MalformedIdentifier{ISBN} ISBN(9801234567890)  # invalid prefix

        # Test EAN13 conversion
        isbn = parse(ISBN, "978-0-439-02348-1")
        ean = convert(EAN13, isbn)
        @test AcademicIdentifiers.idcode(ean) == 978043902348
        @test AcademicIdentifiers.idchecksum(ean) == 1

        # Test equality and hashing
        isbn1 = parse(ISBN, "978-0-439-02348-1")
        isbn2 = ISBN(9780439023481)
        isbn3 = parse(ISBN, "979-8-88645-174-0")

        @test isbn1 == isbn2
        @test isbn1 != isbn3
        @test hash(isbn1) == hash(isbn2)
        @test hash(isbn1) != hash(isbn3)
    end

    @testset "conversions" begin
        # Test ISBN to EAN13 conversion
        isbn = parse(ISBN, "978-0-439-02348-1")
        ean = convert(EAN13, isbn)
        @test shortcode(ean) == "9780439023481"

        # Test EAN13 to ISBN conversion (only works if the EAN13 was created from an ISBN)
        isbn_str = "978-0-439-02348-1"
        isbn_original = parse(ISBN, isbn_str)
        ean_from_isbn = convert(EAN13, isbn_original)
        isbn_back = convert(ISBN, ean_from_isbn)
        @test string(isbn_back) == "ISBN " * isbn_str

        # Test round-trip conversion
        isbn_original2 = parse(ISBN, "978-1-4028-9462-6")
        ean_converted = convert(EAN13, isbn_original2)
        isbn_back2 = convert(ISBN, ean_converted)
        @test string(isbn_original2) == string(isbn_back2)

        # Test EAN13 to ISBN conversion (979 prefix is valid for ISBN)
        ean_979 = parse(EAN13, "9790123456785")  # starts with 979, valid checksum
        @test convert(ISBN, ean_979) isa ISBN
        ean_917 = parse(EAN13, "9170486419502") # Invalid ISBN, but valid EAN13
        @test_throws MalformedIdentifier{ISBN} convert(ISBN, ean_917)
    end
end

@testset "OCN" begin
    valid_examples = [
        # Basic OCN format (various lengths)
        ("1", OCN(1)),
        ("123", OCN(123)),
        ("1234567", OCN(1234567)),
        ("12345678", OCN(12345678)),
        ("123456789", OCN(123456789)),
        ("1234567890", OCN(1234567890)),
        ("12345678901", OCN(12345678901)),
        ("123456789012", OCN(123456789012)),
        # With lowercase prefixes
        ("ocn1234567", OCN(1234567)),
        ("ocn12345678", OCN(12345678)),
        ("ocn123456789", OCN(123456789)),
        ("oclc:1234567", OCN(1234567)),
        ("oclc:12345678", OCN(12345678)),
        ("oclc:123456789", OCN(123456789)),
        # With uppercase prefixes
        ("OCN1234567", OCN(1234567)),
        ("OCLC:12345678", OCN(12345678)),
        ("OCLC:123456789", OCN(123456789)),
        # With (OCoLC)
        ("(OCoLC)ocm00456789", OCN(456789)),
        ("(OCoLC)ocn123456789", OCN(123456789)),
        ("(OCoLC)on1234567890", OCN(1234567890)),
        # URL format
        ("https://worldcat.org/oclc/1234567", OCN(1234567)),
        ("https://worldcat.org/oclc/12345678", OCN(12345678)),
        ("https://www.worldcat.org/oclc/123456789", OCN(123456789))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(OCN, input_str) == tryparse(OCN, input_str) == expected
            @test parse(OCN, shortcode(expected)) == expected
            @test parse(OCN, string(expected)) == expected
            @test parse(OCN, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("1", "ocm1", "https://worldcat.org/oclc/1"),
            ("123", "ocm123", "https://worldcat.org/oclc/123"),
            ("1234567", "ocm1234567", "https://worldcat.org/oclc/1234567"),
            ("12345678", "ocm12345678", "https://worldcat.org/oclc/12345678"),
            ("123456789", "ocn123456789", "https://worldcat.org/oclc/123456789"),
            ("1234567890", "on1234567890", "https://worldcat.org/oclc/1234567890"),
            ("12345678901", "on12345678901", "https://worldcat.org/oclc/12345678901"),
            ("123456789012", "on123456789012", "https://worldcat.org/oclc/123456789012")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(OCN, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Invalid content
            "",                      # Empty string
            "   ",                   # Whitespace only
            "invalid",               # Completely invalid
            # Note: "0" and leading zeros like "00123" are actually accepted by the parser
            "123456a",               # Non-numeric characters
            "abc123",                # Non-numeric prefix
            "123.456",               # Decimal point
            "123,456",               # Comma
            "123-456",               # Hyphen
            "123_456",               # Underscore
            "123 456",               # Space
            # Too long (OCNs are typically up to 12 digits)
            "12345678901234567890123456789012", # Way too long
            # Note: "1234567890123456" (16 digits) is actually accepted by the parser
            # Invalid prefixes
            "ocln123456",            # Wrong prefix
            "oclc :123456",          # Space after colon
            # Note: "oclc123456" and "oclc: 123456" are actually accepted by the parser
            # URL format errors
            "https://worldcat.org/oclc/",  # Missing number
            "https://worldcat.org/oclc/abc", # Non-numeric in URL
            # Unicode edge cases
            "1234567🔢",             # emoji
            "１234567",              # full-width 1
            "1234567　",             # full-width space
            "12３4567",              # full-width 3
            "１２３４５６７"           # all full-width
        ]

        for bad_ocn in malformed_examples
            @test_throws MalformedIdentifier{OCN} parse(OCN, bad_ocn)
            @test tryparse(OCN, bad_ocn) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        ocn_int = OCN(1234567)
        ocn_str = parse(OCN, "1234567")
        @test shortcode(ocn_int) == shortcode(ocn_str)
        @test string(ocn_int) == "ocm1234567"

        # Test various sizes
        ocn_small = OCN(1)
        ocn_large = OCN(123456789012)
        @test shortcode(ocn_small) == "1"
        @test shortcode(ocn_large) == "123456789012"
        @test string(ocn_small) == "ocm1"
        @test string(ocn_large) == "on123456789012"

        # Test invalid integer inputs
        # Note: OCN(0) is actually allowed by the constructor
        @test_throws InexactError OCN(-1) # Negative not allowed

        # Test equality and hashing
        ocn1 = parse(OCN, "1234567")
        ocn2 = OCN(1234567)
        ocn3 = parse(OCN, "12345678")

        @test ocn1 == ocn2
        @test ocn1 != ocn3
        @test hash(ocn1) == hash(ocn2)
        @test hash(ocn1) != hash(ocn3)
    end
end

@testset "ORCID" begin
    valid_examples = [
        # Basic ORCID format
        ("0000-0002-1825-0097", ORCID(21825009, 7)),
        ("0000-0003-1419-2405", ORCID(31419240, 5)),
        ("0000-0001-5109-3700", ORCID(15109370, 0)),
        ("0000-0002-1694-233X", ORCID(21694233, 10)),
        ("0000-0000-0000-0001", ORCID(0, 1)),
        ("0000-0001-0000-0009", ORCID(10000000, 9)),
        # With lowercase prefix
        ("orcid:0000-0002-1825-0097", ORCID(21825009, 7)),
        ("orcid:0000-0003-1419-2405", ORCID(31419240, 5)),
        ("orcid:0000-0001-5109-3700", ORCID(15109370, 0)),
        ("orcid:0000-0002-1694-233X", ORCID(21694233, 10)),
        # With uppercase prefix
        ("ORCID:0000-0002-1825-0097", ORCID(21825009, 7)),
        ("ORCID:0000-0003-1419-2405", ORCID(31419240, 5)),
        ("ORCID:0000-0001-5109-3700", ORCID(15109370, 0)),
        ("ORCID:0000-0002-1694-233X", ORCID(21694233, 10)),
        # URL format
        ("https://orcid.org/0000-0002-1825-0097", ORCID(21825009, 7)),
        ("https://orcid.org/0000-0003-1419-2405", ORCID(31419240, 5)),
        ("https://orcid.org/0000-0001-5109-3700", ORCID(15109370, 0)),
        # Without hyphens (should be parsed correctly)
        ("0000000218250097", ORCID(21825009, 7)),
        ("0000000314192405", ORCID(31419240, 5)),
        ("000000021694233X", ORCID(21694233, 10))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(ORCID, input_str) == tryparse(ORCID, input_str) == expected
            @test parse(ORCID, shortcode(expected)) == expected
            @test parse(ORCID, string(expected)) == expected
            @test parse(ORCID, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("0000-0002-1825-0097", "https://orcid.org/0000-0002-1825-0097", "https://orcid.org/0000-0002-1825-0097"),
            ("0000-0003-1419-2405", "https://orcid.org/0000-0003-1419-2405", "https://orcid.org/0000-0003-1419-2405"),
            ("0000-0001-5109-3700", "https://orcid.org/0000-0001-5109-3700", "https://orcid.org/0000-0001-5109-3700"),
            ("0000-0002-1694-233X", "https://orcid.org/0000-0002-1694-233X", "https://orcid.org/0000-0002-1694-233X"),
            ("0000-0000-0000-0001", "https://orcid.org/0000-0000-0000-0001", "https://orcid.org/0000-0000-0000-0001"),
            ("0000-0001-0000-0009", "https://orcid.org/0000-0001-0000-0009", "https://orcid.org/0000-0001-0000-0009")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(ORCID, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Wrong length
            "0000-0000-0000-00000",  # Too many digits (17 total)
            # Note: Cases below cause ChecksumViolation, moved to checksum section
            # "0000-0000-0000-000",    # Too few digits (15 total) - causes ChecksumViolation
            # "0000-0000-0000",        # Missing last group - causes ChecksumViolation
            # "0000-0000",             # Missing last two groups - causes ChecksumViolation
            # "0000",                  # Missing last three groups - causes ChecksumViolation
            # Invalid format structure
            "0000_0002_1825_0097",   # Wrong separators
            "0000.0002.1825.0097",   # Wrong separators
            "0000 0002 1825 0097",   # Space separators
            # Note: "0000--0002--1825--0097" is actually accepted by parser (normalization)
            # Note: "000000021825009" causes ChecksumViolation, moved to checksum section
            "00000002182500971",     # Extra digit
            # Invalid characters
            "000X-0002-1825-0097",   # Non-numeric in wrong position
            "0000-000X-1825-0097",   # Non-numeric in wrong position
            "0000-0002-182X-0097",   # Non-numeric in wrong position
            "abcd-0000-0000-0000",   # Non-numeric
            "0000-0002-1825-0097Y",  # Invalid check character (Y)
            "0000-0002-1825-0097Z",  # Invalid check character (Z)
            # Cases that throw ChecksumViolation should be in checksum errors section
            # "0000-0000-0000-0000",   # Wrong checksum (should be 1) - moved to checksum section
            # "0000-0002-1825-0099",   # Wrong checksum (should be 7) - moved to checksum section
            # Empty or invalid
            "",                      # Empty string
            "   ",                   # Whitespace only
            "invalid",               # Completely invalid
            # Unicode edge cases
            "0000‑0002‑1825‑0097",    # figure dash
            "0000−0002−1825−0097",    # minus sign
            "0000-0002-1825-0097🔢",  # emoji
            "０000-0002-1825-0097",   # full-width zero
            "0000-０002-1825-0097",   # full-width zero in middle
            "0000-0002-1825-００97",  # full-width zeros at end
            "０００0-0002-1825-0097"   # multiple full-width zeros
        ]

        for bad_orcid in malformed_examples
            @test_throws MalformedIdentifier{ORCID} parse(ORCID, bad_orcid)
            @test tryparse(ORCID, bad_orcid) === nothing
        end
    end

    @testset "Checksum errors" begin
        checksum_examples = [
            "0000-0002-1825-0096",  # Wrong checksum (should be 7)
            "0000-0002-1825-0098",  # Wrong checksum (should be 7)
            "0000-0002-1825-0099",  # Wrong checksum (should be 7)
            "0000-0003-1419-2404",  # Wrong checksum (should be 5)
            "0000-0003-1419-2406",  # Wrong checksum (should be 5)
            "0000-0003-1419-2407",  # Wrong checksum (should be 5)
            "0000-0001-5109-3701",  # Wrong checksum (should be 0)
            "0000-0001-5109-3702",  # Wrong checksum (should be 0)
            "0000-0002-1694-2330",  # Wrong checksum (should be X/10)
            "0000-0002-1694-2331",  # Wrong checksum (should be X/10)
            "0000-0000-0000-0000",  # Wrong checksum (should be 1)
            "0000-0001-0000-0000",  # Wrong checksum (should be 9)
            # Cases moved from malformed section that cause ChecksumViolation
            "0000-0000-0000-000",   # Too few digits but parsed as 0 with wrong checksum
            "0000-0000-0000",       # Missing last group but parsed as 0 with wrong checksum
            "0000-0000",            # Missing groups but parsed as 0 with wrong checksum
            "0000",                 # Missing groups but parsed as 0 with wrong checksum
            "000000021825009"       # Missing last digit but creates valid structure with wrong checksum
        ]

        for bad_orcid in checksum_examples
            @test_throws ChecksumViolation{ORCID} parse(ORCID, bad_orcid)
            @test tryparse(ORCID, bad_orcid) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        orcid_int = ORCID(21825009, 7)
        @test AcademicIdentifiers.idcode(orcid_int) == 21825009
        @test AcademicIdentifiers.idchecksum(orcid_int) == 7
        @test shortcode(orcid_int) == "0000-0002-1825-0097"

        # Test X checksum with integer constructor
        orcid_with_x = ORCID(21694233, 10)
        @test AcademicIdentifiers.idchecksum(orcid_with_x) == 10
        @test endswith(shortcode(orcid_with_x), "X")

        # Test various valid ORCIDs
        orcid_zero = ORCID(0, 1)
        orcid_small = ORCID(10000000, 9)
        @test shortcode(orcid_zero) == "0000-0000-0000-0001"
        @test shortcode(orcid_small) == "0000-0001-0000-0009"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ORCID} ORCID(12345678901234567, 5)  # Too many digits
        @test_throws MalformedIdentifier{ORCID} ORCID(-1, 0)  # Negative number
        @test_throws ChecksumViolation{ORCID} ORCID(21825009, 8)  # Wrong checksum
        @test_throws ChecksumViolation{ORCID} ORCID(0, 0)  # Wrong checksum for zero

        # Test case sensitivity for X
        @test shortcode(parse(ORCID, "0000-0002-1694-233x")) == "0000-0002-1694-233X"

        # Test equality and hashing
        orcid1 = parse(ORCID, "0000-0002-1825-0097")
        orcid2 = ORCID(21825009, 7)
        orcid3 = parse(ORCID, "0000-0003-1419-2405")

        @test orcid1 == orcid2
        @test orcid1 != orcid3
        @test hash(orcid1) == hash(orcid2)
        @test hash(orcid1) != hash(orcid3)
    end
end

@testset "OpenAlexID" begin
    valid_examples = [
        # Works (W) type
        ("W2741809807", OpenAlexID{:W}(2741809807)),
        ("W1234567890", OpenAlexID{:W}(1234567890)),
        ("W0000000001", OpenAlexID{:W}(1)),
        # Authors (A) type
        ("A2208157607", OpenAlexID{:A}(2208157607)),
        ("A1234567890", OpenAlexID{:A}(1234567890)),
        ("A0000000001", OpenAlexID{:A}(1)),
        # Sources (S) type
        ("S2741809807", OpenAlexID{:S}(2741809807)),
        ("S1234567890", OpenAlexID{:S}(1234567890)),
        ("S0000000001", OpenAlexID{:S}(1)),
        # Institutions (I) type
        ("I2741809807", OpenAlexID{:I}(2741809807)),
        ("I1234567890", OpenAlexID{:I}(1234567890)),
        ("I0000000001", OpenAlexID{:I}(1)),
        # Concepts (C) type
        ("C2741809807", OpenAlexID{:C}(2741809807)),
        ("C1234567890", OpenAlexID{:C}(1234567890)),
        ("C0000000001", OpenAlexID{:C}(1)),
        # Publishers (P) type
        ("P2741809807", OpenAlexID{:P}(2741809807)),
        ("P1234567890", OpenAlexID{:P}(1234567890)),
        # Funders (F) type
        ("F2741809807", OpenAlexID{:F}(2741809807)),
        ("F1234567890", OpenAlexID{:F}(1234567890)),
        # Lowercase entity types (should work)
        ("w2741809807", OpenAlexID{:W}(2741809807)),
        ("a2208157607", OpenAlexID{:A}(2208157607)),
        ("s2741809807", OpenAlexID{:S}(2741809807)),
        # With lowercase prefix
        ("openalex:W2741809807", OpenAlexID{:W}(2741809807)),
        ("openalex:A2208157607", OpenAlexID{:A}(2208157607)),
        ("openalex:S2741809807", OpenAlexID{:S}(2741809807)),
        # With uppercase prefix
        ("OpenAlex:W2741809807", OpenAlexID{:W}(2741809807)),
        ("OPENALEX:A2208157607", OpenAlexID{:A}(2208157607)),
        # URL format
        ("https://openalex.org/W2741809807", OpenAlexID{:W}(2741809807)),
        ("https://openalex.org/A2208157607", OpenAlexID{:A}(2208157607)),
        ("https://openalex.org/S2741809807", OpenAlexID{:S}(2741809807)),
        ("https://openalex.org/I2741809807", OpenAlexID{:I}(2741809807)),
        ("https://openalex.org/C2741809807", OpenAlexID{:C}(2741809807)),
        # With a uninformative prefix
        ("https://openalex.org/works/W2741809807", OpenAlexID{:W}(2741809807)),
        ("https://openalex.org/authors/A2208157607", OpenAlexID{:A}(2208157607)),
        ("https://openalex.org/sources/S2741809807", OpenAlexID{:S}(2741809807)),
        ("https://openalex.org/institutions/I2741809807", OpenAlexID{:I}(2741809807)),
        ("https://openalex.org/concepts/C2741809807", OpenAlexID{:C}(2741809807))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(OpenAlexID, input_str) == parse(typeof(expected), input_str)
            @test parse(OpenAlexID, input_str) == tryparse(OpenAlexID, input_str) == expected
            @test parse(OpenAlexID, shortcode(expected)) == expected
            @test parse(OpenAlexID, string(expected)) == expected
            @test parse(OpenAlexID, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("W2741809807", "https://openalex.org/W2741809807", "https://openalex.org/W2741809807"),
            ("A2208157607", "https://openalex.org/A2208157607", "https://openalex.org/A2208157607"),
            ("S2741809807", "https://openalex.org/S2741809807", "https://openalex.org/S2741809807"),
            ("I2741809807", "https://openalex.org/I2741809807", "https://openalex.org/I2741809807"),
            ("C2741809807", "https://openalex.org/C2741809807", "https://openalex.org/C2741809807"),
            ("P2741809807", "https://openalex.org/P2741809807", "https://openalex.org/P2741809807"),
            ("F2741809807", "https://openalex.org/F2741809807", "https://openalex.org/F2741809807"),
            ("W1234567890", "https://openalex.org/W1234567890", "https://openalex.org/W1234567890"),
            ("A1", "https://openalex.org/A1", "https://openalex.org/A1")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(OpenAlexID, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Invalid entity types
            "X2741809807",       # Invalid prefix (X)
            "Y2741809807",       # Invalid prefix (Y)
            "Z2741809807",       # Invalid prefix (Z)
            "B2741809807",       # Invalid prefix (B)
            "D2741809807",       # Invalid prefix (D)
            "Q2741809807",       # Invalid prefix (Q)
            "R2741809807",       # Invalid prefix (R)
            # Missing parts
            "W",                 # Missing number
            "A",                 # Missing number
            "2741809807",        # Missing prefix
            "0000000001",        # Missing prefix
            # Invalid number format
            "W274180980a",       # Non-numeric
            "A220815760b",       # Non-numeric
            "S27418098c7",       # Non-numeric
            "I2741809807x",      # Non-numeric suffix
            "C274180980.7",      # Decimal point
            "W2741809807-",      # Hyphen
            "A2741809807_",      # Underscore
            # Empty or invalid
            "",                  # Empty string
            "   ",               # Whitespace only
            "invalid",           # Completely invalid
            # Unicode edge cases
            "W2741809807🔢",     # emoji
            "Ｗ2741809807",      # full-width W
            "W２741809807",      # full-width 2
            "W2741809８07",      # full-width 8
            "Ａ2208157607"       # full-width A
        ]

        for bad_id in malformed_examples
            @test_throws MalformedIdentifier{OpenAlexID} parse(OpenAlexID, bad_id)
            @test tryparse(OpenAlexID, bad_id) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructors
        w_id = OpenAlexID{:W}(2741809807)
        a_id = OpenAlexID{:A}(2208157607)
        s_id = OpenAlexID{:S}(2741809807)
        i_id = OpenAlexID{:I}(2741809807)
        c_id = OpenAlexID{:C}(2741809807)

        @test shortcode(w_id) == "W2741809807"
        @test shortcode(a_id) == "A2208157607"
        @test shortcode(s_id) == "S2741809807"
        @test shortcode(i_id) == "I2741809807"
        @test shortcode(c_id) == "C2741809807"

        # Test type parameter extraction
        @test typeof(w_id) == OpenAlexID{:W}
        @test typeof(a_id) == OpenAlexID{:A}
        @test typeof(s_id) == OpenAlexID{:S}
        @test typeof(i_id) == OpenAlexID{:I}
        @test typeof(c_id) == OpenAlexID{:C}

        # Test invalid integer inputs
        @test_throws InexactError OpenAlexID{:A}(-1) # Negative not allowed (UInt64 conversion error)

        # Test that zero is allowed
        zero_id = OpenAlexID{:W}(0)
        @test shortcode(zero_id) == "W0"

        # Test equality and hashing
        w1 = parse(OpenAlexID, "W2741809807")
        w2 = OpenAlexID{:W}(2741809807)
        a1 = parse(OpenAlexID, "A2208157607")

        @test w1 == w2
        @test w1 != a1
        @test hash(w1) == hash(w2)
        @test hash(w1) != hash(a1)

        # Test kind mismatch
        @test_throws MalformedIdentifier{OpenAlexID{:W}} parse(OpenAlexID{:W}, "A2741809807")
        @test_throws MalformedIdentifier{OpenAlexID{:A}} parse(OpenAlexID{:A}, "W2741809807")

        # Test typed parsing
        @test parse(OpenAlexID{:W}, "W2741809807") isa OpenAlexID{:W}
        @test parse(OpenAlexID{:A}, "A2208157607") isa OpenAlexID{:A}
    end
end

@testset "RAiD" begin
    valid_examples = [
        # Basic DOI format (RAiD is essentially a DOI wrapper)
        ("10.1000/182", RAiD(DOI(SubString("10.1000"), SubString("182")))),
        ("10.1038/nature12373", RAiD(DOI(SubString("10.1038"), SubString("nature12373")))),
        ("10.1016/j.cell.2020.01.001", RAiD(DOI(SubString("10.1016"), SubString("j.cell.2020.01.001")))),
        ("10.1093/nar/gkaa1100", RAiD(DOI(SubString("10.1093"), SubString("nar/gkaa1100")))),
        ("10.5555/12345678", RAiD(DOI(SubString("10.5555"), SubString("12345678")))),
        # With lowercase prefix
        ("raid:10.1000/182", RAiD(DOI(SubString("10.1000"), SubString("182")))),
        ("raid:10.1038/nature12373", RAiD(DOI(SubString("10.1038"), SubString("nature12373")))),
        ("raid:10.1016/j.cell.2020.01.001", RAiD(DOI(SubString("10.1016"), SubString("j.cell.2020.01.001")))),
        # With uppercase prefix
        ("RAID:10.1000/182", RAiD(DOI(SubString("10.1000"), SubString("182")))),
        ("RAiD:10.1038/nature12373", RAiD(DOI(SubString("10.1038"), SubString("nature12373")))),
        # URL format
        ("https://raid.org/10.1000/182", RAiD(DOI(SubString("10.1000"), SubString("182")))),
        ("https://raid.org/10.1038/nature12373", RAiD(DOI(SubString("10.1038"), SubString("nature12373")))),
        ("https://raid.org/10.1016/j.cell.2020.01.001", RAiD(DOI(SubString("10.1016"), SubString("j.cell.2020.01.001"))))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(RAiD, input_str) == tryparse(RAiD, input_str) == expected
            @test parse(RAiD, shortcode(expected)) == expected
            @test parse(RAiD, string(expected)) == expected
            @test parse(RAiD, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("10.1000/182", "https://raid.org/10.1000/182", "https://raid.org/10.1000/182"),
            ("10.1038/nature12373", "https://raid.org/10.1038/nature12373", "https://raid.org/10.1038/nature12373"),
            ("10.1016/j.cell.2020.01.001", "https://raid.org/10.1016/j.cell.2020.01.001", "https://raid.org/10.1016/j.cell.2020.01.001"),
            ("10.1093/nar/gkaa1100", "https://raid.org/10.1093/nar/gkaa1100", "https://raid.org/10.1093/nar/gkaa1100"),
            ("10.5555/12345678", "https://raid.org/10.5555/12345678", "https://raid.org/10.5555/12345678")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(RAiD, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        # Cases that throw MalformedIdentifier{DOI} (internal DOI parsing failures)
        doi_malformed_examples = [
            "invalid",              # No DOI structure
            "",                     # Empty string
            "   ",                  # Whitespace only
            "1000/182",            # Missing prefix
            "10.1000/",            # Missing suffix
            "10.1000",             # Missing suffix entirely
            "10.1000182",          # Missing slash
            "raid:",               # Prefix only
            "RAID:",               # Prefix only uppercase
            # Unicode edge cases that are actually invalid
            "10．1000/182",        # full-width period
            "10.1000／182",        # full-width slash
            "１0.1000/182",         # full-width 1
            "10.１000/182",        # full-width 1
        ]

        for bad_raid in doi_malformed_examples
            @test_throws MalformedIdentifier{DOI} parse(RAiD, bad_raid)
            @test tryparse(RAiD, bad_raid) === nothing
        end

        # Cases that should throw MalformedIdentifier{RAiD} (truly malformed RAiD)
        raid_malformed_examples = [
        # Currently no cases throw MalformedIdentifier{RAiD} - they all throw DOI exceptions
        ]

        # Skip this test section since no cases throw MalformedIdentifier{RAiD}
        # for bad_raid in raid_malformed_examples
        #     @test_throws MalformedIdentifier{RAiD} parse(RAiD, bad_raid)
        #     @test tryparse(RAiD, bad_raid) === nothing
        # end
    end

    @testset "Miscellaneous" begin
        # Test DOI extraction
        raid = parse(RAiD, "10.1000/182")
        @test raid.id.registrant == "10.1000"
        @test raid.id.object == "182"

        # Test DOI conversion
        raid_complex = parse(RAiD, "10.1016/j.cell.2020.01.001")
        doi_converted = convert(DOI, raid_complex)
        @test shortcode(doi_converted) == "10.1016/j.cell.2020.01.001"

        # Test equality and hashing
        raid1 = parse(RAiD, "10.1000/182")
        raid2 = RAiD(DOI(SubString("10.1000"), SubString("182")))
        raid3 = parse(RAiD, "10.1038/nature12373")

        @test raid1 == raid2
        @test raid1 != raid3
        @test hash(raid1) == hash(raid2)
        @test hash(raid1) != hash(raid3)
    end
end

@testset "ROR" begin
    valid_examples = [
        # Basic ROR format
        ("05dxps055", ROR(182377248)),
        ("02jx3x895", ROR(86937512)),
        ("00x6h5n95", ROR(30622901)),
        ("00j6hb750", ROR(19088743)),
        ("04vft6f75", ROR(163047631)),
        ("07zymws49", ROR(268391321)),
        # With lowercase prefix
        ("ror:05dxps055", ROR(182377248)),
        ("ror:02jx3x895", ROR(86937512)),
        ("ror:00x6h5n95", ROR(30622901)),
        # With uppercase prefix
        ("ROR:05dxps055", ROR(182377248)),
        ("ROR:02jx3x895", ROR(86937512)),
        ("ROR:00x6h5n95", ROR(30622901)),
        # URL format
        ("https://ror.org/05dxps055", ROR(182377248)),
        ("https://ror.org/02jx3x895", ROR(86937512)),
        ("https://ror.org/00x6h5n95", ROR(30622901)),
        ("https://ror.org/00j6hb750", ROR(19088743))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(ROR, input_str) == tryparse(ROR, input_str) == expected
            @test parse(ROR, shortcode(expected)) == expected
            @test parse(ROR, string(expected)) == expected
            @test parse(ROR, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("05dxps055", "https://ror.org/05dxps055", "https://ror.org/05dxps055"),
            ("02jx3x895", "https://ror.org/02jx3x895", "https://ror.org/02jx3x895"),
            ("00x6h5n95", "https://ror.org/00x6h5n95", "https://ror.org/00x6h5n95"),
            ("00j6hb750", "https://ror.org/00j6hb750", "https://ror.org/00j6hb750"),
            ("04vft6f75", "https://ror.org/04vft6f75", "https://ror.org/04vft6f75"),
            ("07zymws49", "https://ror.org/07zymws49", "https://ror.org/07zymws49")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(ROR, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Wrong length
            "05dxps05",         # Too short (8 characters)
            "105dxps055",       # Too long (10 characters)
            "5dxps055",         # Too short (8 characters)
            "05dxps0550",       # Too long (10 characters)
            # Invalid start character (must start with 0)
            "15dxps055",        # Invalid start character (1)
            "25dxps055",        # Invalid start character (2)
            "a5dxps055",        # Invalid start character (a)
            "x5dxps055",        # Invalid start character (x)
            "01234567a",  # Invalid checksum character (a not allowed in numeric checksum)
            "01234567b",  # Invalid checksum character (b not allowed)
            "09abcdefg",  # Invalid checksum character (g not allowed)

            # Invalid characters
            "05dxps05z",        # Invalid character (z not allowed)
            "05dxps.55",        # Invalid character (period)
            "05dxps-55",        # Invalid character (hyphen)
            "05dxps_55",        # Invalid character (underscore)
            "05dxps 55",        # Invalid character (space)
            # Empty or invalid
            "",                 # Empty string
            "   ",              # Whitespace only
            "invalid",          # Completely invalid
            "123456789",        # All numeric
            "abcdefghi",        # All letters
            # Unicode edge cases
            "05dxps055🔢",      # emoji
            "05dx🔤s055",       # emoji letter
            "０5dxps055",       # full-width zero
            "05ｄxps055",       # full-width letter
            "05dxps０55"        # full-width zero at end
        ]

        for bad_ror in malformed_examples
            @test_throws MalformedIdentifier{ROR} parse(ROR, bad_ror)
            @test tryparse(ROR, bad_ror) === nothing
        end
    end

    @testset "Checksum errors" begin
        checksum_examples = [
            "05dxps054",  # Wrong checksum (should be 55)
            "05dxps056",  # Wrong checksum (should be 55)
            "05dxps057",  # Wrong checksum (should be 55)
            "02jx3x894",  # Wrong checksum (should be 95)
            "02jx3x896",  # Wrong checksum (should be 95)
            "02jx3x897",  # Wrong checksum (should be 95)
            "00x6h5n94",  # Wrong checksum (should be 95)
            "00x6h5n96",  # Wrong checksum (should be 95)
            "05dxpso55",  # Invalid character interpreted as wrong checksum
            "0fedcba98",  # Wrong checksum (should be 33)
            "0fedcba99"   # Wrong checksum (should be 33)
        ]

        for bad_ror in checksum_examples
            @test_throws ChecksumViolation{ROR} parse(ROR, bad_ror)
            @test tryparse(ROR, bad_ror) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        ror_int = ROR(182377248, 55)
        @test AcademicIdentifiers.idcode(ror_int) == 182377248
        @test AcademicIdentifiers.idchecksum(ror_int) == 55
        @test shortcode(ror_int) == "05dxps055"

        # Test other constructors
        ror_int2 = ROR(86937512, 95)
        ror_int3 = ROR(30622901, 95)
        ror_int4 = ROR(19088743, 50)
        @test shortcode(ror_int2) == "02jx3x895"
        @test shortcode(ror_int3) == "00x6h5n95"
        @test shortcode(ror_int4) == "00j6hb750"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ROR} ROR(-1, 50)  # Negative number
        @test_throws ChecksumViolation{ROR} ROR(0, 0)      # Wrong checksum
        @test_throws ChecksumViolation{ROR} ROR(182377248, 54)  # Wrong checksum
        @test_throws ChecksumViolation{ROR} ROR(86937512, 94)   # Wrong checksum

        # Test code and checksum extraction
        ror_cases = [
            ("05dxps055", 182377248, 55),
            ("02jx3x895", 86937512, 95),
            ("00x6h5n95", 30622901, 95),
            ("00j6hb750", 19088743, 50),
            ("04vft6f75", 163047631, 75),
            ("07zymws49", 268391321, 49)
        ]
        for (idstr, code, csum) in ror_cases
            ror = parse(ROR, idstr)
            @test AcademicIdentifiers.idcode(ror) == code
            @test AcademicIdentifiers.idchecksum(ror) == csum
        end

        # Test equality and hashing
        ror1 = parse(ROR, "05dxps055")
        ror2 = ROR(182377248, 55)
        ror3 = parse(ROR, "02jx3x895")

        @test ror1 == ror2
        @test ror1 != ror3
        @test hash(ror1) == hash(ror2)
        @test hash(ror1) != hash(ror3)
    end
end

@testset "PMID" begin
    valid_examples = [
        # Basic PMID format (various lengths)
        ("1", PMID(1)),
        ("123", PMID(123)),
        ("1234567", PMID(1234567)),
        ("12345678", PMID(12345678)),
        ("87654321", PMID(87654321)),
        ("12345678", PMID(12345678)),
        ("87654321", PMID(87654321)),
        ("11111111", PMID(11111111)),
        # With lowercase prefix
        ("pmid:1234567", PMID(1234567)),
        ("pmid:12345678", PMID(12345678)),
        ("pmid:87654321", PMID(87654321)),
        # With uppercase prefix
        ("PMID:1234567", PMID(1234567)),
        ("PMID:12345678", PMID(12345678)),
        ("PMID:87654321", PMID(87654321)),
        # With space after colon (parser is lenient)
        ("pmid: 123456", PMID(123456)),
        ("PMID: 123456", PMID(123456)),
        # With various whitespace (parser strips leading/trailing whitespace)
        ("  1234567  ", PMID(1234567)),
        ("1234567\t", PMID(1234567)),
        # URL format
        ("https://pubmed.ncbi.nlm.nih.gov/1234567", PMID(1234567)),
        ("https://pubmed.ncbi.nlm.nih.gov/12345678", PMID(12345678)),
        ("https://pubmed.ncbi.nlm.nih.gov/87654321", PMID(87654321))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(PMID, input_str) == tryparse(PMID, input_str) == expected
            @test parse(PMID, shortcode(expected)) == expected
            @test parse(PMID, string(expected)) == expected
            @test parse(PMID, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("1", "PMID:1", "https://pubmed.ncbi.nlm.nih.gov/1"),
            ("123", "PMID:123", "https://pubmed.ncbi.nlm.nih.gov/123"),
            ("1234567", "PMID:1234567", "https://pubmed.ncbi.nlm.nih.gov/1234567"),
            ("12345678", "PMID:12345678", "https://pubmed.ncbi.nlm.nih.gov/12345678"),
            ("87654321", "PMID:87654321", "https://pubmed.ncbi.nlm.nih.gov/87654321"),
            ("11111111", "PMID:11111111", "https://pubmed.ncbi.nlm.nih.gov/11111111")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(PMID, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Too many digits (more than 8)
            "123456789",         # 9 digits
            "1234567890",        # 10 digits
            "12345678901",       # 11 digits
            # Invalid content
            "",                  # Empty string
            "   ",               # Whitespace only
            "invalid",           # Completely invalid
            "123456a",           # Non-numeric characters
            "abc123",            # Non-numeric prefix
            "123.456",           # Decimal point
            "123,456",           # Comma
            "123-456",           # Hyphen
            "123_456",           # Underscore
            "123 456",           # Space in middle
            # Too long
            "12345678901234567890123456789012", # Way too long
            # Invalid prefixes
            "pmid123456",        # Missing colon
            "pmid :123456",      # Space before colon
            # URL format errors
            "https://pubmed.ncbi.nlm.nih.gov/",  # Missing number
            "https://pubmed.ncbi.nlm.nih.gov/abc", # Non-numeric in URL
            "https://www.ncbi.nlm.nih.gov/pubmed/1234567", # This URL pattern not supported
            # Unicode edge cases
            "1234567🔢",         # emoji
            "１234567",          # full-width 1
            "12３4567",          # full-width 3
            "１２３４５６７"       # all full-width
        ]

        for bad_pmid in malformed_examples
            @test_throws MalformedIdentifier{PMID} parse(PMID, bad_pmid)
            @test tryparse(PMID, bad_pmid) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        pmid_int = PMID(12345678)
        pmid_str = parse(PMID, "12345678")
        @test shortcode(pmid_int) == shortcode(pmid_str)
        @test string(pmid_int) == "PMID:12345678"

        # Test various sizes
        pmid_small = PMID(1)
        pmid_large = PMID(12345678)
        @test shortcode(pmid_small) == "1"
        @test shortcode(pmid_large) == "12345678"
        @test string(pmid_small) == "PMID:1"
        @test string(pmid_large) == "PMID:12345678"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{PMID} PMID(-1) # Negative not allowed

        # Test equality and hashing
        pmid1 = parse(PMID, "12345678")
        pmid2 = PMID(12345678)
        pmid3 = parse(PMID, "87654321")

        @test pmid1 == pmid2
        @test pmid1 != pmid3
        @test hash(pmid1) == hash(pmid2)
        @test hash(pmid1) != hash(pmid3)
    end
end

@testset "PMCID" begin
    valid_examples = [
        # Basic PMCID format (various lengths)
        ("PMC1", PMCID(1)),
        ("PMC123", PMCID(123)),
        ("PMC123456", PMCID(123456)),
        ("PMC1234567", PMCID(1234567)),
        ("PMC87654321", PMCID(87654321)),
        ("PMC12345678", PMCID(12345678)),
        ("PMC11111111", PMCID(11111111)),
        # With lowercase prefix
        ("pmcid:PMC123456", PMCID(123456)),
        ("pmcid:PMC1234567", PMCID(1234567)),
        ("pmcid:PMC87654321", PMCID(87654321)),
        # With uppercase prefix
        ("PMCID:PMC123456", PMCID(123456)),
        ("PMCID:PMC1234567", PMCID(1234567)),
        ("PMCID:PMC87654321", PMCID(87654321)),
        # Special valid cases
        ("PMC0", PMCID(0)),              # Zero is valid
        ("PMC00123", PMCID(123)),        # Leading zeros are stripped
        ("PMC123　", PMCID(123)),        # Trailing full-width space stripped
        # Cases without PMC prefix (parser is lenient)
        ("123456", PMCID(123456)),       # Missing PMC prefix - parser adds it
        # PMCID prefix cases
        ("pmcid:123456", PMCID(123456)), # PMCID prefix without PMC
        # URL format
        ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC123456", PMCID(123456)),
        ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1234567", PMCID(1234567)),
        ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC87654321", PMCID(87654321)),
        ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12345678", PMCID(12345678))
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(PMCID, input_str) == tryparse(PMCID, input_str) == expected
            @test parse(PMCID, shortcode(expected)) == expected
            @test parse(PMCID, string(expected)) == expected
            @test parse(PMCID, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("PMC1", "PMC1", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1"),
            ("PMC123", "PMC123", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC123"),
            ("PMC123456", "PMC123456", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC123456"),
            ("PMC1234567", "PMC1234567", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1234567"),
            ("PMC87654321", "PMC87654321", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC87654321"),
            ("PMC12345678", "PMC12345678", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12345678")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(PMCID, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Too many digits (more than 8)
            "PMC123456789",      # 9 digits
            "PMC1234567890",     # 10 digits
            "PMC12345678901",    # 11 digits
            # Missing or wrong prefix (but not "123456" - that's valid)
            "PCM123456",         # Wrong prefix
            "PNC123456",         # Wrong prefix
            "MC123456",          # Missing P
            "PC123456",          # Missing M
            "CMC123456",         # Wrong first letter
            # Missing number
            "PMC",               # Missing number
            "PMC ",              # Missing number with space
            # Invalid numbers (Note: PMC0 is actually valid, and leading zeros are stripped)
            "PMCabc123",         # Non-numeric
            "PMC123abc",         # Non-numeric suffix
            "PMC123.456",        # Decimal point
            "PMC123,456",        # Comma
            "PMC123-456",        # Hyphen
            "PMC123_456",        # Underscore
            "PMC123 456",        # Space
            # Empty or invalid
            "",                  # Empty string
            "   ",               # Whitespace only
            "invalid",           # Completely invalid
            # Too long (PMCIDs are typically up to 10 digits after PMC)
            "PMC12345678901234567890", # Way too long
            "PMC12345678901",    # Too long
            # Invalid prefixes (but not "pmcid:123456" - that's valid)
            "pmcid: PMC123456",  # Space after colon
            # URL format errors
            "https://www.ncbi.nlm.nih.gov/pmc/articles/", # Missing PMC
            "https://www.ncbi.nlm.nih.gov/pmc/articles/abc", # Non-PMC format
            "https://pmc.ncbi.nlm.nih.gov/articles/PMC123456", # This URL pattern not supported
            # Unicode edge cases
            "PMC123456🔢",       # emoji
            "ＰMC123456",        # full-width P
            "PMC１23456",        # full-width 1
            "PMC12３456",        # full-width 3
            "ＰＭＣ123456"        # all full-width PMC
        ]

        for bad_pmcid in malformed_examples
            @test_throws MalformedIdentifier{PMCID} parse(PMCID, bad_pmcid)
            @test tryparse(PMCID, bad_pmcid) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        pmcid_int = PMCID(123456)
        pmcid_str = parse(PMCID, "PMC123456")
        @test shortcode(pmcid_int) == shortcode(pmcid_str)
        @test string(pmcid_int) == "PMC123456"

        # Test various sizes
        pmcid_small = PMCID(1)
        pmcid_large = PMCID(12345678)  # 8 digits max
        @test shortcode(pmcid_small) == "PMC1"
        @test shortcode(pmcid_large) == "PMC12345678"
        @test string(pmcid_small) == "PMC1"
        @test string(pmcid_large) == "PMC12345678"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{PMCID} PMCID(-1) # Negative not allowed

        # Test equality and hashing
        pmcid1 = parse(PMCID, "PMC123456")
        pmcid2 = PMCID(123456)
        pmcid3 = parse(PMCID, "PMC87654321")

        @test pmcid1 == pmcid2
        @test pmcid1 != pmcid3
        @test hash(pmcid1) == hash(pmcid2)
        @test hash(pmcid1) != hash(pmcid3)
    end
end

@testset "VIAF" begin
    valid_examples = [
        # Basic VIAF format (various lengths)
        ("1", VIAF(1)),
        ("123", VIAF(123)),
        ("1234567", VIAF(1234567)),
        ("12345678", VIAF(12345678)),
        ("87654321", VIAF(87654321)),
        ("123456789", VIAF(123456789)),
        ("1234567890", VIAF(1234567890)),
        ("4000000000", VIAF(4000000000)),
        ("4294967295", VIAF(4294967295)),
        # With lowercase prefix
        ("viaf:1234567", VIAF(1234567)),
        ("viaf:12345678", VIAF(12345678)),
        ("viaf:87654321", VIAF(87654321)),
        # With uppercase prefix
        ("VIAF:1234567", VIAF(1234567)),
        ("VIAF:12345678", VIAF(12345678)),
        ("VIAF:87654321", VIAF(87654321)),
        # URL format
        ("https://viaf.org/viaf/1234567", VIAF(1234567)),
        ("https://viaf.org/viaf/12345678", VIAF(12345678)),
        ("https://viaf.org/viaf/87654321", VIAF(87654321)),
        ("https://viaf.org/viaf/1234567890", VIAF(1234567890)),
        # Edge cases that are valid
        ("0", VIAF(0)),                  # Zero is valid for VIAF
        ("00123", VIAF(123))             # Leading zeros are stripped
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(VIAF, input_str) == tryparse(VIAF, input_str) == expected
            @test parse(VIAF, shortcode(expected)) == expected
            @test parse(VIAF, string(expected)) == expected
            @test parse(VIAF, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("1", "https://viaf.org/viaf/1", "https://viaf.org/viaf/1"),
            ("123", "https://viaf.org/viaf/123", "https://viaf.org/viaf/123"),
            ("1234567", "https://viaf.org/viaf/1234567", "https://viaf.org/viaf/1234567"),
            ("12345678", "https://viaf.org/viaf/12345678", "https://viaf.org/viaf/12345678"),
            ("87654321", "https://viaf.org/viaf/87654321", "https://viaf.org/viaf/87654321"),
            ("123456789", "https://viaf.org/viaf/123456789", "https://viaf.org/viaf/123456789"),
            ("4000000000", "https://viaf.org/viaf/4000000000", "https://viaf.org/viaf/4000000000")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(VIAF, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # UInt32 overflow (too large for VIAF)
            "12345678901",       # Exceeds UInt32 max
            "123456789012",      # Exceeds UInt32 max
            "4294967296",        # UInt32 max + 1
            # Invalid content
            "",                  # Empty string
            "   ",               # Whitespace only
            "invalid",           # Completely invalid
            "123456a",               # Non-numeric characters
            "abc123",                # Non-numeric prefix
            "123.456",               # Decimal point
            "123,456",               # Comma
            "123-456",               # Hyphen
            "123_456",               # Underscore
            "123 456",               # Space
            # Too long (VIAFs are typically up to 15 digits)
            "12345678901234567890123456789012", # Way too long
            "1234567890123456",      # Too long
            # Invalid prefixes
            "viaf123456",            # Missing colon
            "viaf :123456",          # Space after colon
            "viaf: 123456",          # Space after colon
            # URL format errors
            "https://viaf.org/viaf/",  # Missing number
            "https://viaf.org/viaf/abc", # Non-numeric in URL
            # Unicode edge cases
            "1234567🔢",             # emoji
            "１234567",              # full-width 1
            "1234567　",             # full-width space
            "12３4567",              # full-width 3
            "１２３４５６７"           # all full-width
        ]

        for bad_viaf in malformed_examples
            @test_throws MalformedIdentifier{VIAF} parse(VIAF, bad_viaf)
            @test tryparse(VIAF, bad_viaf) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        viaf_int = VIAF(12345678)
        viaf_str = parse(VIAF, "12345678")
        @test shortcode(viaf_int) == shortcode(viaf_str)
        @test string(viaf_int) == "https://viaf.org/viaf/12345678"

        # Test various sizes
        viaf_small = VIAF(1)
        viaf_large = VIAF(4294967295)  # UInt32 max
        @test shortcode(viaf_small) == "1"
        @test shortcode(viaf_large) == "4294967295"
        @test string(viaf_small) == "https://viaf.org/viaf/1"
        @test string(viaf_large) == "https://viaf.org/viaf/4294967295"

        # Test invalid integer inputs (UInt32 overflow, not MalformedIdentifier)
        @test_throws InexactError VIAF(4294967296)  # UInt32 max + 1
        @test_throws InexactError VIAF(-1) # Negative not allowed

        # Test equality and hashing
        viaf1 = parse(VIAF, "12345678")
        viaf2 = VIAF(12345678)
        viaf3 = parse(VIAF, "87654321")

        @test viaf1 == viaf2
        @test viaf1 != viaf3
        @test hash(viaf1) == hash(viaf2)
        @test hash(viaf1) != hash(viaf3)
    end
end

@testset "Wikidata" begin
    valid_examples = [
        # Basic Wikidata format (various lengths)
        ("Q1", Wikidata(1)),
        ("Q42", Wikidata(42)),
        ("Q123", Wikidata(123)),
        ("Q123456", Wikidata(123456)),
        ("Q999999999", Wikidata(999999999)),
        ("Q1234567890", Wikidata(1234567890)),
        ("Q12345678901", Wikidata(12345678901)),
        # With lowercase prefix
        ("wikidata:Q42", Wikidata(42)),
        ("wikidata:Q123456", Wikidata(123456)),
        ("wikidata:Q999999999", Wikidata(999999999)),
        # With uppercase prefix
        ("Wikidata:Q42", Wikidata(42)),
        ("WIKIDATA:Q123456", Wikidata(123456)),
        ("WIKIDATA:Q999999999", Wikidata(999999999)),
        # URL format
        ("https://www.wikidata.org/wiki/Q42", Wikidata(42)),
        ("https://www.wikidata.org/wiki/Q123456", Wikidata(123456)),
        ("https://www.wikidata.org/wiki/Q999999999", Wikidata(999999999)),
        # Edge cases that are valid
        ("Q00123", Wikidata(123)),           # Leading zeros are stripped
        ("Q12345678901234567890", Wikidata(12345678901234567890)), # Large numbers within UInt64 range
        ("Q1234567890123", Wikidata(1234567890123))       # Also within UInt64 range
    ]

    @testset "Parse/tryparse equivalence and invariants" begin
        for (input_str, expected) in valid_examples
            @test parse(Wikidata, input_str) == tryparse(Wikidata, input_str) == expected
            @test parse(Wikidata, shortcode(expected)) == expected
            @test parse(Wikidata, string(expected)) == expected
            @test parse(Wikidata, purl(expected)) == expected
            @test eval(Meta.parse(repr(expected))) == expected
        end
    end

    @testset "Format consistency" begin
        format_examples = [
            # (shortstr, str, purlstr)
            ("Q1", "https://www.wikidata.org/wiki/Q1", "https://www.wikidata.org/wiki/Q1"),
            ("Q42", "https://www.wikidata.org/wiki/Q42", "https://www.wikidata.org/wiki/Q42"),
            ("Q123", "https://www.wikidata.org/wiki/Q123", "https://www.wikidata.org/wiki/Q123"),
            ("Q123456", "https://www.wikidata.org/wiki/Q123456", "https://www.wikidata.org/wiki/Q123456"),
            ("Q999999999", "https://www.wikidata.org/wiki/Q999999999", "https://www.wikidata.org/wiki/Q999999999"),
            ("Q1234567890", "https://www.wikidata.org/wiki/Q1234567890", "https://www.wikidata.org/wiki/Q1234567890")
        ]

        for (shortstr, str, purlstr) in format_examples
            value = parse(Wikidata, shortstr)
            @test shortcode(value) == shortstr
            @test string(value) == str
            @test purl(value) == purlstr
        end
    end

    @testset "Malformed identifiers" begin
        malformed_examples = [
            # Missing or wrong prefix
            "42",                # Missing Q prefix
            "123456",            # Missing Q prefix
            "P123",              # Wrong prefix (P is for properties)
            "L123",              # Wrong prefix (L is for lexemes)
            "M123",              # Wrong prefix (M is for media files)
            "R123",              # Wrong prefix
            "X123",              # Wrong prefix
            # Missing number
            "Q",                 # Missing number
            "Q ",                # Missing number with space
            # Invalid numbers (Note: Q00123 with leading zeros is actually valid)
            "Qabc",              # Non-numeric
            "Q123abc",           # Non-numeric suffix
            "Q123.456",          # Decimal point
            "Q123,456",          # Comma
            "Q123-456",          # Hyphen
            "Q123_456",          # Underscore
            "Q123 456",          # Space
            # Wrong case
            "q42",               # Lowercase q
            "q123456",           # Lowercase q
            # Empty or invalid
            "",                  # Empty string
            "   ",               # Whitespace only
            "invalid",           # Completely invalid
            # Too long (exceeds UInt64 max: 18446744073709551615)
            "Q18446744073709551616", # UInt64 max + 1
            "Q99999999999999999999", # Way beyond UInt64 max
            # Invalid prefixes
            "wikidata:42",       # Missing Q in prefix
            "wikidata: Q42",     # Space after colon
            # URL format errors
            "https://www.wikidata.org/wiki/", # Missing Q ID
            "https://www.wikidata.org/wiki/42", # Missing Q prefix in URL
            "https://wikidata.org/entity/Q42",  # URL parsing issue
            # Unicode edge cases
            "Q42🔢",             # emoji
            "Ｑ42",              # full-width Q
            "Q４2",              # full-width 4
            "Q４２",             # full-width 42
            "Q１23456",          # full-width 1
            "Ｑ１２３４５６"       # all full-width
        ]

        for bad_wd in malformed_examples
            @test_throws MalformedIdentifier{Wikidata} parse(Wikidata, bad_wd)
            @test tryparse(Wikidata, bad_wd) === nothing
        end
    end

    @testset "Miscellaneous" begin
        # Test integer constructor
        wd_int = Wikidata(42)
        wd_str = parse(Wikidata, "Q42")
        @test shortcode(wd_int) == shortcode(wd_str)
        @test string(wd_int) == "https://www.wikidata.org/wiki/Q42"

        # Test various sizes
        wd_small = Wikidata(1)
        wd_large = Wikidata(18446744073709551615)  # UInt64 max
        @test shortcode(wd_small) == "Q1"
        @test shortcode(wd_large) == "Q18446744073709551615"
        @test string(wd_small) == "https://www.wikidata.org/wiki/Q1"
        @test string(wd_large) == "https://www.wikidata.org/wiki/Q18446744073709551615"

        # Test code extraction
        wikidata_cases = [
            ("Q1", 1),
            ("Q42", 42),
            ("Q123456", 123456),
            ("Q999999999", 999999999)
        ]
        for (idstr, code) in wikidata_cases
            wikidata = parse(Wikidata, idstr)
            @test wikidata.id == code
        end

        # Test equality and hashing
        wd1 = parse(Wikidata, "Q42")
        wd2 = Wikidata(42)
        wd3 = parse(Wikidata, "Q123456")

        @test wd1 == wd2
        @test wd1 != wd3
        @test hash(wd1) == hash(wd2)
        @test hash(wd1) != hash(wd3)
    end
end
