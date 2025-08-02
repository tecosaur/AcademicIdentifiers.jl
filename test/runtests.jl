using AcademicIdentifiers
using AcademicIdentifiers: MalformedIdentifier, ChecksumViolation, idcode, idchecksum, shortcode, purl
using AcademicIdentifiers: EAN13

using JSON, JSON3

using Test

@testset "ArXiv" begin
    @testset "valid new scheme" begin
        valid_new_arxivs = [
            "0704.0001",      # 2007, basic format
            "1412.7878",      # 2014, 4-digit number
            "1501.00001",     # 2015, 5-digit number
            "2301.12345",     # 2023, modern format
            "9912.12345"      # 2099, future format
        ]
        for arxiv in valid_new_arxivs
            @test parse(ArXiv, arxiv) isa ArXiv
            @test shortcode(parse(ArXiv, arxiv)) == arxiv
        end
    end

    @testset "valid old scheme" begin
        valid_old_arxivs = [
            "hep-th/9901001",     # 1999, hep-th
            "astro-ph/0001001",   # 2000, astro-ph
            "math.CA/0611800",    # 2006, math with subclass
            "cs.AI/0501001",      # 2005, cs with subclass
            "q-bio.BM/0501001"    # 2005, q-bio with subclass
        ]
        for arxiv in valid_old_arxivs
            @test parse(ArXiv, arxiv) isa ArXiv
            @test shortcode(parse(ArXiv, arxiv)) == arxiv
        end
    end

    @testset "valid with versions" begin
        valid_versioned_arxivs = [
            ("0704.0001v1", "0704.0001v1"),
            ("hep-th/9901001v1", "hep-th/9901001v1"),
            ("math.CA/0611800v2", "math.CA/0611800v2"),
            ("9912.12345v2", "9912.12345v2")
        ]
        for (input, expected) in valid_versioned_arxivs
            @test parse(ArXiv, input) isa ArXiv
            @test shortcode(parse(ArXiv, input)) == expected
        end
    end

    @testset "Malformed" begin
        malformed_arxivs = [
            # New scheme malformed
            "0704",              # Missing paper number
            "070.0001",          # Wrong date format
            "0704.000a",         # Non-numeric paper number
            "13xx.12345",        # Non-numeric date

            # Old scheme malformed
            "astro-ph/001001",   # Wrong number length
            "bad-cat/0001001",   # Invalid category
            "astro-ph/000100a",  # Non-numeric number
            "astro-ph0001001",   # Missing slash

            # General malformed
            "",                  # Empty string
            "invalid"            # Completely invalid
        ]
        for bad_arxiv in malformed_arxivs
            @test_throws MalformedIdentifier{ArXiv} parse(ArXiv, bad_arxiv)
        end
    end

    @testset "Unicode edge cases" begin
        unicode_arxivs = [
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
        for bad_arxiv in unicode_arxivs
            @test_throws MalformedIdentifier{ArXiv} parse(ArXiv, bad_arxiv)
        end
    end

    @testset "Utils" begin
        # Test new scheme
        arxiv_new = parse(ArXiv, "2301.12345")
        @test shortcode(arxiv_new) == "2301.12345"
        @test purl(arxiv_new) == "https://arxiv.org/abs/2301.12345"
        @test sprint(print, arxiv_new) == "arXiv:2301.12345"

        # Test old scheme
        arxiv_old = parse(ArXiv, "hep-th/9901001")
        @test shortcode(arxiv_old) == "hep-th/9901001"
        @test purl(arxiv_old) == "https://arxiv.org/abs/hep-th/9901001"
        @test sprint(print, arxiv_old) == "arXiv:hep-th/9901001"

        # Test case sensitivity for prefixes
        @test shortcode(parse(ArXiv, "ArXiv:2301.12345")) == "2301.12345"
        @test shortcode(parse(ArXiv, "ARXIV:2301.12345")) == "2301.12345"
        @test shortcode(parse(ArXiv, "arxiv:2301.12345")) == "2301.12345"
        @test shortcode(parse(ArXiv, "ArXiv:hep-th/9901001")) == "hep-th/9901001"

        # Test URL parsing
        @test shortcode(parse(ArXiv, "https://arxiv.org/abs/2301.12345")) == "2301.12345"
        @test shortcode(parse(ArXiv, "https://arxiv.org/abs/hep-th/9901001")) == "hep-th/9901001"

        # Test categories with periods
        math_arxiv = parse(ArXiv, "math.AG/0501001")
        @test shortcode(math_arxiv) == "math.AG/0501001"
        cs_arxiv = parse(ArXiv, "cs.AI/0501001")
        @test shortcode(cs_arxiv) == "cs.AI/0501001"

        # Test tryparse
        @test tryparse(ArXiv, "2301.12345") isa ArXiv
        @test tryparse(ArXiv, "hep-th/9901001") isa ArXiv
        @test tryparse(ArXiv, "invalid") === nothing
        @test tryparse(ArXiv, "") === nothing

        # Test eval(show(x)) == x
        arxiv_examples = [
            parse(ArXiv, "2301.12345"),
            parse(ArXiv, "1412.7878v2"),
            parse(ArXiv, "hep-th/9901001"),
            parse(ArXiv, "math.CA/0611800v1")
        ]
        for arxiv in arxiv_examples
            @test eval(Meta.parse(repr(arxiv))) == arxiv
        end

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

        # Test three-arg show (MIME"text/plain") with reference strings
        arxiv_simple = parse(ArXiv, "2301.12345")
        show_output = sprint(show, MIME("text/plain"), arxiv_simple)
        @test show_output == "ArXiv:2301.12345"
        vec_show_output = sprint(show, MIME("text/plain"), arxiv_simple, context = :typeinfo => ArXiv)
        @test vec_show_output == "2301.12345"
    end
end

@testset "DOI" begin
    @testset "Valid" begin
        valid_dois = [
            "10.1000/182",
            "10.1038/nature12373",
            "10.1016/j.cell.2020.01.001",
            "10.1093/nar/gkaa1100"
        ]
        for doi in valid_dois
            @test shortcode(parse(DOI, doi)) == doi
        end

        # Test prefix parsing
        @test shortcode(parse(DOI, "doi:10.1000/182")) == "10.1000/182"
        @test shortcode(parse(DOI, "DOI:10.1038/nature12373")) == "10.1038/nature12373"
    end

    @testset "Unicode edge cases" begin
        # DOI is quite permissive and accepts Unicode characters
        # Test cases that actually do cause issues
        unicode_dois = [
            "10.1000/182🔢",    # emoji at end might cause issues
        ]
        for bad_doi in unicode_dois
            # DOI constructor is very permissive, so we test that it at least doesn't crash
            @test parse(DOI, bad_doi) isa DOI
        end

        # Test cases that are accepted (DOI is permissive with Unicode)
        accepted_unicode_dois = [
            "10．1000/182",     # full-width period
            "10.1000／182",     # full-width slash
            "１0.1000/182",      # full-width 1
            "10.１000/182",     # full-width 1
            "１０.1000/182",    # full-width 10
        ]
        for doi_input in accepted_unicode_dois
            @test_throws MalformedIdentifier{DOI} parse(DOI, doi_input)
        end
    end

    @testset "Utils" begin
        doi = parse(DOI, "10.1000/182")

        # Test shortcode
        @test shortcode(doi) == "10.1000/182"

        # Test purl
        @test purl(doi) == "https://doi.org/10.1000/182"

        # Test output formatting
        @test sprint(print, doi) == "doi:10.1000/182"

        # Test tryparse
        @test tryparse(DOI, "10.1000/182") isa DOI
        @test tryparse(DOI, "doi:10.1038/nature12373") isa DOI
        @test tryparse(DOI, "invalid") === nothing
        @test tryparse(DOI, "") === nothing

        # Test eval(show(x)) == x
        doi_examples = [
            parse(DOI, "10.1000/182"),
            parse(DOI, "10.1038/nature12373"),
            parse(DOI, "doi:10.1016/j.cell.2020.01.001")
        ]
        for doi in doi_examples
            @test eval(Meta.parse(repr(doi))) == doi
        end

        # Test hash and equality with case-folding
        doi1 = parse(DOI, "10.1000/ABC")
        doi2 = parse(DOI, "10.1000/abc")
        doi3 = parse(DOI, "10.1000/AbC")
        doi4 = parse(DOI, "10.1000/xyz")

        @test doi1 == doi2
        @test doi1 == doi3
        @test doi2 == doi3
        @test doi1 != doi4

        @test hash(doi1) == hash(doi2)
        @test hash(doi1) == hash(doi3)
        @test hash(doi2) == hash(doi3)
        @test hash(doi1) != hash(doi4)
    end
end

@testset "EAN13" begin
    @testset "Valid" begin
        valid_ean13s = [
            "9780439023481",
            "9798886451740",
            "9781402894626"
        ]
        for ean in valid_ean13s
            @test shortcode(parse(EAN13, ean)) == ean
        end
    end

    @testset "Malformed" begin
        malformed_ean13s = [
            "97804390234812",  # Too many digits
            "978043902348a",   # Non-numeric
            "97804390234",     # Too few digits
            "",
            "97804390234"      # Too few digits
        ]
        for bad_ean in malformed_ean13s
            @test_throws Union{MalformedIdentifier{EAN13},ChecksumViolation{EAN13}} parse(EAN13, bad_ean)
        end
    end

    @testset "Checksum" begin
        valid_ean13s = [
            "9780439023481",
            "9798886451740",
            "9781402894626"
        ]

        wrong_ean13s = String[]
        for ean in valid_ean13s
            ean_stem = ean[1:end-1]
            correct_check = ean[end]
            for check_digit in '0':'9'
                if check_digit != correct_check
                    push!(wrong_ean13s, ean_stem * check_digit)
                end
            end
        end
        for ean in wrong_ean13s
            @test_throws ChecksumViolation{EAN13} parse(EAN13, ean)
        end
    end

    @testset "Integer constructor" begin
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
    end

    @testset "Unicode edge cases" begin
        unicode_ean13s = [
            "9780439023481🔢", # emoji
        ]
        for bad_ean in unicode_ean13s
            @test_throws MalformedIdentifier{EAN13} parse(EAN13, bad_ean)
        end

        # Test cases with full-width Unicode digits
        unicode_ean13s_fullwidth = [
            "９780439023481",   # full-width 9
            "９７80439023481",  # full-width 97
            "978043902348１",   # full-width 1
            "９７８0439023481",  # full-width 978
        ]
        for bad_ean in unicode_ean13s_fullwidth
            @test_throws MalformedIdentifier{EAN13} parse(EAN13, bad_ean)
        end
    end

    @testset "Utils" begin
        ean13_cases = [
            ("9780439023481", 978043902348, 1),
            ("9798886451740", 979888645174, 0),
            ("9781402894626", 978140289462, 6)
        ]
        for (idstr, code, csum) in ean13_cases
            ean13 = parse(EAN13, idstr)
            @test idcode(ean13) == code
            @test idchecksum(ean13) == csum
            @test shortcode(ean13) == idstr
            @test purl(ean13) === nothing  # EAN13 doesn't have a standard purl
            @test sprint(print, ean13) == idstr
        end

        # Test tryparse
        @test tryparse(EAN13, "9780439023481") isa EAN13
        @test tryparse(EAN13, "invalid") === nothing
        @test tryparse(EAN13, "") === nothing

        # Test eval(show(x)) == x
        ean13_examples = [
            parse(EAN13, "9780439023481"),
            parse(EAN13, "9798886451740"),
            parse(EAN13, "9781402894626")
        ]
        for ean13 in ean13_examples
            @test eval(Meta.parse(repr(ean13))) == ean13
        end
    end
end

@testset "ISNI" begin
    @testset "Valid" begin
        valid_isnis = [
            "0000 0001 2281 955X",
            "0000 0000 8389 1195",
            "0000 0004 0600 5291"
        ]
        for isni in valid_isnis
            @test parse(ISNI, isni) isa ISNI
            @test shortcode(parse(ISNI, isni)) == isni
        end
    end

    @testset "Malformed" begin
        malformed_isnis = [
            "",                    # Empty string
            "abcd efgh ijkl mnop", # Non-numeric
            "0000 0001 2281 95",  # Too short
            "0000 0001 2281 955XX", # Too long
            "0000 0001 2281 955Y"   # Invalid check digit
        ]
        for bad_isni in malformed_isnis
            @test_throws Union{MalformedIdentifier{ISNI},ChecksumViolation{ISNI}} parse(ISNI, bad_isni)
        end
    end

    @testset "Checksum" begin
        valid_isnis = [
            "0000 0001 2281 955X",
            "0000 0000 8389 1195",
            "0000 0004 0600 5291"
        ]

        wrong_isnis = String[]
        for isni in valid_isnis
            isni_stem = isni[1:end-1]
            correct_check = isni[end]
            for check_char in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X']
                if check_char != correct_check
                    push!(wrong_isnis, isni_stem * check_char)
                end
            end
        end
        for isni in wrong_isnis
            @test_throws ChecksumViolation{ISNI} parse(ISNI, isni)
        end
    end

    @testset "Integer constructor" begin
        # Test integer constructor with correct checksum
        isni_int = ISNI(12281955, 10)  # X = 10
        isni_str = parse(ISNI, "0000 0001 2281 955X")
        @test AcademicIdentifiers.idcode(isni_int) == AcademicIdentifiers.idcode(isni_str)
        @test AcademicIdentifiers.idchecksum(isni_int) == AcademicIdentifiers.idchecksum(isni_str)
        @test shortcode(isni_int) == shortcode(isni_str)

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ISNI} ISNI(12345678901234567, 1)  # Too many digits
        @test_throws ChecksumViolation{ISNI} ISNI(12281955, 2)  # Wrong checksum
    end

    @testset "Unicode edge cases" begin
        unicode_isnis = [
            "0000 0001 2281 955X🔢", # emoji
            "０000 0001 2281 955X",   # full-width zero
            "0000 ０001 2281 955X",   # full-width zero
        ]
        for bad_isni in unicode_isnis
            @test_throws MalformedIdentifier{ISNI} parse(ISNI, bad_isni)
        end
    end

    @testset "Utils" begin
        isni_cases = [
            ("0000 0001 2281 955X", 12281955, 10),
            ("0000 0000 8389 1195", 8389119, 5),
            ("0000 0004 0600 5291", 40600529, 1)
        ]
        for (idstr, code, csum) in isni_cases
            isni = parse(ISNI, idstr)
            @test AcademicIdentifiers.idcode(isni) == code
            @test AcademicIdentifiers.idchecksum(isni) == csum
            @test shortcode(isni) == idstr
            expected_purl = "https://isni.org/isni/" * replace(idstr, " " => "")
            @test purl(isni) == expected_purl
            @test sprint(print, isni) == expected_purl
        end

        # Test prefix parsing
        @test shortcode(parse(ISNI, "isni:0000 0001 2281 955X")) == "0000 0001 2281 955X"
        @test shortcode(parse(ISNI, "https://isni.org/isni/000000012281955X")) == "0000 0001 2281 955X"

        # Test tryparse
        @test tryparse(ISNI, "0000 0001 2281 955X") isa ISNI
        @test tryparse(ISNI, "invalid") === nothing
        @test tryparse(ISNI, "") === nothing

        # Test eval(show(x)) == x
        isni_examples = [
            parse(ISNI, "0000 0001 2281 955X"),
            parse(ISNI, "0000 0000 8389 1195"),
            parse(ISNI, "0000 0004 0600 5291")
        ]
        for isni in isni_examples
            @test eval(Meta.parse(repr(isni))) == isni
        end
    end
end

@testset "ISSN" begin
    @testset "Valid" begin
        valid_issns = [
            "0317-8471",
            "1050-124X",
            "2049-3630"
        ]
        for issn in valid_issns
            @test shortcode(parse(ISSN, issn)) == issn
        end
    end

    @testset "Malformed" begin
        malformed_issns = [
            "0317-84712",  # Too many digits
            "abcd-efgh"   # Non-numeric
        ]
        for bad_issn in malformed_issns
            @test_throws MalformedIdentifier{ISSN} parse(ISSN, bad_issn)
        end

        # These throw different types of exceptions
        @test_throws MalformedIdentifier{ISSN} parse(ISSN, "")  # Empty string
        @test_throws ChecksumViolation{ISSN} parse(ISSN, "031-8471")    # Too few digits
        @test_throws ChecksumViolation{ISSN} parse(ISSN, "0317-847")     # Missing check digit
    end

    @testset "Checksum" begin
        valid_issns = [
            "0317-8471",
            "1050-124X",
            "2049-3630"
        ]

        wrong_issns = String[]
        for issn in valid_issns
            issn_stem = issn[1:end-1]
            correct_check = issn[end]
            for check_char in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X']
                if check_char != correct_check
                    push!(wrong_issns, issn_stem * check_char)
                end
            end
        end
        for issn in wrong_issns
            @test_throws ChecksumViolation{ISSN} parse(ISSN, issn)
        end
    end

    @testset "Integer constructor" begin
        # Test integer constructor with correct checksum
        issn_int = ISSN(317847, 1)
        issn_str = parse(ISSN, "0317-8471")
        @test idcode(issn_int) == idcode(issn_str)
        @test idchecksum(issn_int) == idchecksum(issn_str)
        @test shortcode(issn_int) == shortcode(issn_str)

        # Test another integer constructor with X checksum
        issn_int_x = ISSN(1050124, 10)
        issn_str_x = parse(ISSN, "1050-124X")
        @test shortcode(issn_int_x) == shortcode(issn_str_x)

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ISSN} ISSN(12345678, 1)  # Too many digits
        @test_throws ChecksumViolation{ISSN} ISSN(317847, 2)  # Wrong checksum
    end

    @testset "Unicode edge cases" begin
        unicode_issns = [
            "0317‑8471",     # figure dash
            "0317−8471",     # minus sign
            "0317-8471🔢",   # emoji
        ]
        for bad_issn in unicode_issns
            @test_throws MalformedIdentifier{ISSN} parse(ISSN, bad_issn)
        end

        # Test cases with full-width Unicode digits
        unicode_issns_fullwidth = [
            "０317-8471",     # full-width zero
            "０３17-8471",    # full-width 03
            "0317-８471",    # full-width 8
        ]
        for bad_issn in unicode_issns_fullwidth
            @test_throws MalformedIdentifier{ISSN} parse(ISSN, bad_issn)
        end
    end

    @testset "Utils" begin
        issn_cases = [
            ("0317-8471", 317847, 1),
            ("1050-124X", 1050124, 10)
        ]
        for (idstr, code, csum) in issn_cases
            issn = parse(ISSN, idstr)
            @test idcode(issn) == code
            @test idchecksum(issn) == csum
            @test shortcode(issn) == idstr
            @test purl(issn) == "https://portal.issn.org/resource/ISSN/$idstr"
            @test sprint(print, issn) == "ISSN $idstr"
        end

        # Test case sensitivity
        @test shortcode(parse(ISSN, "ISSN:0317-8471")) == "0317-8471"
        @test shortcode(parse(ISSN, "issn:0317-8471")) == "0317-8471"
        @test shortcode(parse(ISSN, "1050-124x")) == "1050-124X"

        # Test tryparse
        @test tryparse(ISSN, "0317-8471") isa ISSN
        @test tryparse(ISSN, "invalid") === nothing
        @test tryparse(ISSN, "") === nothing

        # Test eval(show(x)) == x
        issn_examples = [
            parse(ISSN, "0317-8471"),
            parse(ISSN, "1050-124X")
        ]
        for issn in issn_examples
            @test eval(Meta.parse(repr(issn))) == issn
        end
    end
end

@testset "ISBN" begin
    @testset "Valid" begin
        valid_isbns = [
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
        for isbn in valid_isbns
            @test shortcode(parse(ISBN, isbn)) == "ISBN " * isbn
        end

        # Test prefix parsing
        @test string(parse(ISBN, "isbn:978-0-439-02348-1")) == "ISBN 978-0-439-02348-1"
        @test string(parse(ISBN, "ISBN:0-684-84328-5")) == "ISBN 0-684-84328-5"
    end

    @testset "Malformed" begin
        malformed_isbns = [
            "fghdf hdfjhfgj"
            "085131ffff"
            "9971☃02100"
            "X604250590"
            "960X250590"
            "96042X0590"
            ""
            "12345"
            "123456789"
            "12345678901"
            "123456789012"
            "12345678901234"
        ]
        for bad_isbn in malformed_isbns
            @test_throws MalformedIdentifier{ISBN} parse(ISBN, bad_isbn)
        end
    end

    @testset "Checksum" begin
        valid_isbns = [
            "99921-58-10-7", "9971-5-0210-0", "960-425-059-0", "80-902734-1-6",
            "85-359-0277-5", "1-84356-028-3", "0-684-84328-5", "0-8044-2957-X",
            "0-85131-041-9", "0-439-02348-3", "978-0-439-02348-1", "979-8-88645-174-0",
            "978-1-4028-9462-6", "978-1-56619-909-4", "978-0-321-53496-5", "978-3-16-148410-0"
        ]

        wrong_isbns = String[]
        for isbn in valid_isbns
            istem = isbn[1:end-1]
            plaincode = replace(isbn, '-' => "")
            if length(plaincode) == 13
                correct_check = isbn[end]
                for cdigit in '0':'9'
                    if cdigit != correct_check
                        push!(wrong_isbns, istem * cdigit)
                    end
                end
            else
                for cdigit in '0':'9'
                    if endswith(isbn, cdigit)
                        push!(wrong_isbns, istem * 'X')
                    else
                        push!(wrong_isbns, istem * cdigit)
                    end
                end
            end
        end

        for isbn in wrong_isbns
            if length(replace(isbn, '-' => "")) == 13
                @test_throws ChecksumViolation{EAN13} parse(ISBN, isbn)
            else
                @test_throws ChecksumViolation{ISBN} parse(ISBN, isbn)
            end
        end
    end

    @testset "Integer constructor" begin
        # Test 13-digit integer constructor
        isbn_int = ISBN(9780439023481)
        isbn_str = parse(ISBN, "978-0-439-02348-1")
        @test shortcode(isbn_int) == shortcode(isbn_str)
        @test string(isbn_int) == "ISBN 978-0-439-02348-1"

        # Test another valid 13-digit ISBN
        isbn_int2 = ISBN(9781402894626)
        isbn_str2 = parse(ISBN, "978-1-4028-9462-6")
        @test shortcode(isbn_int2) == shortcode(isbn_str2)

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ISBN} ISBN(123456789012)  # 12 digits
        @test_throws MalformedIdentifier{ISBN} ISBN(12345678901234)  # 14 digits
        @test_throws MalformedIdentifier{ISBN} ISBN(9771234567890)  # doesn't start with 978/979
    end

    @testset "Unicode edge cases" begin
        unicode_isbns = [
            "978-0-439-02348-🔢",  # emoji digit
            "978-0-439-02348-①",   # circled digit
            "978‑0‑439‑02348‑1",    # figure dash instead of hyphen
            "978−0−439−02348−1",    # minus sign instead of hyphen
        ]
        for bad_isbn in unicode_isbns
            @test_throws MalformedIdentifier{ISBN} parse(ISBN, bad_isbn)
        end

        # Test cases with full-width Unicode digits
        unicode_isbns_fullwidth = [
            "978-０-439-02348-1",   # full-width zero
            "９78-0-439-02348-1",   # full-width 9
            "978-0-４39-02348-1",   # full-width 4
            "978-0-439-０2348-1",   # full-width 0
        ]
        for bad_isbn in unicode_isbns_fullwidth
            @test_throws MalformedIdentifier{ISBN} parse(ISBN, bad_isbn)
        end
    end

    @testset "Utils" begin
        isbn = parse(ISBN, "978-0-439-02348-1")

        # Test shortcode
        @test shortcode(isbn) == "ISBN 978-0-439-02348-1"

        # Test purl (ISBN doesn't have a standard purl)
        @test purl(isbn) === nothing

        # Test output formatting
        @test sprint(print, isbn) == "ISBN 978-0-439-02348-1"
        @test string(isbn) == "ISBN 978-0-439-02348-1"

        # Test EAN13 conversion
        ean = convert(EAN13, isbn)
        @test idcode(ean) == 978043902348
        @test idchecksum(ean) == 1

        # Test tryparse
        @test tryparse(ISBN, "978-0-439-02348-1") isa ISBN
        @test tryparse(ISBN, "invalid") === nothing
        @test tryparse(ISBN, "") === nothing

        # Test eval(show(x)) == x
        isbn_examples = [
            parse(ISBN, "978-0-439-02348-1"),
            parse(ISBN, "0-684-84328-5"),
            parse(ISBN, "ISBN 978-1-4028-9462-6")
        ]
        for isbn_ex in isbn_examples
            @test eval(Meta.parse(repr(isbn_ex))) == isbn_ex
        end
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
    end
end

@testset "OCN" begin
    @testset "Valid" begin
        valid_ocns = [
            "1234567",
            "12345678",
            "123456789",
            "1234567890",
            "ocn1234567",
            "oclc:12345678",
            "ocm1234567",
            "on123456789",
            "(ocolc)12345678"
        ]
        for ocn in valid_ocns
            @test parse(OCN, ocn) isa OCN
        end
    end

    @testset "Malformed" begin
        malformed_ocns = [
            "",              # Empty string
            "abc123",        # Non-numeric characters
            "123abc",        # Mixed alphanumeric
            "12.345",        # Decimal point
            "12 345",        # Space in number
            "ocn",           # Prefix without number
            "oclc:",         # Prefix without number
            "123-456"        # Hyphen
        ]
        for bad_ocn in malformed_ocns
            @test_throws MalformedIdentifier{OCN} parse(OCN, bad_ocn)
        end
    end

    @testset "Unicode edge cases" begin
        unicode_ocns = [
            "123456🔢",      # emoji
            "１234567",      # full-width 1
            "1２34567",      # full-width 2
            "12３4567",      # full-width 3
        ]
        for bad_ocn in unicode_ocns
            @test_throws MalformedIdentifier{OCN} parse(OCN, bad_ocn)
        end
    end

    @testset "Utils" begin
        ocn_cases = [
            ("1234567", 1234567, "ocm1234567"),      # < 10^8
            ("12345678", 12345678, "ocm12345678"),   # < 10^8
            ("123456789", 123456789, "ocn123456789"), # < 10^9
            ("1234567890", 1234567890, "on1234567890") # >= 10^9
        ]
        for (input, code, formatted) in ocn_cases
            ocn = parse(OCN, input)
            @test idcode(ocn) == code
            @test shortcode(ocn) == input
            @test purl(ocn) == "https://worldcat.org/oclc/$input"
            @test sprint(print, ocn) == formatted
        end

        # Test prefix parsing
        @test shortcode(parse(OCN, "ocn:1234567")) == "1234567"
        @test shortcode(parse(OCN, "oclc:1234567")) == "1234567"
        @test shortcode(parse(OCN, "https://worldcat.org/oclc/1234567")) == "1234567"

        # Test tryparse
        @test tryparse(OCN, "1234567") isa OCN
        @test tryparse(OCN, "invalid") === nothing
        @test tryparse(OCN, "") === nothing

        # Test eval(show(x)) == x
        ocn_examples = [
            parse(OCN, "1234567"),
            parse(OCN, "12345678"),
            parse(OCN, "123456789")
        ]
        for ocn in ocn_examples
            @test eval(Meta.parse(repr(ocn))) == ocn
        end
    end
end

@testset "ORCID" begin
    @testset "Valid" begin
        valid_orcids = [
            "0000-0002-1825-0097",
            "0000-0003-1419-2405",
            "0000-0001-5109-3700"
        ]
        for orcid in valid_orcids
            @test shortcode(parse(ORCID, orcid)) == orcid
        end
    end

    @testset "Malformed" begin
        malformed_orcids = [
            "0000-0000-0000-00000",
            "000X-0000-0000-0000",
            "abcd-0000-0000-0000",
            ""
        ]
        for bad_orcid in malformed_orcids
            @test_throws Union{MalformedIdentifier{ORCID},ArgumentError} parse(ORCID, bad_orcid)
        end
    end

    @testset "Checksum" begin
        valid_orcids = [
            "0000-0002-1825-0097",
            "0000-0003-1419-2405",
            "0000-0001-5109-3700"
        ]

        wrong_orcids = String[]
        for orcid in valid_orcids
            orcid_stem = orcid[1:end-1]
            correct_check = orcid[end]
            for check_char in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X']
                if check_char != correct_check
                    push!(wrong_orcids, orcid_stem * check_char)
                end
            end
        end
        for orcid in wrong_orcids
            @test_throws ChecksumViolation{ORCID} parse(ORCID, orcid)
        end
    end

    @testset "Integer constructor" begin
        # Test integer constructor with correct checksum
        orcid_int = ORCID(218250097, 10)  # Fixed: correct ID and checksum X (10)
        orcid_str = parse(ORCID, "0000-0021-8250-097X")
        @test idcode(orcid_int) == idcode(orcid_str)
        @test idchecksum(orcid_int) == idchecksum(orcid_str)

        # Test another integer constructor
        orcid_int2 = ORCID(18250097, 0)
        @test shortcode(orcid_int2) == "0000-0001-8250-0970"

        # Test X checksum with integer constructor
        orcid_with_x = ORCID(218250097, 10)
        @test idchecksum(orcid_with_x) == 10
        @test endswith(shortcode(orcid_with_x), "X")

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ORCID} ORCID(12345678901234567, 5)  # Too many digits
        @test_throws ChecksumViolation{ORCID} ORCID(2182500971, 7)  # Wrong checksum
    end

    @testset "Unicode edge cases" begin
        unicode_orcids = [
            "0000‑0002‑1825‑0097",   # figure dash
            "0000−0002−1825−0097",   # minus sign
            "0000-0002-1825-0097🔢", # emoji
        ]
        for bad_orcid in unicode_orcids
            @test_throws MalformedIdentifier{ORCID} parse(ORCID, bad_orcid)
        end

        # Test cases with full-width Unicode digits
        unicode_orcids_fullwidth = [
            "０000-0002-1825-0097",  # full-width zero
            "0000-０002-1825-0097",  # full-width zero
            "0000-0002-1825-００97",  # full-width zeros
        ]
        for bad_orcid in unicode_orcids_fullwidth
            @test_throws MalformedIdentifier{ORCID} parse(ORCID, bad_orcid)
        end
    end

    @testset "Utils" begin
        orcid_cases = [
            ("0000-0002-1825-0097", 21825009, 7),
            ("0000-0002-1694-233X", 21694233, 10)
        ]
        for (idstr, code, csum) in orcid_cases
            orcid = parse(ORCID, idstr)
            @test idcode(orcid) == code
            @test idchecksum(orcid) == csum
            @test shortcode(orcid) == idstr
            @test purl(orcid) == "https://orcid.org/$idstr"
            @test sprint(print, orcid) == "ORCID:$idstr"
        end

        # Test case sensitivity
        @test shortcode(parse(ORCID, "ORCID:0000-0002-1825-0097")) == "0000-0002-1825-0097"
        @test shortcode(parse(ORCID, "orcid:0000-0002-1825-0097")) == "0000-0002-1825-0097"
        @test shortcode(parse(ORCID, "0000-0002-1694-233x")) == "0000-0002-1694-233X"

        # Test tryparse
        @test tryparse(ORCID, "0000-0002-1825-0097") isa ORCID
        @test tryparse(ORCID, "invalid") === nothing
        @test tryparse(ORCID, "") === nothing

        # Test eval(show(x)) == x
        orcid_examples = [
            parse(ORCID, "0000-0002-1825-0097"),
            parse(ORCID, "0000-0002-1694-233X"),
            parse(ORCID, "0000-0003-1419-2405")
        ]
        for orcid in orcid_examples
            @test eval(Meta.parse(repr(orcid))) == orcid
        end
    end
end

@testset "OpenAlexID" begin
    @testset "Valid" begin
        valid_openalex_ids = [
            ("W2741809807", :W),    # Work
            ("A2208157607", :A),    # Author
            ("S2741809807", :S),    # Source
            ("I2741809807", :I),    # Institution
            ("C2741809807", :C),    # Concept
            ("P2741809807", :P),    # Publisher
            ("F2741809807", :F)     # Funder
        ]
        for (id, kind) in valid_openalex_ids
            @test parse(OpenAlexID{kind}, id) isa OpenAlexID{kind}
            @test shortcode(parse(OpenAlexID{kind}, id)) == id
        end

        # Test generic OpenAlexID parsing
        for (id, kind) in valid_openalex_ids
            @test parse(OpenAlexID, id) isa OpenAlexID{kind}
            @test shortcode(parse(OpenAlexID, id)) == id
        end
    end

    @testset "Malformed" begin
        malformed_openalex_ids = [
            "",              # Empty string
            "X2741809807",   # Invalid kind prefix
            "W",             # No number
            "W274180980a",   # Non-numeric
            "2741809807",    # Missing prefix
            "WW2741809807"   # Double prefix
        ]
        for bad_id in malformed_openalex_ids
            @test_throws MalformedIdentifier parse(OpenAlexID, bad_id)
        end

        # Test kind mismatch
        @test_throws MalformedIdentifier{OpenAlexID{:W}} parse(OpenAlexID{:W}, "A2741809807")
        @test_throws MalformedIdentifier{OpenAlexID{:A}} parse(OpenAlexID{:A}, "W2741809807")
    end

    @testset "Unicode edge cases" begin
        unicode_openalex_ids = [
            "W2741809807🔢", # emoji
            "Ｗ2741809807",   # full-width W
            "W２741809807",   # full-width 2
            "W2７41809807",   # full-width 7
        ]
        for bad_id in unicode_openalex_ids
            @test_throws MalformedIdentifier parse(OpenAlexID, bad_id)
        end
    end

    @testset "Utils" begin
        openalex_cases = [
            ("W2741809807", :W, 2741809807),
            ("A2208157607", :A, 2208157607),
            ("S2741809807", :S, 2741809807)
        ]
        for (idstr, kind, num) in openalex_cases
            openalex_id = parse(OpenAlexID{kind}, idstr)
            @test shortcode(openalex_id) == idstr
            @test purl(openalex_id) == "https://openalex.org/$idstr"
            @test sprint(print, openalex_id) == idstr
        end

        # Test prefix parsing
        @test shortcode(parse(OpenAlexID, "openalex:W2741809807")) == "W2741809807"
        @test shortcode(parse(OpenAlexID, "https://openalex.org/W2741809807")) == "W2741809807"

        # Test tryparse
        @test tryparse(OpenAlexID, "W2741809807") isa OpenAlexID
        @test tryparse(OpenAlexID, "invalid") === nothing
        @test tryparse(OpenAlexID, "") === nothing

        # Test eval(show(x)) == x
        openalex_examples = [
            parse(OpenAlexID, "W2741809807"),
            parse(OpenAlexID, "A2208157607"),
            parse(OpenAlexID, "S2741809807")
        ]
        for openalex in openalex_examples
            @test eval(Meta.parse(repr(openalex))) == openalex
        end
    end
end

@testset "RAiD" begin
    @testset "Valid" begin
        valid_raids = [
            "10.1000/182",
            "10.1038/nature12373",
            "10.1016/j.cell.2020.01.001"
        ]
        for raid in valid_raids
            @test parse(RAiD, raid) isa RAiD
            @test shortcode(parse(RAiD, raid)) == raid
        end
    end

    @testset "Malformed" begin
        # RAiD uses DOI validation, so invalid DOIs should fail
        malformed_raids = [
            "",              # Empty string
            "invalid",       # Not a DOI format
            "notadoi"        # Not DOI format
        ]
        for bad_raid in malformed_raids
            @test_throws MalformedIdentifier parse(RAiD, bad_raid)
        end
    end

    @testset "Unicode edge cases" begin
        # RAiD validation inherits from DOI, test some Unicode issues
        unicode_raids = [
            "10.1000/182🔢", # emoji
        ]
        for bad_raid in unicode_raids
            raid = parse(RAiD, bad_raid)
            @test raid isa RAiD  # RAiD/DOI is quite permissive
        end
    end

    @testset "Utils" begin
        raid_cases = [
            "10.1000/182",
            "10.1038/nature12373",
            "10.1016/j.cell.2020.01.001"
        ]
        for idstr in raid_cases
            raid = parse(RAiD, idstr)
            @test shortcode(raid) == idstr
            @test purl(raid) == "https://raid.org/$idstr"
            @test sprint(print, raid) == "https://raid.org/$idstr"
        end

        # Test prefix parsing
        @test shortcode(parse(RAiD, "raid:10.1000/182")) == "10.1000/182"
        @test shortcode(parse(RAiD, "https://raid.org/10.1000/182")) == "10.1000/182"

        # Test DOI conversion
        raid = parse(RAiD, "10.1000/182")
        doi = convert(DOI, raid)
        @test shortcode(doi) == "10.1000/182"

        # Test tryparse
        @test tryparse(RAiD, "10.1000/182") isa RAiD
        @test tryparse(RAiD, "invalid") === nothing
        @test tryparse(RAiD, "") === nothing

        # Test eval(show(x)) == x
        raid_examples = [
            parse(RAiD, "10.1000/182"),
            parse(RAiD, "10.1038/nature12373"),
            parse(RAiD, "10.1016/j.cell.2020.01.001")
        ]
        for raid in raid_examples
            @test eval(Meta.parse(repr(raid))) == raid
        end
    end
end

@testset "ROR" begin
    @testset "Valid" begin
        valid_rors = [
            "05dxps055",
            "02jx3x895",
            "00x6h5n95"
        ]
        for ror in valid_rors
            @test shortcode(parse(ROR, ror)) == ror
        end
    end

    @testset "Malformed" begin
        malformed_rors = [
            "05dxps05",
            "105dxps055",
            "15dxps055",
            ""
        ]
        for bad_ror in malformed_rors
            @test_throws MalformedIdentifier{ROR} parse(ROR, bad_ror)
        end
    end

    @testset "Checksum" begin
        valid_rors = [
            "05dxps055",
            "02jx3x895",
            "00x6h5n95"
        ]

        wrong_rors = String[]
        for ror in valid_rors
            ror_base = ror[1:7]
            correct_check = ror[8:9]
            for check1 in '0':'9', check2 in '0':'9'
                check = string(check1, check2)
                if check != correct_check
                    push!(wrong_rors, ror_base * check)
                end
            end
        end
        for ror in wrong_rors
            @test_throws ChecksumViolation{ROR} parse(ROR, ror)
        end
    end

    @testset "Integer constructor" begin
        # Test integer constructor with correct checksum
        ror_int = ROR(123456, 76)  # Fixed: correct checksum is 76, not 55
        @test idcode(ror_int) == 123456
        @test idchecksum(ror_int) == 76
        @test shortcode(ror_int) == "0003rj076"

        # Test another integer constructor
        # Test with a known ROR: 05dxps055
        # First decode 5dxps to get the integer
        ror_str = parse(ROR, "05dxps055")
        ror_code = idcode(ror_str)
        ror_check = idchecksum(ror_str)
        ror_int2 = ROR(ror_code, ror_check)
        @test shortcode(ror_str) == shortcode(ror_int2)

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{ROR} ROR(-1, 50)  # Negative number
        @test_throws ChecksumViolation{ROR} ROR(123456, 55)  # Wrong checksum
    end

    @testset "Unicode edge cases" begin
        unicode_rors = [
            "05dxps055🔢",  # emoji
            "05dx🔤s055",   # emoji letter
        ]
        for bad_ror in unicode_rors
            @test_throws MalformedIdentifier{ROR} parse(ROR, bad_ror)
        end

        # Test cases with full-width Unicode characters
        unicode_rors_fullwidth = [
            "０5dxps055",    # full-width zero
            "05ｄxps055",    # full-width d
            "05dxps０55",   # full-width zero
        ]
        for bad_ror in unicode_rors_fullwidth
            @test_throws MalformedIdentifier{ROR} parse(ROR, bad_ror)
        end
    end

    @testset "Utils" begin
        ror_cases = [
            ("05dxps055", 55),
            ("02jx3x895", 95),
            ("00x6h5n95", 95)
        ]
        for (idstr, csum) in ror_cases
            ror = parse(ROR, idstr)
            @test idcode(ror) isa Integer
            @test idchecksum(ror) == csum
            @test shortcode(ror) == idstr
            @test purl(ror) == "https://ror.org/$idstr"
            @test sprint(print, ror) == "https://ror.org/$idstr"
        end

        # Test case sensitivity
        @test shortcode(parse(ROR, "ROR:05dxps055")) == "05dxps055"
        @test shortcode(parse(ROR, "ror:05dxps055")) == "05dxps055"

        # Test tryparse
        @test tryparse(ROR, "05dxps055") isa ROR
        @test tryparse(ROR, "invalid") === nothing
        @test tryparse(ROR, "") === nothing

        # Test eval(show(x)) == x
        ror_examples = [
            parse(ROR, "05dxps055"),
            parse(ROR, "02jx3x895"),
            parse(ROR, "00x6h5n95")
        ]
        for ror in ror_examples
            @test eval(Meta.parse(repr(ror))) == ror
        end
    end
end

@testset "PMID" begin
    @testset "Valid" begin
        valid_pmids = [
            "12345678",
            "1234567",
            "87654321"
        ]
        for pmid in valid_pmids
            @test shortcode(parse(PMID, pmid)) == pmid
        end
    end

    @testset "Malformed" begin
        malformed_pmids = [
            "123456789",  # Too long
            "12345abc",
            ""
        ]
        for bad_pmid in malformed_pmids
            @test_throws MalformedIdentifier{PMID} parse(PMID, bad_pmid)
        end
    end

    @testset "Integer constructor" begin
        # Test integer constructor
        pmid_int = PMID(12345678)
        pmid_str = parse(PMID, "12345678")
        @test shortcode(pmid_int) == shortcode(pmid_str)
        @test idcode(pmid_int) == idcode(pmid_str)

        # Test another integer
        pmid_int2 = PMID(1234567)
        @test shortcode(pmid_int2) == "1234567"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{PMID} PMID(-1)  # Negative
        @test_throws MalformedIdentifier{PMID} PMID(123456789)  # Too large
    end

    @testset "Unicode edge cases" begin
        unicode_pmids = [
            "12345678🔢",  # emoji
        ]
        for bad_pmid in unicode_pmids
            @test_throws MalformedIdentifier{PMID} parse(PMID, bad_pmid)
        end

        # Test cases with full-width Unicode digits
        unicode_pmids_fullwidth = [
            "１2345678",    # full-width 1
            "１２345678",   # full-width 12
            "1234567８",   # full-width 8
        ]
        for bad_pmid in unicode_pmids_fullwidth
            @test_throws MalformedIdentifier{PMID} parse(PMID, bad_pmid)
        end
    end

    @testset "Utils" begin
        pmid_cases = [
            ("12345678", 12345678),
            ("1234567", 1234567),
            ("87654321", 87654321)
        ]
        for (idstr, code) in pmid_cases
            pmid = parse(PMID, idstr)
            @test idcode(pmid) == code
            @test shortcode(pmid) == idstr
            @test purl(pmid) == "https://pubmed.ncbi.nlm.nih.gov/$idstr"
            @test sprint(print, pmid) == "PMID:$idstr"
        end

        # Test case sensitivity
        @test shortcode(parse(PMID, "PMID:12345678")) == "12345678"
        @test shortcode(parse(PMID, "pmid:12345678")) == "12345678"

        # Test tryparse
        @test tryparse(PMID, "12345678") isa PMID
        @test tryparse(PMID, "invalid") === nothing
        @test tryparse(PMID, "") === nothing

        # Test eval(show(x)) == x
        pmid_examples = [
            parse(PMID, "12345678"),
            parse(PMID, "1234567"),
            parse(PMID, "87654321")
        ]
        for pmid in pmid_examples
            @test eval(Meta.parse(repr(pmid))) == pmid
        end
    end
end

@testset "PMCID" begin
    @testset "Valid" begin
        valid_pmcids = [
            "PMC123456",
            "PMC1234567",
            "PMC87654321"
        ]
        for pmcid in valid_pmcids
            @test shortcode(parse(PMCID, pmcid)) == pmcid
        end
    end

    @testset "Malformed" begin
        malformed_pmcids = [
            "PMC123456789",  # Too long
            "PMC12345abc",
            "PMC",
            ""
        ]
        for bad_pmcid in malformed_pmcids
            @test_throws MalformedIdentifier{PMCID} parse(PMCID, bad_pmcid)
        end
    end

    @testset "Integer constructor" begin
        # Test integer constructor
        pmcid_int = PMCID(123456)
        pmcid_str = parse(PMCID, "PMC123456")
        @test idcode(pmcid_int) == idcode(pmcid_str)
        @test shortcode(pmcid_int) == shortcode(pmcid_str)

        # Test another integer
        pmcid_int2 = PMCID(1234567)
        @test shortcode(pmcid_int2) == "PMC1234567"

        # Test invalid integer inputs
        @test_throws MalformedIdentifier{PMCID} PMCID(-1)  # Negative
        @test_throws MalformedIdentifier{PMCID} PMCID(123456789)  # Too large
    end

    @testset "Unicode edge cases" begin
        unicode_pmcids = [
            "PMC123456🔢",   # emoji
        ]
        for bad_pmcid in unicode_pmcids
            @test_throws MalformedIdentifier{PMCID} parse(PMCID, bad_pmcid)
        end

        # Test cases with full-width Unicode characters
        unicode_pmcids_fullwidth = [
            "ＰＭＣ123456",    # full-width PMC
            "PMC１23456",     # full-width 1
            "PMC１２３456",   # full-width 123
            "PMC12345６",    # full-width 6
        ]
        for bad_pmcid in unicode_pmcids_fullwidth
            @test_throws MalformedIdentifier{PMCID} parse(PMCID, bad_pmcid)
        end
    end

    @testset "Utils" begin
        pmcid_cases = [
            ("PMC123456", 123456),
            ("PMC1234567", 1234567),
            ("PMC87654321", 87654321)
        ]
        for (idstr, code) in pmcid_cases
            pmcid = parse(PMCID, idstr)
            @test idcode(pmcid) == code
            @test shortcode(pmcid) == idstr
            @test purl(pmcid) == "https://www.ncbi.nlm.nih.gov/pmc/articles/$idstr"
            @test sprint(print, pmcid) == idstr
        end

        # Test case sensitivity
        @test shortcode(parse(PMCID, "PMC123456")) == "PMC123456"
        @test shortcode(parse(PMCID, "pmc123456")) == "PMC123456"

        # Test tryparse
        @test tryparse(PMCID, "PMC123456") isa PMCID
        @test tryparse(PMCID, "invalid") === nothing
        @test tryparse(PMCID, "") === nothing

        # Test eval(show(x)) == x
        pmcid_examples = [
            parse(PMCID, "PMC123456"),
            parse(PMCID, "PMC1234567"),
            parse(PMCID, "PMC87654321")
        ]
        for pmcid in pmcid_examples
            @test eval(Meta.parse(repr(pmcid))) == pmcid
        end
    end
end

@testset "VIAF" begin
    @testset "Valid" begin
        valid_viafs = [
            "12345678",
            "87654321",
            "1234567890"
        ]
        for viaf in valid_viafs
            @test parse(VIAF, viaf) isa VIAF
            @test shortcode(parse(VIAF, viaf)) == viaf
        end
    end

    @testset "Malformed" begin
        malformed_viafs = [
            "",              # Empty string
            "abc123",        # Non-numeric characters
            "123abc",        # Mixed alphanumeric
            "12.345",        # Decimal point
            "12 345",        # Space in number
            "123-456"        # Hyphen
        ]
        for bad_viaf in malformed_viafs
            @test_throws MalformedIdentifier{VIAF} parse(VIAF, bad_viaf)
        end
    end

    @testset "Unicode edge cases" begin
        unicode_viafs = [
            "12345678🔢",    # emoji
            "１2345678",     # full-width 1
            "1２345678",     # full-width 2
            "12３45678",     # full-width 3
        ]
        for bad_viaf in unicode_viafs
            @test_throws MalformedIdentifier{VIAF} parse(VIAF, bad_viaf)
        end
    end

    @testset "Utils" begin
        viaf_cases = [
            "12345678",
            "87654321",
            "1234567890"
        ]
        for idstr in viaf_cases
            viaf = parse(VIAF, idstr)
            @test idcode(viaf) == parse(UInt32, idstr)
            @test shortcode(viaf) == idstr
            @test purl(viaf) == "https://viaf.org/viaf/$idstr"
            @test sprint(print, viaf) == "https://viaf.org/viaf/$idstr"
        end

        # Test prefix parsing
        @test shortcode(parse(VIAF, "viaf:12345678")) == "12345678"
        @test shortcode(parse(VIAF, "https://viaf.org/viaf/12345678")) == "12345678"

        # Test tryparse
        @test tryparse(VIAF, "12345678") isa VIAF
        @test tryparse(VIAF, "invalid") === nothing
        @test tryparse(VIAF, "") === nothing

        # Test eval(show(x)) == x
        viaf_examples = [
            parse(VIAF, "12345678"),
            parse(VIAF, "87654321"),
            parse(VIAF, "123456789")
        ]
        for viaf in viaf_examples
            @test eval(Meta.parse(repr(viaf))) == viaf
        end
    end
end

@testset "Wikidata" begin
    @testset "Valid" begin
        valid_wikidatas = [
            "Q42",
            "Q123456",
            "Q999999999"
        ]
        for wd in valid_wikidatas
            @test shortcode(parse(Wikidata, wd)) == wd
        end
    end

    @testset "Malformed" begin
        malformed_wikidatas = [
            "42",       # Missing Q prefix
            "P123",     # Wrong prefix
            "Qabc",     # Non-numeric
            "Q",        # No number
            "",
            "q42"       # Lowercase (if not handled)
        ]
        for bad_wd in malformed_wikidatas
            @test_throws MalformedIdentifier{Wikidata} parse(Wikidata, bad_wd)
        end
    end

    @testset "Unicode edge cases" begin
        unicode_wikidatas = [
            "Q42🔢",     # emoji
        ]
        for bad_wd in unicode_wikidatas
            @test_throws MalformedIdentifier{Wikidata} parse(Wikidata, bad_wd)
        end

        # Test cases with full-width Unicode characters
        unicode_wikidatas_fullwidth = [
            "Ｑ42",      # full-width Q
            "Q４2",      # full-width 4
            "Q４２",     # full-width 42
            "Q１23456",  # full-width 1
        ]
        for bad_wd in unicode_wikidatas_fullwidth
            @test_throws MalformedIdentifier{Wikidata} parse(Wikidata, bad_wd)
        end
    end

    @testset "Utils" begin
        wikidata_cases = [
            "Q42",
            "Q123456",
            "Q999999999"
        ]
        for idstr in wikidata_cases
            wikidata = parse(Wikidata, idstr)
            @test shortcode(wikidata) == idstr
            @test purl(wikidata) == "https://www.wikidata.org/wiki/$idstr"
            @test sprint(print, wikidata) == "https://www.wikidata.org/wiki/$idstr"
        end

        # Test tryparse
        @test tryparse(Wikidata, "Q42") isa Wikidata
        @test tryparse(Wikidata, "invalid") === nothing
        @test tryparse(Wikidata, "") === nothing

        # Test eval(show(x)) == x
        wikidata_examples = [
            parse(Wikidata, "Q42"),
            parse(Wikidata, "Q123456"),
            parse(Wikidata, "Q999999999")
        ]
        for wikidata in wikidata_examples
            @test eval(Meta.parse(repr(wikidata))) == wikidata
        end
    end
end

@testset "Error messages" begin
    # Test MalformedIdentifier error formatting
    try
        parse(ISBN, "invalid")
    catch e
        @test e isa MalformedIdentifier{ISBN}
        error_msg = sprint(showerror, e)
        @test contains(error_msg, "ISBN")
        @test contains(error_msg, "invalid")
    end

    # Test ChecksumViolation error formatting
    try
        parse(ISBN, "978-0-439-02348-2")  # Wrong checksum
    catch e
        @test e isa ChecksumViolation
        error_msg = sprint(showerror, e)
        @test contains(error_msg, "Checksum violation")
        @test contains(error_msg, "correct checksum")
    end
end

let test_cases = [
        (ArXiv, ["2301.12345", "hep-th/9901001", "0704.0001v1"]),
        (DOI, ["10.1000/182", "10.1038/nature12373"]),
        (ORCID, ["0000-0002-1825-0097", "0000-0003-1419-2405"]),
        (ISSN, ["0317-8471", "1050-124X"]),
        (ISBN, ["978-0-439-02348-1", "0-684-84328-5"]),
        (ISNI, ["0000 0001 2281 955X", "0000 0000 8389 1195"]),
        (OCN, ["1234567", "12345678"]),
        (RAiD, ["10.1000/182", "10.1038/nature12373"]),
        (ROR, ["05dxps055", "02jx3x895"]),
        (PMID, ["12345678", "1234567"]),
        (PMCID, ["PMC123456", "PMC1234567"]),
        (VIAF, ["12345678", "87654321"]),
        (Wikidata, ["Q42", "Q123456"]),
        (OpenAlexID{:W}, ["W2741809807"]),
        (OpenAlexID{:A}, ["A2208157607"]),
        (OpenAlexID{:S}, ["S2741809807"])]

    @testset "JSON Extension" begin
        for (IdType, examples) in test_cases
            @testset "$IdType" begin
                for example in examples
                    id = parse(IdType, example)
                    @show id
                    @show JSON.json(id)
                    @show JSON.parse(JSON.json(id), IdType)
                    @test JSON.parse(JSON.json(id), IdType) == id
                end
            end
        end
        @testset "Malformed Identifier Handling" begin
            # Test AcademicIdentifiers-specific error cases, not general JSON3 errors
            malformed_cases = [
                (ArXiv, "not-an-arxiv"),
                (DOI, "invalid-doi"),
                (ORCID, "bad-orcid-format"),
                (ISBN, "not-an-isbn"),
                (OpenAlexID{:W}, "not-an-openalex")
            ]
            for (IdType, malformed_str) in malformed_cases
                json_str = string('"', malformed_str, '"')
                @test_throws MalformedIdentifier JSON.parse(json_str, IdType)
            end
        end
    end

    @testset "JSON3 Extension" begin
        for (IdType, examples) in test_cases
            @testset "$IdType" begin
                for example in examples
                    id = parse(IdType, example)
                    @test JSON3.read(JSON3.write(id), IdType) == id
                end
            end
        end
        @testset "Malformed Identifier Handling" begin
            # Test AcademicIdentifiers-specific error cases, not general JSON3 errors
            malformed_cases = [
                (ArXiv, "not-an-arxiv"),
                (DOI, "invalid-doi"),
                (ORCID, "bad-orcid-format"),
                (ISBN, "not-an-isbn"),
                (OpenAlexID{:W}, "not-an-openalex")
            ]
            for (IdType, malformed_str) in malformed_cases
                json_str = string('"', malformed_str, '"')
                @test_throws MalformedIdentifier JSON3.read(json_str, IdType)
            end
        end
    end
end
