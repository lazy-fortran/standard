#!/usr/bin/env python3
"""
Fortran 2023 (ISO/IEC 1539-1:2023) Comprehensive Test Suite

Tests the complete Fortran 2023 implementation covering all incremental 
improvements over Fortran 2018, focusing on error corrections and 
new features that prepare for LazyFortran2025.

F2023 New Features Tested:
- Enhanced enumerated types (ENUM improvements)
- Conditional expressions (ternary operator ? :)  
- IEEE arithmetic enhancements (IEEE_MAX, IEEE_MIN functions)
- BOZ constant improvements in array constructors
- NAMELIST enhancements (PUBLIC groups with PRIVATE variables)
- SYSTEM_CLOCK improvements (same kind requirements)
- Enhanced type safety and error corrections

Test Coverage Philosophy:
This suite validates that F2023 grammar correctly recognizes all incremental
improvements while maintaining seamless compatibility with F2018.
Serves as the foundation validation for LazyFortran2025 type inference.
"""

import sys
import os
import pytest
from pathlib import Path

# Add grammars directory to Python path for generated parsers
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../grammars/generated/modern'))
sys.path.append(str(Path(__file__).parent.parent))

from fixture_utils import load_fixture

try:
    from antlr4 import InputStream, CommonTokenStream
    from Fortran2023Lexer import Fortran2023Lexer
    # Parser import may fail if grammar has issues - graceful handling
    try:
        from Fortran2023Parser import Fortran2023Parser
        PARSER_AVAILABLE = True
    except ImportError:
        PARSER_AVAILABLE = False
        Fortran2023Parser = None
except ImportError as e:
    pytest.skip(f"Fortran 2023 grammar not built: {e}", allow_module_level=True)


class TestFortran2023Lexer:
    """Test Fortran 2023 lexer with incremental F2023 improvements."""

    def create_lexer(self, input_text):
        """Create lexer for F2023 input text."""
        input_stream = InputStream(input_text)
        lexer = Fortran2023Lexer(input_stream)
        return lexer

    def get_tokens(self, input_text):
        """Get all tokens from F2023 input text."""
        lexer = self.create_lexer(input_text)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:  # EOF
                break
            tokens.append(token)
        return tokens

    def test_lexer_compilation(self):
        """Test that Fortran 2023 lexer compiles and instantiates."""
        lexer = self.create_lexer("program test")
        assert lexer is not None

    def test_enhanced_enumeration_keywords(self):
        """Test F2023 enhanced enumeration keywords."""
        enum_keywords = {
            'ENUMERATOR': Fortran2023Lexer.ENUMERATOR
        }
        
        for keyword, expected_token in enum_keywords.items():
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, f"Expected {expected_token} for '{keyword}', got {tokens[0].type}"

    def test_conditional_expression_operator(self):
        """Test F2023 conditional expression operator (ternary)."""
        operators = {
            '?': Fortran2023Lexer.QUESTION
        }
        
        for op, expected_token in operators.items():
            tokens = self.get_tokens(op)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, f"Operator '{op}' expected type {expected_token}, got {tokens[0].type}"

    def test_enhanced_ieee_arithmetic_functions(self):
        """Test F2023 enhanced IEEE arithmetic functions."""
        ieee_functions = {
            'IEEE_MAX': Fortran2023Lexer.IEEE_MAX,
            'IEEE_MIN': Fortran2023Lexer.IEEE_MIN,
            'IEEE_MAX_MAG': Fortran2023Lexer.IEEE_MAX_MAG,
            'IEEE_MIN_MAG': Fortran2023Lexer.IEEE_MIN_MAG
        }
        
        for func, expected_token in ieee_functions.items():
            tokens = self.get_tokens(func)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, f"IEEE function '{func}' expected type {expected_token}, got {tokens[0].type}"

    def test_enhanced_intrinsic_constants(self):
        """Test F2023 enhanced intrinsic constant keywords."""
        constants = {
            'LOGICAL_KINDS': Fortran2023Lexer.LOGICAL_KINDS,
            'CHARACTER_KINDS': Fortran2023Lexer.CHARACTER_KINDS
        }

        for const, expected_token in constants.items():
            tokens = self.get_tokens(const)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token

    def test_degree_trig_intrinsics(self):
        """Test F2023 degree-based trigonometric intrinsic keywords.

        ISO/IEC 1539-1:2023 Section 16.9:
        - ACOSD: Arc cosine in degrees (Section 16.9.3)
        - ASIND: Arc sine in degrees (Section 16.9.17)
        - ATAND: Arc tangent in degrees (Section 16.9.21)
        - ATAN2D: Arc tangent of Y/X in degrees (Section 16.9.22)
        - COSD: Cosine with argument in degrees (Section 16.9.51)
        - SIND: Sine with argument in degrees (Section 16.9.170)
        - TAND: Tangent with argument in degrees (Section 16.9.186)
        """
        trig_intrinsics = {
            'ACOSD': Fortran2023Lexer.ACOSD,
            'ASIND': Fortran2023Lexer.ASIND,
            'ATAND': Fortran2023Lexer.ATAND,
            'ATAN2D': Fortran2023Lexer.ATAN2D,
            'COSD': Fortran2023Lexer.COSD,
            'SIND': Fortran2023Lexer.SIND,
            'TAND': Fortran2023Lexer.TAND
        }

        for intrinsic, expected_token in trig_intrinsics.items():
            tokens = self.get_tokens(intrinsic)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"Degree trig intrinsic '{intrinsic}' expected type "
                f"{expected_token}, got {tokens[0].type}"
            )

    def test_degree_trig_case_insensitive(self):
        """Test degree-based trig intrinsics are case-insensitive per ISO standard."""
        test_cases = ['sind', 'SIND', 'Sind', 'SiNd', 'cosd', 'COSD', 'Cosd']

        for case in test_cases:
            tokens = self.get_tokens(case)
            assert len(tokens) >= 1
            base_name = case.upper()
            if base_name == 'SIND':
                assert tokens[0].type == Fortran2023Lexer.SIND
            elif base_name == 'COSD':
                assert tokens[0].type == Fortran2023Lexer.COSD

    def test_pi_trig_intrinsics(self):
        """Test F2023 pi-scaled trigonometric intrinsic keywords.

        ISO/IEC 1539-1:2023 Section 16.9:
        - ACOSPI: Arc cosine / pi (Section 16.9.4)
        - ASINPI: Arc sine / pi (Section 16.9.18)
        - ATANPI: Arc tangent / pi (Section 16.9.23)
        - ATAN2PI: Arc tangent of Y/X / pi (Section 16.9.24)
        - COSPI: Cosine of pi * x (Section 16.9.52)
        - SINPI: Sine of pi * x (Section 16.9.171)
        - TANPI: Tangent of pi * x (Section 16.9.187)
        """
        pi_intrinsics = {
            'ACOSPI': Fortran2023Lexer.ACOSPI,
            'ASINPI': Fortran2023Lexer.ASINPI,
            'ATANPI': Fortran2023Lexer.ATANPI,
            'ATAN2PI': Fortran2023Lexer.ATAN2PI,
            'COSPI': Fortran2023Lexer.COSPI,
            'SINPI': Fortran2023Lexer.SINPI,
            'TANPI': Fortran2023Lexer.TANPI
        }

        for intrinsic, expected_token in pi_intrinsics.items():
            tokens = self.get_tokens(intrinsic)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"Pi-scaled trig intrinsic '{intrinsic}' expected type "
                f"{expected_token}, got {tokens[0].type}"
            )

    def test_pi_trig_case_insensitive(self):
        """Test pi-scaled trig intrinsics are case-insensitive per ISO standard."""
        test_cases = ['sinpi', 'SINPI', 'Sinpi', 'SiNpI', 'cospi', 'COSPI', 'Cospi']

        for case in test_cases:
            tokens = self.get_tokens(case)
            assert len(tokens) >= 1
            base_name = case.upper()
            if base_name == 'SINPI':
                assert tokens[0].type == Fortran2023Lexer.SINPI
            elif base_name == 'COSPI':
                assert tokens[0].type == Fortran2023Lexer.COSPI

    def test_f2018_compatibility(self):
        """Test that F2023 maintains full F2018 compatibility."""
        # Test key F2018 constructs still work
        # Note: RANK_STAR, RANK_DEFAULT, ASSUMED_RANK, LOCALITY, DEFAULT_ACCESS removed
        # in issue #434 - these were dead tokens with no parser usage
        # Note: SELECT_RANK removed in issue #441 - compound token never used (parser uses SELECT + RANK_KEYWORD)
        f2018_keywords = [
            'CO_SUM', 'CO_MIN', 'CO_MAX', 'CO_REDUCE', 'CO_BROADCAST',
            'IMAGE_STATUS', 'FAILED_IMAGES', 'STOPPED_IMAGES',
            'RANDOM_INIT', 'REPEATABLE', 'IMAGE_DISTINCT',
            'REDUCE', 'OUT_OF_RANGE', 'COSHAPE', 'TEAM_NUMBER',
            'FORM_TEAM', 'CHANGE_TEAM', 'END_TEAM', 'TEAM_TYPE'
        ]
        
        for keyword in f2018_keywords:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1, f"F2018 keyword '{keyword}' not recognized in F2023"
            # Should have corresponding token type
            token_type = tokens[0].type
            assert hasattr(Fortran2023Lexer, keyword), f"Token {keyword} missing in F2023 lexer"

    def test_f2003_oop_compatibility(self):
        """Test that F2023 maintains F2003 OOP compatibility."""
        # Test key F2003 OOP constructs
        oop_keywords = [
            'CLASS', 'EXTENDS', 'ABSTRACT', 'PROCEDURE', 'GENERIC',
            'FINAL', 'NOPASS', 'PASS', 'DEFERRED', 'NON_OVERRIDABLE',
            'ALLOCATABLE', 'POINTER', 'TARGET'
        ]
        
        for keyword in oop_keywords:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1, f"F2003 OOP keyword '{keyword}' not recognized in F2023"

    def test_f90_module_compatibility(self):
        """Test that F2023 maintains F90 module system compatibility."""
        # Test key F90 module constructs
        module_keywords = [
            'MODULE', 'USE', 'ONLY', 'PUBLIC', 'PRIVATE', 'INTERFACE',
            'CONTAINS', 'END_MODULE', 'END_INTERFACE'
        ]

        for keyword in module_keywords:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1, f"F90 module keyword '{keyword}' not recognized in F2023"

    def test_typeof_classof_keywords(self):
        """Test F2023 TYPEOF and CLASSOF type inference keywords.

        ISO/IEC 1539-1:2023 Section 7.3.2.1:
        - R703: typeof-type-spec is TYPEOF ( data-ref )
        - R704: classof-type-spec is CLASSOF ( data-ref )
        """
        type_inference_keywords = {
            'TYPEOF': Fortran2023Lexer.TYPEOF,
            'CLASSOF': Fortran2023Lexer.CLASSOF
        }

        for keyword, expected_token in type_inference_keywords.items():
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"Type inference keyword '{keyword}' expected type "
                f"{expected_token}, got {tokens[0].type}"
            )

    def test_typeof_classof_case_insensitive(self):
        """Test TYPEOF/CLASSOF keywords are case-insensitive per ISO standard."""
        test_cases = [
            ('typeof', Fortran2023Lexer.TYPEOF),
            ('TYPEOF', Fortran2023Lexer.TYPEOF),
            ('TypeOf', Fortran2023Lexer.TYPEOF),
            ('classof', Fortran2023Lexer.CLASSOF),
            ('CLASSOF', Fortran2023Lexer.CLASSOF),
            ('ClassOf', Fortran2023Lexer.CLASSOF)
        ]

        for keyword, expected_token in test_cases:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"'{keyword}' expected type {expected_token}, got {tokens[0].type}"
            )

    def test_string_intrinsic_keywords(self):
        """Test F2023 SPLIT and TOKENIZE string intrinsic keywords.

        ISO/IEC 1539-1:2023 Section 16.9:
        - Section 16.9.180: SPLIT - split string into tokens
        - Section 16.9.197: TOKENIZE - tokenize string
        """
        string_intrinsics = {
            'SPLIT': Fortran2023Lexer.SPLIT,
            'TOKENIZE': Fortran2023Lexer.TOKENIZE
        }

        for intrinsic, expected_token in string_intrinsics.items():
            tokens = self.get_tokens(intrinsic)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"String intrinsic '{intrinsic}' expected type "
                f"{expected_token}, got {tokens[0].type}"
            )

    def test_string_intrinsic_case_insensitive(self):
        """Test SPLIT/TOKENIZE keywords are case-insensitive per ISO standard."""
        test_cases = [
            ('split', Fortran2023Lexer.SPLIT),
            ('SPLIT', Fortran2023Lexer.SPLIT),
            ('Split', Fortran2023Lexer.SPLIT),
            ('tokenize', Fortran2023Lexer.TOKENIZE),
            ('TOKENIZE', Fortran2023Lexer.TOKENIZE),
            ('Tokenize', Fortran2023Lexer.TOKENIZE)
        ]

        for keyword, expected_token in test_cases:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"'{keyword}' expected type {expected_token}, got {tokens[0].type}"
            )

    def test_c_interop_procedure_keywords(self):
        """Test F2023 C interoperability procedure keywords.

        ISO/IEC 1539-1:2023 Section 18.2.3:
        - Section 18.2.3.7: C_F_STRPOINTER - convert C string to Fortran pointer
        - Section 18.2.3.8: F_C_STRING - convert Fortran string to C format
        """
        c_interop_procedures = {
            'C_F_STRPOINTER': Fortran2023Lexer.C_F_STRPOINTER,
            'F_C_STRING': Fortran2023Lexer.F_C_STRING
        }

        for proc, expected_token in c_interop_procedures.items():
            tokens = self.get_tokens(proc)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"C interop procedure '{proc}' expected type "
                f"{expected_token}, got {tokens[0].type}"
            )

    def test_c_interop_procedure_case_insensitive(self):
        """Test C_F_STRPOINTER/F_C_STRING keywords are case-insensitive."""
        test_cases = [
            ('c_f_strpointer', Fortran2023Lexer.C_F_STRPOINTER),
            ('C_F_STRPOINTER', Fortran2023Lexer.C_F_STRPOINTER),
            ('C_f_StrPointer', Fortran2023Lexer.C_F_STRPOINTER),
            ('f_c_string', Fortran2023Lexer.F_C_STRING),
            ('F_C_STRING', Fortran2023Lexer.F_C_STRING),
            ('F_c_String', Fortran2023Lexer.F_C_STRING)
        ]

        for keyword, expected_token in test_cases:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, (
                f"'{keyword}' expected type {expected_token}, got {tokens[0].type}"
            )


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="Parser not available")
class TestFortran2023Parser:
    """Test Fortran 2023 parser with F2023 enhancements."""

    def create_parser(self, input_text):
        """Create parser for F2023 input text."""
        input_stream = InputStream(input_text)
        lexer = Fortran2023Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Fortran2023Parser(token_stream)
        return parser

    def test_parser_compilation(self):
        """Test that F2023 parser compiles and can parse basic constructs."""
        test_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "basic_program.f90",
        )
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Basic F2023 program parsing failed: {e}")

    def test_enhanced_enumeration_parsing(self):
        """Test F2023 enhanced enumeration parsing."""
        enum_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "enum_program.f90",
        )
        parser = self.create_parser(enum_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 enhanced enumeration parsing failed: {e}")

    def test_conditional_expression_parsing(self):
        """Test F2023 conditional expression parsing."""
        conditional_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "conditional_expression.f90",
        )
        parser = self.create_parser(conditional_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 conditional expression parsing failed: {e}")

    def test_conditional_expr_rule_basic(self):
        """Test conditional_expr_f2023 rule parses basic ternary.

        ISO/IEC 1539-1:2023 Section 10.1.5 R1020:
        conditional-expr is ( conditional-test ? consequent
                             [ : conditional-test ? consequent ]... : consequent )
        """
        code = "(x > y ? x : y)"
        parser = self.create_parser(code)
        try:
            tree = parser.conditional_expr_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Basic conditional expression parsing failed: {e}")

    def test_conditional_expr_rule_chained(self):
        """Test conditional_expr_f2023 rule parses chained conditionals.

        ISO/IEC 1539-1:2023 Section 10.1.5 R1020 supports chained:
        ( cond1 ? val1 : cond2 ? val2 : default )
        """
        code = "(c < 0 ? 0 : c > 100 ? 100 : c)"
        parser = self.create_parser(code)
        try:
            tree = parser.conditional_expr_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Chained conditional expression parsing failed: {e}")

    def test_conditional_expr_rule_with_arithmetic(self):
        """Test conditional_expr_f2023 rule with arithmetic expressions.

        ISO/IEC 1539-1:2023 Section 10.1.5 R1022:
        consequent is scalar-expr (any scalar expression is valid)
        """
        code = "(a + b > 10 ? a * 2 : b * 3)"
        parser = self.create_parser(code)
        try:
            tree = parser.conditional_expr_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Conditional with arithmetic parsing failed: {e}")

    def test_enhanced_ieee_parsing(self):
        """Test F2023 enhanced IEEE arithmetic function parsing."""
        ieee_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "ieee_program.f90",
        )
        parser = self.create_parser(ieee_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 IEEE arithmetic parsing failed: {e}")

    def test_enhanced_boz_array_constructor(self):
        """Test F2023 enhanced BOZ in array constructors."""
        boz_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "boz_array_constructor.f90",
        )
        parser = self.create_parser(boz_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 BOZ array constructor parsing failed: {e}")

    def test_degree_trig_intrinsics_parsing(self):
        """Test F2023 degree-based trigonometric intrinsics parsing.

        ISO/IEC 1539-1:2023 Section 16.9:
        - ACOSD, ASIND, ATAND, ATAN2D: Inverse functions returning degrees
        - COSD, SIND, TAND: Functions with argument in degrees
        """
        trig_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "degree_trig_intrinsics.f90",
        )
        parser = self.create_parser(trig_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 degree trigonometric intrinsics parsing failed: {e}")

    def test_pi_trig_intrinsics_parsing(self):
        """Test F2023 pi-scaled trigonometric intrinsics parsing.

        ISO/IEC 1539-1:2023 Section 16.9:
        - ACOSPI, ASINPI, ATANPI, ATAN2PI: Inverse functions divided by pi
        - COSPI, SINPI, TANPI: Functions with argument multiplied by pi
        """
        trig_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "pi_trig_intrinsics.f90",
        )
        parser = self.create_parser(trig_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 pi-scaled trigonometric intrinsics parsing failed: {e}")

    def test_f2018_compatibility_parsing(self):
        """Test that F2023 maintains F2018 parsing compatibility."""
        f2018_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "f2018_compat_program.f90",
        )
        parser = self.create_parser(f2018_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2018 compatibility in F2023 parsing failed: {e}")

    def test_typeof_classof_parsing(self):
        """Test F2023 TYPEOF/CLASSOF type inference parsing.

        ISO/IEC 1539-1:2023 Section 7.3.2.1:
        - R703: typeof-type-spec is TYPEOF ( data-ref )
        - R704: classof-type-spec is CLASSOF ( data-ref )
        """
        typeof_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "typeof_classof.f90",
        )
        parser = self.create_parser(typeof_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 TYPEOF/CLASSOF type inference parsing failed: {e}")

    def test_string_intrinsics_parsing(self):
        """Test F2023 SPLIT/TOKENIZE string intrinsic parsing.

        ISO/IEC 1539-1:2023 Section 16.9.180 and 16.9.197
        """
        string_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "string_intrinsics.f90",
        )
        parser = self.create_parser(string_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 string intrinsic parsing failed: {e}")

    def test_at_edit_descriptor_parsing(self):
        """Test F2023 AT edit descriptor parsing.

        ISO/IEC 1539-1:2023 Section 13 (Input/output editing)
        J3/22-007 R1307: char-string-edit-desc adds AT edit descriptor

        The AT edit descriptor outputs character values with trailing
        blanks automatically trimmed. It takes no width specification.
        """
        at_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "at_edit_descriptor.f90",
        )
        parser = self.create_parser(at_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 AT edit descriptor parsing failed: {e}")

    def test_simple_procedures_parsing(self):
        """Test F2023 SIMPLE prefix-spec parsing for procedures (Section 15.8)."""
        simple_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "simple_procedures.f90",
        )
        parser = self.create_parser(simple_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 SIMPLE procedure parsing failed: {e}")

    def test_do_concurrent_reduce_parsing(self):
        """Test F2023 DO CONCURRENT REDUCE locality specifier parsing.

        ISO/IEC 1539-1:2023 Section 11.1.7.5:
        R1130: locality-spec adds REDUCE ( reduce-operation : variable-name-list )

        The REDUCE locality specifier declares reduction variables for
        DO CONCURRENT. Supported reduce-operations include:
        +, *, .AND., .OR., .EQV., .NEQV., MAX, MIN, IAND, IEOR, IOR
        """
        reduce_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "do_concurrent_reduce.f90",
        )
        parser = self.create_parser(reduce_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 DO CONCURRENT REDUCE parsing failed: {e}")

    def test_c_f_strpointer_parsing(self):
        """Test F2023 C_F_STRPOINTER procedure parsing.

        ISO/IEC 1539-1:2023 Section 18.2.3.7:
        C_F_STRPOINTER(CSTRARRAY, FSTRPTR [, NCHARS])

        Converts a C null-terminated string to a Fortran deferred-length
        character pointer. Part of the ISO_C_BINDING module.
        """
        c_interop_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "c_f_strpointer.f90",
        )
        parser = self.create_parser(c_interop_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 C_F_STRPOINTER parsing failed: {e}")

    def test_f_c_string_parsing(self):
        """Test F2023 F_C_STRING function parsing.

        ISO/IEC 1539-1:2023 Section 18.2.3.8:
        F_C_STRING(STRING [, ASIS])

        Transformational function that returns a C-compatible null-terminated
        string. Part of the ISO_C_BINDING module.
        """
        f_c_string_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "f_c_string.f90",
        )
        parser = self.create_parser(f_c_string_input)

        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 F_C_STRING parsing failed: {e}")


class TestFortran2023Foundation:
    """Test F2023 as foundation for LazyFortran2025."""

    def test_f2023_foundation_readiness(self):
        """Verify F2023 is ready as foundation for LazyFortran2025."""
        # Test that F2023 lexer can be imported (foundation requirement)
        assert Fortran2023Lexer is not None
        
        # Test basic functionality
        input_stream = InputStream("program lazy_fortran")
        lexer = Fortran2023Lexer(input_stream)
        token = lexer.nextToken()
        assert token is not None
        assert token.type == Fortran2023Lexer.PROGRAM

    def test_complete_standards_chain(self):
        """Test complete standards inheritance chain to F2023."""
        # Test that key features from all eras are available
        required_features = [
            # FORTRAN era
            'IF', 'DO', 'GOTO', 'INTEGER', 'REAL',
            # F77 structured programming
            'CHARACTER', 'THEN', 'ELSE', 'ENDIF',
            # F90 modern programming
            'MODULE', 'ALLOCATABLE', 'POINTER', 'INTERFACE',
            # F2003 OOP
            'CLASS', 'EXTENDS', 'PROCEDURE', 'ABSTRACT',
            # F2008 parallel programming
            'CONCURRENT', 'CONTIGUOUS',
            # F2018 teams and events
            'CO_SUM', 'FORM_TEAM',  # Note: SELECT_RANK removed in #441 (dead token, parser uses SELECT + RANK_KEYWORD)
            # F2023 enhancements
            'ENUMERATOR', 'QUESTION', 'IEEE_MAX',
            # F2023 degree-based trigonometric intrinsics
            'ACOSD', 'ASIND', 'ATAND', 'ATAN2D', 'COSD', 'SIND', 'TAND',
            # F2023 pi-scaled trigonometric intrinsics
            'ACOSPI', 'ASINPI', 'ATANPI', 'ATAN2PI', 'COSPI', 'SINPI', 'TANPI',
            # F2023 TYPEOF/CLASSOF type inference (Section 7.3.2.1)
            'TYPEOF', 'CLASSOF',
            # F2023 string intrinsics (Section 16.9.180, 16.9.197)
            'SPLIT', 'TOKENIZE',
            # F2023 C interoperability procedures (Section 18.2.3)
            'C_F_STRPOINTER', 'F_C_STRING'
        ]
        
        for feature in required_features:
            assert hasattr(Fortran2023Lexer, feature), f"Missing feature: {feature}"

    def test_lazyfortran2025_preparation(self):
        """Test F2023 preparation for LazyFortran2025 extension."""
        # This test verifies F2023 grammar can be used as import base for LazyF2025
        
        # Test that key extension points are available
        extension_points = [
            'program_unit_f2023',     # Can be extended for LazyF2025 program units
            'expr_f2023',             # Can be extended for LazyF2025 expressions
            'type_declaration_stmt_f2023'  # Can be extended for type inference
        ]
        
        if PARSER_AVAILABLE:
            for extension_point in extension_points:
                assert hasattr(Fortran2023Parser, extension_point), f"Missing extension point: {extension_point}"

    def test_all_historical_compatibility(self):
        """Test compatibility across entire FORTRAN/Fortran history."""
        # Test that constructs from different eras coexist
        mixed_era_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "mixed_era_program.f90",
        )

        lexer = Fortran2023Lexer(InputStream(mixed_era_input))
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        
        # Should successfully tokenize mixed historical constructs
        assert len(tokens) > 20, "Mixed historical constructs not properly tokenized"


class TestFortran2023ErrorCorrections:
    """Test F2023 error corrections and improvements."""

    def test_ieee_function_changes(self):
        """Test F2023 IEEE function behavioral changes."""
        # F2023 changed IEEE_MAX_NUM, IEEE_MIN_NUM behavior with NaN
        # New functions IEEE_MAX, IEEE_MIN have different semantics
        ieee_functions = ['IEEE_MAX', 'IEEE_MIN', 'IEEE_MAX_MAG', 'IEEE_MIN_MAG']
        
        for func in ieee_functions:
            tokens = Fortran2023Lexer(InputStream(func))
            token = tokens.nextToken()
            assert token.type == getattr(Fortran2023Lexer, func)

    def test_system_clock_improvements(self):
        """Test F2023 SYSTEM_CLOCK same-kind requirements."""
        # In F2023, all SYSTEM_CLOCK integer arguments must have same kind
        system_clock_input = "call system_clock(count, count_rate, count_max)"
        
        tokens = Fortran2023Lexer(InputStream(system_clock_input))
        token_list = []
        while True:
            token = tokens.nextToken()
            if token.type == -1:
                break
            token_list.append(token)
        
        # Should tokenize successfully (semantic checking is compiler's job)
        assert len(token_list) > 5

    def test_namelist_enhancements(self):
        """Test F2023 NAMELIST enhancements."""
        # F2023: PUBLIC namelist groups may contain PRIVATE variables
        namelist_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive_extra",
            "namelist_enhancements_module.f90",
        )
        
        lexer = Fortran2023Lexer(InputStream(namelist_input))
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        
        # Should tokenize successfully 
        assert len(tokens) > 10


if __name__ == '__main__':
    """Run Fortran 2023 comprehensive test suite."""
    print("Running Fortran 2023 (ISO/IEC 1539-1:2023) Comprehensive Test Suite...")
    print("=" * 80)
    print("Testing latest ISO standard with incremental improvements over F2018")
    print("F2023 Key Features: Enhanced ENUM, Conditional Expressions, IEEE Improvements")
    print("Foundation Status: Ready for LazyFortran2025 type inference extensions")
    print("=" * 80)
    
    # Run the tests
    pytest.main([__file__, '-v'])
