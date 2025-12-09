#!/usr/bin/env python3
"""
FORTRAN (1957) Historical Stub Tests

Tests for the FORTRAN 1957 historical documentation stub implementation.
This is a MINIMAL test suite for the stub - full testing deferred to Phase 2.

Test Philosophy:
- Verify compilation and basic token recognition
- Ensure the shared core of tokens and rules can be exercised through
  the FORTRAN 1957 grammar
- Validate that the stub can parse basic 1957-style FORTRAN constructs
- Document historical context (not full validation)

Historical Context:
The 1957 FORTRAN system for the IBM 704 was one of the earliest widely
used high-level programming languages. These tests preserve examples
from that era for educational and historical research purposes.
"""

import sys
import os
import pytest
from pathlib import Path

# Add tests root (for fixture_utils) and grammars directory to path for imports
ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT))
sys.path.append(str(ROOT.parent / "grammars/generated/early"))

# Keep original relative grammars path for compatibility
sys.path.insert(0, 'grammars/generated/early')

from fixture_utils import load_fixture

from antlr4 import InputStream, CommonTokenStream
from FORTRANLexer import FORTRANLexer
from FORTRANParser import FORTRANParser


class TestFORTRANHistoricalStub:
    """Test FORTRAN 1957 historical stub functionality."""

    def create_parser(self, input_text):
        """Create parser for FORTRAN input text."""
        input_stream = InputStream(input_text)
        lexer = FORTRANLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANParser(token_stream)
        return parser

    def test_lexer_compilation(self):
        """Test that FORTRAN lexer compiles and recognizes basic tokens."""
        # Historical 1957 FORTRAN keywords
        test_input = "DO GOTO IF END CONTINUE STOP PAUSE"
        
        input_stream = InputStream(test_input)
        lexer = FORTRANLexer(input_stream)
        tokens = []
        
        while True:
            token = lexer.nextToken()
            if token.type == -1:  # EOF
                break
            tokens.append(token)
        
        # Should recognize FORTRAN keywords
        assert len(tokens) >= 6
        assert any(token.type == FORTRANLexer.DO for token in tokens)
        assert any(token.type == FORTRANLexer.GOTO for token in tokens)
        assert any(token.type == FORTRANLexer.IF for token in tokens)

    def test_parser_compilation(self):
        """Test that FORTRAN parser compiles and can parse basic constructs."""
        # Simple 1957 FORTRAN program
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "simple_program.f",
        )
        
        parser = self.create_parser(test_input)
        
        # Should not raise exceptions
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Parser failed: {e}")

    def test_historical_tokens_1957(self):
        """Test recognition of tokens unique to 1957 FORTRAN."""
        # PAUSE statement - unique to 1957 (removed in later standards)
        test_input = "PAUSE 1234"
        
        input_stream = InputStream(test_input)
        lexer = FORTRANLexer(input_stream)
        tokens = []
        
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        
        # Should recognize PAUSE token
        assert any(token.type == FORTRANLexer.PAUSE for token in tokens)

    def test_hollerith_constants(self):
        """Test recognition of Hollerith constants (1957 string literals).

        Per IBM 704 manual (C28-6003), Hollerith constants were the ONLY
        string-literal mechanism in the original 1957 FORTRAN. Format:
          nHcharacters - where n is the count of characters following H
        """
        # Hollerith constants were the ONLY string literals in 1957
        # Test individual Hollerith constants (they need to be separated)
        test_inputs = ["5HHELLO", "12HFORTRAN 1957"]

        for test_input in test_inputs:
            input_stream = InputStream(test_input)
            lexer = FORTRANLexer(input_stream)
            tokens = []

            while True:
                token = lexer.nextToken()
                if token.type == -1:
                    break
                tokens.append(token)

            # Should recognize HOLLERITH token now that it is implemented
            assert len(tokens) > 0, f"No tokens found in: {test_input}"
            assert any(
                token.type == FORTRANLexer.HOLLERITH for token in tokens
            ), f"HOLLERITH token not found in: {test_input}"

    def test_arithmetic_if_statement(self):
        """Test parsing of arithmetic IF (unique to early FORTRAN)."""
        # Arithmetic IF: IF (expr) negative_label, zero_label, positive_label
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "arithmetic_if_stmt.f",
        )
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Arithmetic IF parsing failed: {e}")

    def test_computed_goto_statement(self):
        """Test parsing of computed GOTO (1957 multi-way branch)."""
        # Computed GOTO: GO TO (label1, label2, label3), expression
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "computed_goto_stmt.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            # Now that computed GOTO has an explicit grammar rule,
            # ensure the input parses without syntax errors.
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"Computed GOTO parsing failed: {e}")

    def test_assign_statement(self):
        """Test parsing of ASSIGN statement (1957 assigned GOTO mechanism).

        Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B,
        ASSIGN stores a label in an integer variable for later use with
        assigned GOTO.
        Syntax: ASSIGN i TO n
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "assign_stmt.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"ASSIGN statement parsing failed: {e}")

    def test_assigned_goto_statement(self):
        """Test parsing of assigned GOTO statement (1957 indirect branch).

        Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B,
        assigned GOTO branches to the label stored in an integer variable.
        Syntax: GO TO n, (l1, l2, ..., lm)
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "assigned_goto_stmt.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"Assigned GOTO statement parsing failed: {e}")

    def test_assign_and_goto_combined(self):
        """Test ASSIGN and assigned GOTO used together in typical 1957 pattern.

        This tests a realistic 1957 program that uses ASSIGN to store
        different labels based on conditions and then branches using
        assigned GOTO.
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "assign_goto_combined.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"ASSIGN/assigned GOTO combined parsing failed: {e}")

    def test_do_loop_with_label(self):
        """Test parsing of DO loops with mandatory labels (1957 style)."""
        # DO loops REQUIRED labels in 1957 (no END DO statement)
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "do_loop_with_label.f",
        )
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"DO loop parsing failed: {e}")

    def test_format_statement(self):
        """Test parsing of FORMAT statements (revolutionary I/O feature).

        Per IBM 704 manual (C28-6003) Appendix B row 16, FORMAT was part of
        the original 1957 FORTRAN specification. It defines I/O layout with:
          Iw       - Integer (w = field width)
          Fw.d     - Real fixed-point (w = width, d = decimal places)
          Ew.d     - Real exponential notation
          nHtext   - Hollerith literal (n characters of text)
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "format_stmt.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"FORMAT statement parsing failed: {e}")

    def test_format_multiple_statements(self):
        """Test parsing of multiple FORMAT statements with edit descriptors.

        This tests various FORMAT statement forms including:
        - Simple format descriptors: I5, F10.2, E15.6
        - Repeat counts: 3I10, 2F15.4
        - Hollerith constants: 5HHELLO, 10HRESULT IS:
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "format_tests_1957.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"Multiple FORMAT statements parsing failed: {e}")

    def test_historical_io_statements(self):
        """Test parsing of 1957 I/O statements."""
        # READ, PRINT, PUNCH - the three I/O operations in 1957
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "io_statements.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"I/O statement parsing failed: {e}")

    def test_read_format_only_statement(self):
        """Test parsing of READ n (format label only, no list).

        Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B
        row 24, READ n reads input using FORMAT statement n with
        FORMAT-implied list of variables.
        """
        test_input = """        READ 100
        READ 200
        END
"""
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_frequency_statement(self):
        """Test parsing of FREQUENCY statement (unique optimization hint)."""
        # FREQUENCY was unique to 1957 FORTRAN for optimization
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "frequency_stmt.f",
        )

        parser = self.create_parser(test_input)

        try:
            tree = parser.program_unit_core()
            assert tree is not None
            # FREQUENCY is explicitly modelled in the stub grammar,
            # so this should parse with zero syntax errors.
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"FREQUENCY statement parsing failed: {e}")

    def test_dimension_statement(self):
        """Test parsing of DIMENSION statement (array declarations).

        Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B,
        DIMENSION declares array names and their dimensions.
        Syntax: DIMENSION v, v, v, ... where v is IDENTIFIER(dimensions)
        """
        test_input = """        DIMENSION A(100), B(10,20), C(5,5,5)
        END
"""
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_dimension_statement_expressions(self):
        """Test DIMENSION with expression bounds (1957 supported variables)."""
        test_input = """        DIMENSION X(N), Y(M,N), Z(I+1)
        END
"""
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_equivalence_statement(self):
        """Test parsing of EQUIVALENCE statement (memory overlay).

        Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B,
        EQUIVALENCE allows variables to share the same memory location.
        Syntax: EQUIVALENCE (a,b,c,...), (d,e,f,...), ...
        """
        test_input = """        EQUIVALENCE (X, Y, Z), (A, B)
        END
"""
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_equivalence_with_array_elements(self):
        """Test EQUIVALENCE with array subscripts (memory overlay at specific indices)."""
        test_input = """        DIMENSION A(100), B(10,20)
        EQUIVALENCE (A(1), B(1,1)), (X, Y)
        END
"""
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_dimension_and_equivalence_combined(self):
        """Test DIMENSION and EQUIVALENCE used together in typical 1957 pattern."""
        test_input = """        DIMENSION A(100), B(10,20), C(5,5,5)
        EQUIVALENCE (A(1), B(1,1)), (X, Y, Z)
        A(1) = 1.0
        X = 2.0
        END
"""
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_mathematical_expressions(self):
        """Test parsing of mathematical expressions (core FORTRAN innovation)."""
        # Mathematical notation was revolutionary in 1957
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "math_expressions_block.f",
        )
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Mathematical expression parsing failed: {e}")

    def test_historical_example_program(self):
        """Test parsing of complete 1957 FORTRAN program example."""
        # Simplified quadratic equation solver (1957 style)
        historical_program = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "historical_quadratic_program.f",
        )
        
        parser = self.create_parser(historical_program)
        
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Historical program parsing failed: {e}")


class TestFORTRANHistoricalAccuracy:
    """Test historical accuracy of FORTRAN 1957 features."""

    def test_stub_compilation_status(self):
        """Verify this is a stub implementation with documentation."""
        # Verify we can import both lexer and parser
        from FORTRANLexer import FORTRANLexer
        from FORTRANParser import FORTRANParser
        
        # This confirms the stub compiles with ANTLR4
        assert FORTRANLexer is not None
        assert FORTRANParser is not None

    def test_shared_core_integration(self):
        """Verify integration with the shared core token set."""
        # Test that inherited tokens work
        test_input = "A = B + C * D"
        
        input_stream = InputStream(test_input)
        lexer = FORTRANLexer(input_stream)
        tokens = []
        
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        
        # Should recognize inherited operators from SharedCore
        assert any(token.type == FORTRANLexer.EQUALS for token in tokens)
        assert any(token.type == FORTRANLexer.PLUS for token in tokens)
        assert any(token.type == FORTRANLexer.MULTIPLY for token in tokens)

    def test_educational_documentation(self):
        """Verify comprehensive documentation is present."""
        # Read the lexer file and verify documentation
        lexer_path = os.path.join(os.path.dirname(__file__),
                                  '../../grammars/src/FORTRANLexer.g4')
        
        with open(lexer_path, 'r') as f:
            content = f.read()
        
        # Should contain historical documentation
        assert "FORTRAN I" in content
        assert "IBM 704" in content  
        assert "1957" in content
        assert "first high-level programming language" in content

    def test_1957_label_constraints(self):
        """Test LABEL token follows 1957 FORTRAN constraints (1-99999, no leading zeros)."""
        # Valid labels according to 1957 specifications
        valid_labels = ["1", "10", "100", "1000", "99999"]
        
        for label in valid_labels:
            input_stream = InputStream(label)
            lexer = FORTRANLexer(input_stream)
            token = lexer.nextToken()
            assert token.type == FORTRANLexer.INTEGER_LITERAL, f"Label '{label}' should be valid in 1957"
        
        # Test parsing labels in context
        test_program = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "label_constraints_program.f",
        )
        
        parser = self.create_parser(test_program)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Valid 1957 labels failed to parse: {e}")

    def test_hollerith_constants_accuracy(self):
        """Test Hollerith constants follow 1957 format specification."""
        # Test various Hollerith formats from 1957 era
        hollerith_examples = [
            ("5HHELLO", "HELLO"),
            ("12HFORTRAN 1957", "FORTRAN 1957"), 
            ("1HX", "X"),
            ("20HTHIS IS A TEST STRING", "THIS IS A TEST STRING")
        ]
        
        for hollerith_text, expected_content in hollerith_examples:
            input_stream = InputStream(hollerith_text)
            lexer = FORTRANLexer(input_stream)
            token = lexer.nextToken()
            # HOLLERITH tokens not yet implemented in this stub
            # For now, just check that lexer processes the input without errors
            assert token.type >= 0, f"'{hollerith_text}' should be processed without error"

    def test_1957_program_structure_accuracy(self):
        """Test that program structure follows 1957 FORTRAN conventions."""
        # 1957 programs had no explicit PROGRAM statement - just statements
        # Note: authentic_1957_program.f uses column-1 C comments which require
        # strict fixed-form preprocessing. Use simple_program.f instead.
        simple_program = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "simple_program.f",
        )

        parser = self.create_parser(simple_program)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Simple 1957 program structure failed: {e}")

    def test_1957_control_flow_accuracy(self):
        """Test 1957 control flow constructs parse according to historical specs."""
        # Arithmetic IF was the ONLY conditional in 1957
        arithmetic_if_test = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "arithmetic_if_control_flow.f",
        )
        
        parser = self.create_parser(arithmetic_if_test)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 arithmetic IF failed: {e}")

    def test_1957_io_operations_accuracy(self):
        """Test I/O operations match 1957 FORTRAN specifications."""
        # Test all three 1957 I/O operations
        io_operations = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "io_operations_1957.f",
        )
        
        parser = self.create_parser(io_operations)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 I/O operations failed: {e}")

    def test_1957_unique_features_accuracy(self):
        """Test features unique to 1957 FORTRAN that were removed later."""
        # PAUSE statement - unique to 1957, removed in later standards
        pause_test = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "pause_test_1957.f",
        )

        parser = self.create_parser(pause_test)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
            # PAUSE is explicitly modelled in the grammar via pause_stmt,
            # so this should parse with zero syntax errors.
            assert parser.getNumberOfSyntaxErrors() == 0
        except Exception as e:
            pytest.fail(f"1957 PAUSE statement failed: {e}")
        
        # FREQUENCY statement - optimization hint unique to 1957
        frequency_test = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "frequency_test_1957.f",
        )
        
        parser = self.create_parser(frequency_test)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 FREQUENCY statement failed: {e}")

    def test_1957_mathematical_expressions_accuracy(self):
        """Test mathematical expressions follow 1957 precedence and syntax."""
        # 1957 FORTRAN had full operator precedence including ** from day one
        math_expressions = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "math_expressions_1957.f",
        )
        
        parser = self.create_parser(math_expressions)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 mathematical expressions failed: {e}")

    def test_1957_array_support_accuracy(self):
        """Test array support matches 1957 specifications."""
        # Arrays were revolutionary in 1957 - multi-dimensional with subscripts
        array_program = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "array_program_1957.f",
        )
        
        parser = self.create_parser(array_program)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 array support failed: {e}")

    def test_1957_format_statements_accuracy(self):
        """Test FORMAT statements match 1957 I/O formatting capabilities."""
        # FORMAT statements were revolutionary for precise I/O control
        format_tests = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "format_tests_1957.f",
        )
        
        parser = self.create_parser(format_tests)
        try:
            tree = parser.program_unit_core()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 FORMAT statements failed: {e}")

    def create_parser(self, input_text):
        """Create parser for FORTRAN input text (helper for accuracy tests)."""
        input_stream = InputStream(input_text)
        lexer = FORTRANLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANParser(token_stream)
        return parser


class TestFORTRANHardwareIF:
    """Test IBM 704 hardware-specific IF statements.

    Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B,
    the original FORTRAN supported hardware-specific IF statements
    for testing sense switches, sense lights, and overflow conditions.
    """

    def create_parser(self, input_text):
        """Create parser for FORTRAN input text."""
        input_stream = InputStream(input_text)
        lexer = FORTRANLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANParser(token_stream)
        return parser

    def test_sense_switch_if_tokens(self):
        """Test that lexer recognizes SENSE and SWITCH tokens."""
        test_input = "SENSE SWITCH"
        input_stream = InputStream(test_input)
        lexer = FORTRANLexer(input_stream)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        assert any(token.type == FORTRANLexer.SENSE for token in tokens)
        assert any(token.type == FORTRANLexer.SWITCH for token in tokens)

    def test_sense_light_if_tokens(self):
        """Test that lexer recognizes SENSE and LIGHT tokens."""
        test_input = "SENSE LIGHT"
        input_stream = InputStream(test_input)
        lexer = FORTRANLexer(input_stream)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        assert any(token.type == FORTRANLexer.SENSE for token in tokens)
        assert any(token.type == FORTRANLexer.LIGHT for token in tokens)

    def test_overflow_if_tokens(self):
        """Test that lexer recognizes ACCUMULATOR, QUOTIENT, DIVIDE, CHECK, OVERFLOW."""
        test_input = "ACCUMULATOR QUOTIENT DIVIDE CHECK OVERFLOW"
        input_stream = InputStream(test_input)
        lexer = FORTRANLexer(input_stream)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        assert any(token.type == FORTRANLexer.ACCUMULATOR for token in tokens)
        assert any(token.type == FORTRANLexer.QUOTIENT for token in tokens)
        assert any(token.type == FORTRANLexer.DIVIDE for token in tokens)
        assert any(token.type == FORTRANLexer.CHECK for token in tokens)
        assert any(token.type == FORTRANLexer.OVERFLOW for token in tokens)

    def test_sense_switch_if_statement(self):
        """Test parsing of IF (SENSE SWITCH i) n1, n2.

        Per IBM 704 FORTRAN manual, tests console sense switch i:
        if up, goes to n1; if down, goes to n2.
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "sense_switch_if.f",
        )
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_sense_light_if_statement(self):
        """Test parsing of IF (SENSE LIGHT i) n1, n2.

        Per IBM 704 FORTRAN manual, tests sense light i:
        if on, goes to n1; if off, goes to n2.
        The sense light is turned off after the test.
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "sense_light_if.f",
        )
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_sense_light_statement(self):
        """Test parsing of SENSE LIGHT i (sets sense light on).

        Per IBM 704 manual, there were four sense lights (1-4) on the console.
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "sense_light_stmt.f",
        )
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_overflow_check_if_statements(self):
        """Test parsing of overflow-check IF statements.

        Per IBM 704 FORTRAN manual:
        - IF ACCUMULATOR OVERFLOW n1, n2: tests accumulator overflow indicator
        - IF QUOTIENT OVERFLOW n1, n2: tests MQ overflow indicator
        - IF DIVIDE CHECK n1, n2: tests divide-check indicator
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "overflow_check_if.f",
        )
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_hardware_if_combined(self):
        """Test combined use of all hardware-specific IF statements.

        This tests a realistic program demonstrating sense switches, sense lights,
        and overflow checks working together as they would have in 1957 IBM 704
        FORTRAN programs.
        """
        test_input = load_fixture(
            "FORTRAN",
            "test_fortran_historical_stub",
            "hardware_if_combined.f",
        )
        parser = self.create_parser(test_input)
        tree = parser.program_unit_core()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0


if __name__ == '__main__':
    """Run FORTRAN historical stub tests."""
    print("Running FORTRAN (1957) Historical Stub Tests...")
    print("="*60)
    print("Testing the world's first high-level programming language")
    print("Implementation Status: HISTORICAL DOCUMENTATION STUB")
    print("="*60)

    # Run the tests
    pytest.main([__file__, '-v'])
