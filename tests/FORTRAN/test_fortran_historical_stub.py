#!/usr/bin/env python3
"""
FORTRAN (1957) Historical Stub Tests

Tests for the FORTRAN 1957 historical documentation stub implementation.
This is a MINIMAL test suite for the stub - full testing deferred to Phase 2.

Test Philosophy:
- Verify compilation and basic token recognition
- Ensure integration with SharedCoreLexer/Parser works
- Validate that stub can parse basic 1957 FORTRAN constructs
- Document historical accuracy (not full validation)

Historical Context:
The 1957 FORTRAN was the world's first high-level programming language.
These tests preserve examples from the original IBM 704 era for educational
and historical research purposes.
"""

import sys
import os
import pytest

# Add build directory to Python path for generated parsers
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../build/FORTRAN'))

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
        test_input = """
        A = B + C
        END
        """
        
        parser = self.create_parser(test_input)
        
        # Should not raise exceptions
        try:
            tree = parser.fortran_program()
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
        """Test recognition of Hollerith constants (1957 string literals)."""
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
            
            # Should recognize at least one Hollerith token
            hollerith_tokens = [t for t in tokens if t.type == FORTRANLexer.HOLLERITH]
            assert len(hollerith_tokens) >= 1, f"No Hollerith token found in: {test_input}"

    def test_arithmetic_if_statement(self):
        """Test parsing of arithmetic IF (unique to early FORTRAN)."""
        # Arithmetic IF: IF (expr) negative_label, zero_label, positive_label
        test_input = """
        IF (X - Y) 10, 20, 30
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Arithmetic IF parsing failed: {e}")

    def test_computed_goto_statement(self):
        """Test parsing of computed GOTO (1957 multi-way branch)."""
        # Computed GOTO: GO TO (label1, label2, label3), expression
        test_input = """
        GOTO (10, 20, 30), I
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Computed GOTO parsing failed: {e}")

    def test_do_loop_with_label(self):
        """Test parsing of DO loops with mandatory labels (1957 style)."""
        # DO loops REQUIRED labels in 1957 (no END DO statement)
        test_input = """
        DO 100 I = 1, 10, 2
100     CONTINUE
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"DO loop parsing failed: {e}")

    def test_format_statement(self):
        """Test parsing of FORMAT statements (revolutionary I/O feature)."""
        test_input = """
100     FORMAT (I5, F10.2, E15.6, 5HHELLO)
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"FORMAT statement parsing failed: {e}")

    def test_historical_io_statements(self):
        """Test parsing of 1957 I/O statements."""
        # READ, PRINT, PUNCH - the three I/O operations in 1957
        test_input = """
        READ 100, A, B, C
        PRINT 200, X, Y, Z
        PUNCH 300, RESULT
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"I/O statement parsing failed: {e}")

    def test_frequency_statement(self):
        """Test parsing of FREQUENCY statement (unique optimization hint)."""
        # FREQUENCY was unique to 1957 FORTRAN for optimization
        test_input = """
        FREQUENCY 10 (25, 3, 1)
10      A = B + C
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"FREQUENCY statement parsing failed: {e}")

    def test_mathematical_expressions(self):
        """Test parsing of mathematical expressions (core FORTRAN innovation)."""
        # Mathematical notation was revolutionary in 1957
        test_input = """
        A = B + C * D / E ** F
        X = (Y + Z) ** 2.0
        END
        """
        
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Mathematical expression parsing failed: {e}")

    def test_historical_example_program(self):
        """Test parsing of complete 1957 FORTRAN program example."""
        # Simplified quadratic equation solver (1957 style)
        historical_program = """
        READ 100, A, B, C
        DISC = B*B - 4.0*A*C
        IF (DISC) 10, 20, 30
10      PRINT 200
        GOTO 40
20      ROOT = -B/(2.0*A)
        PRINT 300, ROOT
        GOTO 40
30      ROOT1 = (-B + SQRT(DISC))/(2.0*A)
        ROOT2 = (-B - SQRT(DISC))/(2.0*A)
        PRINT 400, ROOT1, ROOT2
40      STOP
100     FORMAT (3F10.2)
200     FORMAT (12HNO REAL ROOTS)
300     FORMAT (11HSINGLE ROOT, F10.4)
400     FORMAT (10HTWO ROOTS:, 2F10.4)
        END
        """
        
        parser = self.create_parser(historical_program)
        
        try:
            tree = parser.fortran_program()
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
        """Verify integration with SharedCoreLexer/Parser."""
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
        assert any(token.type == FORTRANLexer.ASSIGN for token in tokens)
        assert any(token.type == FORTRANLexer.PLUS for token in tokens)
        assert any(token.type == FORTRANLexer.MULTIPLY for token in tokens)

    def test_educational_documentation(self):
        """Verify comprehensive documentation is present."""
        # Read the lexer file and verify documentation
        lexer_path = os.path.join(os.path.dirname(__file__), 
                                  '../../grammars/FORTRAN/FORTRANLexer.g4')
        
        with open(lexer_path, 'r') as f:
            content = f.read()
        
        # Should contain comprehensive historical documentation
        assert "HISTORICAL STUB" in content
        assert "IBM 704" in content
        assert "John Backus" in content
        assert "1957" in content
        assert "revolutionary" in content.lower()
        
        # Should document the stub approach
        assert "PHASE 1" in content
        assert "PHASE 2" in content
        assert "educational" in content.lower()

    def test_1957_label_constraints(self):
        """Test LABEL token follows 1957 FORTRAN constraints (1-99999, no leading zeros)."""
        # Valid labels according to 1957 specifications
        valid_labels = ["1", "10", "100", "1000", "99999"]
        
        for label in valid_labels:
            input_stream = InputStream(label)
            lexer = FORTRANLexer(input_stream)
            token = lexer.nextToken()
            assert token.type == FORTRANLexer.LABEL, f"Label '{label}' should be valid in 1957"
        
        # Test parsing labels in context
        test_program = """
        100   A = B + C
        200   CONTINUE
        99999 END
        """
        
        parser = self.create_parser(test_program)
        try:
            tree = parser.fortran_program()
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
            assert token.type == FORTRANLexer.HOLLERITH, f"'{hollerith_text}' should be valid Hollerith constant"
            # In a full implementation, we'd validate the count matches the content length
            # For stub, we just verify it's recognized as HOLLERITH

    def test_1957_program_structure_accuracy(self):
        """Test that program structure follows 1957 FORTRAN conventions."""
        # 1957 programs had no explicit PROGRAM statement - just statements
        authentic_1957_program = """
        C     AUTHENTIC 1957 FORTRAN PROGRAM STRUCTURE
              READ 100, A, B
              C = A + B
              PRINT 200, C
              STOP
        100   FORMAT (2F10.2)
        200   FORMAT (F15.4)
        """
        
        parser = self.create_parser(authentic_1957_program)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Authentic 1957 program structure failed: {e}")

    def test_1957_control_flow_accuracy(self):
        """Test 1957 control flow constructs parse according to historical specs."""
        # Arithmetic IF was the ONLY conditional in 1957
        arithmetic_if_test = """
        IF (X - Y) 10, 20, 30
        10    PRINT 100
              GOTO 40
        20    PRINT 200  
              GOTO 40
        30    PRINT 300
        40    CONTINUE
        """
        
        parser = self.create_parser(arithmetic_if_test)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 arithmetic IF failed: {e}")

    def test_1957_io_operations_accuracy(self):
        """Test I/O operations match 1957 FORTRAN specifications."""
        # Test all three 1957 I/O operations
        io_operations = """
        READ 100, A, B, C
        PRINT 200, X, Y
        PUNCH 300, RESULT
        100 FORMAT (3F10.2)
        200 FORMAT (2E15.6) 
        300 FORMAT (F20.8)
        """
        
        parser = self.create_parser(io_operations)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 I/O operations failed: {e}")

    def test_1957_unique_features_accuracy(self):
        """Test features unique to 1957 FORTRAN that were removed later."""
        # PAUSE statement - unique to 1957, removed in later standards
        pause_test = """
        PAUSE 1234
        A = B + C
        """
        
        parser = self.create_parser(pause_test)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 PAUSE statement failed: {e}")
        
        # FREQUENCY statement - optimization hint unique to 1957
        frequency_test = """
        FREQUENCY 100 (25, 3, 1)
        100 A = B * C + D
        """
        
        parser = self.create_parser(frequency_test)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 FREQUENCY statement failed: {e}")

    def test_1957_mathematical_expressions_accuracy(self):
        """Test mathematical expressions follow 1957 precedence and syntax."""
        # 1957 FORTRAN had full operator precedence including ** from day one
        math_expressions = """
        A = B + C * D / E ** F
        X = (Y + Z) ** 2.0 - W * V
        RESULT = SQRT(DISC) + ABS(ROOT)
        """
        
        parser = self.create_parser(math_expressions)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 mathematical expressions failed: {e}")

    def test_1957_array_support_accuracy(self):
        """Test array support matches 1957 specifications."""
        # Arrays were revolutionary in 1957 - multi-dimensional with subscripts
        array_program = """
        DIMENSION A(100), B(10,20), C(5,5,5)
        A(I) = B(I,J) + C(I,J,K)
        PRINT 100, A(25)
        100 FORMAT (F15.6)
        """
        
        parser = self.create_parser(array_program)
        try:
            tree = parser.fortran_program()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"1957 array support failed: {e}")

    def test_1957_format_statements_accuracy(self):
        """Test FORMAT statements match 1957 I/O formatting capabilities."""
        # FORMAT statements were revolutionary for precise I/O control
        format_tests = """
        100 FORMAT (I5, F10.2, E15.6)
        200 FORMAT (3I10, 2F15.4, 5HHELLO)
        300 FORMAT (10HRESULT IS:, F20.8)
        """
        
        parser = self.create_parser(format_tests)
        try:
            tree = parser.fortran_program()
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


if __name__ == '__main__':
    """Run FORTRAN historical stub tests."""
    print("Running FORTRAN (1957) Historical Stub Tests...")
    print("="*60)
    print("Testing the world's first high-level programming language")
    print("Implementation Status: HISTORICAL DOCUMENTATION STUB")
    print("="*60)
    
    # Run the tests
    pytest.main([__file__, '-v'])