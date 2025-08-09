#!/usr/bin/env python3
"""
Test suite for FORTRAN IV Parser - FORTRAN IV (1962)
Tests for FORTRAN IV specific features: LOGICAL, DOUBLE PRECISION, COMPLEX
"""

import sys
import os
import unittest
from pathlib import Path

# Add grammars directory to path for imports
sys.path.insert(0, 'grammars')

try:
    from antlr4 import InputStream, CommonTokenStream
    from FORTRANIVLexer import FORTRANIVLexer
    from FORTRANIVParser import FORTRANIVParser
except ImportError as e:
    print(f"Import error: {e}")
    FORTRANIVParser = None


class TestFORTRANIVParser(unittest.TestCase):
    """Test FORTRAN IV (1962) parser rules - Data Type Revolution"""
    
    def setUp(self):
        """Set up test fixtures"""
        if FORTRANIVParser is None:
            self.skipTest("FORTRANIVParser not available")
    
    def parse(self, text, rule_name='fortran_program'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRANIVLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANIVParser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_logical_data_type(self):
        """Test LOGICAL data type (introduced in FORTRAN IV, 1962)"""
        test_cases = [
            "LOGICAL FLAG",
            "LOGICAL BOOL1, BOOL2",
            "LOGICAL ARRAY(10)"
        ]
        
        for text in test_cases:
            with self.subTest(logical_declaration=text):
                # Test as a type specification
                tree = self.parse(text, 'type_spec')
                self.assertIsNotNone(tree)
    
    def test_double_precision_data_type(self):
        """Test DOUBLE PRECISION data type (introduced in FORTRAN IV, 1962)"""
        test_cases = [
            "DOUBLE PRECISION X",
            "DOUBLE PRECISION A, B, C",
            "DOUBLE PRECISION MATRIX(5,5)"
        ]
        
        for text in test_cases:
            with self.subTest(double_precision=text):
                # Test as a type specification
                tree = self.parse(text, 'type_spec')
                self.assertIsNotNone(tree)
    
    def test_complex_data_type(self):
        """Test COMPLEX data type (introduced in FORTRAN IV, 1962)"""
        test_cases = [
            "COMPLEX Z",
            "COMPLEX C1, C2",
            "COMPLEX IMPEDANCE(100)"
        ]
        
        for text in test_cases:
            with self.subTest(complex_declaration=text):
                # Test as a type specification
                tree = self.parse(text, 'type_spec')
                self.assertIsNotNone(tree)
    
    def test_logical_literals(self):
        """Test logical literals .TRUE. and .FALSE. (FORTRAN IV, 1962)"""
        test_cases = [
            ".TRUE.",
            ".FALSE."
        ]
        
        for text in test_cases:
            with self.subTest(logical_literal=text):
                tree = self.parse(text, 'logical_literal')
                self.assertIsNotNone(tree)
    
    def test_logical_operators(self):
        """Test logical operators .AND., .OR., .NOT. (FORTRAN IV, 1962)"""
        test_cases = [
            ".TRUE. .AND. .FALSE.",
            ".TRUE. .OR. .FALSE.", 
            ".NOT. .TRUE.",
            "A .EQ. B .AND. C .GT. D",
            ".NOT. (X .LT. Y)"
        ]
        
        for text in test_cases:
            with self.subTest(logical_expression=text):
                tree = self.parse(text, 'logical_expr')
                self.assertIsNotNone(tree)
    
    def test_relational_operators(self):
        """Test enhanced relational operators (FORTRAN IV, 1962)"""
        test_cases = [
            "A .EQ. B",
            "X .NE. Y",
            "I .LT. J",
            "P .LE. Q", 
            "R .GT. S",
            "U .GE. V"
        ]
        
        for text in test_cases:
            with self.subTest(relational=text):
                tree = self.parse(text, 'relational_expr')
                self.assertIsNotNone(tree)
    
    def test_logical_if_statement(self):
        """Test logical IF statement (introduced in FORTRAN IV, 1962)"""
        test_cases = [
            "IF (.TRUE.) GOTO 100",
            "IF (A .GT. B) X = Y",
            "IF (.NOT. FLAG) STOP",
            "IF (A .EQ. B .AND. C .NE. D) PRINT X"
        ]
        
        for text in test_cases:
            with self.subTest(logical_if=text):
                tree = self.parse(text, 'logical_if_stmt')
                self.assertIsNotNone(tree)
    
    def test_double_precision_literals(self):
        """Test double precision literals (FORTRAN IV, 1962)"""
        test_cases = [
            "3.14159D0",
            "1.0D+10",
            "2.5D-3",
            "1D20"
        ]
        
        for text in test_cases:
            with self.subTest(double_literal=text):
                tree = self.parse(text, 'double_precision_literal')
                self.assertIsNotNone(tree)
    
    def test_complex_literals(self):
        """Test complex literals (FORTRAN IV, 1962)"""
        test_cases = [
            "(1.0, 2.0)",
            "(3.14, 0.0)",
            "(A, B)"
        ]
        
        for text in test_cases:
            with self.subTest(complex_literal=text):
                tree = self.parse(text, 'complex_literal')
                self.assertIsNotNone(tree)
    
    def test_mixed_mode_arithmetic(self):
        """Test mixed-mode arithmetic (FORTRAN IV feature)"""
        test_cases = [
            "I + X",           # INTEGER + REAL
            "X * 2",           # REAL * INTEGER  
            "Z + (1.0, 0.0)",  # COMPLEX + COMPLEX
            "A + B * C",       # Mixed expressions
        ]
        
        for text in test_cases:
            with self.subTest(mixed_arithmetic=text):
                tree = self.parse(text, 'expr')
                self.assertIsNotNone(tree)
    
    def test_enhanced_function_calls(self):
        """Test function calls with FORTRAN IV intrinsics"""
        test_cases = [
            "DBLE(X)",          # Convert to double precision
            "CMPLX(A, B)",      # Create complex number
            "REAL(Z)",          # Real part of complex
            "AIMAG(Z)",         # Imaginary part of complex
            "ABS(X)",           # Absolute value
            "SQRT(Y)"           # Square root
        ]
        
        for text in test_cases:
            with self.subTest(function_call=text):
                tree = self.parse(text, 'function_call')
                self.assertIsNotNone(tree)
    
    def test_fortran_iv_program_structure(self):
        """Test complete FORTRAN IV program with new features"""
        program_text = """LOGICAL FLAG
DOUBLE PRECISION PI
COMPLEX Z
INTEGER I
REAL X

FLAG = .TRUE.
PI = 3.14159265358979D0
Z = CMPLX(1.0, 2.0)
I = 42
X = 3.14

IF (FLAG .AND. I .GT. 0) THEN
    X = REAL(Z) + PI
    PRINT X
END IF
STOP
END"""
        
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
    
    def test_data_type_revolution_features(self):
        """Test the complete data type revolution of FORTRAN IV"""
        # Test that all new data types can be used together
        test_code = """LOGICAL READY
DOUBLE PRECISION PRECISION_VALUE  
COMPLEX IMPEDANCE
INTEGER COUNT
REAL VOLTAGE

READY = .FALSE.
PRECISION_VALUE = 1.23456789012345D0
IMPEDANCE = (3.0, 4.0)
COUNT = 0
VOLTAGE = 120.0

IF (.NOT. READY) THEN
    COUNT = COUNT + 1
    READY = COUNT .GT. 10
END IF

IF (ABS(IMPEDANCE) .GT. 5.0 .AND. READY) THEN
    VOLTAGE = VOLTAGE * SQRT(2.0D0)
    PRINT VOLTAGE
END IF
END"""
        
        tree = self.parse(test_code, 'main_program')
        self.assertIsNotNone(tree)
        
        # Verify that the tree contains meaningful structure
        self.assertTrue(len(str(tree.getText())) > 50)  # Non-trivial program


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)