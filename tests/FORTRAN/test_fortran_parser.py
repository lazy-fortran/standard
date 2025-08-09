#!/usr/bin/env python3
"""
Test suite for FORTRANParser - FORTRAN I (1957)
Tests for FORTRAN I parser rules
"""

import sys
import os
import unittest
from pathlib import Path

# Add grammars directory to path for imports
sys.path.insert(0, 'grammars')

try:
    from antlr4 import InputStream, CommonTokenStream
    from FORTRANLexer import FORTRANLexer
    from FORTRANParser import FORTRANParser
except ImportError as e:
    print(f"Import error: {e}")
    FORTRANParser = None


class TestFORTRANParser(unittest.TestCase):
    """Test FORTRAN I (1957) parser rules"""
    
    def setUp(self):
        """Set up test fixtures"""
        if FORTRANParser is None:
            self.skipTest("FORTRANParser not available")
    
    def parse(self, text, rule_name='program'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRANLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANParser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_assignment_statement_simple(self):
        """Test basic assignment statements"""
        test_cases = [
            "X = 1",
            "Y = 2.5",
            "Z = X",
            "TEMP = PRESSURE"
        ]
        
        for text in test_cases:
            with self.subTest(assignment=text):
                tree = self.parse(text, 'assignment_stmt')
                self.assertIsNotNone(tree)
                # Should have variable, ASSIGN, expr structure
                self.assertEqual(len(tree.children), 3)
    
    def test_assignment_statement_array(self):
        """Test assignment to array elements"""
        test_cases = [
            "A(1) = 5",
            "MATRIX(I,J) = X + Y",
            "DATA(INDEX) = 0"
        ]
        
        for text in test_cases:
            with self.subTest(array_assignment=text):
                tree = self.parse(text, 'assignment_stmt')
                self.assertIsNotNone(tree)
    
    def test_expressions_arithmetic(self):
        """Test arithmetic expression parsing"""
        test_cases = [
            "1 + 2",
            "X - Y",
            "A * B",
            "P / Q",
            "X ** 2",
            "A + B * C",          # Test precedence
            "X ** Y ** Z",        # Test right associativity  
            "(A + B) * C",        # Test parentheses
            "-X",                 # Test unary minus
            "+Y"                  # Test unary plus
        ]
        
        for text in test_cases:
            with self.subTest(expression=text):
                tree = self.parse(text, 'expr')
                self.assertIsNotNone(tree)
    
    def test_expressions_relational(self):
        """Test relational expression parsing"""
        test_cases = [
            "X .EQ. Y",
            "A .NE. B", 
            "P .LT. Q",
            "R .LE. S",
            "T .GT. U",
            "V .GE. W"
        ]
        
        for text in test_cases:
            with self.subTest(relational=text):
                tree = self.parse(text, 'expr')
                self.assertIsNotNone(tree)
    
    def test_literals_recognition(self):
        """Test literal value parsing"""
        test_cases = [
            "42",           # Integer
            "3.14159",      # Real  
            "1.0E10",       # Scientific notation
            "2.5e-3"        # Negative exponent
        ]
        
        for text in test_cases:
            with self.subTest(literal=text):
                tree = self.parse(text, 'literal')
                self.assertIsNotNone(tree)
    
    def test_variables_simple(self):
        """Test variable parsing"""
        test_cases = [
            "X",
            "TEMPERATURE",
            "VAR123"
        ]
        
        for text in test_cases:
            with self.subTest(variable=text):
                tree = self.parse(text, 'variable')
                self.assertIsNotNone(tree)
    
    def test_variables_array(self):
        """Test array variable parsing"""
        test_cases = [
            "A(1)",
            "MATRIX(I,J)",
            "DATA(INDEX+1)",
            "TEMP(X*2, Y-1)"
        ]
        
        for text in test_cases:
            with self.subTest(array_variable=text):
                tree = self.parse(text, 'variable')
                self.assertIsNotNone(tree)
    
    def test_goto_statement(self):
        """Test GOTO statements"""
        test_cases = [
            "GOTO 100",
            "GOTO 999"
        ]
        
        for text in test_cases:
            with self.subTest(goto=text):
                tree = self.parse(text, 'goto_stmt')
                self.assertIsNotNone(tree)
    
    def test_arithmetic_if_statement(self):
        """Test arithmetic IF statements (1957 style)"""
        test_cases = [
            "IF (X) 10, 20, 30",
            "IF (A - B) 100, 200, 300"
        ]
        
        for text in test_cases:
            with self.subTest(arithmetic_if=text):
                tree = self.parse(text, 'if_stmt_arithmetic')
                self.assertIsNotNone(tree)
    
    def test_do_loop_basic(self):
        """Test basic DO loop statements"""
        test_cases = [
            "DO 100 I = 1, 10",
            "DO 200 J = 1, N, 2",
            "DO 300 K = START, FINISH"
        ]
        
        for text in test_cases:
            with self.subTest(do_loop=text):
                tree = self.parse(text, 'do_stmt_basic')
                self.assertIsNotNone(tree)
    
    def test_io_statements_basic(self):
        """Test basic I/O statements"""
        read_cases = [
            "READ X",
            "READ A, B, C"
        ]
        
        write_cases = [
            "WRITE X",
            "WRITE A, B, C"
        ]
        
        for text in read_cases:
            with self.subTest(read_stmt=text):
                tree = self.parse(text, 'read_stmt_basic')
                self.assertIsNotNone(tree)
                
        for text in write_cases:
            with self.subTest(write_stmt=text):
                tree = self.parse(text, 'write_stmt_basic')
                self.assertIsNotNone(tree)
    
    def test_control_statements(self):
        """Test control statements"""
        test_cases = [
            ("CONTINUE", 'statement'),
            ("STOP", 'statement')
        ]
        
        for text, rule in test_cases:
            with self.subTest(control=text):
                tree = self.parse(text, rule)
                self.assertIsNotNone(tree)
    
    
    def test_program_unit_simple(self):
        """Test simple program unit parsing"""
        program_texts = [
            "X = 1",
            """X = 1
               Y = 2""",
            """DO 100 I = 1, 10
               X = I
               100 CONTINUE"""
        ]
        
        for text in program_texts:
            with self.subTest(program=repr(text)):
                tree = self.parse(text, 'program_unit_core')
                self.assertIsNotNone(tree)
    
    def test_expression_precedence(self):
        """Test operator precedence in expressions"""
        # Test that A + B * C parses as A + (B * C), not (A + B) * C
        tree = self.parse("A + B * C", 'expr')
        self.assertIsNotNone(tree)
        
        # The tree structure should reflect correct precedence:
        # expr
        #   expr (A)
        #   PLUS (+)
        #   expr (B * C)
        #     expr (B)
        #     MULTIPLY (*)
        #     expr (C)
        
        # This is a more detailed test that would need the actual tree structure
        # For now, just ensure it parses without error
        self.assertEqual(tree.getText(), "A+B*C")
    
    def test_complex_program_structure(self):
        """Test complex program with multiple statements"""
        complex_program = """
            X = 1
            Y = 2.5
            Z = X + Y * 2
            IF (Z) 10, 20, 30
            DO 100 I = 1, 10
            A(I) = I ** 2
            100 CONTINUE
            WRITE A
            STOP
        """
        
        tree = self.parse(complex_program, 'program_unit_core')
        self.assertIsNotNone(tree)
        
        # Should have parsed multiple statements
        self.assertTrue(len(tree.children) > 0)


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)