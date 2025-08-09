#!/usr/bin/env python3
"""
Test suite for FORTRAN II Parser - FORTRAN II (1958)
Tests for FORTRAN II specific features
"""

import sys
import os
import unittest
from pathlib import Path

# Add grammars directory to path for imports
sys.path.insert(0, 'grammars')

try:
    from antlr4 import InputStream, CommonTokenStream
    from FORTRANIILexer import FORTRANIILexer
    from FORTRANIIParser import FORTRANIIParser
except ImportError as e:
    print(f"Import error: {e}")
    FORTRANIIParser = None


class TestFORTRANIIParser(unittest.TestCase):
    """Test FORTRAN II (1958) parser rules"""
    
    def setUp(self):
        """Set up test fixtures"""
        if FORTRANIIParser is None:
            self.skipTest("FORTRANIIParser not available - grammar not yet implemented")
    
    def parse(self, text, rule_name='program_unit_core'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRANIILexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANIIParser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_call_statements(self):
        """Test CALL statements (introduced in FORTRAN II, 1958)"""
        test_cases = [
            "CALL SUBRTN",
            "CALL CALC(X, Y, Z)",
            "CALL PROCESS(A, B)",
            "CALL INIT"
        ]
        
        for text in test_cases:
            with self.subTest(call_stmt=text):
                tree = self.parse(text, 'call_stmt')
                self.assertIsNotNone(tree)
                # Should have CALL keyword and subroutine name
                self.assertTrue(len(tree.children) >= 2)
    
    def test_function_definition(self):
        """Test FUNCTION definitions (introduced in FORTRAN II)"""
        function_text = """FUNCTION MAX(A, B)
            IF (A - B) 10, 20, 20
            10 MAX = B
            RETURN
            20 MAX = A
            RETURN
            END"""
        
        tree = self.parse(function_text, 'function_subprogram')
        self.assertIsNotNone(tree)
    
    def test_subroutine_definition(self):
        """Test SUBROUTINE definitions (introduced in FORTRAN II)"""
        subroutine_text = """SUBROUTINE SWAP(X, Y)
            TEMP = X
            X = Y
            Y = TEMP
            RETURN
            END"""
        
        tree = self.parse(subroutine_text, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
    
    def test_common_statement(self):
        """Test COMMON statements (introduced in FORTRAN II)"""
        test_cases = [
            "COMMON A, B, C",
            "COMMON /BLOCK1/ X, Y, Z",
            "COMMON /DATA/ ARRAY(100)"
        ]
        
        for text in test_cases:
            with self.subTest(common_stmt=text):
                tree = self.parse(text, 'common_stmt')
                self.assertIsNotNone(tree)


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)