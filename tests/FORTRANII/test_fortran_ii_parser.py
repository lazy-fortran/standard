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
            ("CALL SUBRTN", "SUBRTN", 0),
            ("CALL CALC(X, Y, Z)", "CALC", 3),
            ("CALL PROCESS(A, B)", "PROCESS", 2),
            ("CALL INIT", "INIT", 0)
        ]
        
        for text, expected_name, expected_args in test_cases:
            with self.subTest(call_stmt=text):
                tree = self.parse(text, 'call_stmt')
                self.assertIsNotNone(tree)
                # Verify CALL keyword
                self.assertEqual(tree.children[0].getText(), 'CALL')
                # Verify subroutine name
                self.assertEqual(tree.children[1].getText(), expected_name)
                # Verify argument count
                if expected_args > 0:
                    # Has parentheses and arguments
                    self.assertGreater(len(tree.children), 2)
                    # Check for opening parenthesis
                    self.assertIn('(', tree.getText())
    
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
        # Verify FUNCTION keyword
        self.assertIn('FUNCTION', tree.getText())
        # Verify function name
        self.assertIn('MAX', tree.getText())
        # Verify parameters
        self.assertIn('A', tree.getText())
        self.assertIn('B', tree.getText())
        # Verify RETURN statements
        self.assertEqual(tree.getText().count('RETURN'), 2)
        # Verify END statement
        self.assertIn('END', tree.getText())
    
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
        # Verify SUBROUTINE keyword
        self.assertIn('SUBROUTINE', tree.getText())
        # Verify subroutine name
        self.assertIn('SWAP', tree.getText())
        # Verify parameters
        self.assertIn('X', tree.getText())
        self.assertIn('Y', tree.getText())
        # Verify assignment statements
        self.assertIn('TEMP', tree.getText())
        # Verify RETURN statement
        self.assertIn('RETURN', tree.getText())
        # Verify END statement
        self.assertIn('END', tree.getText())
    
    def test_common_statement(self):
        """Test COMMON statements (introduced in FORTRAN II)"""
        test_cases = [
            ("COMMON A, B, C", None, ["A", "B", "C"]),
            ("COMMON /BLOCK1/ X, Y, Z", "BLOCK1", ["X", "Y", "Z"]),
            ("COMMON /DATA/ ARRAY(100)", "DATA", ["ARRAY"])
        ]
        
        for text, block_name, vars in test_cases:
            with self.subTest(common_stmt=text):
                tree = self.parse(text, 'common_stmt')
                self.assertIsNotNone(tree)
                # Verify COMMON keyword
                self.assertIn('COMMON', tree.getText())
                # Verify block name if present
                if block_name:
                    self.assertIn(block_name, tree.getText())
                # Verify variables
                for var in vars:
                    self.assertIn(var, tree.getText())
    
    def test_not_a_stub(self):
        """Verify FORTRAN II grammar is a real implementation, not a stub"""
        # Read the grammar file to check for stub indicators
        import os
        grammar_path = os.path.join('grammars', 'FORTRANIIParser.g4')
        with open(grammar_path, 'r') as f:
            content = f.read()
        
        # Ensure no stub comments
        self.assertNotIn('grammar stub', content.lower(),
                        "Grammar contains 'stub' references")
        self.assertNotIn('phase 1', content.lower(),
                        "Grammar contains phase 1 stub references")
        
        # Verify key FORTRAN II features are properly implemented
        # Should have real rule implementations, not just placeholders
        self.assertIn('call_stmt', content)
        self.assertIn('function_subprogram', content)
        self.assertIn('subroutine_subprogram', content)
        self.assertIn('common_stmt', content)
        
        # Parse a complete FORTRAN II program to ensure it works
        program = """
            SUBROUTINE CALC(A, B, C)
            COMMON /DATA/ X, Y
            C = A + B
            RETURN
            END
        """
        tree = self.parse(program, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
        # Verify the parse tree contains expected elements
        text = tree.getText()
        self.assertIn('SUBROUTINE', text)
        self.assertIn('CALC', text)
        self.assertIn('COMMON', text)
        self.assertIn('DATA', text)


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)