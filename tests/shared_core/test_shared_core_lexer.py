#!/usr/bin/env python3
"""
Test suite for SharedCoreLexer - TDD RED phase
Tests for universal FORTRAN tokens present from 1957 to 2023
"""

import sys
import os
import unittest
from pathlib import Path

# Add build directory to path for imports
project_root = Path(__file__).parent.parent.parent
build_dir = project_root / "build" / "shared_core"
sys.path.insert(0, str(build_dir))

try:
    from antlr4 import InputStream, CommonTokenStream
    from SharedCoreLexer import SharedCoreLexer
except ImportError as e:
    print(f"Import error (expected in RED phase): {e}")
    SharedCoreLexer = None


class TestSharedCoreLexer(unittest.TestCase):
    """Test universal FORTRAN tokens (1957-2023)"""
    
    def setUp(self):
        """Set up test fixtures"""
        if SharedCoreLexer is None:
            self.skipTest("SharedCoreLexer not yet implemented (RED phase)")
    
    def create_lexer(self, text):
        """Helper to create lexer from text"""
        input_stream = InputStream(text)
        return SharedCoreLexer(input_stream)
    
    def get_tokens(self, text):
        """Helper to get all tokens from text"""
        lexer = self.create_lexer(text)
        tokens = []
        token = lexer.nextToken()
        while token.type != -1:  # EOF token type
            tokens.append(token)
            token = lexer.nextToken()
        return tokens
    
    def test_keywords_universal_1957(self):
        """Test keywords present since FORTRAN 1957"""
        test_cases = [
            ("IF", "IF"),
            ("GOTO", "GOTO"),
            ("DO", "DO"),
            ("END", "END"),
            ("CONTINUE", "CONTINUE"),
            ("STOP", "STOP")
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(keyword=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type], 
                    expected_type
                )
    
    def test_data_types_universal(self):
        """Test universal data type keywords"""
        test_cases = [
            ("INTEGER", "INTEGER"),
            ("REAL", "REAL")
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(datatype=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    expected_type
                )
    
    def test_operators_arithmetic(self):
        """Test arithmetic operators (1957-present)"""
        test_cases = [
            ("=", "ASSIGN"),
            ("+", "PLUS"),
            ("-", "MINUS"),
            ("*", "MULTIPLY"),
            ("/", "DIVIDE"),
            ("**", "POWER")
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(operator=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    expected_type
                )
    
    def test_operators_relational_1957_style(self):
        """Test relational operators in 1957 style"""
        test_cases = [
            (".EQ.", "EQ"),
            (".NE.", "NE"),
            (".LT.", "LT"),
            (".LE.", "LE"),
            (".GT.", "GT"),
            (".GE.", "GE")
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(operator=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    expected_type
                )
    
    def test_delimiters(self):
        """Test delimiter tokens"""
        test_cases = [
            ("(", "LPAREN"),
            (")", "RPAREN"),
            (",", "COMMA"),
            (":", "COLON")
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(delimiter=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    expected_type
                )
    
    def test_integer_literals(self):
        """Test integer literal recognition"""
        test_cases = [
            "0",
            "1",
            "42",
            "1957",
            "2023",
            "999999"
        ]
        
        for text in test_cases:
            with self.subTest(integer=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    "INTEGER_LITERAL"
                )
    
    def test_real_literals(self):
        """Test real number literal recognition"""
        test_cases = [
            "0.0",
            "1.0",
            "3.14159",
            "2.71828",
            "1.0E10",
            "1.0e-10",
            "3.14E+5",
            "999.999e999"
        ]
        
        for text in test_cases:
            with self.subTest(real=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    "REAL_LITERAL"
                )
    
    def test_identifiers(self):
        """Test identifier recognition (variable names)"""
        test_cases = [
            "A",
            "X",
            "VAR",
            "MATRIX",
            "ARRAY1",
            "X123",
            "TEMPERATURE",
            "PRESSURE2023",
            "VAR_NAME",
            "MAX_VALUE",
            "X_1",
            "LONG_VARIABLE_NAME_WITH_UNDERSCORES"
        ]
        
        for text in test_cases:
            with self.subTest(identifier=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                self.assertEqual(tokens[0].text, text)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    "IDENTIFIER"
                )
    
    def test_case_insensitive_keywords(self):
        """Test that keywords are case-insensitive"""
        test_cases = [
            ("if", "IF"),
            ("If", "IF"),
            ("IF", "IF"),
            ("goto", "GOTO"),
            ("GoTo", "GOTO"),
            ("GOTO", "GOTO"),
            ("real", "REAL"),
            ("Real", "REAL"),
            ("REAL", "REAL")
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(text=text):
                tokens = self.get_tokens(text)
                self.assertEqual(len(tokens), 1)
                lexer = self.create_lexer(text)
                self.assertEqual(
                    lexer.symbolicNames[tokens[0].type],
                    expected_type
                )
    
    def test_whitespace_handling(self):
        """Test that whitespace is properly handled"""
        text = "  IF   (   X   )   GOTO   100  "
        tokens = self.get_tokens(text)
        
        # Should get IF, LPAREN, IDENTIFIER, RPAREN, GOTO, INTEGER_LITERAL
        self.assertEqual(len(tokens), 6)
        lexer = self.create_lexer(text)
        self.assertEqual(lexer.symbolicNames[tokens[0].type], "IF")
        self.assertEqual(lexer.symbolicNames[tokens[1].type], "LPAREN")
        self.assertEqual(lexer.symbolicNames[tokens[2].type], "IDENTIFIER")
        self.assertEqual(lexer.symbolicNames[tokens[3].type], "RPAREN")
        self.assertEqual(lexer.symbolicNames[tokens[4].type], "GOTO")
        self.assertEqual(lexer.symbolicNames[tokens[5].type], "INTEGER_LITERAL")
    
    def test_complex_expression_tokenization(self):
        """Test tokenization of a complex expression"""
        text = "A = B + C * D ** 2 - E / F"
        tokens = self.get_tokens(text)
        
        expected_types = [
            "IDENTIFIER",  # A
            "ASSIGN",      # =
            "IDENTIFIER",  # B
            "PLUS",        # +
            "IDENTIFIER",  # C
            "MULTIPLY",    # *
            "IDENTIFIER",  # D
            "POWER",       # **
            "INTEGER_LITERAL",  # 2
            "MINUS",       # -
            "IDENTIFIER",  # E
            "DIVIDE",      # /
            "IDENTIFIER"   # F
        ]
        
        self.assertEqual(len(tokens), len(expected_types))
        lexer = self.create_lexer(text)
        for i, expected_type in enumerate(expected_types):
            with self.subTest(position=i):
                self.assertEqual(
                    lexer.symbolicNames[tokens[i].type],
                    expected_type
                )


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)