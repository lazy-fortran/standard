#!/usr/bin/env python3
"""
Test suite for FORTRAN 77 Parser - FORTRAN 77 (1977)
Tests for FORTRAN 77 structured programming features: CHARACTER, IF-THEN-ELSE
"""

import sys
import os
import unittest
from pathlib import Path

# Add tests root (for fixture_utils) and grammars directory to path for imports
ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT))
sys.path.append(str(ROOT.parent / "grammars"))

try:
    from antlr4 import InputStream, CommonTokenStream  # type: ignore
    from FORTRAN77Lexer import FORTRAN77Lexer  # type: ignore
    from FORTRAN77Parser import FORTRAN77Parser  # type: ignore
    from fixture_utils import load_fixture
except ImportError as e:
    print(f"Import error: {e}")
    FORTRAN77Parser = None


class TestFORTRAN77Parser(unittest.TestCase):
    """Test FORTRAN 77 (1977) parser rules - Structured Programming Revolution"""
    
    def setUp(self):
        """Set up test fixtures"""
        if FORTRAN77Parser is None:
            self.skipTest("FORTRAN77Parser not available")
    
    def parse(self, text, rule_name='main_program'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRAN77Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRAN77Parser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_character_data_type(self):
        """Test CHARACTER data type (NEW in FORTRAN 77)"""
        test_cases = [
            "CHARACTER NAME",
            "CHARACTER*10 TITLE",
            "CHARACTER*(*) MESSAGE"
        ]
        
        for text in test_cases:
            with self.subTest(character_type=text):
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)
    
    def test_if_then_else_construct(self):
        """Test IF-THEN-ELSE construct (NEW in FORTRAN 77)"""
        test_cases = [
            """IF (X .GT. 0) THEN
    PRINT *, 'POSITIVE'
END IF""",
            """IF (A .EQ. B) THEN
    RESULT = 1
ELSE
    RESULT = 0
END IF""",
            """IF (X .LT. 0) THEN
    PRINT *, 'NEGATIVE'
ELSE IF (X .GT. 0) THEN
    PRINT *, 'POSITIVE'  
ELSE
    PRINT *, 'ZERO'
END IF"""
        ]
        
        for text in test_cases:
            with self.subTest(if_construct=text):
                tree = self.parse(text, 'block_if_construct')
                self.assertIsNotNone(tree)
    
    def test_character_expressions(self):
        """Test character expressions and operations (FORTRAN 77)"""
        test_cases = [
            "HELLO",
            "FIRST // SECOND"
        ]
        
        for text in test_cases:
            with self.subTest(char_expr=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)
    
    def test_enhanced_do_loops(self):
        """Test enhanced DO loops with floating point (FORTRAN 77)"""
        test_cases = [
            "DO 100 X = 1.0, 10.0, 0.5",
            "DO 200 I = 1, N"
        ]
        
        for text in test_cases:
            with self.subTest(do_stmt=text):
                tree = self.parse(text, 'do_stmt')
                self.assertIsNotNone(tree)
    
    def test_save_statement(self):
        """Test SAVE statement (NEW in FORTRAN 77)"""
        test_cases = [
            "SAVE",
            "SAVE A, B, C",
            "SAVE /COMMON_BLOCK/"
        ]
        
        for text in test_cases:
            with self.subTest(save_stmt=text):
                tree = self.parse(text, 'save_stmt')
                self.assertIsNotNone(tree)
    
    def test_intrinsic_statement(self):
        """Test INTRINSIC statement (NEW in FORTRAN 77)"""
        test_cases = [
            "INTRINSIC SIN, COS, TAN",
            "INTRINSIC SQRT"
        ]
        
        for text in test_cases:
            with self.subTest(intrinsic_stmt=text):
                tree = self.parse(text, 'intrinsic_stmt')
                self.assertIsNotNone(tree)
    
    def test_external_statement(self):
        """Test EXTERNAL statement (NEW in FORTRAN 77)"""
        test_cases = [
            "EXTERNAL FUNC1, FUNC2",
            "EXTERNAL MYSUB"
        ]
        
        for text in test_cases:
            with self.subTest(external_stmt=text):
                tree = self.parse(text, 'external_stmt')
                self.assertIsNotNone(tree)
    
    def test_structured_programming_example(self):
        """Test complete structured programming example (FORTRAN 77)"""
        program_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser",
            "structured_program_example.f",
        )
        
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
    
    def test_fortran77_revolution_features(self):
        """Test the structured programming revolution features"""
        # Complex nested IF-THEN-ELSE structure
        nested_if = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser_extra",
            "nested_if_block.f",
        )
        
        tree = self.parse(nested_if, 'block_if_construct')
        self.assertIsNotNone(tree)
    
    def test_character_string_processing(self):
        """Test character string processing capabilities (FORTRAN 77)

        Per ANSI X3.9-1978 sections 4.7, 5.3, 5.7, and 6, FORTRAN 77 supports:
        - Character constants as apostrophe-delimited strings
        - Doubled apostrophes for embedded quotes
        - Concatenation via //
        - Substring operations via (start:end)
        """
        string_operations = [
            "'HELLO'",
            "'HELLO' // ' WORLD'",
            "'IT''S A TEST'",
            "'X' // 'Y' // 'Z'",
        ]

        for text in string_operations:
            with self.subTest(string_op=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_substring_operations(self):
        """Test character substring operations (FORTRAN 77)

        Per ANSI X3.9-1978 section 5.7, a substring is a contiguous portion
        of a character datum designated by a substring name of the form:
            character-variable-name (e1:e2)
        where e1 and e2 are integer expressions for starting and ending
        positions. The starting position e1 is mandatory; the ending
        position e2 may be omitted to indicate the remainder of the string.
        """
        substring_expressions = [
            "NAME(1:5)",
            "STR(I:J)",
            "TEXT(1:LEN)",
            "WORD(START:END)",
            "LINE(1:)",
        ]

        for text in substring_expressions:
            with self.subTest(substring=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_concatenation_combinations(self):
        """Test character concatenation with mixed operand types (FORTRAN 77)

        Per ANSI X3.9-1978 section 6.2, concatenation is performed by the
        operator //. The result is a character string whose value is the
        value of the left operand followed by the value of the right operand.
        """
        concat_expressions = [
            "A // B",
            "'PREFIX' // NAME",
            "FIRST // ' ' // LAST",
            "NAME(1:5) // SUFFIX",
            "'(' // CODE // ')'",
            "A // B // C // D",
        ]

        for text in concat_expressions:
            with self.subTest(concat=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_function_references(self):
        """Test character function references in expressions (FORTRAN 77)

        Per ANSI X3.9-1978, character functions return character values
        and can be used as primaries in character expressions.
        """
        function_expressions = [
            "TRIM(NAME)",
            "CONCAT(A, B)",
            "UPPER(STR)",
            "FMT(X, Y, Z)",
        ]

        for text in function_expressions:
            with self.subTest(func_ref=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_string_literal_edge_cases(self):
        """Test STRING_LITERAL edge cases per ANSI X3.9-1978 section 4.7

        Character constants may contain any characters from the FORTRAN
        character set. An apostrophe within a character constant is
        represented by two consecutive apostrophes.
        """
        edge_cases = [
            "''",
            "' '",
            "'   '",
            "'A''B''C'",
            "'DON''T PANIC'",
            "'LINE1' // '/' // 'LINE2'",
        ]

        for text in edge_cases:
            with self.subTest(edge_case=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_expression_fixture(self):
        """Test CHARACTER expressions from fixture file"""
        fixture_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser",
            "character_expressions.f",
        )

        tree = self.parse(fixture_text, 'main_program')
        self.assertIsNotNone(tree)

    def test_proper_inheritance(self):
        """Test that FORTRAN77 properly inherits from FORTRAN66"""
        # Read the grammar file
        import os
        grammar_path = os.path.join('grammars', 'FORTRAN77Parser.g4')
        with open(grammar_path, 'r') as f:
            content = f.read()
        
        # Check for proper import
        self.assertIn('import FORTRAN66Parser', content,
                     "FORTRAN77 should import FORTRAN66")
        
        # Check that type_spec rule includes comment about ANTLR4 limitation
        # Count occurrences of type definitions in type_spec rule
        import re
        type_spec_match = re.search(
            r'type_spec\s*:(.*?)(?=\n\w|\nfragment|\Z)', 
            content, re.DOTALL
        )
        if type_spec_match:
            type_spec_content = type_spec_match.group(1)
            # Due to ANTLR4 limitations, must redefine all types
            # But should have comment explaining this
            self.assertIn('ANTLR4', content[:type_spec_match.start() + 500],
                         "Should document ANTLR4 limitation")
            self.assertIn('CHARACTER', type_spec_content,
                         "type_spec should define CHARACTER")
        
        # Test that inherited types still work
        test_cases = [
            ("INTEGER I", "INTEGER"),    # Inherited from FORTRAN I
            ("REAL X", "REAL"),          # Inherited from FORTRAN I
            ("LOGICAL FLAG", "LOGICAL"), # Inherited from FORTRAN IV
            ("COMPLEX Z", "COMPLEX"),    # Inherited from FORTRAN IV
            ("CHARACTER NAME", "CHARACTER") # New in FORTRAN 77
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(declaration=text):
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)
                self.assertIn(expected_type, tree.getText())


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)
