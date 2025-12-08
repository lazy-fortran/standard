#!/usr/bin/env python3
"""
Test suite for strict fixed-form card layout semantics for FORTRAN II.

Tests the StrictFixedFormProcessor against authentic IBM 704 card-image
source files per C28-6000-2 (1958).

Reference: IBM FORTRAN II for the IBM 704 Data Processing System
           (Form C28-6000-2, 1958) Part I, Chapter 2
"""

import sys
import os
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT.parent / "tools"))
sys.path.insert(0, str(ROOT))

try:
    from strict_fixed_form import (
        StrictFixedFormProcessor,
        validate_strict_fixed_form,
        convert_to_lenient,
        CardType,
        Card,
    )
    PROCESSOR_AVAILABLE = True
except ImportError as e:
    print(f"Import error: {e}")
    PROCESSOR_AVAILABLE = False

try:
    from fixture_utils import load_fixture
except ImportError:
    def load_fixture(standard, test_name, fixture_name):
        fixture_path = ROOT / "fixtures" / standard / test_name / fixture_name
        with open(fixture_path, "r") as f:
            return f.read()


class TestCardParsing(unittest.TestCase):
    """Test individual card parsing per C28-6000-2 Chapter 2."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor()

    def test_blank_card(self):
        """Test blank card recognition."""
        card = self.processor.parse_card("", 1)
        self.assertEqual(card.card_type, CardType.BLANK)
        self.assertIsNone(card.label)
        self.assertFalse(card.continuation)

    def test_comment_card_c(self):
        """Test C-comment card (column 1 = C)."""
        card = self.processor.parse_card(
            "C     THIS IS A COMMENT                                    ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_comment_card_star(self):
        """Test *-comment card (column 1 = *)."""
        card = self.processor.parse_card(
            "*     THIS IS ALSO A COMMENT                               ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_statement_card_no_label(self):
        """Test statement card without label."""
        card = self.processor.parse_card(
            "      X = 1.0                                               ", 1
        )
        self.assertEqual(card.card_type, CardType.STATEMENT)
        self.assertIsNone(card.label)
        self.assertFalse(card.continuation)
        self.assertIn("X = 1.0", card.statement_text)

    def test_statement_card_with_label(self):
        """Test statement card with label in columns 1-5."""
        card = self.processor.parse_card(
            "  100 CONTINUE                                              ", 1
        )
        self.assertEqual(card.card_type, CardType.STATEMENT)
        self.assertEqual(card.label, "100")
        self.assertFalse(card.continuation)
        self.assertIn("CONTINUE", card.statement_text)

    def test_continuation_card(self):
        """Test continuation card (column 6 non-blank)."""
        card = self.processor.parse_card(
            "     1                    G, H, I)                          ", 1
        )
        self.assertEqual(card.card_type, CardType.CONTINUATION)
        self.assertTrue(card.continuation)
        self.assertIsNone(card.label)

    def test_sequence_field_extraction(self):
        """Test extraction of sequence field (columns 73-80)."""
        line = "      X = 1.0                                                           00000010"
        card = self.processor.parse_card(line, 1)
        self.assertEqual(card.sequence_field, "00000010")

    def test_column_field_accessors(self):
        """Test column field accessor properties."""
        line = "  100 X = Y                                                             00000020"
        card = self.processor.parse_card(line, 1)
        self.assertEqual(card.column_1_5, "  100")
        self.assertEqual(card.column_6, " ")
        self.assertIn("X = Y", card.column_7_72)
        self.assertEqual(card.column_73_80, "00000020")


class TestValidation(unittest.TestCase):
    """Test validation of card sequences per C28-6000-2 rules."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor()

    def test_valid_simple_program(self):
        """Test validation of valid simple program."""
        source = """\
C     SIMPLE PROGRAM
      X = 1.0
      END
"""
        result = validate_strict_fixed_form(source)
        self.assertTrue(result.valid)
        self.assertEqual(len(result.errors), 0)

    def test_invalid_label_alphabetic(self):
        """Test detection of alphabetic characters in label field."""
        source = """\
ABCDE X = 1.0
      END
"""
        result = validate_strict_fixed_form(source)
        self.assertFalse(result.valid)
        self.assertTrue(any("label" in e.message.lower() for e in result.errors))

    def test_invalid_continuation_with_label(self):
        """Test detection of label on continuation card."""
        # Line 2 has label 10 in cols 1-5 and 1 in col 6 (continuation mark)
        # This is invalid: continuation cards must have blank cols 1-5
        source = """\
      SUBROUTINE TEST(A, B,
   101               C, D)
      END
"""
        result = validate_strict_fixed_form(source)
        self.assertFalse(result.valid)
        self.assertTrue(
            any("continuation" in e.message.lower() and "label" in e.message.lower()
                for e in result.errors)
        )

    def test_invalid_continuation_first_card(self):
        """Test detection of continuation as first card."""
        source = """\
     1 X = Y
      END
"""
        result = validate_strict_fixed_form(source)
        self.assertFalse(result.valid)
        self.assertTrue(
            any("first" in e.message.lower() and "continuation" in e.message.lower()
                for e in result.errors)
        )

    def test_label_range_validation(self):
        """Test label range (1-99999) validation."""
        valid_labels = ["1", "99999", "100", "12345"]
        for label in valid_labels:
            source = f"{label:>5} CONTINUE\n      END\n"
            result = validate_strict_fixed_form(source)
            with self.subTest(label=label):
                self.assertTrue(result.valid, f"Label {label} should be valid")


class TestLenientConversion(unittest.TestCase):
    """Test conversion from strict to lenient format."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor()

    def test_comment_conversion(self):
        """Test C-comments converted to !-comments."""
        source = """\
C     THIS IS A COMMENT
      END
"""
        lenient, _ = convert_to_lenient(source)
        self.assertIn("!", lenient)
        self.assertNotIn("C     ", lenient.split("\n")[0])

    def test_label_preservation(self):
        """Test statement labels preserved in lenient form."""
        source = """\
  100 CONTINUE
      END
"""
        lenient, _ = convert_to_lenient(source)
        self.assertIn("100", lenient)
        self.assertIn("CONTINUE", lenient)

    def test_continuation_joining(self):
        """Test continuation cards joined into single statements."""
        source = """\
      X = A + B +
     1    C + D
      END
"""
        lenient, _ = convert_to_lenient(source)
        lines = [l for l in lenient.split("\n") if l.strip() and not l.startswith("!")]
        stmt_line = lines[0]
        self.assertIn("A + B +", stmt_line)
        self.assertIn("C + D", stmt_line)

    def test_sequence_field_stripped(self):
        """Test sequence numbers (cols 73-80) stripped."""
        source = """\
      X = 1.0                                                           00000010
      END                                                               00000020
"""
        lenient, _ = convert_to_lenient(source)
        self.assertNotIn("00000010", lenient)
        self.assertNotIn("00000020", lenient)


class TestFixtures(unittest.TestCase):
    """Test parsing of strict fixed-form fixtures."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_valid_subroutine_fixture(self):
        """Test valid_subroutine.f fixture validates."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_subroutine.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_function_fixture(self):
        """Test valid_function.f fixture validates."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_function.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_main_fixture(self):
        """Test valid_main.f fixture validates."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_main.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_continuation_fixture(self):
        """Test valid_continuation.f fixture validates."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_continuation.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_star_comment_fixture(self):
        """Test star_comment.f fixture validates."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "star_comment.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_invalid_label_alpha_fixture(self):
        """Test invalid_label_alpha.f fixture fails validation."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "invalid_label_alpha.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertFalse(result.valid)

    def test_invalid_continuation_labeled_fixture(self):
        """Test invalid_continuation_labeled.f fixture fails validation."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "invalid_continuation_labeled.f"
        )
        result = validate_strict_fixed_form(source)
        self.assertFalse(result.valid)


class TestIntegrationWithParser(unittest.TestCase):
    """Test that converted lenient output parses with FORTRANIIParser."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

        sys.path.insert(0, str(ROOT.parent / "grammars/generated/early"))
        try:
            from antlr4 import InputStream, CommonTokenStream
            from FORTRANIILexer import FORTRANIILexer
            from FORTRANIIParser import FORTRANIIParser
            self.InputStream = InputStream
            self.CommonTokenStream = CommonTokenStream
            self.FORTRANIILexer = FORTRANIILexer
            self.FORTRANIIParser = FORTRANIIParser
            self.parser_available = True
        except ImportError:
            self.parser_available = False

    def parse_lenient(self, source):
        """Parse lenient-form source with FORTRANIIParser."""
        if not self.parser_available:
            self.skipTest("FORTRANIIParser not available")

        input_stream = self.InputStream(source)
        lexer = self.FORTRANIILexer(input_stream)
        token_stream = self.CommonTokenStream(lexer)
        parser = self.FORTRANIIParser(token_stream)
        return parser

    def test_converted_subroutine_parses(self):
        """Test converted valid_subroutine.f parses."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_subroutine.f"
        )
        lenient, result = convert_to_lenient(source, strip_comments=True)
        self.assertTrue(result.valid)

        if self.parser_available:
            parser = self.parse_lenient(lenient)
            tree = parser.subroutine_subprogram()
            self.assertEqual(parser.getNumberOfSyntaxErrors(), 0)
            self.assertIn("SWAP", tree.getText())

    def test_converted_function_parses(self):
        """Test converted valid_function.f parses."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_function.f"
        )
        lenient, result = convert_to_lenient(source, strip_comments=True)
        self.assertTrue(result.valid)

        if self.parser_available:
            parser = self.parse_lenient(lenient)
            tree = parser.function_subprogram()
            self.assertEqual(parser.getNumberOfSyntaxErrors(), 0)
            self.assertIn("MAX", tree.getText())

    def test_converted_main_parses(self):
        """Test converted valid_main.f parses."""
        source = load_fixture(
            "FORTRANII", "test_strict_fixed_form", "valid_main.f"
        )
        lenient, result = convert_to_lenient(source, strip_comments=True)
        self.assertTrue(result.valid)

        if self.parser_available:
            parser = self.parse_lenient(lenient)
            tree = parser.main_program()
            self.assertEqual(parser.getNumberOfSyntaxErrors(), 0)


if __name__ == "__main__":
    unittest.main(verbosity=2)
