#!/usr/bin/env python3
"""
Test suite for strict fixed-form card layout semantics for FORTRAN 1957.

Tests the StrictFixedFormProcessor against authentic IBM 704 card-image
source files per C28-6003 (1958).

Reference: FORTRAN Automatic Coding System for the IBM 704 Data Processing
           System (Form C28-6003, October 1958) Chapter I.B
"""

import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT.parent / "tools"))
sys.path.insert(0, str(ROOT))

try:
    from strict_fixed_form import (
        StrictFixedFormProcessor,
        validate_strict_fixed_form,
        validate_strict_fixed_form_1957,
        convert_to_lenient,
        convert_to_lenient_1957,
        CardType,
        Card,
        DIALECT_CONFIG,
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


class TestDialectConfiguration(unittest.TestCase):
    """Test dialect configuration for FORTRAN 1957 vs FORTRAN II."""

    def test_dialect_1957_config(self):
        """Test FORTRAN 1957 dialect configuration."""
        config = DIALECT_CONFIG["1957"]
        self.assertEqual(config["comment_markers"], ("C",))
        self.assertEqual(config["label_max"], 32767)
        self.assertEqual(config["reference"], "C28-6003")
        self.assertEqual(config["parser"], "FORTRANParser")

    def test_dialect_ii_config(self):
        """Test FORTRAN II dialect configuration."""
        config = DIALECT_CONFIG["II"]
        self.assertEqual(config["comment_markers"], ("C", "*"))
        self.assertEqual(config["label_max"], 99999)
        self.assertEqual(config["reference"], "C28-6000-2")
        self.assertEqual(config["parser"], "FORTRANIIParser")


class TestCardParsing1957(unittest.TestCase):
    """Test individual card parsing per C28-6003 Chapter I.B."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor(dialect="1957")

    def test_blank_card(self):
        """Test blank card recognition."""
        card = self.processor.parse_card("", 1)
        self.assertEqual(card.card_type, CardType.BLANK)
        self.assertIsNone(card.label)
        self.assertFalse(card.continuation)

    def test_comment_card_c(self):
        """Test C-comment card (column 1 = C) - authentic 1957."""
        card = self.processor.parse_card(
            "C     THIS IS A COMMENT                                    ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_comment_card_star_parsed(self):
        """Test *-comment card is parsed (but generates warning)."""
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
        line = "      X = 1.0" + " " * 59 + "00000010"
        card = self.processor.parse_card(line, 1)
        self.assertEqual(card.sequence_field, "00000010")


class TestValidation1957(unittest.TestCase):
    """Test validation of card sequences per C28-6003 rules."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_valid_simple_program(self):
        """Test validation of valid simple 1957 program."""
        source = """\
C     SIMPLE PROGRAM
      X = 1.0
      END
"""
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid)
        self.assertEqual(len(result.errors), 0)

    def test_invalid_label_alphabetic(self):
        """Test detection of alphabetic characters in label field."""
        source = """\
ABCDE X = 1.0
      END
"""
        result = validate_strict_fixed_form_1957(source)
        self.assertFalse(result.valid)
        self.assertTrue(any("label" in e.message.lower() for e in result.errors))

    def test_invalid_continuation_with_label(self):
        """Test detection of label on continuation card."""
        source = """\
      X = A +
  1001         B + C
      END
"""
        result = validate_strict_fixed_form_1957(source)
        self.assertFalse(result.valid)
        self.assertTrue(
            any("continuation" in e.message.lower() and "label" in e.message.lower()
                for e in result.errors)
        )

    def test_label_range_1957_valid(self):
        """Test valid label range (1-32767) for 1957."""
        valid_labels = ["1", "100", "32767"]
        for label in valid_labels:
            source = f"{label:>5} CONTINUE\n      END\n"
            result = validate_strict_fixed_form_1957(source)
            with self.subTest(label=label):
                self.assertTrue(result.valid, f"Label {label} should be valid")

    def test_label_range_1957_invalid(self):
        """Test invalid label (>32767) for 1957."""
        source = "99999 CONTINUE\n      END\n"
        result = validate_strict_fixed_form_1957(source)
        self.assertFalse(result.valid)
        self.assertTrue(
            any("out of range" in e.message.lower() for e in result.errors)
        )

    def test_star_comment_warning_1957(self):
        """Test that * comment generates warning in 1957 mode."""
        source = """\
*     NOT HISTORICAL COMMENT
      END
"""
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid)
        self.assertTrue(
            any("not historical" in w.message.lower() for w in result.warnings)
        )

    def test_star_comment_no_warning_ii(self):
        """Test that * comment does not generate warning in II mode."""
        source = """\
*     VALID FORTRAN II COMMENT
      END
"""
        result = validate_strict_fixed_form(source, dialect="II")
        self.assertTrue(result.valid)
        self.assertEqual(len(result.warnings), 0)


class TestLenientConversion1957(unittest.TestCase):
    """Test conversion from strict to lenient format for 1957."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_comment_conversion(self):
        """Test C-comments converted to !-comments."""
        source = """\
C     THIS IS A COMMENT
      END
"""
        lenient, _ = convert_to_lenient_1957(source)
        self.assertIn("!", lenient)
        self.assertNotIn("C     ", lenient.split("\n")[0])

    def test_label_preservation(self):
        """Test statement labels preserved in lenient form."""
        source = """\
  100 CONTINUE
      END
"""
        lenient, _ = convert_to_lenient_1957(source)
        self.assertIn("100", lenient)
        self.assertIn("CONTINUE", lenient)

    def test_continuation_joining(self):
        """Test continuation cards joined into single statements."""
        source = """\
      X = A + B +
     1    C + D
      END
"""
        lenient, _ = convert_to_lenient_1957(source)
        lines = [line for line in lenient.split("\n")
                 if line.strip() and not line.startswith("!")]
        stmt_line = lines[0]
        self.assertIn("A + B +", stmt_line)
        self.assertIn("C + D", stmt_line)

    def test_sequence_field_stripped(self):
        """Test sequence numbers (cols 73-80) stripped."""
        source = """\
      X = 1.0                                                           00000010
      END                                                               00000020
"""
        lenient, _ = convert_to_lenient_1957(source)
        self.assertNotIn("00000010", lenient)
        self.assertNotIn("00000020", lenient)


class TestFixtures1957(unittest.TestCase):
    """Test parsing of strict fixed-form 1957 fixtures."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_valid_arithmetic_fixture(self):
        """Test valid_arithmetic.f fixture validates."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "valid_arithmetic.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_do_loop_fixture(self):
        """Test valid_do_loop.f fixture validates."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "valid_do_loop.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_continuation_fixture(self):
        """Test valid_continuation.f fixture validates."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "valid_continuation.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_if_goto_fixture(self):
        """Test valid_if_goto.f fixture validates."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "valid_if_goto.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_label_range_fixture(self):
        """Test label_range.f fixture validates."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "label_range.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_invalid_label_alpha_fixture(self):
        """Test invalid_label_alpha.f fixture fails validation."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "invalid_label_alpha.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertFalse(result.valid)

    def test_invalid_continuation_labeled_fixture(self):
        """Test invalid_continuation_labeled.f fixture fails validation."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "invalid_continuation_labeled.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertFalse(result.valid)

    def test_invalid_label_out_of_range_fixture(self):
        """Test invalid_label_out_of_range.f fixture fails validation."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "invalid_label_out_of_range.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertFalse(result.valid)
        self.assertTrue(
            any("out of range" in e.message.lower() for e in result.errors)
        )

    def test_star_comment_warning_fixture(self):
        """Test star_comment_warning.f generates warning."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "star_comment_warning.f"
        )
        result = validate_strict_fixed_form_1957(source)
        self.assertTrue(result.valid)
        self.assertTrue(
            any("not historical" in w.message.lower() for w in result.warnings)
        )


class TestIntegrationWithParser1957(unittest.TestCase):
    """Test that converted lenient output parses with FORTRANParser."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

        sys.path.insert(0, str(ROOT.parent / "grammars/generated/early"))
        try:
            from antlr4 import InputStream, CommonTokenStream
            from FORTRANLexer import FORTRANLexer
            from FORTRANParser import FORTRANParser
            self.InputStream = InputStream
            self.CommonTokenStream = CommonTokenStream
            self.FORTRANLexer = FORTRANLexer
            self.FORTRANParser = FORTRANParser
            self.parser_available = True
        except ImportError:
            self.parser_available = False

    def parse_lenient(self, source):
        """Parse lenient-form source with FORTRANParser."""
        if not self.parser_available:
            self.skipTest("FORTRANParser not available")

        input_stream = self.InputStream(source)
        lexer = self.FORTRANLexer(input_stream)
        token_stream = self.CommonTokenStream(lexer)
        parser = self.FORTRANParser(token_stream)
        return parser

    def test_converted_arithmetic_parses(self):
        """Test converted valid_arithmetic.f parses."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "valid_arithmetic.f"
        )
        lenient, result = convert_to_lenient_1957(source, strip_comments=True)
        self.assertTrue(result.valid)

        if self.parser_available:
            parser = self.parse_lenient(lenient)
            tree = parser.program_unit_core()
            self.assertEqual(parser.getNumberOfSyntaxErrors(), 0)

    def test_converted_do_loop_parses(self):
        """Test converted valid_do_loop.f parses."""
        source = load_fixture(
            "FORTRAN", "test_strict_fixed_form", "valid_do_loop.f"
        )
        lenient, result = convert_to_lenient_1957(source, strip_comments=True)
        self.assertTrue(result.valid)

        if self.parser_available:
            parser = self.parse_lenient(lenient)
            tree = parser.program_unit_core()
            self.assertEqual(parser.getNumberOfSyntaxErrors(), 0)


if __name__ == "__main__":
    unittest.main(verbosity=2)
