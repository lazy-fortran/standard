#!/usr/bin/env python3
"""
Test suite for strict fixed-form card layout semantics for FORTRAN 66.

Tests the StrictFixedFormProcessor against authentic fixed-form source files
per ANSI X3.9-1966 Section 3.3 (Source Program Format).

Reference: ANSI X3.9-1966 "USA Standard FORTRAN"
           (American National Standards Institute, March 7, 1966)
           Section 3.3: Source Program Format
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
        validate_strict_fixed_form_66,
        convert_to_lenient_66,
        CardType,
    )
    PROCESSOR_AVAILABLE = True
except ImportError as e:
    print(f"Import error: {e}")
    PROCESSOR_AVAILABLE = False


class TestFORTRAN66CardParsing(unittest.TestCase):
    """Test individual card parsing per X3.9-1966 Section 3.3."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor(dialect="66")

    def test_blank_card(self):
        """Test blank card recognition."""
        card = self.processor.parse_card("", 1)
        self.assertEqual(card.card_type, CardType.BLANK)
        self.assertIsNone(card.label)
        self.assertFalse(card.continuation)

    def test_comment_card_c(self):
        """Test C-comment card (column 1 = C)."""
        card = self.processor.parse_card(
            "C     THIS IS A COMMENT CARD                                   ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_comment_card_star(self):
        """Test *-comment card (column 1 = *)."""
        card = self.processor.parse_card(
            "*     ANOTHER COMMENT STYLE                                     ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_statement_card_with_label(self):
        """Test statement card with label in columns 1-5."""
        card = self.processor.parse_card(
            "  100 CONTINUE                                                  ", 1
        )
        self.assertEqual(card.card_type, CardType.STATEMENT)
        self.assertEqual(card.label, "100")
        self.assertFalse(card.continuation)
        self.assertIn("CONTINUE", card.statement_text)

    def test_continuation_card(self):
        """Test continuation card (column 6 non-blank)."""
        card = self.processor.parse_card(
            "     1    LOGICAL X, Y                                           ", 1
        )
        self.assertEqual(card.card_type, CardType.CONTINUATION)
        self.assertTrue(card.continuation)
        self.assertIsNone(card.label)

    def test_sequence_field_extraction(self):
        """Test extraction of sequence field (columns 73-80)."""
        line = "      X = 1.0                                                           00000010"
        card = self.processor.parse_card(line, 1)
        self.assertEqual(card.sequence_field, "00000010")


class TestFORTRAN66Validation(unittest.TestCase):
    """Test validation of card sequences per X3.9-1966 Section 3.3."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_valid_simple_program(self):
        """Test validation of valid simple FORTRAN 66 program."""
        source = """\
C     SIMPLE FORTRAN 66 PROGRAM
      PROGRAM COMPUTE
      REAL X, Y, Z
      X = 3.14
      Y = 2.71
      Z = X + Y
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")
        self.assertEqual(len(result.errors), 0)

    def test_valid_with_labels(self):
        """Test validation of program with statement labels."""
        source = """\
      PROGRAM TEST
  100 X = 1.0
  200 Y = 2.0
      GOTO 300
  300 CONTINUE
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_fortran66_types(self):
        """Test FORTRAN 66 type declarations validate."""
        source = """\
      PROGRAM TYPES
      INTEGER I
      REAL R
      LOGICAL L
      DOUBLE PRECISION D
      COMPLEX C
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_invalid_label_alphabetic(self):
        """Test detection of alphabetic characters in label field."""
        source = """\
ABCDE X = 1.0
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertFalse(result.valid)
        self.assertTrue(any("label" in e.message.lower() for e in result.errors))

    def test_valid_max_label(self):
        """Test that maximum label 99999 is accepted."""
        # Maximum valid is 99999 (right-justified in columns 1-5)
        source_valid = """\
99999 CONTINUE
      END
"""
        result_valid = validate_strict_fixed_form_66(source_valid)
        self.assertTrue(result_valid.valid, f"Label 99999 should be valid. Errors: {result_valid.errors}")

    def test_invalid_continuation_with_label(self):
        """Test detection of label on continuation card."""
        source = """\
      LOGICAL X, Y,
   101          Z
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertFalse(result.valid)

    def test_valid_continuation_sequence(self):
        """Test valid continuation card sequence."""
        source = """\
      REAL A, B, C, D,
     1     E, F, G, H
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")


class TestFORTRAN66LenientConversion(unittest.TestCase):
    """Test conversion from strict to lenient format."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_comment_conversion(self):
        """Test C-comments converted to !-comments."""
        source = """\
C     THIS IS A COMMENT
      END
"""
        lenient, result = convert_to_lenient_66(source)
        self.assertTrue(result.valid)
        self.assertIn("!", lenient)

    def test_label_preservation(self):
        """Test statement labels preserved in lenient form."""
        source = """\
  100 CONTINUE
      END
"""
        lenient, result = convert_to_lenient_66(source)
        self.assertTrue(result.valid)
        self.assertIn("100", lenient)
        self.assertIn("CONTINUE", lenient)

    def test_continuation_joining(self):
        """Test continuation cards joined into single statements."""
        source = """\
      X = A + B +
     1    C + D
      END
"""
        lenient, result = convert_to_lenient_66(source)
        self.assertTrue(result.valid)
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
        lenient, result = convert_to_lenient_66(source)
        self.assertTrue(result.valid)
        self.assertNotIn("00000010", lenient)
        self.assertNotIn("00000020", lenient)


class TestFORTRAN66DialectConfig(unittest.TestCase):
    """Test FORTRAN 66 dialect configuration."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor(dialect="66")

    def test_dialect_config_labels(self):
        """Test FORTRAN 66 allows labels up to 99999."""
        # Valid: 99999
        source = """\
99999 CONTINUE
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid)

    def test_dialect_config_comment_markers(self):
        """Test FORTRAN 66 allows both C and * comment markers."""
        c_comment = "C     COMMENT\n      END\n"
        star_comment = "*     COMMENT\n      END\n"

        result_c = validate_strict_fixed_form_66(c_comment)
        result_star = validate_strict_fixed_form_66(star_comment)

        self.assertTrue(result_c.valid)
        self.assertTrue(result_star.valid)


class TestFORTRAN66RealWorld(unittest.TestCase):
    """Test real-world FORTRAN 66 code patterns."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_formatted_io_program(self):
        """Test typical FORTRAN 66 formatted I/O program."""
        source = """\
C     FORMATTED I/O EXAMPLE
      PROGRAM IOTEST
      INTEGER N
      REAL X, Y
      READ (5, 100) N, X, Y
  100 FORMAT (I5, 2F10.2)
      Z = X + Y
      WRITE (6, 200) Z
  200 FORMAT (F15.4)
      STOP
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")
        lenient, _ = convert_to_lenient_66(source, strip_comments=True)
        self.assertIn("PROGRAM", lenient)
        self.assertIn("FORMAT", lenient)

    def test_subroutine_example(self):
        """Test FORTRAN 66 subroutine with multiple continuation lines."""
        source = """\
C     SUBROUTINE EXAMPLE
      SUBROUTINE PROCESS(A, B, C,
     1                   D, E, F)
      REAL A, B, C, D, E, F
      A = B + C
      D = E * F
      RETURN
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_common_block_example(self):
        """Test FORTRAN 66 program with COMMON blocks."""
        source = """\
      PROGRAM COMMONTEST
      COMMON /DATA/ X, Y, Z
      REAL X, Y, Z
      X = 1.0
      Y = 2.0
      Z = 3.0
      END
"""
        result = validate_strict_fixed_form_66(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")


if __name__ == "__main__":
    unittest.main(verbosity=2)
