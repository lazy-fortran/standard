#!/usr/bin/env python3
"""
Test suite for strict fixed-form card layout semantics for Fortran 90.

Tests the StrictFixedFormProcessor against Fortran 90 programs in strict
fixed-form per ISO/IEC 1539:1991 Section 3.3.

Reference: ISO/IEC 1539:1991 (WG5 N692) Section 3.3 - Source Form (Fixed)
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
        validate_strict_fixed_form_90,
        convert_to_lenient_90,
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


class TestFortran90CardParsing(unittest.TestCase):
    """Test individual card parsing per ISO/IEC 1539:1991 Section 3.3."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor(dialect="90")

    def test_fortran90_blank_card(self):
        """Test blank card recognition in Fortran 90."""
        card = self.processor.parse_card("", 1)
        self.assertEqual(card.card_type, CardType.BLANK)
        self.assertIsNone(card.label)
        self.assertFalse(card.continuation)

    def test_fortran90_comment_card_c(self):
        """Test C-comment card (column 1 = C) in Fortran 90."""
        card = self.processor.parse_card(
            "C     THIS IS A COMMENT                                    ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_fortran90_comment_card_star(self):
        """Test *-comment card (column 1 = *) in Fortran 90."""
        card = self.processor.parse_card(
            "*     THIS IS ALSO A COMMENT                               ", 1
        )
        self.assertEqual(card.card_type, CardType.COMMENT)

    def test_fortran90_statement_with_label(self):
        """Test statement card with label in Fortran 90."""
        card = self.processor.parse_card(
            "   10 CONTINUE                                              ", 1
        )
        self.assertEqual(card.card_type, CardType.STATEMENT)
        self.assertEqual(card.label, "10")
        self.assertFalse(card.continuation)

    def test_fortran90_continuation_card(self):
        """Test continuation card (column 6 non-blank) in Fortran 90."""
        card = self.processor.parse_card(
            "     1      C = A +                                          ", 1
        )
        self.assertEqual(card.card_type, CardType.CONTINUATION)
        self.assertTrue(card.continuation)

    def test_fortran90_sequence_field_extraction(self):
        """Test extraction of sequence field (columns 73-80) in Fortran 90."""
        line = "      REAL :: X                                                         00000010"
        card = self.processor.parse_card(line, 1)
        self.assertEqual(card.sequence_field, "00000010")


class TestFortran90Validation(unittest.TestCase):
    """Test validation of card sequences for Fortran 90."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_fortran90_valid_simple_program(self):
        """Test validation of valid simple Fortran 90 program."""
        source = """\
C     Simple program
      PROGRAM SIMPLE
      IMPLICIT NONE
      REAL :: X
      X = 1.0
      END PROGRAM SIMPLE
"""
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")
        self.assertEqual(len(result.errors), 0)

    def test_fortran90_valid_module(self):
        """Test validation of valid Fortran 90 module."""
        source = """\
C     Test module
      MODULE TEST_MOD
      IMPLICIT NONE
      CONTAINS
        SUBROUTINE TEST()
        END SUBROUTINE TEST
      END MODULE TEST_MOD
"""
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")
        self.assertEqual(len(result.errors), 0)

    def test_fortran90_invalid_alpha_label(self):
        """Test detection of alphabetic characters in label field."""
        source = """\
ABCDE X = 1.0
      END
"""
        result = validate_strict_fixed_form_90(source)
        self.assertFalse(result.valid)
        self.assertTrue(any("label" in e.message.lower() for e in result.errors))

    def test_fortran90_invalid_continuation_with_label(self):
        """Test detection of label on continuation card."""
        source = """\
C     Invalid: continuation with label
      REAL :: A, B
      A = 1.0 +
   101     2.0
      END
"""
        result = validate_strict_fixed_form_90(source)
        self.assertFalse(result.valid)
        self.assertTrue(
            any("continuation" in e.message.lower() and "label" in e.message.lower()
                for e in result.errors)
        )

    def test_fortran90_label_range_max(self):
        """Test label range maximum (99999) for Fortran 90."""
        source = "99999 CONTINUE\n      END\n"
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_fortran90_label_range_various(self):
        """Test various valid labels for Fortran 90."""
        test_cases = [
            ("    1 CONTINUE\n      END\n", True, "label 1"),
            ("   99 CONTINUE\n      END\n", True, "label 99"),
            ("  999 CONTINUE\n      END\n", True, "label 999"),
            (" 9999 CONTINUE\n      END\n", True, "label 9999"),
            ("99999 CONTINUE\n      END\n", True, "label 99999"),
        ]
        for source, expected_valid, desc in test_cases:
            with self.subTest(label=desc):
                result = validate_strict_fixed_form_90(source)
                self.assertEqual(
                    result.valid, expected_valid,
                    f"{desc}: Errors: {result.errors}"
                )


class TestFortran90LenientConversion(unittest.TestCase):
    """Test conversion from strict to lenient format for Fortran 90."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_fortran90_comment_conversion(self):
        """Test C-comments converted to !-comments in Fortran 90."""
        source = """\
C     THIS IS A COMMENT
      END
"""
        lenient, _ = convert_to_lenient_90(source)
        self.assertIn("!", lenient)

    def test_fortran90_star_comment_conversion(self):
        """Test *-comments converted to !-comments in Fortran 90."""
        source = """\
*     THIS IS A COMMENT
      END
"""
        lenient, _ = convert_to_lenient_90(source)
        self.assertIn("!", lenient)

    def test_fortran90_label_preservation(self):
        """Test statement labels preserved in lenient form."""
        source = """\
   10 CONTINUE
      END
"""
        lenient, _ = convert_to_lenient_90(source)
        self.assertIn("10", lenient)

    def test_fortran90_continuation_joining(self):
        """Test continuation cards joined into single statements."""
        source = """\
      REAL :: X
      X = 1.0 +
     &    2.0
      END
"""
        lenient, _ = convert_to_lenient_90(source)
        lines = [l for l in lenient.split("\n") if l.strip() and not l.startswith("!")]
        # Find the assignment statement
        assign_line = [l for l in lines if "=" in l and ("1.0" in l or "2.0" in l)]
        self.assertTrue(len(assign_line) > 0, "Assignment line not found")
        self.assertIn("1.0", assign_line[0])
        self.assertIn("2.0", assign_line[0])


class TestFortran90Fixtures(unittest.TestCase):
    """Test parsing of strict fixed-form Fortran 90 fixtures."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_valid_program_fixture(self):
        """Test valid_program.f90 fixture validates."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "valid_program.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_with_labels_fixture(self):
        """Test valid_with_labels.f90 fixture validates."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "valid_with_labels.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_with_continuation_fixture(self):
        """Test valid_with_continuation.f90 fixture validates."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "valid_with_continuation.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_star_comment_fixture(self):
        """Test valid_star_comment.f90 fixture validates."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "valid_star_comment.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_valid_module_fixture(self):
        """Test valid_module.f90 fixture validates."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "valid_module.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_invalid_alpha_label_fixture(self):
        """Test invalid_alpha_label.f90 fixture fails validation."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "invalid_alpha_label.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertFalse(result.valid)

    def test_invalid_continuation_with_label_fixture(self):
        """Test invalid_continuation_with_label.f90 fixture fails validation."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form",
            "invalid_continuation_with_label.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertFalse(result.valid)

    def test_invalid_label_out_of_range_fixture(self):
        """Test invalid_label_out_of_range.f90 fixture fails validation."""
        source = load_fixture(
            "Fortran90", "test_strict_fixed_form", "invalid_label_out_of_range.f90"
        )
        result = validate_strict_fixed_form_90(source)
        self.assertFalse(result.valid)


if __name__ == "__main__":
    unittest.main(verbosity=2)
