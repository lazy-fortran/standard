#!/usr/bin/env python3
"""
Test suite for strict fixed-form card layout semantics for FORTRAN 77.

Tests strict card parsing/validation wrappers against ANSI X3.9-1978
fixed-form source assumptions (label field, continuation column, statement
field, sequence field).
"""

import os
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT.parent / "tools"))
sys.path.insert(0, str(ROOT))

try:
    from strict_fixed_form import (
        CardType,
        StrictFixedFormProcessor,
        convert_to_lenient_77,
        validate_strict_fixed_form_77,
    )

    PROCESSOR_AVAILABLE = True
except ImportError as e:
    print(f"Import error: {e}")
    PROCESSOR_AVAILABLE = False


class TestFORTRAN77CardParsing(unittest.TestCase):
    """Basic card parsing checks for FORTRAN 77 strict mode."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")
        self.processor = StrictFixedFormProcessor(dialect="77")

    def test_comment_and_statement_cards(self):
        comment = self.processor.parse_card("C     COMMENT CARD", 1)
        statement = self.processor.parse_card("  100 CONTINUE", 2)

        self.assertEqual(comment.card_type, CardType.COMMENT)
        self.assertEqual(statement.card_type, CardType.STATEMENT)
        self.assertEqual(statement.label, "100")

    def test_continuation_card(self):
        continuation = self.processor.parse_card("     1    A = B + C", 1)
        self.assertEqual(continuation.card_type, CardType.CONTINUATION)
        self.assertTrue(continuation.continuation)


class TestFORTRAN77Validation(unittest.TestCase):
    """Validation behavior for FORTRAN 77 strict mode wrappers."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_valid_simple_program(self):
        source = """\
C     FORTRAN 77 SAMPLE
      PROGRAM DEMO
      INTEGER I
      I = 1
      END
"""
        result = validate_strict_fixed_form_77(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")

    def test_invalid_alpha_label(self):
        source = """\
AB123 CONTINUE
      END
"""
        result = validate_strict_fixed_form_77(source)
        self.assertFalse(result.valid)
        self.assertTrue(any("label" in err.message.lower() for err in result.errors))

    def test_invalid_continuation_with_label(self):
        source = """\
      REAL A, B,
  1001      C
      END
"""
        result = validate_strict_fixed_form_77(source)
        self.assertFalse(result.valid)

    def test_max_label_valid(self):
        source = """\
99999 CONTINUE
      END
"""
        result = validate_strict_fixed_form_77(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")


class TestFORTRAN77LenientConversion(unittest.TestCase):
    """Conversion behavior from strict card form to lenient parser input."""

    def setUp(self):
        if not PROCESSOR_AVAILABLE:
            self.skipTest("StrictFixedFormProcessor not available")

    def test_comment_conversion(self):
        source = """\
C     COMMENT
      END
"""
        lenient, result = convert_to_lenient_77(source)
        self.assertTrue(result.valid)
        self.assertIn("!", lenient)

    def test_continuation_joining(self):
        source = """\
      X = A + B +
     1    C + D
      END
"""
        lenient, result = convert_to_lenient_77(source)
        self.assertTrue(result.valid, f"Errors: {result.errors}")
        self.assertIn("A + B +", lenient)
        self.assertIn("C + D", lenient)


if __name__ == "__main__":
    os.environ["OMP_NUM_THREADS"] = "24"
    unittest.main()
