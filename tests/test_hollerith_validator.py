#!/usr/bin/env python3
"""
Test suite for Hollerith constant validator
Per ANSI X3.9-1966 Section 4.7
"""

import sys
import unittest
from pathlib import Path

# Add tools directory to path
TOOLS_DIR = Path(__file__).resolve().parent.parent / "tools"
sys.path.insert(0, str(TOOLS_DIR))

from hollerith_validator import (
    validate_hollerith,
    validate_hollerith_in_format,
    report_hollerith_violations,
)


class TestHollerthValidator(unittest.TestCase):
    """Test Hollerith constant validation"""

    def test_valid_hollerith_formats(self):
        """Test valid Hollerith constants"""
        valid_cases = [
            "5HHELLO",
            "1H*",
            "10HFORTRAN 66",
            "3H***",
            "11HHELLO WORLD",
            "7HX = 1.5",
            "2H  ",  # Two spaces
            "1H\t",  # One tab character
        ]

        for holleri in valid_cases:
            with self.subTest(hollerith=holleri):
                is_valid, error = validate_hollerith(holleri)
                self.assertTrue(
                    is_valid,
                    f"{holleri} should be valid but got error: {error}"
                )
                self.assertIsNone(error)

    def test_invalid_hollerith_count_too_high(self):
        """Test Hollerith with count too high (n > actual chars)"""
        cases = [
            "5HHELL",   # Declared 5, has 4
            "10HTEST",  # Declared 10, has 4
            "3HH",      # Declared 3, has 1
        ]

        for holleri in cases:
            with self.subTest(hollerith=holleri):
                is_valid, error = validate_hollerith(holleri)
                self.assertFalse(is_valid)
                self.assertIn("count mismatch", error)

    def test_invalid_hollerith_count_too_low(self):
        """Test Hollerith with count too low (n < actual chars)"""
        cases = [
            "3HHELLO",   # Declared 3, has 5
            "2HHELLO",   # Declared 2, has 5
            "1HHELLO",   # Declared 1, has 5
        ]

        for holleri in cases:
            with self.subTest(hollerith=holleri):
                is_valid, error = validate_hollerith(holleri)
                self.assertFalse(is_valid)
                self.assertIn("count mismatch", error)

    def test_invalid_hollerith_zero_count(self):
        """Test Hollerith with zero count"""
        cases = [
            "0H",
            "0HTEST",
        ]

        for holleri in cases:
            with self.subTest(hollerith=holleri):
                is_valid, error = validate_hollerith(holleri)
                self.assertFalse(is_valid)
                self.assertIn("count must be >= 1", error)

    def test_invalid_hollerith_format(self):
        """Test invalid Hollerith format strings"""
        cases = [
            "HHELLO",     # Missing count
            "H5HELLO",    # Count after H
            "HELLO",      # No H
        ]

        for holleri in cases:
            with self.subTest(hollerith=holleri):
                is_valid, error = validate_hollerith(holleri)
                self.assertFalse(is_valid)
                self.assertIn("valid Hollerith format", error)

    def test_hollerith_in_format_statement(self):
        """Test validation of Hollerith in FORMAT statements"""
        valid_format = "100 FORMAT(5HHELLO, I5, 6HWORLD!)"
        errors = validate_hollerith_in_format(valid_format)
        self.assertEqual([], errors)

        invalid_format = "100 FORMAT(5HHELL, I5, 3HHELLO)"
        errors = validate_hollerith_in_format(invalid_format)
        self.assertEqual(2, len(errors))

    def test_report_generation(self):
        """Test human-readable error report generation"""
        # Valid format - no errors
        report = report_hollerith_violations("100 FORMAT(5HHELLO)")
        self.assertEqual("", report)

        # Invalid format - has errors
        report = report_hollerith_violations("100 FORMAT(5HHELL)")
        self.assertIn("Hollerith validation errors", report)
        self.assertIn("5HHELL", report)
        self.assertIn("count mismatch", report)

    def test_hollerith_with_special_characters(self):
        """Test Hollerith with special characters and spacing"""
        valid_cases = [
            "5H$@#*!",   # Special characters
            "3H   ",     # Three spaces
            "7HH+E=mc2",  # Mixed alphanumeric and special (7 chars)
        ]

        for holleri in valid_cases:
            with self.subTest(hollerith=holleri):
                is_valid, error = validate_hollerith(holleri)
                self.assertTrue(
                    is_valid,
                    f"Should accept special chars: {error}"
                )

    def test_hollerith_large_counts(self):
        """Test Hollerith with large counts"""
        # Generate a valid large Hollerith
        large_text = "X" * 100
        large_holleri = f"100H{large_text}"
        is_valid, error = validate_hollerith(large_holleri)
        self.assertTrue(is_valid)

        # Invalid: declared too high
        invalid_holleri = f"101H{large_text}"
        is_valid, error = validate_hollerith(invalid_holleri)
        self.assertFalse(is_valid)


if __name__ == "__main__":
    unittest.main(verbosity=2)
