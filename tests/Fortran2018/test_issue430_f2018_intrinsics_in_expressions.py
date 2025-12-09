#!/usr/bin/env python3
"""Issue #430 - F2018 Intrinsics in Expression Parsing Tests

Tests that F2018 intrinsic function calls can be parsed in expression contexts
after the primary rule override connects intrinsic_function_call_f2018.

ISO/IEC 1539-1:2018 Section 16.9: Intrinsic procedures
ISO/IEC 1539-1:2018 R1023: Primary expressions must include function references
"""

import sys
from pathlib import Path

import pytest

# Ensure we can import shared test fixtures utilities
sys.path.append(str(Path(__file__).parent.parent))
from fixture_utils import load_fixture

# Add grammars directory to Python path for generated parsers
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser


class TestF2018IntrinsicsInExpressions:
    """Test F2018 intrinsic function calls in expression contexts"""

    def parse_code(self, code):
        """Parse F2018 code and return (tree, error_count)"""
        try:
            input_stream = InputStream(code)
            lexer = Fortran2018Lexer(input_stream)
            parser = Fortran2018Parser(CommonTokenStream(lexer))
            tree = parser.program_unit_f2018()
            errors = parser.getNumberOfSyntaxErrors()
            return tree, errors
        except Exception as e:
            return None, 999

    def test_image_status_in_expression(self):
        """IMAGE_STATUS intrinsic must parse in expressions (ISO 16.9.81)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "image_status_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "IMAGE_STATUS expression parsing should not crash"
        assert errors == 0, f"IMAGE_STATUS in expression should parse with 0 errors, got {errors}"

    def test_failed_images_in_expression(self):
        """FAILED_IMAGES intrinsic must parse in expressions (ISO 16.9.73)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "failed_images_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "FAILED_IMAGES expression parsing should not crash"
        assert errors == 0, f"FAILED_IMAGES in expression should parse with 0 errors, got {errors}"

    def test_stopped_images_in_expression(self):
        """STOPPED_IMAGES intrinsic must parse in expressions (ISO 16.9.182)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "stopped_images_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "STOPPED_IMAGES expression parsing should not crash"
        assert errors == 0, f"STOPPED_IMAGES in expression should parse with 0 errors, got {errors}"

    def test_coshape_in_expression(self):
        """COSHAPE intrinsic must parse in expressions (ISO 16.9.52)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "coshape_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "COSHAPE expression parsing should not crash"
        assert errors == 0, f"COSHAPE in expression should parse with 0 errors, got {errors}"

    def test_team_number_in_expression(self):
        """TEAM_NUMBER intrinsic must parse in expressions (ISO 16.9.187)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "team_number_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "TEAM_NUMBER expression parsing should not crash"
        assert errors == 0, f"TEAM_NUMBER in expression should parse with 0 errors, got {errors}"

    def test_out_of_range_in_expression(self):
        """OUT_OF_RANGE intrinsic must parse in expressions (ISO 16.9.140)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "out_of_range_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "OUT_OF_RANGE expression parsing should not crash"
        assert errors == 0, f"OUT_OF_RANGE in expression should parse with 0 errors, got {errors}"

    def test_reduce_in_expression(self):
        """REDUCE intrinsic must parse in expressions (ISO 16.9.161)"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "reduce_expression.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "REDUCE expression parsing should not crash"
        assert errors == 0, f"REDUCE in expression should parse with 0 errors, got {errors}"

    def test_combined_f2018_intrinsics_in_expressions(self):
        """Multiple F2018 intrinsics parse correctly in various expression contexts"""
        code = load_fixture(
            "Fortran2018",
            "test_issue430_f2018_intrinsics_in_expressions",
            "combined_f2018_intrinsics_expressions.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Combined F2018 intrinsics expression parsing should not crash"
        assert errors == 0, f"Combined F2018 intrinsics in expressions should parse with 0 errors, got {errors}"
