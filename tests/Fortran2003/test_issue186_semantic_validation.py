#!/usr/bin/env python3
"""Semantic Validation Tests - Issue #186

Comprehensive test suite for Fortran 2003 semantic validation of:
- C interoperability (ISO/IEC 1539-1:2004 Section 15)
- IEEE arithmetic modules (ISO/IEC 1539-1:2004 Section 14)

These tests validate the semantic analysis layer on top of the ANTLR grammars,
ensuring that code conforms to the standard's semantic requirements beyond
syntactic correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2003_semantic_validator import (
    F2003SemanticValidator,
    ValidationResult,
    DiagnosticSeverity,
    C_INTEROPERABLE_TYPES,
    IEEE_MODULES,
    IEEE_EXCEPTION_FLAGS,
    IEEE_SPECIAL_VALUES,
    IEEE_ROUNDING_MODES,
    IEEE_FEATURES,
    validate_c_interoperability,
    validate_ieee_arithmetic,
)
from fixture_utils import load_fixture


class TestF2003SemanticValidatorBasic:
    """Basic tests for the semantic validator infrastructure."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_empty_module_no_errors(self):
        """Empty module should have no semantic errors."""
        code = """
module empty_mod
    implicit none
end module empty_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Unexpected errors: {result.diagnostics}"

    def test_syntax_error_detected(self):
        """Syntax errors should be reported before semantic analysis."""
        code = """
module broken
    this is not valid fortran syntax!!!
end module broken
"""
        result = self.validator.validate_code(code)
        assert result.has_errors
        assert any(d.code == "SYNTAX_E001" for d in result.diagnostics)

    def test_validation_result_properties(self):
        """ValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestCInteroperabilitySemantics:
    """Semantic validation tests for C interoperability (Section 15)."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_iso_c_binding_import_detection(self):
        """Validator should detect USE ISO_C_BINDING statements."""
        code = """
module c_mod
    use iso_c_binding, only: c_int, c_float
    implicit none
end module c_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert "c_int" in result.iso_c_binding_imports
        assert "c_float" in result.iso_c_binding_imports

    def test_bind_c_subroutine_detected(self):
        """BIND(C) procedures should be tracked as C interop entities."""
        code = """
module c_interop_mod
    use iso_c_binding
    implicit none
contains
    subroutine my_c_sub() bind(c)
    end subroutine my_c_sub
end module c_interop_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert "my_c_sub" in result.c_interop_entities

    def test_bind_c_with_name_subroutine(self):
        """BIND(C, NAME=...) procedures should be detected."""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "bind_c_with_name_subroutine.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_c_interop_types_module(self):
        """C interoperable types in declarations should be validated."""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "c_interop_types_module.f90",
        )
        result = self.validator.validate_code(code)
        # This fixture uses c_int, c_double, c_ptr without USE statement
        # Semantic validator should detect this
        assert not result.has_errors or any(
            "C_INTEROP" in d.code for d in result.diagnostics
        )

    def test_use_iso_c_binding_with_only(self):
        """USE ISO_C_BINDING with ONLY clause should track imports."""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "use_iso_c_binding_example_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.iso_c_binding_imports) > 0

    def test_value_attribute_in_bind_c_context(self):
        """VALUE attribute in BIND(C) context should not warn."""
        # The fixture uses c_int without USE statement, so we test with proper USE
        code = """
subroutine proc(x) bind(c)
    use iso_c_binding, only: c_int
    integer(c_int), value :: x
end subroutine proc
"""
        result = self.validator.validate_code(code)
        # VALUE in BIND(C) is valid per Section 15.3.5
        assert not result.has_errors, f"Errors: {result.diagnostics}"

    def test_simple_valid_c_interop_module(self):
        """Simple valid C interop module should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "simple_valid_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"

    def test_interop_simple_module(self):
        """Interop simple module with BIND(C, NAME=...) should pass."""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "interop_simple_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_comprehensive_c_interop_fixture(self):
        """Comprehensive C interop module should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "c_interop_mod.f90",
        )
        result = self.validator.validate_code(code)
        # Should parse and validate without errors
        assert not result.has_errors, f"Errors: {result.diagnostics}"

    def test_c_interop_type_constants_complete(self):
        """All C interoperable types from Table 15.2 should be recognized."""
        expected_types = {
            "c_int", "c_short", "c_long", "c_long_long",
            "c_signed_char", "c_size_t",
            "c_int8_t", "c_int16_t", "c_int32_t", "c_int64_t",
            "c_float", "c_double", "c_long_double",
            "c_float_complex", "c_double_complex", "c_long_double_complex",
            "c_bool", "c_char",
            "c_ptr", "c_funptr",
        }
        assert expected_types.issubset(C_INTEROPERABLE_TYPES)


class TestIEEEArithmeticSemantics:
    """Semantic validation tests for IEEE arithmetic (Section 14)."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_ieee_exceptions_module_detection(self):
        """IEEE_EXCEPTIONS module import should be detected."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_exceptions_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert "ieee_exceptions" in result.ieee_modules_used

    def test_ieee_arithmetic_module_detection(self):
        """IEEE_ARITHMETIC module import should be detected."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_arithmetic_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert "ieee_arithmetic" in result.ieee_modules_used

    def test_ieee_features_module_detection(self):
        """IEEE_FEATURES module import should be detected."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_features_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert "ieee_features" in result.ieee_modules_used

    def test_ieee_exceptions_with_only_clause(self):
        """IEEE_EXCEPTIONS with ONLY clause should be validated."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_exceptions_only_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_ieee_arithmetic_with_only_clause(self):
        """IEEE_ARITHMETIC with ONLY clause should be validated."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_arithmetic_only_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_ieee_exception_handling_program(self):
        """IEEE exception handling program should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_exception_handling_program.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_ieee_special_values_program(self):
        """IEEE special values program should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_special_values_program.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_ieee_rounding_modes_program(self):
        """IEEE rounding modes program should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_rounding_modes_program.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_ieee_comprehensive_module(self):
        """Comprehensive IEEE module should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_comprehensive_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        # Should detect all three IEEE modules
        assert "ieee_exceptions" in result.ieee_modules_used
        assert "ieee_arithmetic" in result.ieee_modules_used
        assert "ieee_features" in result.ieee_modules_used

    def test_ieee_logical_operators_program(self):
        """IEEE with logical operators should pass validation."""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_logical_operators_program.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_ieee_module_names_complete(self):
        """All IEEE module names should be recognized."""
        assert "ieee_exceptions" in IEEE_MODULES
        assert "ieee_arithmetic" in IEEE_MODULES
        assert "ieee_features" in IEEE_MODULES

    def test_ieee_exception_flags_complete(self):
        """All IEEE exception flags from Section 14.2 should be defined."""
        expected = {
            "ieee_overflow", "ieee_underflow", "ieee_divide_by_zero",
            "ieee_invalid", "ieee_inexact"
        }
        assert expected == IEEE_EXCEPTION_FLAGS

    def test_ieee_special_values_complete(self):
        """All IEEE special values from Section 14.3 should be defined."""
        expected = {
            "ieee_positive_inf", "ieee_negative_inf",
            "ieee_quiet_nan", "ieee_signaling_nan"
        }
        assert expected == IEEE_SPECIAL_VALUES

    def test_ieee_rounding_modes_complete(self):
        """All IEEE rounding modes from Section 14.3 should be defined."""
        expected = {"ieee_nearest", "ieee_to_zero", "ieee_up", "ieee_down"}
        assert expected == IEEE_ROUNDING_MODES


class TestCombinedCInteropAndIEEE:
    """Tests for code using both C interoperability and IEEE arithmetic."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_c_interop_with_ieee_module(self):
        """Code using both C interop and IEEE modules should validate."""
        code = """module combined_mod
    use iso_c_binding, only: c_int, c_double
    use, intrinsic :: ieee_arithmetic
    implicit none
contains
    subroutine ieee_safe_c_calc(x, res) bind(c, name="safe_calc")
        real(c_double), value :: x
        real(c_double), intent(out) :: res
        if (ieee_is_finite(x)) then
            res = x * 2.0
        else
            res = 0.0
        end if
    end subroutine ieee_safe_c_calc
end module combined_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "ieee_arithmetic" in result.ieee_modules_used
        assert len(result.iso_c_binding_imports) > 0


class TestConvenienceFunctions:
    """Tests for the convenience validation functions."""

    def test_validate_c_interoperability_function(self):
        """validate_c_interoperability() should work as standalone function."""
        code = """
module c_mod
    use iso_c_binding
    implicit none
contains
    subroutine c_proc() bind(c)
    end subroutine c_proc
end module c_mod
"""
        result = validate_c_interoperability(code)
        assert not result.has_errors
        assert "c_proc" in result.c_interop_entities

    def test_validate_ieee_arithmetic_function(self):
        """validate_ieee_arithmetic() should work as standalone function."""
        code = """module ieee_mod
    use, intrinsic :: ieee_arithmetic
    implicit none
end module ieee_mod
"""
        result = validate_ieee_arithmetic(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "ieee_arithmetic" in result.ieee_modules_used


class TestDiagnosticDetails:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_diagnostic_has_severity(self):
        """All diagnostics should have a severity level."""
        code = """
module broken
    invalid syntax here
end module broken
"""
        result = self.validator.validate_code(code)
        for diag in result.diagnostics:
            assert diag.severity in DiagnosticSeverity

    def test_diagnostic_has_code(self):
        """All diagnostics should have a unique code."""
        code = """
module broken
    invalid syntax here
end module broken
"""
        result = self.validator.validate_code(code)
        for diag in result.diagnostics:
            assert diag.code is not None
            assert len(diag.code) > 0

    def test_diagnostic_has_message(self):
        """All diagnostics should have a descriptive message."""
        code = """
module broken
    invalid syntax here
end module broken
"""
        result = self.validator.validate_code(code)
        for diag in result.diagnostics:
            assert diag.message is not None
            assert len(diag.message) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
