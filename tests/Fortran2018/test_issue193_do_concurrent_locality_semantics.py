#!/usr/bin/env python3
"""Semantic Validation Tests for F2018 DO CONCURRENT with Locality - Issue #193

Comprehensive test suite for Fortran 2018 DO CONCURRENT with locality specifiers
per ISO/IEC 1539-1:2018:
- DO CONCURRENT with locality specifiers (Section 11.1.7.2)
- LOCAL, LOCAL_INIT, SHARED, DEFAULT(NONE) semantics (R1130)
- Iteration independence with locality constraints (Section 11.1.7.5)
- Variable declaration and usage requirements
- Interactions with coarrays and teams

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that DO CONCURRENT with locality code conforms to the standard beyond
syntactic correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2018_do_concurrent_locality_validator import (
    F2018DoConcurrentLocalityValidator,
    DoConcurrentLocalityValidationResult,
    DiagnosticSeverity,
    validate_do_concurrent_locality,
)
from fixture_utils import load_fixture


class TestDoConcurrentLocalityValidatorBasic:
    """Basic tests for the DO CONCURRENT locality semantic validator."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

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
        """DoConcurrentLocalityValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestDoConcurrentLocalBasicSemantics:
    """Tests for LOCAL locality specifier semantics."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_local_basic_detected(self):
        """DO CONCURRENT with LOCAL specifier should be detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_local_basic.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0
        construct = result.do_concurrent_constructs[0]
        assert "temp" in construct.local_variables

    def test_local_variable_usage_warning(self):
        """LOCAL variable referenced without assignment should warn.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.4, LOCAL variables are
        undefined at the beginning of each iteration.
        """
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i, temp
        integer :: a(10)
        do concurrent (i = 1:10) local(temp)
            a(i) = temp
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        warnings = [d for d in result.diagnostics if d.code == "LOC_W002"]
        assert len(warnings) > 0
        assert warnings[0].severity == DiagnosticSeverity.WARNING


class TestDoConcurrentLocalInitSemantics:
    """Tests for LOCAL_INIT locality specifier semantics."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_local_init_detected(self):
        """DO CONCURRENT with LOCAL_INIT specifier should be detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_local_init.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0
        construct = result.do_concurrent_constructs[0]
        assert "accum" in construct.local_init_variables


class TestDoConcurrentSharedSemantics:
    """Tests for SHARED locality specifier semantics."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_shared_detected(self):
        """DO CONCURRENT with SHARED specifier should be detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_shared.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0
        construct = result.do_concurrent_constructs[0]
        assert "factor" in construct.shared_variables
        assert "global_size" in construct.shared_variables

    def test_shared_assignment_warning(self):
        """Assigning to SHARED variable should produce warning.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.5, assigning to SHARED
        variables may violate iteration independence.
        """
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_violation_shared_assign.f90",
        )
        result = self.validator.validate_code(code)
        warnings = [d for d in result.diagnostics if d.code == "LOC_W003"]
        assert len(warnings) > 0
        assert warnings[0].severity == DiagnosticSeverity.WARNING
        assert "counter" in warnings[0].message


class TestDoConcurrentDefaultNoneSemantics:
    """Tests for DEFAULT(NONE) locality specifier semantics."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_default_none_valid(self):
        """DO CONCURRENT with complete DEFAULT(NONE) should pass."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_default_none.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0
        construct = result.do_concurrent_constructs[0]
        assert construct.has_default_none

    def test_default_none_missing_locality_error(self):
        """DEFAULT(NONE) with missing locality should produce error.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.5, when DEFAULT(NONE) is
        specified, all variables must have explicit locality.
        """
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_violation_default_none_missing.f90",
        )
        result = self.validator.validate_code(code)
        assert result.has_errors
        errors = [d for d in result.diagnostics if d.code == "LOC_E009"]
        assert len(errors) > 0
        assert errors[0].iso_section == "11.1.7.5"


class TestDoConcurrentCombinedLocalitySemantics:
    """Tests for combined locality specifiers."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_combined_locality_detected(self):
        """DO CONCURRENT with multiple locality specs should be detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_combined_locality.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0
        construct = result.do_concurrent_constructs[0]
        assert "work_val" in construct.local_variables
        assert "scale_factor" in construct.shared_variables


class TestDoConcurrentLocalityViolations:
    """Tests for locality specifier violations."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_index_in_locality_error(self):
        """Index variable in locality spec should produce error.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.4, the index-name of a
        concurrent-control shall not appear in a locality-spec.
        """
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_violation_index_in_local.f90",
        )
        result = self.validator.validate_code(code)
        assert result.has_errors
        errors = [d for d in result.diagnostics if d.code == "LOC_E007"]
        assert len(errors) > 0
        assert errors[0].iso_section == "11.1.7.4"
        assert "i" in errors[0].message

    def test_duplicate_locality_error(self):
        """Variable in multiple locality specs should produce error.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.4, a variable-name shall
        not appear in more than one locality-spec.
        """
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_violation_duplicate.f90",
        )
        result = self.validator.validate_code(code)
        assert result.has_errors
        errors = [d for d in result.diagnostics if d.code == "LOC_E008"]
        assert len(errors) > 0
        assert errors[0].iso_section == "11.1.7.4"
        assert "temp" in errors[0].message


class TestDoConcurrentMaskWithLocality:
    """Tests for DO CONCURRENT with mask expression and locality."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_mask_with_locality(self):
        """DO CONCURRENT with mask and locality should be detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_with_mask_and_locality.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0


class TestDoConcurrentNestedLocalitySemantics:
    """Tests for nested DO CONCURRENT with locality."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_nested_locality_detected(self):
        """Nested DO CONCURRENT with locality should produce INFO."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_nested_locality.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        nested_info = [d for d in result.diagnostics if d.code == "LOC_I002"]
        assert len(nested_info) > 0
        assert nested_info[0].severity == DiagnosticSeverity.INFO


class TestDoConcurrentStopViolationsWithLocality:
    """Tests for STOP violations in DO CONCURRENT with locality."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_stop_violation_detected(self):
        """STOP within DO CONCURRENT with locality should produce ERROR."""
        code = load_fixture(
            "Fortran2018",
            "test_issue193_do_concurrent_locality_semantics",
            "do_concurrent_violation_stop.f90",
        )
        result = self.validator.validate_code(code)
        stop_errors = [d for d in result.diagnostics if d.code == "LOC_E001"]
        assert len(stop_errors) > 0
        assert stop_errors[0].iso_section == "11.1.7.5"


class TestDoConcurrentLocalityInlineCode:
    """Tests for inline DO CONCURRENT code with locality."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_inline_local_specifier(self):
        """Inline DO CONCURRENT with LOCAL should be validated."""
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i, temp
        real :: a(10)
        do concurrent (i = 1:10) local(temp)
            temp = i * 2
            a(i) = real(temp)
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert "temp" in construct.local_variables

    def test_inline_shared_specifier(self):
        """Inline DO CONCURRENT with SHARED should be validated."""
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i
        real :: factor
        real :: a(10)
        factor = 2.5
        do concurrent (i = 1:10) shared(factor)
            a(i) = real(i) * factor
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert "factor" in construct.shared_variables

    def test_inline_default_none_complete(self):
        """Inline DO CONCURRENT with complete DEFAULT(NONE) should pass."""
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i, temp
        real :: a(10)
        do concurrent (i = 1:10) local(temp) shared(a) default(none)
            temp = i
            a(i) = real(temp)
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert construct.has_default_none


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_do_concurrent_locality_function(self):
        """validate_do_concurrent_locality() should work as standalone."""
        code = """
module test
    implicit none
contains
    subroutine work()
        integer :: i, temp
        real :: a(10)
        do concurrent (i = 1:10) local(temp)
            temp = i
            a(i) = real(temp)
        end do
    end subroutine work
end module test
"""
        result = validate_do_concurrent_locality(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

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


class TestISOComplianceValidation:
    """Tests verifying ISO/IEC 1539-1:2018 compliance checks."""

    def setup_method(self):
        self.validator = F2018DoConcurrentLocalityValidator()

    def test_locality_error_references_iso_section(self):
        """Locality errors should reference ISO section 11.1.7."""
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i
        integer :: a(10)
        do concurrent (i = 1:10) local(i)
            a(i) = i
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        locality_errors = [
            d for d in result.diagnostics if d.code.startswith("LOC_E")
        ]
        assert len(locality_errors) > 0
        assert any(
            d.iso_section is not None and "11.1.7" in d.iso_section
            for d in locality_errors
        )

    def test_stop_error_references_iso_section(self):
        """STOP error in locality context should reference ISO 11.1.7.5."""
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i, temp
        integer :: a(10)
        do concurrent (i = 1:10) local(temp)
            temp = i
            stop
            a(i) = temp
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        stop_errors = [d for d in result.diagnostics if "STOP" in d.message]
        assert len(stop_errors) > 0
        assert any(d.iso_section == "11.1.7.5" for d in stop_errors)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
