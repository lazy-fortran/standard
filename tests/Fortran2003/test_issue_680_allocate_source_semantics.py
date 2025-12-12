#!/usr/bin/env python3
"""Semantic Validation Tests for F2003 ALLOCATE SOURCE= - Issue #680

Comprehensive test suite for Fortran 2003 ALLOCATE SOURCE= semantic validation
per ISO/IEC 1539-1:2004 (J3/03-007), Section 6.3.1:

- C622: Allocate objects must be nonprocedure pointers or allocatable variables
- C630: No alloc-opt may appear more than once
- C631: If SOURCE=, TYPE-SPEC must not appear and ALLOCATION-LIST has
         exactly one object
- C632-C633: SOURCE expression rank/kind compatibility (partial validation)

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that ALLOCATE SOURCE= code conforms to the standard beyond syntactic
correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2003_allocate_source_validator import (
    F2003AllocateSourceValidator,
    AllocateSourceResult,
    DiagnosticSeverity,
    validate_allocate_source,
    validate_allocate_source_file,
)
from fixture_utils import load_fixture


class TestF2003AllocateSourceValidatorBasic:
    """Basic tests for the ALLOCATE SOURCE= semantic validator infrastructure."""

    def setup_method(self):
        self.validator = F2003AllocateSourceValidator()

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
        """AllocateSourceResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestAllocateSourceValidConstraints:
    """Semantic validation tests for ALLOCATE SOURCE= constraints (C631)."""

    def setup_method(self):
        self.validator = F2003AllocateSourceValidator()

    def test_allocate_source_single_object_valid(self):
        """ALLOCATE with SOURCE= and single object is valid per C631."""
        code = """
program allocate_source_valid
    implicit none
    integer, allocatable :: arr(:)
    integer :: template(10)
    allocate(arr, source=template)
end program allocate_source_valid
"""
        result = self.validator.validate_code(code)
        # Should not have C631 errors
        has_c631_error = any(
            d.code == "C631-E002" for d in result.diagnostics
        )
        assert not has_c631_error

    def test_allocate_source_multiple_objects_error(self):
        """ALLOCATE with SOURCE= and multiple objects violates C631."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_source_multiple_objects.f90",
        )
        result = self.validator.validate_code(code)
        has_c631_error = any(
            d.code == "C631-E002" for d in result.diagnostics
        )
        assert has_c631_error, "Expected C631-E002 error for multiple objects"

    def test_allocate_source_with_type_spec_error(self):
        """ALLOCATE with both SOURCE= and TYPE-SPEC violates C631."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_source_with_type_spec.f90",
        )
        result = self.validator.validate_code(code)
        has_c631_error = any(
            d.code == "C631-E001" for d in result.diagnostics
        )
        assert has_c631_error, "Expected C631-E001 error for TYPE-SPEC with SOURCE="


class TestAllocateOptConstraints:
    """Semantic validation tests for alloc-opt uniqueness (C630)."""

    def setup_method(self):
        self.validator = F2003AllocateSourceValidator()

    def test_allocate_duplicate_stat_error(self):
        """ALLOCATE with duplicate STAT= violates C630."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_duplicate_stat.f90",
        )
        result = self.validator.validate_code(code)
        has_c630_error = any(
            d.code == "C630-E001" for d in result.diagnostics
        )
        assert has_c630_error, "Expected C630-E001 error for duplicate STAT="

    def test_allocate_duplicate_errmsg_error(self):
        """ALLOCATE with duplicate ERRMSG= violates C630."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_duplicate_errmsg.f90",
        )
        result = self.validator.validate_code(code)
        has_c630_error = any(
            d.code == "C630-E002" for d in result.diagnostics
        )
        assert has_c630_error, "Expected C630-E002 error for duplicate ERRMSG="

    def test_allocate_single_options_valid(self):
        """ALLOCATE with single STAT= and ERRMSG= is valid per C630."""
        code = """
program allocate_single_opts
    implicit none
    integer, allocatable :: arr(:)
    integer :: stat
    character(len=256) :: msg
    allocate(arr(100), stat=stat, errmsg=msg)
end program allocate_single_opts
"""
        result = self.validator.validate_code(code)
        has_c630_error = any(
            d.code.startswith("C630") for d in result.diagnostics
        )
        assert not has_c630_error


class TestAllocateStatementTracking:
    """Tests for ALLOCATE statement tracking and analysis."""

    def setup_method(self):
        self.validator = F2003AllocateSourceValidator()

    def test_allocate_statements_tracked(self):
        """ALLOCATE statements should be tracked in result."""
        code = """
program track_alloc
    implicit none
    integer, allocatable :: a(:), b(:)
    allocate(a(10))
    allocate(b(20))
end program track_alloc
"""
        result = self.validator.validate_code(code)
        assert len(result.allocate_statements) >= 2

    def test_allocate_source_flag_tracked(self):
        """ALLOCATE with SOURCE= should be marked has_source=True."""
        code = """
program track_source
    implicit none
    integer, allocatable :: arr(:)
    integer :: template(10)
    allocate(arr, source=template)
end program track_source
"""
        result = self.validator.validate_code(code)
        if result.allocate_statements:
            alloc = result.allocate_statements[0]
            assert alloc.get("has_source", False), \
                "SOURCE= should be tracked in ALLOCATE statement"

    def test_allocate_object_names_tracked(self):
        """ALLOCATE object names should be tracked."""
        code = """
program track_names
    implicit none
    integer, allocatable :: arr(:)
    allocate(arr(100))
end program track_names
"""
        result = self.validator.validate_code(code)
        if result.allocate_statements:
            alloc = result.allocate_statements[0]
            objects = alloc.get("objects", [])
            assert len(objects) > 0, "Object names should be tracked"


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2003AllocateSourceValidator()

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

    def test_constraint_diagnostics_reference_iso_section(self):
        """Constraint violation diagnostics should reference ISO section."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_source_multiple_objects.f90",
        )
        result = self.validator.validate_code(code)
        constraint_diags = [
            d for d in result.diagnostics
            if d.code.startswith("C")
        ]
        if constraint_diags:
            for diag in constraint_diags:
                assert diag.iso_section is not None, \
                    f"Constraint {diag.code} should reference ISO section"


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_allocate_source_function(self):
        """validate_allocate_source() should work as standalone function."""
        code = """
program alloc_test
    implicit none
    integer, allocatable :: arr(:)
    integer :: template(10)
    allocate(arr, source=template)
end program alloc_test
"""
        result = validate_allocate_source(code)
        assert not result.has_errors
        assert len(result.allocate_statements) >= 1

    def test_validate_allocate_source_file_function(self):
        """validate_allocate_source_file() should work for files."""
        result = validate_allocate_source_file(
            "tests/fixtures/Fortran2003/"
            "test_issue_680_allocate_source_semantics/"
            "allocate_source_valid.f90"
        )
        # Valid fixture should not have C631 errors
        has_c631_error = any(
            d.code.startswith("C631") for d in result.diagnostics
        )
        assert not has_c631_error


class TestISOComplianceValidation:
    """Tests verifying ISO/IEC 1539-1:2004 compliance checks."""

    def setup_method(self):
        self.validator = F2003AllocateSourceValidator()

    def test_iso_section_631_in_diagnostics(self):
        """C631 constraint violations should reference Section 6.3.1."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_source_multiple_objects.f90",
        )
        result = self.validator.validate_code(code)
        c631_diags = [
            d for d in result.diagnostics
            if d.code.startswith("C631") and d.iso_section
        ]
        if c631_diags:
            for diag in c631_diags:
                assert "6.3.1" in diag.iso_section

    def test_iso_section_630_in_diagnostics(self):
        """C630 constraint violations should reference Section 6.3.1."""
        code = load_fixture(
            "Fortran2003",
            "test_issue_680_allocate_source_semantics",
            "allocate_duplicate_stat.f90",
        )
        result = self.validator.validate_code(code)
        c630_diags = [
            d for d in result.diagnostics
            if d.code.startswith("C630") and d.iso_section
        ]
        if c630_diags:
            for diag in c630_diags:
                assert "6.3.1" in diag.iso_section


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
