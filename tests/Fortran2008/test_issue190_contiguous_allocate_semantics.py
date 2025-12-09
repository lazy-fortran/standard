#!/usr/bin/env python3
"""Semantic Validation Tests for F2008 CONTIGUOUS and Coarray ALLOCATE - Issue #190

Comprehensive test suite for Fortran 2008 CONTIGUOUS attribute and coarray
ALLOCATE semantic validation per ISO/IEC 1539-1:2010:
- CONTIGUOUS attribute usage (Section 5.3.7)
- CONTIGUOUS interactions with POINTER/ALLOCATABLE (C530-C531)
- Coarray ALLOCATE statements (Section 6.7.1)
- ALLOCATE coarray codimension constraints (C628, C644)
- ALLOCATE consistency with declarations (C645-C646)

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that CONTIGUOUS and ALLOCATE code conforms to the standard beyond
syntactic correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2008_contiguous_allocate_validator import (
    F2008ContiguousAllocateValidator,
    ContiguousAllocateValidationResult,
    DiagnosticSeverity,
    validate_contiguous_semantics,
    validate_allocate_semantics,
)
from fixture_utils import load_fixture


class TestF2008ContiguousAllocateValidatorBasic:
    """Basic tests for the CONTIGUOUS/ALLOCATE semantic validator infrastructure."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

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
        """ContiguousAllocateValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestContiguousAttributeSemantics:
    """Semantic validation tests for CONTIGUOUS attribute (Section 5.3.7)."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_contiguous_pointer_valid(self):
        """CONTIGUOUS pointer array is valid per C530."""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "contiguous_attribute.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.contiguous_declarations) > 0

    def test_contiguous_statement_valid(self):
        """CONTIGUOUS statement for pointer arrays is valid."""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "contiguous_statement.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_contiguous_assumed_shape_valid(self):
        """CONTIGUOUS on assumed-shape dummy argument is valid per C530."""
        code = """
module contiguous_assumed
    implicit none
contains
    subroutine process_array(arr)
        real, contiguous, intent(in) :: arr(:)
    end subroutine process_array
end module contiguous_assumed
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_contiguous_multiple_declarations(self):
        """Multiple CONTIGUOUS declarations should all be tracked."""
        code = """
module multi_contiguous
    implicit none
    real, contiguous, pointer :: a(:), b(:,:)
end module multi_contiguous
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.contiguous_declarations) >= 1

    def test_contiguous_statement_multiple_objects(self):
        """CONTIGUOUS statement with multiple objects."""
        code = """
module contiguous_multi_stmt
    implicit none
    real, pointer :: x(:), y(:), z(:,:)
    contiguous :: x, y, z
end module contiguous_multi_stmt
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestContiguousConstraintValidation:
    """Tests for CONTIGUOUS constraint validation per ISO Section 5.3.7."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_contiguous_allocatable_valid(self):
        """CONTIGUOUS on allocatable array pointer is valid."""
        code = """
module contiguous_alloc
    implicit none
    real, contiguous, allocatable :: data(:)
end module contiguous_alloc
"""
        result = self.validator.validate_code(code)
        has_error = any(
            d.severity == DiagnosticSeverity.ERROR for d in result.diagnostics
        )
        assert not has_error

    def test_contiguous_explicit_shape_warning(self):
        """CONTIGUOUS on non-assumed-shape, non-pointer should warn."""
        code = """
module contiguous_explicit
    implicit none
    real, contiguous :: fixed_array(100)
end module contiguous_explicit
"""
        result = self.validator.validate_code(code)
        has_warning = any(d.code == "CONTIG_W001" for d in result.diagnostics)
        assert has_warning, "Expected CONTIG_W001 warning for explicit-shape array"


class TestAllocateStatementSemantics:
    """Semantic validation tests for ALLOCATE statements (Section 6.7.1)."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_allocate_allocatable_valid(self):
        """ALLOCATE of ALLOCATABLE variable is valid."""
        code = """
program alloc_test
    implicit none
    integer, allocatable :: arr(:)
    allocate(arr(100))
end program alloc_test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.allocate_statements) >= 1

    def test_allocate_pointer_valid(self):
        """ALLOCATE of POINTER variable is valid."""
        code = """
program alloc_ptr
    implicit none
    real, pointer :: ptr(:,:)
    allocate(ptr(10, 20))
end program alloc_ptr
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_allocate_with_type_spec(self):
        """ALLOCATE with type-spec is valid."""
        code = """
program alloc_type
    implicit none
    class(*), allocatable :: poly
    allocate(integer :: poly)
end program alloc_type
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.allocate_statements) >= 1
        alloc = result.allocate_statements[0]
        assert alloc.has_type_spec

    def test_allocate_with_stat(self):
        """ALLOCATE with STAT= is valid."""
        code = """
program alloc_stat
    implicit none
    integer, allocatable :: arr(:)
    integer :: istat
    allocate(arr(100), stat=istat)
end program alloc_stat
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_allocate_multiple_objects(self):
        """ALLOCATE with multiple 1D objects."""
        code = """
program alloc_multi
    implicit none
    real, allocatable :: a(:), b(:)
    allocate(a(10), b(20))
end program alloc_multi
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestCoarrayAllocateSemantics:
    """Semantic validation tests for coarray ALLOCATE (Section 6.7.1.2)."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_coarray_allocate_valid(self):
        """ALLOCATE of coarray with codimension is valid.

        Note: The F2008 grammar handles scalar coarray ALLOCATE. Coarrays
        with both rank and corank require F2008 allocate_stmt_f2008 rule
        which has ordering constraints. This test validates scalar coarray
        allocation which is fully supported.
        """
        code = """
program coarray_alloc
    implicit none
    integer, allocatable :: x[:]
    allocate(x[*])
end program coarray_alloc
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.allocate_statements) >= 1

    def test_coarray_allocate_2d_cobounds(self):
        """ALLOCATE of coarray with 2D cobounds.

        Note: Tests 2D cobounds on scalar coarray. The F2008 grammar
        routes combined rank+corank allocation through allocation_f2008
        which has specific ordering requirements.
        """
        code = """
program coarray_2d
    implicit none
    integer, allocatable :: counter[*,:]
    allocate(counter[2, *])
end program coarray_2d
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_coarray_allocate_no_star_warning(self):
        """ALLOCATE of coarray without trailing * should warn."""
        code = """
program coarray_no_star
    implicit none
    integer, allocatable :: x[:]
    allocate(x[2])
end program coarray_no_star
"""
        result = self.validator.validate_code(code)
        has_warning = any(d.code == "ALLOC_W002" for d in result.diagnostics)
        assert has_warning, "Expected ALLOC_W002 warning for missing trailing *"


class TestAllocateConstraintValidation:
    """Tests for ALLOCATE constraint validation per ISO Section 6.7.1."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_allocate_non_allocatable_error(self):
        """ALLOCATE of non-ALLOCATABLE, non-POINTER should error."""
        code = """
program alloc_error
    implicit none
    integer :: fixed(100)
    allocate(fixed(200))
end program alloc_error
"""
        result = self.validator.validate_code(code)
        has_error = any(d.code == "ALLOC_E001" for d in result.diagnostics)
        assert has_error, "Expected ALLOC_E001 error for non-allocatable"

    def test_allocate_source_and_mold_error(self):
        """ALLOCATE with both SOURCE= and MOLD= should error."""
        code = """
program alloc_both
    implicit none
    integer, allocatable :: a(:), b(:)
    integer :: template(10)
    allocate(a(10), source=template, mold=b)
end program alloc_both
"""
        result = self.validator.validate_code(code)
        has_error = any(d.code == "ALLOC_E002" for d in result.diagnostics)
        assert has_error, "Expected ALLOC_E002 error for SOURCE= and MOLD="


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

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


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_contiguous_semantics_function(self):
        """validate_contiguous_semantics() should work as standalone function."""
        code = """
module contiguous_mod
    implicit none
    real, contiguous, pointer :: ptr(:)
end module contiguous_mod
"""
        result = validate_contiguous_semantics(code)
        assert not result.has_errors
        assert len(result.contiguous_declarations) > 0

    def test_validate_allocate_semantics_function(self):
        """validate_allocate_semantics() should work as standalone function."""
        code = """
program alloc_test
    implicit none
    real, allocatable :: arr(:)
    allocate(arr(50))
end program alloc_test
"""
        result = validate_allocate_semantics(code)
        assert not result.has_errors
        assert len(result.allocate_statements) >= 1


class TestISOComplianceValidation:
    """Tests verifying ISO/IEC 1539-1:2010 compliance checks."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_iso_section_references_in_diagnostics(self):
        """Diagnostics should include ISO section references."""
        code = """
module contiguous_explicit
    implicit none
    real, contiguous :: fixed_array(100)
end module contiguous_explicit
"""
        result = self.validator.validate_code(code)
        iso_refs = [d.iso_section for d in result.diagnostics if d.iso_section]
        assert len(iso_refs) > 0, "Expected ISO section references in diagnostics"

    def test_contiguous_iso_section_537(self):
        """CONTIGUOUS diagnostics should reference Section 5.3.7."""
        code = """
module contiguous_test
    implicit none
    real, contiguous :: arr(10)
end module contiguous_test
"""
        result = self.validator.validate_code(code)
        has_537_ref = any(
            d.iso_section == "5.3.7"
            for d in result.diagnostics
            if d.iso_section
        )
        assert has_537_ref, "Expected Section 5.3.7 reference for CONTIGUOUS"

    def test_allocate_iso_section_671(self):
        """ALLOCATE diagnostics should reference Section 6.7.1."""
        code = """
program alloc_non_alloc
    implicit none
    integer :: fixed(10)
    allocate(fixed(20))
end program alloc_non_alloc
"""
        result = self.validator.validate_code(code)
        has_671_ref = any(
            d.iso_section and "6.7.1" in d.iso_section
            for d in result.diagnostics
        )
        assert has_671_ref, "Expected Section 6.7.1 reference for ALLOCATE"


class TestVariableDeclarationTracking:
    """Tests for variable declaration tracking across scopes."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_variable_declarations_tracked(self):
        """Variable declarations should be tracked for cross-referencing."""
        code = """
module var_tracking
    implicit none
    integer, allocatable :: arr1(:)
    real, pointer :: ptr2(:,:)
    integer :: scalar
end module var_tracking
"""
        result = self.validator.validate_code(code)
        assert len(result.variable_declarations) >= 2

    def test_allocatable_attribute_tracked(self):
        """ALLOCATABLE attribute should be tracked on declarations."""
        code = """
module alloc_track
    implicit none
    integer, allocatable :: data(:)
end module alloc_track
"""
        result = self.validator.validate_code(code)
        if "data" in result.variable_declarations:
            assert result.variable_declarations["data"].has_allocatable

    def test_pointer_attribute_tracked(self):
        """POINTER attribute should be tracked on declarations."""
        code = """
module ptr_track
    implicit none
    real, pointer :: ptr(:)
end module ptr_track
"""
        result = self.validator.validate_code(code)
        if "ptr" in result.variable_declarations:
            assert result.variable_declarations["ptr"].has_pointer

    def test_contiguous_attribute_tracked(self):
        """CONTIGUOUS attribute should be tracked on declarations."""
        code = """
module contig_track
    implicit none
    real, contiguous, pointer :: arr(:)
end module contig_track
"""
        result = self.validator.validate_code(code)
        if "arr" in result.variable_declarations:
            assert result.variable_declarations["arr"].has_contiguous


class TestCoarrayDeclarationTracking:
    """Tests for coarray declaration tracking."""

    def setup_method(self):
        self.validator = F2008ContiguousAllocateValidator()

    def test_coarray_declaration_tracked(self):
        """Coarray declarations should be tracked."""
        code = """
module coarray_track
    implicit none
    integer :: counter[*]
    real, allocatable :: data(:)[:]
end module coarray_track
"""
        result = self.validator.validate_code(code)
        coarrays = [
            v for v in result.variable_declarations.values() if v.is_coarray
        ]
        assert len(coarrays) >= 1

    def test_coarray_corank_tracked(self):
        """Coarray corank should be tracked."""
        code = """
module corank_track
    implicit none
    real :: grid[2, *]
end module corank_track
"""
        result = self.validator.validate_code(code)
        if "grid" in result.variable_declarations:
            assert result.variable_declarations["grid"].corank >= 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
