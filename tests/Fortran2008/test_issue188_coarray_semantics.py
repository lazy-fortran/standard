#!/usr/bin/env python3
"""Semantic Validation Tests for F2008 Coarrays - Issue #188

Comprehensive test suite for Fortran 2008 coarray semantic validation per
ISO/IEC 1539-1:2010:
- Coarray declarations (Sections 2.4.7, 5.3.6)
- Image control statements (Section 8.5)
- Image intrinsic functions (Section 13.7)
- Coarray reference constraints (Section 6.6)

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that coarray code conforms to the standard beyond syntactic correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2008_coarray_validator import (
    F2008CoarrayValidator,
    CoarrayValidationResult,
    DiagnosticSeverity,
    IMAGE_INTRINSICS,
    SYNC_STATEMENT_TYPES,
    validate_coarray_semantics,
    validate_image_control,
)
from fixture_utils import load_fixture


class TestF2008CoarrayValidatorBasic:
    """Basic tests for the coarray semantic validator infrastructure."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

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
        """CoarrayValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestCoarrayDeclarationSemantics:
    """Semantic validation tests for coarray declarations (Section 5.3.6)."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

    def test_basic_coarray_declaration_detected(self):
        """Basic coarray declaration with [*] should be detected."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "coarray_declaration_star.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.coarray_declarations) > 0

    def test_coarray_declaration_with_explicit_bounds(self):
        """Coarray with explicit bounds should be validated."""
        code = """
module coarray_explicit
    implicit none
    integer :: data[2, *]
end module coarray_explicit
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_allocatable_coarray_valid(self):
        """Allocatable coarray with deferred codimension is valid."""
        code = """
module alloc_coarray
    implicit none
    integer, allocatable :: data(:)[:]
end module alloc_coarray
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_multiple_coarray_declarations(self):
        """Multiple coarray declarations should all be tracked."""
        code = """
module multi_coarray
    implicit none
    integer :: counter[*]
    real    :: values(100)[*]
    integer :: matrix(10,10)[*]
end module multi_coarray
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.coarray_declarations) >= 1

    def test_coarray_in_derived_type_context(self):
        """Coarray usage context should be validated."""
        code = """
module coarray_context
    implicit none
    real :: x[*]
contains
    subroutine work()
        x[this_image()] = 1.0
    end subroutine work
end module coarray_context
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestImageControlSemantics:
    """Semantic validation tests for image control statements (Section 8.5)."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

    def test_sync_all_statement_detected(self):
        """SYNC ALL statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "sync_all.f90",
        )
        result = self.validator.validate_code(code)
        sync_all_count = sum(
            1 for s in result.image_control_statements if s.stmt_type == "sync_all"
        )
        assert sync_all_count > 0

    def test_sync_images_statement_detected(self):
        """SYNC IMAGES statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "sync_images.f90",
        )
        result = self.validator.validate_code(code)
        sync_images_count = sum(
            1 for s in result.image_control_statements if s.stmt_type == "sync_images"
        )
        assert sync_images_count > 0

    def test_sync_memory_statement_detected(self):
        """SYNC MEMORY statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "sync_memory.f90",
        )
        result = self.validator.validate_code(code)
        sync_memory_count = sum(
            1 for s in result.image_control_statements if s.stmt_type == "sync_memory"
        )
        assert sync_memory_count > 0

    def test_sync_all_with_stat(self):
        """SYNC ALL with STAT= should be validated."""
        code = """
program sync_stat
    integer :: istat
    sync all(stat=istat)
end program sync_stat
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_sync_images_with_image_set(self):
        """SYNC IMAGES with image set should track the image set."""
        code = """
program sync_set
    sync images(*)
    sync images([1, 2, 3])
end program sync_set
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.image_control_statements) >= 2

    def test_sync_statements_tracked(self):
        """Multiple SYNC statements should all be tracked.

        Note: PURE procedure validation requires grammar support for PURE prefix
        at the program_unit level, which is tracked separately. This test
        verifies SYNC statement tracking.
        """
        code = """
program sync_tracking
    sync all
    sync memory
    sync images(*)
end program sync_tracking
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.image_control_statements) >= 3

    def test_comprehensive_sync_operations(self):
        """Comprehensive coarray module with sync operations."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "comprehensive_coarray_mod.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.image_control_statements) > 0


class TestImageIntrinsicSemantics:
    """Semantic validation tests for image intrinsic functions (Section 13.7)."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

    def test_this_image_detected(self):
        """THIS_IMAGE() usage should be detected."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "this_image_test.f90",
        )
        result = self.validator.validate_code(code)
        assert "this_image" in result.image_intrinsics_used

    def test_num_images_detected(self):
        """NUM_IMAGES() usage should be detected."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "num_images_test.f90",
        )
        result = self.validator.validate_code(code)
        assert "num_images" in result.image_intrinsics_used

    def test_image_intrinsics_in_expressions(self):
        """Image intrinsics in expressions should be tracked."""
        code = """
module image_expr
    implicit none
contains
    subroutine work()
        integer :: me, total
        me = this_image()
        total = num_images()
    end subroutine work
end module image_expr
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert "this_image" in result.image_intrinsics_used
        assert "num_images" in result.image_intrinsics_used

    def test_image_intrinsics_constants(self):
        """IMAGE_INTRINSICS constant set should include standard intrinsics."""
        expected = {"this_image", "num_images", "image_index", "lcobound", "ucobound"}
        assert expected.issubset(IMAGE_INTRINSICS)


class TestCoarrayReferenceSemantics:
    """Semantic validation tests for coarray references (Section 6.6)."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

    def test_coarray_section_reference(self):
        """Coarray section reference with image selector."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "coarray_section_reference.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_remote_coarray_reference(self):
        """Remote coarray reference should be tracked.

        Note: The grammar supports coarray references in lhs_expression.
        This test uses a supported pattern.
        """
        code = """
program remote_ref
    integer :: x[*]
    x[1] = 42
end program remote_ref
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_local_coarray_reference(self):
        """Local coarray reference with this_image()."""
        code = """
module local_ref
    implicit none
    integer :: x[*]
contains
    subroutine set_local()
        x[this_image()] = 42
    end subroutine set_local
end module local_ref
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestCoarrayWellFormednessChecks:
    """Tests for coarray well-formedness semantic checks."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

    def test_coarray_no_sync_info(self):
        """Coarray without sync statements should produce INFO diagnostic."""
        code = """
module no_sync
    implicit none
    integer :: data[*]
end module no_sync
"""
        result = self.validator.validate_code(code)
        has_sync_info = any(
            d.code == "SYNC_I001" for d in result.diagnostics
        )
        assert has_sync_info, "Expected SYNC_I001 info for coarray without sync"

    def test_coarray_with_sync_no_info(self):
        """Coarray with sync statements should not produce sync-needed INFO."""
        code = """
module with_sync
    implicit none
    integer :: data[*]
contains
    subroutine work()
        sync all
    end subroutine work
end module with_sync
"""
        result = self.validator.validate_code(code)
        has_no_sync_info = not any(
            d.code == "SYNC_I001" for d in result.diagnostics
        )
        assert has_no_sync_info


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_coarray_semantics_function(self):
        """validate_coarray_semantics() should work as standalone function."""
        code = """
module coarray_mod
    implicit none
    real :: x[*]
end module coarray_mod
"""
        result = validate_coarray_semantics(code)
        assert not result.has_errors
        assert len(result.coarray_declarations) > 0

    def test_validate_image_control_function(self):
        """validate_image_control() should work as standalone function."""
        code = """
program sync_test
    sync all
    sync memory
end program sync_test
"""
        result = validate_image_control(code)
        assert not result.has_errors
        assert len(result.image_control_statements) >= 2


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

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

    def test_sync_statement_types_complete(self):
        """SYNC_STATEMENT_TYPES should include all F2008 sync statements."""
        expected = {"sync_all", "sync_images", "sync_memory"}
        assert expected == SYNC_STATEMENT_TYPES


class TestISOComplianceValidation:
    """Tests verifying ISO/IEC 1539-1:2010 compliance checks."""

    def setup_method(self):
        self.validator = F2008CoarrayValidator()

    def test_iso_section_references_in_info_diagnostics(self):
        """Diagnostics for coarray without sync should reference ISO sections.

        Tests that INFO diagnostics include ISO section references per standard.
        """
        code = """
module coarray_no_sync
    implicit none
    integer :: data[*]
end module coarray_no_sync
"""
        result = self.validator.validate_code(code)
        iso_refs = [d.iso_section for d in result.diagnostics if d.iso_section]
        assert len(iso_refs) > 0, "Expected ISO section references in diagnostics"

    def test_coarray_declarations_reference_sections(self):
        """Coarray declaration validation should reference ISO sections.

        Verifies that coarray-related diagnostics cite ISO standard sections.
        """
        code = """
module multi_coarray_iso
    implicit none
    integer :: counter[*]
    real :: data(100)[*]
end module multi_coarray_iso
"""
        result = self.validator.validate_code(code)
        info_diags = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.INFO
        ]
        assert len(info_diags) > 0, "Expected INFO diagnostics for coarray code"
        assert any(
            d.iso_section for d in info_diags
        ), "Expected ISO section references in diagnostics"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
