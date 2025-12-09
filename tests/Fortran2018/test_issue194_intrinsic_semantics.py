#!/usr/bin/env python3
"""Issue #194 - Fortran 2018 Intrinsic Procedures Semantic Validation Tests

Tests semantic validation for F2018 new and extended intrinsic procedures
per ISO/IEC 1539-1:2018 (Fortran 2018 standard).

This test module validates:
- Image status functions (Section 16.9.73, 16.9.81, 16.9.182)
- Collective functions (Section 16.9.52, 16.9.187)
- RANDOM_INIT subroutine (Section 16.9.152)
- REDUCE function (Section 16.9.161)
- OUT_OF_RANGE function (Section 16.9.140)
"""

import sys
from pathlib import Path

import pytest

sys.path.append(str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "tools"))

from f2018_intrinsic_validator import (
    DiagnosticSeverity,
    F2018IntrinsicValidator,
    validate_intrinsic_semantics,
)
from fixture_utils import load_fixture


class TestF2018IntrinsicSemantics:
    """Test F2018 intrinsic procedure semantic validation."""

    def test_validator_instantiation(self):
        """Validator can be instantiated."""
        validator = F2018IntrinsicValidator()
        assert validator is not None

    def test_validate_empty_program(self):
        """Validation of minimal program produces no errors."""
        code = """program empty
end program empty
"""
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert result.error_count == 0

    def test_image_status_intrinsics_detected(self):
        """IMAGE_STATUS, FAILED_IMAGES, STOPPED_IMAGES are detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "image_status_intrinsics.f90",
        )
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert len(result.image_status_calls) >= 1

    def test_collective_function_intrinsics_detected(self):
        """COSHAPE and TEAM_NUMBER are detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "collective_function_intrinsics.f90",
        )
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert len(result.collective_function_calls) >= 1

    def test_random_init_intrinsic_valid_usage(self):
        """RANDOM_INIT with both required arguments validates correctly."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "random_init_intrinsic.f90",
        )
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert len(result.random_init_calls) >= 1

    def test_random_init_in_pure_procedure_error(self):
        """RANDOM_INIT in PURE procedure produces error per ISO Section 16.9.152."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "random_init_in_pure.f90",
        )
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        intr_errors = [e for e in errors if e.code == "INTR_E001"]
        assert len(intr_errors) >= 1, (
            "RANDOM_INIT in PURE procedure should produce INTR_E001 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.152"
        )
        assert "pure" in intr_errors[0].message.lower()

    def test_reduce_intrinsic_basic_usage(self):
        """REDUCE intrinsic with basic usage is detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "reduce_intrinsic.f90",
        )
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert len(result.reduce_calls) >= 1

    def test_out_of_range_intrinsic_usage(self):
        """OUT_OF_RANGE intrinsic is detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "out_of_range_intrinsic.f90",
        )
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert len(result.out_of_range_calls) >= 1

    def test_combined_f2018_intrinsics(self):
        """Combined F2018 intrinsics program detects all intrinsics."""
        code = load_fixture(
            "Fortran2018",
            "test_issue194_intrinsic_semantics",
            "combined_f2018_intrinsics.f90",
        )
        result = validate_intrinsic_semantics(code)
        assert not result.has_errors
        assert len(result.intrinsic_calls) >= 1


class TestF2018IntrinsicDiagnosticDetails:
    """Test diagnostic message details for F2018 intrinsics."""

    def test_random_init_missing_repeatable_warning(self):
        """RANDOM_INIT without REPEATABLE produces warning."""
        code = """program test
    call random_init(image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        warnings = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.WARNING
        ]
        repeatable_warnings = [w for w in warnings if "repeatable" in w.message.lower()]
        assert len(repeatable_warnings) >= 1, (
            "RANDOM_INIT without REPEATABLE should warn per ISO Section 16.9.152"
        )

    def test_random_init_missing_image_distinct_warning(self):
        """RANDOM_INIT without IMAGE_DISTINCT produces warning."""
        code = """program test
    call random_init(repeatable=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        warnings = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.WARNING
        ]
        image_warnings = [
            w for w in warnings if "image_distinct" in w.message.lower()
        ]
        assert len(image_warnings) >= 1, (
            "RANDOM_INIT without IMAGE_DISTINCT should warn per ISO Section 16.9.152"
        )

    def test_diagnostics_include_iso_section_references(self):
        """Diagnostics include ISO section references."""
        code = """program test
    call random_init(repeatable=.true., image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        info_diags = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.INFO
        ]
        section_refs = [d for d in info_diags if d.iso_section is not None]
        assert len(section_refs) >= 1, "Diagnostics should include ISO section refs"


class TestF2018IntrinsicValidationResult:
    """Test F2018IntrinsicValidationResult properties."""

    def test_result_properties(self):
        """Validation result has expected properties."""
        code = """program test
    call random_init(repeatable=.true., image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert hasattr(result, "has_errors")
        assert hasattr(result, "error_count")
        assert hasattr(result, "warning_count")
        assert hasattr(result, "diagnostics")
        assert hasattr(result, "intrinsic_calls")

    def test_result_tracks_intrinsic_calls(self):
        """Validation result tracks intrinsic call categories."""
        code = """program test
    call random_init(repeatable=.true., image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert hasattr(result, "random_init_calls")
        assert len(result.random_init_calls) >= 1


class TestFileValidation:
    """Test file-based validation."""

    def test_validate_nonexistent_file(self):
        """Validating nonexistent file produces FILE_E001 error."""
        validator = F2018IntrinsicValidator()
        result = validator.validate_file("/nonexistent/path/to/file.f90")
        file_errors = [d for d in result.diagnostics if d.code == "FILE_E001"]
        assert len(file_errors) == 1
        assert "not found" in file_errors[0].message.lower()


class TestPureProcedureDetection:
    """Test PURE procedure context detection."""

    def test_pure_subroutine_detected(self):
        """PURE subroutine context is correctly detected."""
        code = """module test_mod
contains
    pure subroutine my_pure_sub()
        call random_init(repeatable=.true., image_distinct=.true.)
    end subroutine my_pure_sub
end module test_mod
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics if d.code == "INTR_E001"
        ]
        assert len(errors) >= 1

    def test_elemental_subroutine_detected(self):
        """ELEMENTAL subroutine context is correctly detected as PURE."""
        code = """module test_mod
contains
    elemental subroutine my_elem_sub(x)
        real, intent(inout) :: x
        call random_init(repeatable=.true., image_distinct=.true.)
    end subroutine my_elem_sub
end module test_mod
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics if d.code == "INTR_E001"
        ]
        assert len(errors) >= 1

    def test_non_pure_subroutine_no_error(self):
        """Non-PURE subroutine allows RANDOM_INIT."""
        code = """module test_mod
contains
    subroutine my_impure_sub()
        call random_init(repeatable=.true., image_distinct=.true.)
    end subroutine my_impure_sub
end module test_mod
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics if d.code == "INTR_E001"
        ]
        assert len(errors) == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
