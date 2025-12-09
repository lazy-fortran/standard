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

    def test_random_init_missing_repeatable_error(self):
        """RANDOM_INIT without REPEATABLE produces error per ISO Section 16.9.152."""
        code = """program test
    call random_init(image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        repeatable_errors = [e for e in errors if e.code == "INTR_E002"]
        assert len(repeatable_errors) >= 1, (
            "RANDOM_INIT without REPEATABLE should produce INTR_E002 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.152"
        )
        assert "non-compliant" in repeatable_errors[0].message.lower()

    def test_random_init_missing_image_distinct_error(self):
        """RANDOM_INIT without IMAGE_DISTINCT produces error per ISO Section 16.9.152."""
        code = """program test
    call random_init(repeatable=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        image_errors = [e for e in errors if e.code == "INTR_E003"]
        assert len(image_errors) >= 1, (
            "RANDOM_INIT without IMAGE_DISTINCT should produce INTR_E003 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.152"
        )
        assert "non-compliant" in image_errors[0].message.lower()

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


class TestColumnTracking:
    """Test column position tracking for intrinsic calls (Issue #303)."""

    def test_random_init_column_position(self):
        """RANDOM_INIT call records correct column position."""
        code = """program test
    call random_init(repeatable=.true., image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert len(result.random_init_calls) == 1
        call = result.random_init_calls[0]
        assert call.line == 2
        assert call.column == 5

    def test_random_init_column_with_leading_spaces(self):
        """RANDOM_INIT column accounts for leading whitespace."""
        code = """program test
        call random_init(repeatable=.true., image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert len(result.random_init_calls) == 1
        call = result.random_init_calls[0]
        assert call.line == 2
        assert call.column == 9

    def test_image_status_column_position(self):
        """IMAGE_STATUS call records correct column position."""
        code = """program test
    integer :: status
    status = image_status(1)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert len(result.image_status_calls) == 1
        call = result.image_status_calls[0]
        assert call.line == 3
        assert call.column == 14

    def test_reduce_column_position(self):
        """REDUCE call records correct column position."""
        code = """program test
    integer :: arr(10), total
    total = reduce(arr, add_func)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert len(result.reduce_calls) == 1
        call = result.reduce_calls[0]
        assert call.line == 3
        assert call.column == 13

    def test_out_of_range_column_position(self):
        """OUT_OF_RANGE call records correct column position."""
        code = """program test
    logical :: flag
    flag = out_of_range(1.0d100, 0.0)
end program test
"""
        result = validate_intrinsic_semantics(code)
        assert len(result.out_of_range_calls) == 1
        call = result.out_of_range_calls[0]
        assert call.line == 3
        assert call.column == 12

    def test_diagnostics_include_column(self):
        """Diagnostics include column position."""
        code = """program test
    call random_init(repeatable=.true., image_distinct=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        info_diags = [
            d for d in result.diagnostics
            if d.severity.name == "INFO" and d.code == "INTR_I006"
        ]
        assert len(info_diags) >= 1
        assert info_diags[0].column == 5
        assert info_diags[0].line == 2

    def test_error_diagnostic_includes_column(self):
        """Error diagnostics include column position."""
        code = """module test_mod
contains
    pure subroutine my_pure()
        call random_init(repeatable=.true., image_distinct=.true.)
    end subroutine my_pure
end module test_mod
"""
        result = validate_intrinsic_semantics(code)
        errors = [d for d in result.diagnostics if d.code == "INTR_E001"]
        assert len(errors) >= 1
        assert errors[0].column == 9
        assert errors[0].line == 4

    def test_error_diagnostic_includes_column_for_missing_arg(self):
        """Error diagnostics for missing args include column position."""
        code = """program test
    call random_init(repeatable=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [d for d in result.diagnostics if d.code == "INTR_E003"]
        assert len(errors) >= 1
        assert errors[0].column == 5
        assert errors[0].line == 2


class TestReduceArgumentValidation:
    """Test REDUCE intrinsic argument validation per ISO Section 16.9.161."""

    def test_reduce_with_both_required_args(self):
        """REDUCE with ARRAY and OPERATION does not produce errors."""
        code = """program test
    integer :: arr(10), result_val
    result_val = reduce(arr, add_op)
end program test
"""
        result = validate_intrinsic_semantics(code)
        reduce_errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR and d.code.startswith("INTR_E00")
        ]
        reduce_specific = [e for e in reduce_errors if "reduce" in e.message.lower()]
        assert len(reduce_specific) == 0, (
            "REDUCE with both ARRAY and OPERATION should not produce errors"
        )

    def test_reduce_missing_array_error(self):
        """REDUCE without ARRAY produces INTR_E004 error."""
        code = """program test
    integer :: result_val
    result_val = reduce()
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [d for d in result.diagnostics if d.code == "INTR_E004"]
        assert len(errors) >= 1, (
            "REDUCE without ARRAY should produce INTR_E004 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.161"
        )
        assert "non-compliant" in errors[0].message.lower()
        assert "array" in errors[0].message.lower()

    def test_reduce_missing_operation_error(self):
        """REDUCE with only ARRAY produces INTR_E005 error."""
        code = """program test
    integer :: arr(10), result_val
    result_val = reduce(arr)
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [d for d in result.diagnostics if d.code == "INTR_E005"]
        assert len(errors) >= 1, (
            "REDUCE without OPERATION should produce INTR_E005 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.161"
        )
        assert "non-compliant" in errors[0].message.lower()
        assert "operation" in errors[0].message.lower()

    def test_reduce_with_keyword_args(self):
        """REDUCE with keyword arguments validates correctly."""
        code = """program test
    integer :: arr(10), result_val
    result_val = reduce(array=arr, operation=add_op)
end program test
"""
        result = validate_intrinsic_semantics(code)
        reduce_errors = [
            d for d in result.diagnostics
            if d.code in ("INTR_E004", "INTR_E005")
        ]
        assert len(reduce_errors) == 0, (
            "REDUCE with keyword ARRAY and OPERATION should not produce errors"
        )

    def test_reduce_with_optional_args(self):
        """REDUCE with optional DIM and MASK is detected."""
        code = """program test
    integer :: arr(10), result_val
    logical :: mask_arr(10)
    result_val = reduce(arr, add_op, dim=1, mask=mask_arr)
end program test
"""
        result = validate_intrinsic_semantics(code)
        info_diags = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.INFO
        ]
        dim_info = [d for d in info_diags if d.code == "INTR_I009"]
        mask_info = [d for d in info_diags if d.code == "INTR_I010"]
        assert len(dim_info) >= 1, "DIM argument should be detected"
        assert len(mask_info) >= 1, "MASK argument should be detected"


class TestOutOfRangeArgumentValidation:
    """Test OUT_OF_RANGE intrinsic argument validation per ISO Section 16.9.140."""

    def test_out_of_range_with_both_required_args(self):
        """OUT_OF_RANGE with X and MOLD does not produce errors."""
        code = """program test
    real :: x
    integer :: mold
    logical :: is_out
    is_out = out_of_range(x, mold)
end program test
"""
        result = validate_intrinsic_semantics(code)
        oor_errors = [
            d for d in result.diagnostics
            if d.code in ("INTR_E006", "INTR_E007")
        ]
        assert len(oor_errors) == 0, (
            "OUT_OF_RANGE with both X and MOLD should not produce errors"
        )

    def test_out_of_range_missing_x_error(self):
        """OUT_OF_RANGE without X produces INTR_E006 error."""
        code = """program test
    logical :: is_out
    is_out = out_of_range()
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [d for d in result.diagnostics if d.code == "INTR_E006"]
        assert len(errors) >= 1, (
            "OUT_OF_RANGE without X should produce INTR_E006 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.140"
        )
        assert "non-compliant" in errors[0].message.lower()

    def test_out_of_range_missing_mold_error(self):
        """OUT_OF_RANGE with only X produces INTR_E007 error."""
        code = """program test
    real :: x
    logical :: is_out
    is_out = out_of_range(x)
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [d for d in result.diagnostics if d.code == "INTR_E007"]
        assert len(errors) >= 1, (
            "OUT_OF_RANGE without MOLD should produce INTR_E007 error "
            "per ISO/IEC 1539-1:2018 Section 16.9.140"
        )
        assert "non-compliant" in errors[0].message.lower()
        assert "mold" in errors[0].message.lower()

    def test_out_of_range_with_keyword_args(self):
        """OUT_OF_RANGE with keyword arguments validates correctly."""
        code = """program test
    real :: x_val
    integer :: mold_val
    logical :: is_out
    is_out = out_of_range(x=x_val, mold=mold_val)
end program test
"""
        result = validate_intrinsic_semantics(code)
        oor_errors = [
            d for d in result.diagnostics
            if d.code in ("INTR_E006", "INTR_E007")
        ]
        assert len(oor_errors) == 0, (
            "OUT_OF_RANGE with keyword X and MOLD should not produce errors"
        )

    def test_out_of_range_with_round_option(self):
        """OUT_OF_RANGE with ROUND option is detected."""
        code = """program test
    real :: x
    integer :: mold
    logical :: is_out
    is_out = out_of_range(x, mold, round=.true.)
end program test
"""
        result = validate_intrinsic_semantics(code)
        info_diags = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.INFO
        ]
        round_info = [d for d in info_diags if d.code == "INTR_I011"]
        assert len(round_info) >= 1, "ROUND argument should be detected"


class TestIssue304ISOCompliance:
    """Test ISO compliance markers for issue #304 requirements."""

    def test_random_init_missing_args_marked_non_compliant(self):
        """Missing RANDOM_INIT arguments are marked NON-COMPLIANT."""
        code = """program test
    call random_init()
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR
        ]
        non_compliant = [e for e in errors if "non-compliant" in e.message.lower()]
        assert len(non_compliant) >= 2, (
            "Both missing REPEATABLE and IMAGE_DISTINCT should be marked NON-COMPLIANT"
        )

    def test_reduce_missing_args_marked_non_compliant(self):
        """Missing REDUCE arguments are marked NON-COMPLIANT."""
        code = """program test
    integer :: result_val
    result_val = reduce()
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR
        ]
        non_compliant = [e for e in errors if "non-compliant" in e.message.lower()]
        assert len(non_compliant) >= 2, (
            "Both missing ARRAY and OPERATION should be marked NON-COMPLIANT"
        )

    def test_out_of_range_missing_args_marked_non_compliant(self):
        """Missing OUT_OF_RANGE arguments are marked NON-COMPLIANT."""
        code = """program test
    logical :: is_out
    is_out = out_of_range()
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR
        ]
        non_compliant = [e for e in errors if "non-compliant" in e.message.lower()]
        assert len(non_compliant) >= 2, (
            "Both missing X and MOLD should be marked NON-COMPLIANT"
        )

    def test_all_errors_include_iso_section_reference(self):
        """All error diagnostics include ISO section references."""
        code = """program test
    call random_init()
    integer :: r
    r = reduce()
    logical :: o
    o = out_of_range()
end program test
"""
        result = validate_intrinsic_semantics(code)
        errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR
        ]
        errors_without_section = [e for e in errors if e.iso_section is None]
        assert len(errors_without_section) == 0, (
            "All error diagnostics must include ISO section references"
        )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
