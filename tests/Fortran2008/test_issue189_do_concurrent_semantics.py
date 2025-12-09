#!/usr/bin/env python3
"""Semantic Validation Tests for F2008 DO CONCURRENT - Issue #189

Comprehensive test suite for Fortran 2008 DO CONCURRENT semantic validation per
ISO/IEC 1539-1:2010:
- DO CONCURRENT construct structure (Section 8.1.6.6)
- Iteration independence constraints (Section 8.1.6.6.4)
- Variable assignment restrictions (Section 8.1.6.6.4)
- Side-effect restrictions (Section 8.1.6.6.4)
- Coarray interaction rules (Sections 8.1.6.6.4, 8.5)

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that DO CONCURRENT code conforms to the standard beyond syntactic
correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2008_do_concurrent_validator import (
    F2008DoConcurrentValidator,
    DoConcurrentValidationResult,
    DiagnosticSeverity,
    validate_do_concurrent,
)
from fixture_utils import load_fixture


class TestDoConcurrentValidatorBasic:
    """Basic tests for the DO CONCURRENT semantic validator infrastructure."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

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
        """DoConcurrentValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestDoConcurrentBasicSemantics:
    """Semantic validation tests for basic DO CONCURRENT constructs."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_basic_do_concurrent_detected(self):
        """Basic DO CONCURRENT construct should be detected and tracked."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_basic.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0

    def test_do_concurrent_with_mask(self):
        """DO CONCURRENT with mask expression should be validated."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_with_mask.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_do_concurrent_multi_index(self):
        """DO CONCURRENT with multiple indices should be validated."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_multi_index.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_basic_do_concurrent_inline(self):
        """Basic DO CONCURRENT inline code should parse without errors."""
        code = """
module test
    implicit none
contains
    subroutine proc()
        integer :: i
        real :: a(10)
        do concurrent (i = 1:10)
            a(i) = real(i)
        end do
    end subroutine proc
end module test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1


class TestDoConcurrentStopViolations:
    """Tests for STOP and ERROR STOP violations in DO CONCURRENT."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_stop_violation_detected(self):
        """STOP within DO CONCURRENT should produce ERROR diagnostic."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_violation_stop.f90",
        )
        result = self.validator.validate_code(code)
        assert result.has_errors
        stop_errors = [d for d in result.diagnostics if d.code == "DO_CONC_E001"]
        assert len(stop_errors) > 0
        assert stop_errors[0].iso_section == "8.1.6.6.4"

    def test_error_stop_violation_detected(self):
        """ERROR STOP within DO CONCURRENT should produce ERROR diagnostic."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_violation_error_stop.f90",
        )
        result = self.validator.validate_code(code)
        assert result.has_errors
        error_stop_errors = [
            d for d in result.diagnostics if d.code == "DO_CONC_E002"
        ]
        assert len(error_stop_errors) > 0
        assert error_stop_errors[0].iso_section == "8.1.6.6.4"


class TestDoConcurrentImageControlViolations:
    """Tests for image control statement violations in DO CONCURRENT."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_sync_all_violation_detected(self):
        """SYNC ALL within DO CONCURRENT should produce ERROR diagnostic."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_violation_sync.f90",
        )
        result = self.validator.validate_code(code)
        assert result.has_errors
        sync_errors = [d for d in result.diagnostics if d.code == "DO_CONC_E003"]
        assert len(sync_errors) > 0
        assert sync_errors[0].iso_section == "8.1.6.6.4"

    def test_sync_images_violation(self):
        """SYNC IMAGES within DO CONCURRENT should produce ERROR diagnostic."""
        code = """
program sync_images_violation
    implicit none
    integer :: i
    do concurrent (i = 1:10)
        sync images(*)
    end do
end program sync_images_violation
"""
        result = self.validator.validate_code(code)
        assert result.has_errors
        sync_errors = [d for d in result.diagnostics if d.code == "DO_CONC_E004"]
        assert len(sync_errors) > 0

    def test_sync_memory_violation(self):
        """SYNC MEMORY within DO CONCURRENT should produce ERROR diagnostic."""
        code = """
program sync_memory_violation
    implicit none
    integer :: i
    do concurrent (i = 1:10)
        sync memory
    end do
end program sync_memory_violation
"""
        result = self.validator.validate_code(code)
        assert result.has_errors
        sync_errors = [d for d in result.diagnostics if d.code == "DO_CONC_E005"]
        assert len(sync_errors) > 0


class TestDoConcurrentIOViolations:
    """Tests for I/O operation warnings in DO CONCURRENT."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_print_warning_detected(self):
        """PRINT within DO CONCURRENT should produce WARNING diagnostic."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_violation_print.f90",
        )
        result = self.validator.validate_code(code)
        io_warnings = [d for d in result.diagnostics if d.code == "DO_CONC_W001"]
        assert len(io_warnings) > 0
        assert io_warnings[0].severity == DiagnosticSeverity.WARNING
        assert io_warnings[0].iso_section == "8.1.6.6.4"


class TestDoConcurrentNestedConstructs:
    """Tests for nested DO CONCURRENT constructs."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_nested_do_concurrent_detected(self):
        """Nested DO CONCURRENT should be detected and produce INFO."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_nested.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors
        nested_info = [d for d in result.diagnostics if d.code == "DO_CONC_I002"]
        assert len(nested_info) > 0
        assert nested_info[0].severity == DiagnosticSeverity.INFO


class TestDoConcurrentVariableDependencies:
    """Tests for variable dependency tracking within DO CONCURRENT."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_variable_reference_tracking(self):
        """Variable references should be tracked within DO CONCURRENT."""
        code = """
program var_ref_test
    implicit none
    integer :: i
    real :: a(10), b(10)
    do concurrent (i = 1:10)
        a(i) = b(i) + 1.0
    end do
end program var_ref_test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert "a" in construct.assigned_variables
        assert "b" in construct.referenced_variables

    def test_assign_and_reference_same_variable_info(self):
        """Assigning and referencing same variable should produce INFO.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, iterations must be
        independent. A variable defined in one iteration shall not be
        referenced in another - this pattern warrants an INFO diagnostic.
        """
        code = """
program assign_ref_test
    implicit none
    integer :: i
    real :: x(10)
    do concurrent (i = 1:10)
        x(i) = x(i) + 1.0
    end do
end program assign_ref_test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        info_diags = [d for d in result.diagnostics if d.code == "DO_CONC_I001"]
        assert len(info_diags) > 0
        assert info_diags[0].severity == DiagnosticSeverity.INFO
        assert info_diags[0].iso_section == "8.1.6.6.4"
        assert "x" in info_diags[0].message

    def test_mask_expression_variables_tracked(self):
        """Variables in DO CONCURRENT mask should be tracked for dependencies.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6, the mask expression is
        evaluated for each iteration. Variables in the mask must be tracked
        to detect potential iteration dependencies.
        """
        code = """
module mask_var_mod
    implicit none
contains
    subroutine mask_var_test()
        integer :: i
        real :: a(10), b(10), thresh
        thresh = 0.5
        b = 1.0
        do concurrent (i = 1:10, b(i) > thresh)
            a(i) = real(i)
        end do
    end subroutine mask_var_test
end module mask_var_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert construct.has_mask
        assert "b" in construct.referenced_variables
        assert "thresh" in construct.referenced_variables

    def test_if_condition_variables_tracked(self):
        """Variables in IF conditions within DO CONCURRENT should be tracked.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, all variable references
        within DO CONCURRENT must be tracked for iteration independence,
        including those in conditional expressions.
        """
        code = """
module if_cond_mod
    implicit none
contains
    subroutine if_cond_test()
        integer :: i
        real :: a(10), cond_flag(10)
        cond_flag = 1.0
        do concurrent (i = 1:10)
            if (cond_flag(i) > 0.5) then
                a(i) = real(i)
            end if
        end do
    end subroutine if_cond_test
end module if_cond_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert "cond_flag" in construct.referenced_variables

    def test_multiple_if_conditions_tracked(self):
        """Variables in multiple IF conditions within DO CONCURRENT are tracked.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, all variable references
        within DO CONCURRENT must be tracked for iteration independence,
        including those in multiple conditional expressions.
        """
        code = """
module multi_if_mod
    implicit none
contains
    subroutine multi_if_test()
        integer :: i
        real :: a(10), cond_flag(10), cond_val(10)
        cond_flag = 1.0
        cond_val = 2.0
        do concurrent (i = 1:10)
            if (cond_flag(i) > 0.5) then
                a(i) = 1.0
            end if
            if (cond_val(i) < 1.0) then
                a(i) = 2.0
            end if
        end do
    end subroutine multi_if_test
end module multi_if_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert "cond_flag" in construct.referenced_variables
        assert "cond_val" in construct.referenced_variables

    def test_procedure_argument_variables_tracked(self):
        """Variables passed to procedure calls within DO CONCURRENT are tracked.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, variables passed as
        procedure arguments must be tracked for iteration independence,
        as they may be modified or referenced across iterations.
        """
        code = """
module proc_arg_mod
    implicit none
contains
    subroutine proc_arg_test()
        integer :: i
        real :: a(10), b(10)
        b = 1.0
        do concurrent (i = 1:10)
            call work_proc(a(i), b(i))
        end do
    end subroutine proc_arg_test
    subroutine work_proc(x, y)
        real :: x, y
        x = y * 2.0
    end subroutine work_proc
end module proc_arg_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) >= 1
        construct = result.do_concurrent_constructs[0]
        assert "a" in construct.referenced_variables
        assert "b" in construct.referenced_variables

    def test_mask_with_assigned_variable_produces_info(self):
        """Mask referencing assigned variable should produce DO_CONC_I001.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, if a variable is both
        assigned in the loop body and referenced in the mask expression,
        an INFO diagnostic should flag this potential dependency.
        """
        code = """
module mask_assign_mod
    implicit none
contains
    subroutine mask_assign_test()
        integer :: i
        real :: a(10)
        a = 0.0
        do concurrent (i = 1:10, a(i) < 10.0)
            a(i) = real(i)
        end do
    end subroutine mask_assign_test
end module mask_assign_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors
        info_diags = [d for d in result.diagnostics if d.code == "DO_CONC_I001"]
        assert len(info_diags) > 0
        assert any("a" in d.message for d in info_diags)


class TestDoConcurrentProcedureCalls:
    """Tests for procedure calls within DO CONCURRENT."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_procedure_call_tracked(self):
        """Procedure calls within DO CONCURRENT should be tracked."""
        code = load_fixture(
            "Fortran2008",
            "test_issue189_do_concurrent_semantics",
            "do_concurrent_with_procedure_call.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestDoConcurrentCoarrayInteraction:
    """Tests for coarray interaction within DO CONCURRENT."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_coarray_remote_reference_warning(self):
        """Coarray remote reference within DO CONCURRENT should be tracked."""
        code = """
program coarray_ref
    implicit none
    integer :: i
    real :: x[*]
    do concurrent (i = 1:10)
        x[1] = real(i)
    end do
end program coarray_ref
"""
        result = self.validator.validate_code(code)
        assert len(result.do_concurrent_constructs) >= 1
        if result.do_concurrent_constructs:
            construct = result.do_concurrent_constructs[0]
            assert construct.contains_coarray_ref


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_do_concurrent_function(self):
        """validate_do_concurrent() should work as standalone function."""
        code = """
module do_conc_mod
    implicit none
contains
    subroutine work()
        integer :: i
        real :: a(10)
        do concurrent (i = 1:10)
            a(i) = real(i)
        end do
    end subroutine work
end module do_conc_mod
"""
        result = validate_do_concurrent(code)
        assert not result.has_errors
        assert len(result.do_concurrent_constructs) > 0


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

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
    """Tests verifying ISO/IEC 1539-1:2010 compliance checks."""

    def setup_method(self):
        self.validator = F2008DoConcurrentValidator()

    def test_stop_error_references_iso_section(self):
        """STOP error diagnostic should reference ISO section 8.1.6.6.4."""
        code = """
program stop_test
    integer :: i
    do concurrent (i = 1:10)
        stop
    end do
end program stop_test
"""
        result = self.validator.validate_code(code)
        stop_errors = [d for d in result.diagnostics if "STOP" in d.message]
        assert len(stop_errors) > 0
        assert any(d.iso_section == "8.1.6.6.4" for d in stop_errors)

    def test_sync_error_references_iso_section(self):
        """SYNC error diagnostic should reference ISO section 8.1.6.6.4."""
        code = """
program sync_test
    integer :: i
    do concurrent (i = 1:10)
        sync all
    end do
end program sync_test
"""
        result = self.validator.validate_code(code)
        sync_errors = [
            d for d in result.diagnostics if "image control" in d.message.lower()
        ]
        assert len(sync_errors) > 0
        assert any(d.iso_section == "8.1.6.6.4" for d in sync_errors)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
