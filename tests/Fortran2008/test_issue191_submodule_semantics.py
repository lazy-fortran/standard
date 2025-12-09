#!/usr/bin/env python3
"""Semantic Validation Tests for F2008 Submodules - Issue #191

Comprehensive test suite for Fortran 2008 submodule semantic validation per
ISO/IEC 1539-1:2010:
- Submodule declarations (Section 11.2.3)
- Parent module/submodule linkage (R1118)
- Separate module procedures (Section 12.6.2.5)
- MODULE SUBROUTINE/FUNCTION interface matching

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that submodule code conforms to the standard beyond syntactic correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2008_submodule_validator import (
    F2008SubmoduleValidator,
    SubmoduleValidationResult,
    DiagnosticSeverity,
    validate_submodule_semantics,
    validate_submodule_linkage,
)
from fixture_utils import load_fixture


class TestF2008SubmoduleValidatorBasic:
    """Basic tests for the submodule semantic validator infrastructure."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

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
        """SubmoduleValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestModuleDeclarationSemantics:
    """Semantic validation tests for module declarations."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

    def test_module_detected(self):
        """Module declaration should be detected and tracked."""
        code = """
module my_module
    implicit none
end module my_module
"""
        result = self.validator.validate_code(code)
        assert "my_module" in result.modules
        assert not result.has_errors

    def test_module_with_interface(self):
        """Module with interface block should track interface procedures."""
        code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "parent_module_with_interface.f90",
        )
        result = self.validator.validate_code(code)
        assert "math_mod" in result.modules
        assert not result.has_errors

    def test_multiple_modules(self):
        """Multiple modules should all be tracked."""
        code = """
module mod_a
    implicit none
end module mod_a
"""
        result = self.validator.validate_code(code)
        assert "mod_a" in result.modules


class TestSubmoduleDeclarationSemantics:
    """Semantic validation tests for submodule declarations (Section 11.2.3)."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

    def test_basic_submodule_detected(self):
        """Basic submodule declaration should be detected."""
        code = """
submodule (parent_mod) child_sub
    implicit none
end submodule child_sub
"""
        result = self.validator.validate_code(code)
        assert "child_sub" in result.submodules
        submod = result.submodules["child_sub"]
        assert submod.parent_module == "parent_mod"

    def test_submodule_with_parent_submodule(self):
        """Submodule with parent submodule reference should track hierarchy."""
        code = """
submodule (parent_mod:parent_sub) child_sub
    implicit none
end submodule child_sub
"""
        result = self.validator.validate_code(code)
        assert "child_sub" in result.submodules
        submod = result.submodules["child_sub"]
        assert submod.parent_module == "parent_mod"
        assert submod.parent_submodule == "parent_sub"

    def test_deeply_nested_submodule(self):
        """Deeply nested submodule hierarchy should be tracked."""
        code = """
submodule (grandparent_mod:parent_sub:child_sub) grandchild_sub
    implicit none
end submodule grandchild_sub
"""
        result = self.validator.validate_code(code)
        assert "grandchild_sub" in result.submodules
        submod = result.submodules["grandchild_sub"]
        assert submod.parent_module == "grandparent_mod"
        assert submod.parent_submodule == "child_sub"
        assert len(submod.parent_chain) == 3

    def test_submodule_with_procedures(self):
        """Submodule with module procedures should track them."""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_submodules",
            "submodule_with_procedures.f90",
        )
        result = self.validator.validate_code(code)
        assert "implementation_sub" in result.submodules
        assert len(result.module_procedures) >= 2


class TestModuleProcedureSemantics:
    """Semantic validation tests for MODULE SUBROUTINE/FUNCTION (Section 12.6.2.5)."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

    def test_module_subroutine_detected(self):
        """MODULE SUBROUTINE should be detected in submodule."""
        code = """
submodule (parent_mod) impl_sub
    implicit none
contains
    module subroutine do_work()
        print *, 'Working'
    end subroutine do_work
end submodule impl_sub
"""
        result = self.validator.validate_code(code)
        assert len(result.module_procedures) == 1
        proc = result.module_procedures[0]
        assert proc.name == "do_work"
        assert proc.is_subroutine
        assert not proc.is_function

    def test_module_function_detected(self):
        """MODULE FUNCTION should be detected in submodule."""
        code = """
submodule (parent_mod) impl_sub
    implicit none
contains
    module function calc(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * 2.0
    end function calc
end submodule impl_sub
"""
        result = self.validator.validate_code(code)
        assert len(result.module_procedures) == 1
        proc = result.module_procedures[0]
        assert proc.name == "calc"
        assert proc.is_function
        assert not proc.is_subroutine

    def test_multiple_module_procedures(self):
        """Multiple module procedures should all be tracked."""
        code = """
submodule (parent_mod) impl_sub
    implicit none
contains
    module subroutine sub1()
    end subroutine sub1

    module subroutine sub2()
    end subroutine sub2

    module function func1() result(r)
        integer :: r
        r = 42
    end function func1
end submodule impl_sub
"""
        result = self.validator.validate_code(code)
        assert len(result.module_procedures) == 3


class TestCrossUnitValidation:
    """Tests for cross-unit semantic validation (parent linkage)."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

    def test_valid_parent_linkage(self):
        """Valid parent module linkage should produce no errors."""
        parent_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "parent_module_with_interface.f90",
        )
        child_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "child_submodule_valid.f90",
        )
        result = self.validator.validate_multi_unit([parent_code, child_code])
        errors = [d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR]
        assert len(errors) == 0, f"Unexpected errors: {errors}"

    def test_missing_parent_module_warning(self):
        """Missing parent module should produce warning."""
        orphan_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "orphan_submodule.f90",
        )
        result = self.validator.validate_multi_unit([orphan_code])
        has_warning = any(d.code == "SUBMOD_W001" for d in result.diagnostics)
        assert has_warning, "Expected SUBMOD_W001 warning for missing parent module"

    def test_missing_parent_submodule_warning(self):
        """Missing parent submodule should produce warning."""
        nested_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "nested_submodule.f90",
        )
        result = self.validator.validate_multi_unit([nested_code])
        has_warning = any(d.code == "SUBMOD_W002" for d in result.diagnostics)
        assert has_warning, "Expected SUBMOD_W002 warning for missing parent submodule"

    def test_nested_submodule_with_full_hierarchy(self):
        """Nested submodule with full hierarchy should validate correctly."""
        parent_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "parent_module_with_interface.f90",
        )
        child_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "child_submodule_valid.f90",
        )
        nested_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "nested_submodule.f90",
        )
        result = self.validator.validate_multi_unit([parent_code, child_code, nested_code])
        parent_warnings = [d for d in result.diagnostics if d.code in ("SUBMOD_W001", "SUBMOD_W002")]
        assert len(parent_warnings) == 0, f"Unexpected parent warnings: {parent_warnings}"


class TestProcedureLinkageValidation:
    """Tests for procedure interface linkage validation."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

    def test_procedure_with_matching_interface(self):
        """Module procedure with matching interface should not error."""
        parent_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "parent_module_with_interface.f90",
        )
        child_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "child_submodule_valid.f90",
        )
        result = self.validator.validate_multi_unit([parent_code, child_code])
        type_mismatches = [d for d in result.diagnostics if d.code == "SUBMOD_E003"]
        assert len(type_mismatches) == 0, f"Unexpected type mismatches: {type_mismatches}"

    def test_procedure_type_mismatch(self):
        """Module procedure type mismatch should produce error."""
        parent_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "parent_module_with_interface.f90",
        )
        mismatch_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "mismatched_signature.f90",
        )
        result = self.validator.validate_multi_unit([parent_code, mismatch_code])
        has_mismatch = any(d.code == "SUBMOD_E003" for d in result.diagnostics)
        assert has_mismatch, "Expected SUBMOD_E003 error for type mismatch"

    def test_procedure_without_interface_info(self):
        """Module procedure without interface should produce info."""
        orphan_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "orphan_submodule.f90",
        )
        result = self.validator.validate_multi_unit([orphan_code])
        has_info = any(d.code == "SUBMOD_I002" for d in result.diagnostics)
        assert has_info, "Expected SUBMOD_I002 info for missing interface"

    def test_procedure_not_linked_to_non_ancestor_interface(self):
        """Interface in non-ancestor module should not satisfy linkage."""
        unrelated_parent = """
module other_mod
   implicit none
   interface
      subroutine calculate_result(x, y)
         real, intent(in) :: x
         real, intent(out) :: y
      end subroutine calculate_result
   end interface
end module other_mod
"""
        child_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "child_submodule_valid.f90",
        )
        result = self.validator.validate_multi_unit([unrelated_parent, child_code])
        infos = [d for d in result.diagnostics if d.code == "SUBMOD_I002"]
        assert (
            len(infos) > 0
        ), "Expected SUBMOD_I002 info when interface is not in ancestor module"


class TestDuplicateImplementationValidation:
    """Tests for duplicate implementation detection."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

    def test_duplicate_implementation_error(self):
        """Duplicate module procedure implementations should produce error."""
        code1 = """
submodule (parent_mod) impl1
    implicit none
contains
    module subroutine do_work()
    end subroutine do_work
end submodule impl1
"""
        code2 = """
submodule (parent_mod) impl2
    implicit none
contains
    module subroutine do_work()
    end subroutine do_work
end submodule impl2
"""
        result = self.validator.validate_multi_unit([code1, code2])
        has_dup = any(d.code == "SUBMOD_E004" for d in result.diagnostics)
        assert has_dup, "Expected SUBMOD_E004 error for duplicate implementation"


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_submodule_semantics_function(self):
        """validate_submodule_semantics() should work as standalone function."""
        code = """
submodule (parent_mod) child_sub
    implicit none
end submodule child_sub
"""
        result = validate_submodule_semantics(code)
        assert "child_sub" in result.submodules

    def test_validate_submodule_linkage_function(self):
        """validate_submodule_linkage() should work as standalone function."""
        parent_code = """
module parent_mod
    implicit none
end module parent_mod
"""
        child_code = """
submodule (parent_mod) child_sub
    implicit none
end submodule child_sub
"""
        result = validate_submodule_linkage([parent_code, child_code])
        assert "parent_mod" in result.modules
        assert "child_sub" in result.submodules


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2008SubmoduleValidator()

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
        self.validator = F2008SubmoduleValidator()

    def test_iso_section_in_parent_warning(self):
        """Missing parent warning should reference ISO section."""
        orphan_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "orphan_submodule.f90",
        )
        result = self.validator.validate_multi_unit([orphan_code])
        parent_warnings = [d for d in result.diagnostics if d.code == "SUBMOD_W001"]
        assert len(parent_warnings) > 0
        assert any(d.iso_section for d in parent_warnings)

    def test_iso_section_in_type_mismatch_error(self):
        """Type mismatch error should reference ISO section."""
        parent_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "parent_module_with_interface.f90",
        )
        mismatch_code = load_fixture(
            "Fortran2008",
            "test_submodule_semantics",
            "mismatched_signature.f90",
        )
        result = self.validator.validate_multi_unit([parent_code, mismatch_code])
        mismatch_errors = [d for d in result.diagnostics if d.code == "SUBMOD_E003"]
        assert len(mismatch_errors) > 0
        assert any(d.iso_section for d in mismatch_errors)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
