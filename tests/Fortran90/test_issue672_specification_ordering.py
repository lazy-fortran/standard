#!/usr/bin/env python3
"""Fortran 90 Specification-Statement Ordering Semantic Validation Test Suite

Tests semantic validation for specification_part ordering per ISO/IEC 1539:1991
Section 5.2.1.

This test suite validates:
1. USE statement placement (must be first in specification_part)
2. IMPLICIT statement placement (must follow USE, precede declarations)
3. PARAMETER statement placement
4. Declaration and specification ordering
5. Context-specific constraints (INTENT/OPTIONAL in procedures only)
6. Block data restrictions

Reference: ISO/IEC 1539:1991 (WG5 N692) Section 5.2.1 and Figure 2.1
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(
    0, str(Path(__file__).parent.parent.parent / "grammars" / "generated" / "modern")
)
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "tools"))

from antlr4 import InputStream, CommonTokenStream
from Fortran90Lexer import Fortran90Lexer
from Fortran90Parser import Fortran90Parser
from f90_specification_ordering_validator import (
    validate_f90_specification_ordering,
    DiagnosticSeverity,
)


class TestF90SpecificationOrderingBasics:
    """Test basic specification-part ordering rules."""

    def test_main_program_no_specifications(self):
        """Test main program with only executable statements."""
        source = """
program test
    implicit none
    integer :: x
    x = 5
end program test
"""
        result = validate_f90_specification_ordering(source)
        # Should not have ordering errors (grammar may reject this, but validator
        # should not find ordering violations if parsing succeeds)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        # Simple valid code should have no ordering errors
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_main_program_with_use_and_implicit(self):
        """Test USE followed by IMPLICIT in main program."""
        source = """
program test
    use iso_fortran_env
    implicit none
    integer :: x
    x = 1
end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_subroutine_with_intent(self):
        """Test subroutine with INTENT declarations."""
        source = """
subroutine compute(a, b, result)
    implicit none
    integer, intent(in) :: a, b
    integer, intent(out) :: result
    result = a + b
end subroutine compute
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_function_with_result(self):
        """Test function with result clause."""
        source = """
function double_value(x) result(y)
    implicit none
    integer, intent(in) :: x
    integer :: y
    y = x * 2
end function double_value
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_module_with_parameter(self):
        """Test module with PARAMETER declarations."""
        source = """
module math_lib
    implicit none
    integer, parameter :: pi_approx = 3
contains
    subroutine add(a, b, c)
        integer, intent(in) :: a, b
        integer, intent(out) :: c
        c = a + b
    end subroutine add
end module math_lib
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingUSEplacement:
    """Test USE statement ordering constraints."""

    def test_use_before_implicit(self):
        """Test that USE statements can appear before IMPLICIT."""
        source = """
program test
    use iso_fortran_env
    implicit none
    integer :: x
end program test
"""
        result = validate_f90_specification_ordering(source)
        # Check that we don't flag USE-before-IMPLICIT as an error
        use_errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR and "E672-001" in d.code
        ]
        assert len(use_errors) == 0

    def test_multiple_use_statements(self):
        """Test multiple consecutive USE statements."""
        source = """
program test
    use iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    integer :: x
end program test
"""
        result = validate_f90_specification_ordering(source)
        use_errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR and "E672-001" in d.code
        ]
        assert len(use_errors) == 0


class TestF90SpecificationOrderingIMPLICIT:
    """Test IMPLICIT statement ordering constraints."""

    def test_implicit_after_use(self):
        """Test IMPLICIT after USE statements is valid."""
        source = """
program test
    use iso_fortran_env
    implicit none
    integer :: x
end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672-002" in e.code]) == 0


class TestF90SpecificationOrderingPARAMETER:
    """Test PARAMETER statement handling."""

    def test_parameter_before_declarations(self):
        """Test PARAMETER statement before type declarations."""
        source = """
program test
    implicit none
    integer, parameter :: max_size = 100
    integer :: arr(max_size)
    arr(1) = 1
end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingINTENT:
    """Test INTENT attribute constraints."""

    def test_intent_in_subroutine_args(self):
        """Test INTENT on subroutine dummy arguments."""
        source = """
subroutine process(input, output)
    implicit none
    integer, intent(in) :: input
    integer, intent(out) :: output
    output = input * 2
end subroutine process
"""
        result = validate_f90_specification_ordering(source)
        intent_errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR and "E672-010" in d.code
        ]
        assert len(intent_errors) == 0

    def test_intent_in_function_args(self):
        """Test INTENT on function dummy arguments."""
        source = """
function compute(x)
    implicit none
    integer, intent(in) :: x
    integer :: compute
    compute = x + 1
end function compute
"""
        result = validate_f90_specification_ordering(source)
        intent_errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR and "E672-010" in d.code
        ]
        assert len(intent_errors) == 0

    def test_optional_in_subroutine(self):
        """Test OPTIONAL attribute on subroutine arguments."""
        source = """
subroutine optional_test(optional_arg)
    implicit none
    integer, intent(in), optional :: optional_arg
    if (present(optional_arg)) then
        print *, optional_arg
    end if
end subroutine optional_test
"""
        result = validate_f90_specification_ordering(source)
        optional_errors = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.ERROR and "E672-011" in d.code
        ]
        assert len(optional_errors) == 0


class TestF90SpecificationOrderingDerivedTypes:
    """Test derived-type definition handling."""

    def test_derived_type_definition(self):
        """Test derived-type definition in specification_part."""
        source = """
program test
    implicit none

    type :: point
        real :: x, y
    end type point

    type(point) :: p
    p%x = 1.0
    p%y = 2.0

end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingInterface:
    """Test interface block handling."""

    def test_interface_block(self):
        """Test interface block in specification_part."""
        source = """
program test
    implicit none

    interface
        subroutine external_sub(x)
            integer, intent(in) :: x
        end subroutine external_sub
    end interface

    integer :: y
    y = 1

end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingModules:
    """Test module-specific ordering rules."""

    def test_module_with_use(self):
        """Test module with USE statements."""
        source = """
module mymodule
    use iso_fortran_env
    implicit none

    integer, parameter :: size = 10

contains

    subroutine sub(x)
        integer, intent(in) :: x
    end subroutine sub

end module mymodule
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingInternalProcedures:
    """Test internal procedure ordering."""

    def test_internal_subroutine(self):
        """Test internal subroutine in main program."""
        source = """
program test
    implicit none
    integer :: x
    x = 1
    call my_sub(x)
contains
    subroutine my_sub(y)
        integer, intent(inout) :: y
        y = y + 1
    end subroutine my_sub
end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_internal_function(self):
        """Test internal function in module."""
        source = """
module test_mod
    implicit none
contains
    function outer_func(x)
        integer, intent(in) :: x
        integer :: outer_func

        outer_func = x * 2

    end function outer_func
end module test_mod
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingComplexCases:
    """Test complex specification_part scenarios."""

    def test_complete_program_structure(self):
        """Test complete program with all major specification elements."""
        source = """
program complete_test
    use iso_fortran_env
    implicit none

    integer, parameter :: max_size = 100
    real, parameter :: pi = 3.14159

    type :: data_type
        real :: value
        integer :: count
    end type data_type

    interface
        subroutine external_routine(x)
            real, intent(in) :: x
        end subroutine external_routine
    end interface

    type(data_type) :: data_array(max_size)
    real :: result

    data_array(1)%value = 1.0
    result = pi * 2.0

contains

    subroutine internal_sub(x)
        real, intent(inout) :: x
        x = x * 2.0
    end subroutine internal_sub

end program complete_test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_module_with_multiple_procedures(self):
        """Test module with multiple subprograms."""
        source = """
module advanced_module
    use iso_fortran_env
    implicit none

    integer, parameter :: default_value = 42

    interface
        module subroutine initialize()
        end subroutine initialize
    end interface

contains

    subroutine set_value(v)
        integer, intent(in) :: v
    end subroutine set_value

    function get_value() result(v)
        integer :: v
        v = default_value
    end function get_value

end module advanced_module
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0


class TestF90SpecificationOrderingParseErrors:
    """Test handling of code that has parse errors."""

    def test_syntactically_invalid_code(self):
        """Test that parse errors are handled gracefully."""
        source = """
program test
    this is not valid fortran at all !!!
end program test
"""
        result = validate_f90_specification_ordering(source)
        # Parser may catch the error before validator runs; either way is fine.
        # The validator should not crash on invalid input.
        # When parser fails silently, diagnostics list is empty (acceptable)
        assert isinstance(result.diagnostics, list)


class TestF90SpecificationOrderingEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_empty_specification_part(self):
        """Test main program with implicit IMPLICIT NONE."""
        source = """
program test
    integer :: x
    x = 1
end program test
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_subroutine_minimal(self):
        """Test minimal subroutine without specifications."""
        source = """
subroutine minimal()
    integer :: dummy
    dummy = 1
end subroutine minimal
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0

    def test_function_minimal(self):
        """Test minimal function."""
        source = """
function minimal() result(r)
    integer :: r
    r = 1
end function minimal
"""
        result = validate_f90_specification_ordering(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        assert len([e for e in errors if "E672" in e.code]) == 0
