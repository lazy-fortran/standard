#!/usr/bin/env python3
"""Fortran 90 CYCLE/EXIT Semantic Validation Test Suite (Issue #677)

Tests semantic validation for CYCLE/EXIT statements per ISO/IEC 1539:1991
Section 8.1.4.4.

This test suite validates:
1. CYCLE/EXIT placement within DO constructs
2. Construct-name matching and binding
3. Error detection for invalid placements/bindings

Reference: ISO/IEC 1539:1991 (WG5 N692) Section 8.1.4.4
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
from f90_cycle_exit_validator import (
    validate_f90_cycle_exit,
    DiagnosticSeverity,
)


class TestF90CycleExitBasics:
    """Test basic CYCLE/EXIT functionality."""

    def test_cycle_in_do_construct(self):
        """Test valid CYCLE inside DO construct."""
        source = """
program test
    implicit none
    integer :: i
    do i = 1, 10
        if (i == 5) then
            cycle
        end if
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors
        assert result.error_count == 0

    def test_exit_in_do_construct(self):
        """Test valid EXIT inside DO construct."""
        source = """
program test
    implicit none
    integer :: i
    do i = 1, 10
        if (i == 5) then
            exit
        end if
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors
        assert result.error_count == 0

    def test_nested_do_with_cycle(self):
        """Test CYCLE in nested DO constructs without names."""
        source = """
program test
    implicit none
    integer :: i, j
    do i = 1, 10
        do j = 1, 10
            if (j == 5) cycle
        end do
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_nested_do_with_exit(self):
        """Test EXIT in nested DO constructs without names."""
        source = """
program test
    implicit none
    integer :: i, j
    do i = 1, 10
        do j = 1, 10
            if (j == 5) exit
        end do
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors


class TestF90CycleExitWithNames:
    """Test CYCLE/EXIT with construct names."""

    def test_named_do_cycle_matching(self):
        """Test CYCLE with matching named DO construct."""
        source = """
program test
    implicit none
    integer :: i
    outer: do i = 1, 10
        if (i == 5) then
            cycle outer
        end if
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_named_do_exit_matching(self):
        """Test EXIT with matching named DO construct."""
        source = """
program test
    implicit none
    integer :: i
    outer: do i = 1, 10
        if (i == 5) then
            exit outer
        end if
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_nested_named_do_cycle_inner(self):
        """Test CYCLE in nested named DO constructs targeting inner loop."""
        source = """
program test
    implicit none
    integer :: i, j
    outer: do i = 1, 10
        inner: do j = 1, 10
            if (j == 5) then
                cycle inner
            end if
        end do inner
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_nested_named_do_cycle_outer(self):
        """Test CYCLE in nested named DO constructs targeting outer loop."""
        source = """
program test
    implicit none
    integer :: i, j
    outer: do i = 1, 10
        inner: do j = 1, 10
            if (j == 5) cycle outer
        end do inner
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_nested_named_do_exit_outer(self):
        """Test EXIT in nested named DO constructs targeting outer loop."""
        source = """
program test
    implicit none
    integer :: i, j
    outer: do i = 1, 10
        inner: do j = 1, 10
            if (j == 5) exit outer
        end do inner
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors


class TestF90CycleExitErrors:
    """Test error detection for invalid CYCLE/EXIT."""

    def test_cycle_outside_do(self):
        """Test CYCLE outside any DO construct."""
        source = """
program test
    implicit none
    cycle
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 1
        assert result.diagnostics[0].code == "E001"

    def test_exit_outside_do(self):
        """Test EXIT outside any DO construct."""
        source = """
program test
    implicit none
    exit
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 1
        assert result.diagnostics[0].code == "E003"

    def test_cycle_with_nonexistent_name(self):
        """Test CYCLE with construct name that doesn't exist."""
        source = """
program test
    implicit none
    integer :: i
    outer: do i = 1, 10
        if (i == 5) then
            cycle nonexistent
        end if
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 1
        assert result.diagnostics[0].code == "E002"

    def test_exit_with_nonexistent_name(self):
        """Test EXIT with construct name that doesn't exist."""
        source = """
program test
    implicit none
    integer :: i
    outer: do i = 1, 10
        if (i == 5) then
            exit nonexistent
        end if
    end do outer
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 1
        assert result.diagnostics[0].code == "E004"

    def test_cycle_to_sibling_construct(self):
        """Test CYCLE targeting a sibling DO construct (not in scope)."""
        source = """
program test
    implicit none
    integer :: i, j
    outer: do i = 1, 10
        if (i == 5) then
            exit
        end if
    end do outer
    inner: do j = 1, 10
        if (j == 5) then
            cycle outer
        end if
    end do inner
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 1
        assert result.diagnostics[0].code == "E002"

    def test_exit_to_sibling_construct(self):
        """Test EXIT targeting a sibling DO construct (not in scope)."""
        source = """
program test
    implicit none
    integer :: i, j
    outer: do i = 1, 10
        if (i == 5) then
            exit
        end if
    end do outer
    inner: do j = 1, 10
        if (j == 5) then
            exit outer
        end if
    end do inner
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 1
        assert result.diagnostics[0].code == "E004"

    def test_multiple_cycle_errors(self):
        """Test multiple invalid CYCLE statements."""
        source = """
program test
    implicit none
    integer :: i
    do i = 1, 10
        if (i == 2) then
            cycle invalid1
        end if
        if (i == 3) then
            cycle invalid2
        end if
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.has_errors
        assert result.error_count == 2


class TestF90CycleExitComplexScenarios:
    """Test complex scenarios with CYCLE/EXIT."""

    def test_do_while_with_cycle(self):
        """Test CYCLE in DO WHILE construct."""
        source = """
program test
    implicit none
    integer :: i
    i = 1
    do while (i <= 10)
        if (i == 5) then
            cycle
        end if
        i = i + 1
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_do_while_with_exit(self):
        """Test EXIT in DO WHILE construct."""
        source = """
program test
    implicit none
    integer :: i
    i = 1
    do while (i <= 10)
        if (i == 5) then
            exit
        end if
        i = i + 1
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_triple_nested_do_cycles(self):
        """Test CYCLE in triple-nested DO constructs."""
        source = """
program test
    implicit none
    integer :: i, j, k
    l1: do i = 1, 10
        l2: do j = 1, 10
            l3: do k = 1, 10
                if (k == 5) then
                    cycle l3
                end if
                if (j == 5) then
                    cycle l2
                end if
                if (i == 5) then
                    cycle l1
                end if
            end do l3
        end do l2
    end do l1
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors

    def test_cycle_exit_mixed_in_if(self):
        """Test CYCLE and EXIT mixed in IF constructs."""
        source = """
program test
    implicit none
    integer :: i
    do i = 1, 10
        if (i < 5) then
            cycle
        else if (i > 8) then
            exit
        end if
    end do
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert not result.has_errors


class TestF90CycleExitDiagnostics:
    """Test diagnostic properties."""

    def test_error_has_iso_section(self):
        """Test that error diagnostics include ISO section reference."""
        source = """
program test
    implicit none
    cycle
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.diagnostics
        assert result.diagnostics[0].iso_section == "8.1.4.4.3"

    def test_error_has_severity(self):
        """Test that error diagnostics have ERROR severity."""
        source = """
program test
    implicit none
    exit
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.diagnostics[0].severity == DiagnosticSeverity.ERROR

    def test_error_has_location(self):
        """Test that error diagnostics include line information."""
        source = """
program test
    implicit none
    cycle
end program test
"""
        result = validate_f90_cycle_exit(source)
        assert result.diagnostics[0].line is not None
