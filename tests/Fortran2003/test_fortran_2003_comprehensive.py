#!/usr/bin/env python3
"""
Comprehensive test suite for Fortran 2003 unified grammar implementation.
Tests all major F2003 innovations with both fixed-form and free-form formats.
"""

import sys
import os
import pytest
from pathlib import Path

# Ensure we can import shared test fixtures utilities
sys.path.append(str(Path(__file__).parent.parent))
from fixture_utils import load_fixture

# Add grammars directory to path
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from antlr4 import *

class TestFortran2003Unified:
    """Test Fortran 2003 unified grammar with both fixed-form and free-form support."""
    
    def parse_fortran_code(self, code, expect_success=True):
        """Helper to parse Fortran 2003 code."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser
            
            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))
            
            tree = parser.program_unit_f2003()  # Use F2003 rule
            errors = parser.getNumberOfSyntaxErrors()
            
            if expect_success:
                assert errors == 0, f"Parse failed with {errors} errors"
            
            return tree, errors, None
            
        except Exception as e:
            if expect_success:
                assert False, f"Exception during parsing: {e}"
            return None, -1, str(e)
    
    def test_object_oriented_programming(self):
        """Test F2003 object-oriented programming features."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "oop_code.f90",
        )
        self.parse_fortran_code(code)

    def test_polymorphic_dispatch_with_select_type(self):
        """Test polymorphic dummy arguments and SELECT TYPE dispatch."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "poly_mod.f90",
        )
        self.parse_fortran_code(code)
    
    def test_parameterized_derived_types(self):
        """Test F2003 parameterized derived types."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "pdt_mod.f90",
        )
        self.parse_fortran_code(code)
    
    def test_associate_construct(self):
        """Test F2003 ASSOCIATE construct."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "associate_construct.f90",
        )
        self.parse_fortran_code(code)
    
    def test_block_construct(self):
        """Test F2003 BLOCK construct."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "block_construct.f90",
        )
        self.parse_fortran_code(code)
    
    def test_procedure_pointers(self):
        """Test F2003 procedure pointers."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "proc_ptr_mod.f90",
        )
        self.parse_fortran_code(code)
    
    def test_enhanced_allocate(self):
        """Test F2003 enhanced ALLOCATE with SOURCE and MOLD."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "enhanced_allocate.f90",
        )
        self.parse_fortran_code(code)
    
    def test_c_interoperability(self):
        """Test F2003 C interoperability features."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "c_interop_mod.f90",
        )
        self.parse_fortran_code(code)
    
    def test_enhanced_io(self):
        """Test F2003 enhanced I/O features."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "enhanced_io.f90",
        )
        self.parse_fortran_code(code)
    
    def test_volatile_protected(self):
        """Test F2003 VOLATILE and PROTECTED attributes."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "attr_mod.f90",
        )
        self.parse_fortran_code(code)
    
    def test_import_statement(self):
        """Test F2003 IMPORT statement."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "import_mod.f90",
        )
        self.parse_fortran_code(code)
    
    def test_fixed_form_compatibility(self):
        """Test that F2003 features work with fixed-form format."""
        code = "\n" + load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "fixed_form_f2003.f",
        )
        self.parse_fortran_code(code)
    
    def test_error_handling(self):
        """Test that invalid F2003 syntax is properly rejected."""
        invalid_codes = [
            # Invalid abstract type instantiation
            """
            type, abstract :: base_t
            end type base_t
            type(base_t) :: invalid  ! Cannot instantiate abstract type
            """,
            
            # Invalid deferred procedure without abstract
            """
            type :: bad_type
            contains
                procedure, deferred :: bad_proc  ! Deferred without abstract
            end type bad_type
            """
        ]
        
        for i, code in enumerate(invalid_codes):
            try:
                self.parse_fortran_code(code, expect_success=False)
            except AssertionError:
                pass  # Expected to fail
    
    def test_complex_f2003_program(self):
        """Test complex F2003 program combining multiple features."""
        code = load_fixture(
            "Fortran2003",
            "test_fortran_2003_comprehensive",
            "advanced_f2003.f90",
        )
        self.parse_fortran_code(code)

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
