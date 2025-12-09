#!/usr/bin/env python3
"""Basic F2008 features tests - Foundation

Test core F2008 functionality including:
- Basic coarray syntax
- Submodule declarations  
- DO CONCURRENT constructs
- Enhanced intrinsic functions
- New token recognition
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from fixture_utils import load_fixture

class TestBasicF2008Features:
    """Test basic F2008 functionality"""
    
    def parse_code(self, code):
        """Parse Fortran 2008 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2008Lexer(input_stream)
        parser = Fortran2008Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2008()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_basic_module_inheritance(self):
        """F2008 should inherit basic module support from F2003"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "basic_module.f90",
        )
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Basic module inheritance failed: {errors} errors"
    
    def test_coarray_tokens_recognized(self):
        """Test that F2008 coarray tokens are recognized (future strict test)"""
        # Once issue #83 is fixed, this test should enforce zero syntax errors
        test_cases = [
            (
                "coarray_declaration.f90",
                "coarray declaration",
            ),
            (
                "coarray_sync_all.f90",
                "sync all statement",
            ),
        ]

        for fixture_name, description in test_cases:
            code = load_fixture(
                "Fortran2008",
                "test_basic_f2008_features",
                fixture_name,
            )
            tree, errors = self.parse_code(code)
            assert tree is not None, f"{description} failed to produce parse tree"
            assert errors == 0, f"{description}: expected 0 errors, got {errors}"

    def test_submodule_basic_syntax(self):
        """Test basic submodule syntax recognition (future strict test)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features_extra",
            "submodule_basic_syntax.f90",
        )

        tree, errors = self.parse_code(code)
        assert tree is not None, "Submodule syntax failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for basic submodule, got {errors}"

    def test_do_concurrent_tokens(self):
        """Test DO CONCURRENT token recognition (future strict test)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "do_concurrent.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "DO CONCURRENT failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for DO CONCURRENT, got {errors}"

    def test_enhanced_intrinsic_tokens(self):
        """Test that F2008 intrinsic function tokens are recognized"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "enhanced_intrinsics.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Enhanced intrinsics failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for intrinsics test, got {errors}"
    
    def test_enhanced_intrinsic_named_arguments(self):
        """Test F2008 intrinsic functions with named arguments and keywords"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "enhanced_intrinsics_named_args.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, (
            "Enhanced intrinsics with named arguments failed to produce parse tree"
        )
        assert errors == 0, (
            f"Expected 0 errors for intrinsics named-argument test, got {errors}"
        )
    
    def test_new_integer_kinds(self):
        """Test F2008 enhanced integer kind tokens"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "integer_kinds.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Integer kinds failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for integer kinds, got {errors}"

    def test_error_stop_token(self):
        """Test ERROR STOP statement"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "error_stop.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "ERROR STOP failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for ERROR STOP, got {errors}"
    
    def test_error_stop_no_code(self):
        """Test ERROR STOP without a stop code"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "error_stop_no_code.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "ERROR STOP without code failed to produce parse tree"
        assert errors == 0, (
            f"Expected 0 errors for ERROR STOP without code, got {errors}"
        )

    def test_contiguous_attribute_token(self):
        """Test CONTIGUOUS attribute"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "contiguous_attribute.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "CONTIGUOUS attribute failed to produce parse tree"
        # Track remaining work in issue #87
        assert errors == 0, f"Expected 0 errors for CONTIGUOUS attribute, got {errors}"

    def test_contiguous_standalone_statement(self):
        """Test CONTIGUOUS standalone attribute statement"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "contiguous_statement.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "CONTIGUOUS statement failed to produce parse tree"
        assert errors == 0, (
            f"Expected 0 errors for CONTIGUOUS statement, got {errors}"
        )

    def test_image_intrinsics(self):
        """Test coarray intrinsic functions"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "image_intrinsics.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Image intrinsics failed to produce parse tree"
        # Covered by the broader coarray work in issue #83
        assert errors == 0, f"Expected 0 errors for image intrinsics, got {errors}"

    def test_critical_construct(self):
        """Test CRITICAL construct for coarray synchronization (ISO 8.1.5)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "critical_construct.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "CRITICAL construct failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for CRITICAL construct, got {errors}"

    def test_lock_unlock_statements(self):
        """Test LOCK/UNLOCK statements for coarray synchronization (ISO 8.5.6)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "lock_unlock.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "LOCK/UNLOCK statements failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for LOCK/UNLOCK statements, got {errors}"

    def test_bit_shift_intrinsics(self):
        """Test bit shift intrinsics SHIFTL, SHIFTR, SHIFTA, MASKL, MASKR (ISO 13.7)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "bit_shift_intrinsics.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Bit shift intrinsics failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for bit shift intrinsics, got {errors}"

    def test_bit_reduction_intrinsics(self):
        """Test bitwise reduction intrinsics IALL, IANY, IPARITY (ISO 13.7)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "bit_reduction_intrinsics.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Bit reduction intrinsics failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for bit reduction intrinsics, got {errors}"

    def test_atomic_intrinsics(self):
        """Test ATOMIC_DEFINE and ATOMIC_REF intrinsics (ISO 13.7.19-20)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "atomic_intrinsics.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Atomic intrinsics failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for atomic intrinsics, got {errors}"

    def test_hypot_intrinsic(self):
        """Test HYPOT intrinsic function (ISO 13.7.77)"""
        code = load_fixture(
            "Fortran2008",
            "test_basic_f2008_features",
            "hypot_intrinsic.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "HYPOT intrinsic failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for HYPOT intrinsic, got {errors}"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
