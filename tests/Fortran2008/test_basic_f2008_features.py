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
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser

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
        code = """module test_mod
    implicit none
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Basic module inheritance failed: {errors} errors"
    
    @pytest.mark.skip(reason="Fortran 2008 coarray declarations and sync statements not yet fully implemented (see issue #83)")
    def test_coarray_tokens_recognized(self):
        """Test that F2008 coarray tokens are recognized (future strict test)"""
        # Once issue #83 is fixed, this test should enforce zero syntax errors
        test_cases = [
            ("module test; integer :: x[*]; end module test", "coarray declaration"),
            ("program sync_test\n    sync all\nend program sync_test", "sync all statement"),
        ]

        for code, description in test_cases:
            tree, errors = self.parse_code(code)
            assert tree is not None, f"{description} failed to produce parse tree"
            assert errors == 0, f"{description}: expected 0 errors, got {errors}"

    @pytest.mark.skip(reason="Fortran 2008 submodule grammar still incomplete (see issue #85)")
    def test_submodule_basic_syntax(self):
        """Test basic submodule syntax recognition (future strict test)"""
        code = """submodule (parent_mod) child_sub
    implicit none
end submodule child_sub"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "Submodule syntax failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for basic submodule, got {errors}"

    @pytest.mark.skip(reason="Fortran 2008 DO CONCURRENT syntax not yet fully implemented (see issue #84)")
    def test_do_concurrent_tokens(self):
        """Test DO CONCURRENT token recognition (future strict test)"""
        code = """module test
    contains
    subroutine test_proc()
        do concurrent (i = 1:10)
        end do
    end subroutine test_proc
end module test"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "DO CONCURRENT failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for DO CONCURRENT, got {errors}"

    @pytest.mark.skip(reason="Fortran 2008 intrinsic procedure call syntax not yet fully implemented (see issue #86)")
    def test_enhanced_intrinsic_tokens(self):
        """Test that F2008 intrinsic function tokens are recognized (future strict test)"""
        code = """module test_intrinsics
    implicit none
    contains
    subroutine test_functions()
        real :: x, result_val
        x = 1.0
        result_val = bessel_j0(x)
        result_val = erf(x)
        result_val = gamma(x)
    end subroutine test_functions
end module test_intrinsics"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "Enhanced intrinsics failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for intrinsics test, got {errors}"
    
    def test_new_integer_kinds(self):
        """Test F2008 enhanced integer kind tokens"""
        code = """module test_kinds
    implicit none
    int8 :: small_int
    int16 :: medium_int
    int32 :: normal_int
    int64 :: big_int
end module test_kinds"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "Integer kinds failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for integer kinds, got {errors}"

    @pytest.mark.skip(reason="Fortran 2008 ERROR STOP not yet fully implemented (see issue #87)")
    def test_error_stop_token(self):
        """Test ERROR STOP statement token"""
        code = """program test
    error stop 'Critical error occurred'
end program test"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "ERROR STOP failed to produce parse tree"
        # Track remaining work in issue #87
        assert errors == 0, f"Expected 0 errors for ERROR STOP, got {errors}"
    
    @pytest.mark.skip(reason="Fortran 2008 CONTIGUOUS attribute not yet fully implemented (see issue #87)")
    def test_contiguous_attribute_token(self):
        """Test CONTIGUOUS attribute token"""
        code = """module test_contiguous
    implicit none
    real, contiguous, pointer :: array_ptr(:)
end module test_contiguous"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "CONTIGUOUS attribute failed to produce parse tree"
        # Track remaining work in issue #87
        assert errors == 0, f"Expected 0 errors for CONTIGUOUS attribute, got {errors}"

    @pytest.mark.skip(reason="Fortran 2008 image intrinsics rely on full coarray support (see issue #83)")
    def test_image_intrinsics(self):
        """Test coarray intrinsic functions"""
        code = """program coarray_test
    integer :: my_img, total_imgs
    my_img = this_image()
    total_imgs = num_images()
    print *, 'Image', my_img, 'of', total_imgs
end program coarray_test"""

        tree, errors = self.parse_code(code)
        assert tree is not None, "Image intrinsics failed to produce parse tree"
        # Covered by the broader coarray work in issue #83
        assert errors == 0, f"Expected 0 errors for image intrinsics, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
