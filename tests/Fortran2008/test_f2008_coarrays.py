#!/usr/bin/env python3
"""F2008 Coarray Tests - Comprehensive

Test F2008 coarray functionality including:
- Coarray declarations with [*] and [n] syntax  
- SYNC statements (SYNC ALL, SYNC IMAGES, SYNC MEMORY)
- THIS_IMAGE() and NUM_IMAGES() functions
- Coarray variable references
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser

class TestF2008Coarrays:
    """Test F2008 coarray functionality"""
    
    def parse_code(self, code):
        """Parse Fortran 2008 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2008Lexer(input_stream)
        parser = Fortran2008Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2008()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_coarray_declaration_star(self):
        """Test coarray declaration with [*] - any image"""
        code = """module coarray_mod
    implicit none
    integer :: shared_counter[*]
    real :: data_array(100)[*]
end module coarray_mod"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "Coarray [*] declaration failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for basic coarray syntax, got {errors}"
    
    def test_sync_all_statement(self):
        """Test SYNC ALL statement"""
        code = """program sync_test
    sync all
end program sync_test"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "SYNC ALL failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for SYNC ALL, got {errors}"
    
    def test_sync_images_statement(self):
        """Test SYNC IMAGES statement"""  
        code = """program sync_images_test
    sync images(*)
    sync images([1, 2, 3])
end program sync_images_test"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "SYNC IMAGES failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for SYNC IMAGES, got {errors}"
    
    def test_sync_memory_statement(self):
        """Test SYNC MEMORY statement"""
        code = """program sync_memory_test
    sync memory
end program sync_memory_test"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "SYNC MEMORY failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for SYNC MEMORY, got {errors}"
    
    def test_this_image_function(self):
        """Test THIS_IMAGE() intrinsic function"""
        code = """program this_image_test
    integer :: my_image
    my_image = this_image()
    print *, 'I am image', my_image
end program this_image_test"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "THIS_IMAGE() failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for THIS_IMAGE, got {errors}"
    
    def test_num_images_function(self):
        """Test NUM_IMAGES() intrinsic function"""
        code = """program num_images_test
    integer :: total_images
    total_images = num_images()  
    print *, 'Total images:', total_images
end program num_images_test"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "NUM_IMAGES() failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for NUM_IMAGES, got {errors}"

    def test_coarray_with_sync_comprehensive(self):
        """Comprehensive coarray example with sync operations"""
        code = """module comprehensive_coarray_mod
    implicit none
    integer :: counter[*]
    real :: results(100)[*]
    
contains
    subroutine parallel_work()
        integer :: my_img, total_imgs
        
        my_img = this_image()
        total_imgs = num_images()
        
        counter[my_img] = my_img * 10
        
        sync all
        
        if (my_img == 1) then
            print *, 'All images synchronized'
        end if
        
        sync memory
        
    end subroutine parallel_work
end module comprehensive_coarray_mod"""
        tree, errors = self.parse_code(code)
        assert tree is not None, "Comprehensive coarray failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for comprehensive coarray test, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
