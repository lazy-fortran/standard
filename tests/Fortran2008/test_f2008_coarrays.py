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
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from fixture_utils import load_fixture

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
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "coarray_declaration_star.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Coarray [*] declaration failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for basic coarray syntax, got {errors}"
    
    def test_sync_all_statement(self):
        """Test SYNC ALL statement"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "sync_all.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "SYNC ALL failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for SYNC ALL, got {errors}"
    
    def test_sync_images_statement(self):
        """Test SYNC IMAGES statement"""  
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "sync_images.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "SYNC IMAGES failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for SYNC IMAGES, got {errors}"
    
    def test_sync_memory_statement(self):
        """Test SYNC MEMORY statement"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "sync_memory.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "SYNC MEMORY failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for SYNC MEMORY, got {errors}"
    
    def test_this_image_function(self):
        """Test THIS_IMAGE() intrinsic function"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "this_image_test.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "THIS_IMAGE() failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for THIS_IMAGE, got {errors}"
    
    def test_num_images_function(self):
        """Test NUM_IMAGES() intrinsic function"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "num_images_test.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "NUM_IMAGES() failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for NUM_IMAGES, got {errors}"

    def test_coarray_with_sync_comprehensive(self):
        """Comprehensive coarray example with sync operations"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "comprehensive_coarray_mod.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Comprehensive coarray failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for comprehensive coarray test, got {errors}"

    def test_coarray_section_reference(self):
        """Test coarray array section reference with image selector"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_coarrays",
            "coarray_section_reference.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Coarray section reference failed to produce parse tree"
        assert errors == 0, (
            f"Expected 0 errors for coarray section reference, got {errors}"
        )

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
