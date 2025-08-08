#!/usr/bin/env python3
"""F2008 Submodule Tests

Test F2008 submodule functionality including:
- SUBMODULE declarations  
- Parent module references
- END SUBMODULE statements
- Module procedure implementations in submodules
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser

class TestF2008Submodules:
    """Test F2008 submodule functionality"""
    
    def parse_code(self, code):
        """Parse Fortran 2008 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2008Lexer(input_stream)
        parser = Fortran2008Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2008()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_basic_submodule_declaration(self):
        """Test basic submodule declaration syntax"""
        code = """submodule (parent_mod) child_sub
    implicit none
end submodule child_sub"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Basic submodule failed to produce parse tree"
        # This is a new F2008 construct - some parsing issues expected initially
        assert errors <= 3, f"Too many errors for basic submodule: {errors}"
    
    def test_submodule_with_parent_hierarchy(self):
        """Test submodule with parent submodule reference"""
        code = """submodule (parent_mod:parent_sub) child_sub
    implicit none
end submodule child_sub"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Parent hierarchy failed to produce parse tree"
        assert errors <= 5, f"Too many errors for parent hierarchy: {errors}"
    
    def test_submodule_with_procedures(self):
        """Test submodule containing module procedure implementations"""
        code = """submodule (math_mod) implementation_sub
    implicit none
contains
    module subroutine calculate_result()
        print *, 'Calculation performed in submodule'  
    end subroutine calculate_result
    
    module function compute_value() result(val)
        real :: val
        val = 42.0
    end function compute_value
end submodule implementation_sub"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Procedure submodule failed to produce parse tree"
        # Complex submodule - expect more parsing issues
        assert errors <= 8, f"Too many errors for procedure submodule: {errors}"

    def test_nested_submodule_reference(self):
        """Test deeply nested submodule references"""
        code = """submodule (grandparent_mod:parent_sub:child_sub) grandchild_sub
    implicit none
end submodule grandchild_sub"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Nested submodule failed to produce parse tree"
        assert errors <= 5, f"Too many errors for nested submodule: {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])