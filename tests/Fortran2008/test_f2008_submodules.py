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
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from fixture_utils import load_fixture

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
        code = load_fixture(
            "Fortran2008",
            "test_f2008_submodules",
            "basic_submodule.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Basic submodule failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for basic submodule, got {errors}"
    
    def test_submodule_with_parent_hierarchy(self):
        """Test submodule with parent submodule reference"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_submodules",
            "submodule_parent_hierarchy.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Parent hierarchy failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for parent hierarchy, got {errors}"
    
    def test_submodule_with_procedures(self):
        """Test submodule containing module procedure implementations"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_submodules",
            "submodule_with_procedures.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Procedure submodule failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for procedure submodule, got {errors}"

    def test_nested_submodule_reference(self):
        """Test deeply nested submodule references"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_submodules",
            "nested_submodule_reference.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, "Nested submodule failed to produce parse tree"
        assert errors == 0, f"Expected 0 errors for nested submodule, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
