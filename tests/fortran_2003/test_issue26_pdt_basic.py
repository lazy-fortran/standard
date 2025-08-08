#!/usr/bin/env python3
"""Basic PDT tests for Issue #26 - focusing on what works

Test the basic PDT infrastructure that is parsing correctly.
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

class TestIssue26PDTBasic:
    """Basic PDT tests for what currently works"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_empty_parameterized_type(self):
        """Empty PDT should parse"""
        code = """module test
    type :: matrix(k)
    end type matrix
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Empty PDT failed: {errors} errors"
    
    def test_pdt_with_parameter_definition(self):
        """PDT with parameter definition only should parse"""
        code = """module test
    type :: vector(k)
        integer, kind :: k = 4
    end type vector
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT with parameter def failed: {errors} errors"
    
    def test_pdt_multiple_parameters(self):
        """PDT with multiple parameter definitions should parse"""  
        code = """module test
    type :: matrix(k, n, m)
        integer, kind :: k = 4
        integer, len :: n, m
    end type matrix
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT with multiple params failed: {errors} errors"
    
    def test_pdt_instantiation_basic(self):
        """Basic PDT instantiation should parse"""
        code = """module test
    type :: matrix(k)
        integer, kind :: k = 4
    end type matrix
    
    type(matrix) :: my_matrix
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT instantiation failed: {errors} errors"

    def test_kind_selectors_improved(self):
        """Kind selectors with integers and parameters should work"""
        code = """module test
    real(4) :: x
    real(8) :: y
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Kind selectors failed: {errors} errors"

    @pytest.mark.xfail(reason="PDT components not yet implemented")
    def test_pdt_with_components_real_usage(self):
        """Real PDT usage with components - currently fails"""
        code = """module test
    type :: matrix(k, n, m)
        integer, kind :: k = 4
        integer, len :: n, m
        real(k) :: data(n, m)
    end type matrix
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, "PDT with real components should parse"

    @pytest.mark.xfail(reason="PDT instantiation with parameters not implemented")
    def test_pdt_instantiation_with_params(self):
        """PDT instantiation with actual parameters - currently fails"""
        code = """module test
    type :: vector(k, n)
        integer, kind :: k = 4
        integer, len :: n
        real(k) :: data(n)
    end type vector
    
    type(vector(k=8, n=100)) :: my_vector
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, "PDT instantiation with params should parse"

    @pytest.mark.xfail(reason="Deferred/assumed parameters not implemented")
    def test_pdt_deferred_assumed_params(self):
        """Deferred (:) and assumed (*) parameters - currently fails"""
        code = """module test
    type :: flexible(k, n)
        integer, kind :: k = 4
        integer, len :: n
        real(k) :: data(n)
    end type flexible
    
    type(flexible(k=:, n=*)) :: deferred_vector
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, "Deferred/assumed parameters should parse"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])