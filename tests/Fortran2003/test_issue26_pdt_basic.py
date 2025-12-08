#!/usr/bin/env python3
"""Basic PDT tests for Issue #26 - focusing on what works

Test the basic PDT infrastructure that is parsing correctly.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture

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
        code = load_fixture(
            "Fortran2003",
            "test_issue26_pdt_basic",
            "empty_parameterized_type.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Empty PDT failed: {errors} errors"
    
    def test_pdt_with_parameter_definition(self):
        """PDT with parameter definition only should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue26_pdt_basic",
            "pdt_with_parameter_definition.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT with parameter def failed: {errors} errors"
    
    def test_pdt_multiple_parameters(self):
        """PDT with multiple parameter definitions should parse"""  
        code = load_fixture(
            "Fortran2003",
            "test_issue26_pdt_basic",
            "pdt_multiple_parameters.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT with multiple params failed: {errors} errors"
    
    def test_pdt_instantiation_basic(self):
        """Basic PDT instantiation should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue26_pdt_basic",
            "pdt_instantiation_basic.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT instantiation failed: {errors} errors"

    def test_pdt_as_component_and_procedure_arg(self):
        """PDT used as component and procedure argument should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue26_pdt_basic",
            "pdt_component_and_arg.f90",
        )

        tree, errors = self.parse_code(code)
        assert errors == 0, f"PDT as component/arg failed: {errors} errors"

    def test_kind_selectors_improved(self):
        """Kind selectors with integers and parameters should work"""
        code = load_fixture(
            "Fortran2003",
            "test_issue26_pdt_basic",
            "kind_selectors.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Kind selectors failed: {errors} errors"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
