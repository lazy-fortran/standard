#!/usr/bin/env python3
"""TDD tests for Issue #25 - PROGRAM, ASSOCIATE, BLOCK constructs"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture

class TestIssue25TDD:
    """Test-driven development for Issue #25 fixes"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_minimal_program_unit(self):
        """RED: Minimal PROGRAM should parse (BASELINE - should already work)"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "minimal_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Minimal PROGRAM should parse without errors, got {errors}"
    
    def test_program_with_simple_statement(self):
        """RED: PROGRAM with simple statements should work"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "program_with_stop.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PROGRAM with stop should parse, got {errors}"
    
    def test_minimal_associate_construct(self):
        """RED: Minimal ASSOCIATE should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "minimal_associate_construct.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Minimal ASSOCIATE should parse, got {errors}"
    
    def test_minimal_block_construct(self):
        """RED: Minimal BLOCK should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "minimal_block_construct.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Minimal BLOCK should parse, got {errors}"
    
    def test_associate_with_identifier_alias(self):
        """RED: ASSOCIATE with simple identifier"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "associate_with_identifier.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"ASSOCIATE with identifier should parse, got {errors}"
    
    def test_block_with_local_declaration(self):
        """RED: BLOCK with local variable"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "block_with_local_var.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BLOCK with local declaration should parse, got {errors}"
    
    def test_named_constructs(self):
        """RED: Named ASSOCIATE and BLOCK"""
        code = load_fixture(
            "Fortran2003",
            "test_issue25_tdd",
            "named_constructs_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Named constructs should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
