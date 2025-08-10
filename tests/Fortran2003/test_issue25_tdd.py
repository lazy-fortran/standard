#!/usr/bin/env python3
"""TDD tests for Issue #25 - PROGRAM, ASSOCIATE, BLOCK constructs"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

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
        code = """program test
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Minimal PROGRAM should parse without errors, got {errors}"
    
    def test_program_with_simple_statement(self):
        """RED: PROGRAM with simple statements should work"""
        code = """program test
    stop
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PROGRAM with stop should parse, got {errors}"
    
    def test_minimal_associate_construct(self):
        """RED: Minimal ASSOCIATE should parse"""
        code = """program test
    associate (x => 1)
        stop
    end associate
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Minimal ASSOCIATE should parse, got {errors}"
    
    def test_minimal_block_construct(self):
        """RED: Minimal BLOCK should parse"""
        code = """program test
    block
        stop
    end block
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Minimal BLOCK should parse, got {errors}"
    
    def test_associate_with_identifier_alias(self):
        """RED: ASSOCIATE with simple identifier"""
        code = """program test
    integer :: a
    associate (b => a)
        stop
    end associate  
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"ASSOCIATE with identifier should parse, got {errors}"
    
    def test_block_with_local_declaration(self):
        """RED: BLOCK with local variable"""
        code = """program test
    block
        integer :: local_var
        stop
    end block
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BLOCK with local declaration should parse, got {errors}"
    
    def test_named_constructs(self):
        """RED: Named ASSOCIATE and BLOCK"""
        code = """program test
    my_assoc: associate (x => 1)
        my_block: block
            stop
        end block my_block
    end associate my_assoc
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Named constructs should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])