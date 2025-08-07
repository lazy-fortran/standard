#!/usr/bin/env python3
"""
Simple working test for Fortran 2003 parser to verify basic functionality.
"""

import sys
import os
import pytest
from pathlib import Path

# Add grammars directory to path
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars"))

from antlr4 import *

class TestSimpleFortran2003:
    """Simple tests to verify F2003 parser works correctly."""
    
    def parse_fortran_code(self, code, expect_success=True):
        """Helper to parse Fortran 2003 code."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser
            
            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))
            
            # Parse starting with F2003 program unit
            tree = parser.program_unit_f2003()
            errors = parser.getNumberOfSyntaxErrors()
            
            if expect_success:
                assert errors == 0, f"Parse failed with {errors} errors"
                assert tree is not None, "Parse tree is None"
            
            return tree, errors, None
            
        except Exception as e:
            if expect_success:
                assert False, f"Exception during parsing: {e}"
            return None, -1, str(e)
    
    def validate_parse_tree(self, tree, expected_rule_name):
        """Validate that the parse tree has the expected structure."""
        assert tree is not None, "Parse tree is None"
        
        # Get the rule name from the tree
        rule_name = tree.__class__.__name__.replace('Context', '')
        assert rule_name == expected_rule_name, f"Expected {expected_rule_name}, got {rule_name}"
        
        # Check that tree has children
        assert hasattr(tree, 'children') and tree.children, "Parse tree has no children"
        
        return True
    
    def test_simple_program(self):
        """Test simple F2003 program parsing."""
        simple_program = """program simple
end program simple
"""
        tree, errors, exception = self.parse_fortran_code(simple_program)
        self.validate_parse_tree(tree, "Program_unit_f2003")
    
    def test_simple_module(self):
        """Test simple F2003 module parsing.""" 
        simple_module = """module simple_mod
end module simple_mod
"""
        tree, errors, exception = self.parse_fortran_code(simple_module)
        self.validate_parse_tree(tree, "Program_unit_f2003")
    
    def test_class_declaration(self):
        """Test F2003 CLASS declaration parsing."""
        class_code = """program test_class
class(integer), pointer :: obj
end program test_class
"""
        tree, errors, exception = self.parse_fortran_code(class_code)
        self.validate_parse_tree(tree, "Program_unit_f2003")
    
    def test_procedure_pointer(self):
        """Test F2003 procedure pointer parsing."""
        proc_code = """program test_proc
procedure(interface), pointer :: proc_ptr
end program test_proc
"""
        tree, errors, exception = self.parse_fortran_code(proc_code)
        self.validate_parse_tree(tree, "Program_unit_f2003")
    
    def test_parse_tree_structure(self):
        """Test detailed parse tree structure."""
        code = """program test
end program test
"""
        tree, _, _ = self.parse_fortran_code(code)
        
        # Verify parse tree structure
        assert hasattr(tree, 'main_program_f2003'), "Missing main_program_f2003 in parse tree"
        
        main_prog = tree.main_program_f2003()
        assert main_prog is not None, "main_program_f2003 is None"
        
        # Check for program statement
        prog_stmt = main_prog.program_stmt()
        assert prog_stmt is not None, "program_stmt is None"
        
        # Check for end program statement  
        end_stmt = main_prog.end_program_stmt()
        assert end_stmt is not None, "end_program_stmt is None"
    
    def test_error_handling(self):
        """Test that invalid syntax is properly rejected."""
        invalid_code = "invalid fortran syntax here"
        
        tree, errors, exception = self.parse_fortran_code(invalid_code, expect_success=False)
        assert errors > 0, "Expected parsing errors for invalid code"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])