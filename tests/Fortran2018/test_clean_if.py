#!/usr/bin/env python3
"""Test clean IF construct parsing"""

import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'grammars'))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer  
from Fortran2018Parser import Fortran2018Parser
from antlr4.error.ErrorListener import ErrorListener

class TestErrorListener(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors = []
        
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column}: {msg}")

def test_clean_if():
    # Test just the IF construct without orphaned end
    code = """if (mydummy > 90.and.gggg > 0) then
endif"""
    
    print(f"Testing clean IF construct:")
    print(f"Code: {repr(code)}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    error_listener = TestErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)
    
    try:
        tree = parser.if_construct()
        if error_listener.errors:
            print(f"❌ Errors: {error_listener.errors}")
        else:
            print(f"✅ Success!")
    except Exception as e:
        print(f"❌ Exception: {e}")

def test_wrapped_program():
    # Test wrapped in program
    code = """program test
if (mydummy > 90.and.gggg > 0) then
endif
end program test"""
    
    print(f"\nTesting wrapped in program:")
    print(f"Code: {repr(code)}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    error_listener = TestErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)
    
    try:
        tree = parser.program_unit_f2018()
        if error_listener.errors:
            print(f"❌ Errors: {error_listener.errors}")
        else:
            print(f"✅ Success!")
    except Exception as e:
        print(f"❌ Exception: {e}")

if __name__ == "__main__":
    test_clean_if()
    test_wrapped_program()