#!/usr/bin/env python3
"""Test IF construct parsing"""

import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'grammars'))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer  
from Fortran2018Parser import Fortran2018Parser
from antlr4.error.ErrorListener import ErrorListener

class ParseErrorListener(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors = []
        
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column}: {msg}")

def run_if_construct_case(code, description):
    print(f"Testing: {description}")
    print(f"Code: {code}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    error_listener = ParseErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)
    
    # Try different parsing methods
    methods = [
        ('program_unit_f2018', parser.program_unit_f2018),
    ]
    
    # Wrap in minimal program
    wrapped_code = f"""program test
{code}
end program test"""
    
    print(f"Wrapped: {wrapped_code}")
    input_stream2 = InputStream(wrapped_code)
    lexer2 = Fortran2018Lexer(input_stream2)
    token_stream2 = CommonTokenStream(lexer2)
    parser2 = Fortran2018Parser(token_stream2)
    
    error_listener2 = ParseErrorListener()
    parser2.removeErrorListeners() 
    parser2.addErrorListener(error_listener2)
    
    try:
        tree = parser2.program_unit_f2018()
        if error_listener2.errors:
            print(f"❌ Errors: {error_listener2.errors}")
        else:
            print(f"✅ Success!")
    except Exception as e:
        print(f"❌ Exception: {e}")
    print("-" * 50)

if __name__ == "__main__":
    run_if_construct_case("if (x > 0) then\nendif", "Simple IF-ENDIF")
    run_if_construct_case("if (x > 0) then\nend if", "Simple IF-END IF")
    run_if_construct_case("   if (mydummy > 90.and.gggg > 0) then\n   endif\n   end", "Missing-spaces fragment")
