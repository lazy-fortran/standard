#!/usr/bin/env python3
"""Test IF construct with execution body"""

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

def test_if_variants():
    test_cases = [
        # Empty body (our problematic case)
        """if (mydummy > 90.and.gggg > 0) then
endif""",
        # With statement body
        """if (mydummy > 90.and.gggg > 0) then
   x = 1
endif""",
        # With END IF instead of ENDIF  
        """if (mydummy > 90.and.gggg > 0) then
end if""",
        # With body and END IF
        """if (mydummy > 90.and.gggg > 0) then
   x = 1
end if""",
    ]
    
    for i, code in enumerate(test_cases, 1):
        print(f"Test {i}: {repr(code[:30])}...")
        
        input_stream = InputStream(code)
        lexer = Fortran2018Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Fortran2018Parser(token_stream)
        
        error_listener = ParseErrorListener()
        parser.removeErrorListeners()
        parser.addErrorListener(error_listener)
        
        try:
            tree = parser.if_construct()
            success = len(error_listener.errors) == 0
            print(f"  {'✅' if success else '❌'} {len(error_listener.errors)} errors")
            if error_listener.errors:
                print(f"    First error: {error_listener.errors[0]}")
        except Exception as e:
            print(f"  ❌ Exception: {e}")
        
        print()

if __name__ == "__main__":
    test_if_variants()