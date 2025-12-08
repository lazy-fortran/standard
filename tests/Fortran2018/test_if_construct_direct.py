#!/usr/bin/env python3
"""Test IF construct directly"""

import sys
import os
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'grammars'))
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer  
from Fortran2018Parser import Fortran2018Parser
from antlr4.error.ErrorListener import ErrorListener
from fixture_utils import load_fixture

class ParseErrorListener(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors = []
        
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column}: {msg}")

def test_if_construct_direct():
    code = load_fixture(
        "Fortran2018",
        "test_if_construct",
        "if_construct_only.f90",
    )
    
    print(f"Testing if_construct directly:")
    print(f"Code: {code}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    error_listener = ParseErrorListener()
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

if __name__ == "__main__":
    test_if_construct_direct()
