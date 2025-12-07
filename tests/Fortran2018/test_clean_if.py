#!/usr/bin/env python3
"""Test clean IF construct parsing"""

import sys
from pathlib import Path

# Ensure we can import fixture utilities and grammars
ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT))
sys.path.append(str(ROOT.parent / "grammars"))

from antlr4 import *  # type: ignore
from antlr4.error.ErrorListener import ErrorListener  # type: ignore
from Fortran2018Lexer import Fortran2018Lexer  # type: ignore
from Fortran2018Parser import Fortran2018Parser  # type: ignore
from fixture_utils import load_fixture

class TestErrorListener(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors = []
        
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column}: {msg}")

def test_clean_if():
    # Test just the IF construct without orphaned end
    code = load_fixture(
        "Fortran2018",
        "test_if_construct",
        "if_construct_only.f90",
    )
    
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
    code = load_fixture(
        "Fortran2018",
        "test_clean_if",
        "wrapped_program.f90",
    )
    
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
