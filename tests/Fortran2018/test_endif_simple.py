#!/usr/bin/env python3
"""Test ENDIF parsing in programs"""

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

class ParseErrorListener(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors = []
        
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column}: {msg}")

def run_program_case(code, description):
    print(f"Testing: {description}")
    print(f"Code:\n{code}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    error_listener = ParseErrorListener()
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
    print("-" * 50)

if __name__ == "__main__":
    # Test the exact missing-spaces case wrapped in program
    missing_spaces_wrapped = load_fixture(
        "Fortran2018",
        "test_endif_simple",
        "missing_spaces_wrapped.f90",
    )
    run_program_case(missing_spaces_wrapped, "Missing-spaces wrapped in program")
    
    # Test simpler case
    simple_endif = load_fixture(
        "Fortran2018",
        "test_endif_simple",
        "simple_endif.f90",
    )
    run_program_case(simple_endif, "Simple ENDIF in program")
