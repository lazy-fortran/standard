#!/usr/bin/env python3
"""Test direct ENDIF parsing"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT.parent / "grammars"))

from antlr4 import *  # type: ignore
from Fortran2018Lexer import Fortran2018Lexer  # type: ignore
from Fortran2018Parser import Fortran2018Parser  # type: ignore

def test_direct_parsing():
    code = "endif"
    
    print(f"Testing direct parsing of: {code}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    # Test end_if_stmt directly
    try:
        tree = parser.end_if_stmt()
        print(f"✅ end_if_stmt() works")
    except Exception as e:
        print(f"❌ end_if_stmt() failed: {e}")

if __name__ == "__main__":
    test_direct_parsing()
