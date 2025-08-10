#!/usr/bin/env python3
"""Test token recognition"""

import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'grammars'))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer

def test_token_recognition():
    code = "endif"
    
    print(f"Testing token recognition for: {code}")
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    token_stream.fill()
    
    tokens = token_stream.tokens
    for i, token in enumerate(tokens):
        if token.type != -1:  # Not EOF
            token_name = lexer.symbolicNames[token.type] if token.type < len(lexer.symbolicNames) else f"UNKNOWN({token.type})"
            print(f"Token {i}: {token_name} = '{token.text}'")

if __name__ == "__main__":
    test_token_recognition()