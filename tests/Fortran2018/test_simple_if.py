#!/usr/bin/env python3
"""Test simple IF parsing step by step"""

import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'grammars'))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer  
from Fortran2018Parser import Fortran2018Parser

def test_parser_method(code, method_name):
    print(f"Testing method: {method_name}")
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    try:
        method = getattr(parser, method_name)
        tree = method()
        print(f"✅ Success with {method_name}")
        return True
    except Exception as e:
        print(f"❌ Failed with {method_name}: {e}")
        return False

if __name__ == "__main__":
    simple_if = """if (x > 0) then
endif"""
    
    print("Available methods on parser:")
    parser_methods = [attr for attr in dir(Fortran2018Parser) if 'if' in attr.lower()]
    print(parser_methods)
    print()
    
    # Test specific IF-related methods
    test_methods = ['if_construct', 'block_if_construct']
    
    for method in test_methods:
        if hasattr(Fortran2018Parser, method):
            test_parser_method(simple_if, method)
        else:
            print(f"Method {method} not found")
    
    print("\nTesting execution part:")
    test_parser_method(simple_if, 'execution_part')
    
    print("\nTesting executable construct:")
    test_parser_method(simple_if, 'executable_construct_f2018')