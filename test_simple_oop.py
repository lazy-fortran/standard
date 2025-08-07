#!/usr/bin/env python3

import sys
sys.path.append('grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

def test_individual_constructs():
    print("=== TESTING INDIVIDUAL OOP CONSTRUCTS ===")
    
    tests = [
        ("Minimal type declaration", "type :: point_t\nend type"),
        ("Abstract type declaration", "type, abstract :: shape_t\nend type shape_t"),
        ("Type with inheritance", "type, extends(shape_t) :: circle_t\nend type circle_t"),
        ("Simple type declaration", "type :: point_t\nend type point_t"),
        ("Implicit none statement", "implicit none"),
    ]
    
    # First test tokenization
    test_code = "type, abstract :: shape_t\nend type shape_t"
    print(f"\n=== TOKENIZING TEST ===")
    print(f"Code: {test_code}")
    
    input_stream = InputStream(test_code)
    lexer = Fortran2003Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    
    token_stream.fill()
    for token in token_stream.tokens:
        token_name = lexer.symbolicNames[token.type] if token.type < len(lexer.symbolicNames) else str(token.type)
        print(f"{token.line}:{token.column} {token_name}: '{token.text}' (type={token.type})")
    
    print(f"\n=== END_TYPE token value: {getattr(lexer, 'END_TYPE', 'NOT FOUND')} ===\n")
    
    for name, code in tests:
        print(f"\n--- Testing: {name} ---")
        print(f"Code: {code}")
        
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        # Try parsing as a derived type definition
        try:
            tree = parser.derived_type_def_f2003()
            errors = parser.getNumberOfSyntaxErrors()
            if errors == 0:
                print("SUCCESS: Parsed as derived_type_def_f2003")
            else:
                print(f"FAILED: {errors} errors in derived_type_def_f2003")
        except Exception as e:
            print(f"EXCEPTION in derived_type_def_f2003: {e}")
        
        # Try parsing as implicit statement
        if "implicit" in code:
            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))
            try:
                tree = parser.implicit_stmt()
                errors = parser.getNumberOfSyntaxErrors()
                if errors == 0:
                    print("SUCCESS: Parsed as implicit_stmt")
                else:
                    print(f"FAILED: {errors} errors in implicit_stmt")
            except Exception as e:
                print(f"EXCEPTION in implicit_stmt: {e}")

if __name__ == "__main__":
    test_individual_constructs()