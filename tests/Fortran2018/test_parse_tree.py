#!/usr/bin/env python3
"""Test parse tree structure for IF construct"""

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

def print_tree(node, indent=0):
    """Pretty print parse tree"""
    if hasattr(node, 'symbol'):
        # Terminal node
        print("  " * indent + f"'{node.symbol.text}'")
    else:
        # Non-terminal node
        rule_name = node.__class__.__name__.replace('Context', '')
        print("  " * indent + rule_name)
        if hasattr(node, 'children') and node.children:
            for child in node.children:
                print_tree(child, indent + 1)

def test_if_parse_tree():
    code = """if (mydummy > 90.and.gggg > 0) then
endif"""
    
    print(f"Testing IF construct parse tree:")
    print(f"Code: {repr(code)}")
    print("=" * 50)
    
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)
    
    error_listener = TestErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)
    
    try:
        tree = parser.if_construct()
        
        print("Parse tree structure:")
        print_tree(tree)
        
        print(f"\nErrors: {len(error_listener.errors)}")
        for error in error_listener.errors:
            print(f"  {error}")
            
        # Check if we have a valid tree
        if tree and hasattr(tree, 'children'):
            print(f"\n✅ Parse tree created with {len(tree.children)} children")
            return len(error_listener.errors) == 0
        else:
            print(f"\n❌ No parse tree created")
            return False
            
    except Exception as e:
        print(f"❌ Parse failed with exception: {e}")
        return False

if __name__ == "__main__":
    success = test_if_parse_tree()
    print(f"\nOverall success: {'✅' if success else '❌'}")