#!/usr/bin/env python3
"""Test parse tree structure for IF construct"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT.parent / "grammars"))
sys.path.append(str(ROOT))

from antlr4 import *  # type: ignore
from Fortran2018Lexer import Fortran2018Lexer  # type: ignore
from Fortran2018Parser import Fortran2018Parser  # type: ignore
from antlr4.error.ErrorListener import ErrorListener  # type: ignore
from fixture_utils import load_fixture

class ParseErrorListener(ErrorListener):
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
    code = load_fixture(
        "Fortran2018",
        "test_parse_tree",
        "if_construct_program.f90",
    )

    print(f"Testing IF construct parse tree:")
    print(f"Code: {repr(code)}")
    print("=" * 50)

    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Fortran2018Parser(token_stream)

    error_listener = ParseErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)

    tree = parser.program_unit_f2018()

    print("Parse tree structure:")
    print_tree(tree)

    print(f"\nErrors: {len(error_listener.errors)}")
    for error in error_listener.errors:
        print(f"  {error}")

    assert tree is not None, "Parse tree should not be None"
    assert hasattr(tree, 'children'), "Parse tree should have children attribute"
    assert len(error_listener.errors) == 0, (
        f"Expected no parse errors, got: {error_listener.errors}"
    )
    print(f"\nParse tree created with {len(tree.children)} children")

if __name__ == "__main__":
    test_if_parse_tree()
    print("\nOverall success")
