#!/usr/bin/env python3

import sys
sys.path.append('./grammars')
from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

# Test with debug
code = 'program test\nend program test'
input_stream = InputStream(code)
lexer = Fortran2003Lexer(input_stream)
parser = Fortran2003Parser(CommonTokenStream(lexer))

# Test parsing
print('Testing F2003 program_unit_f2003 parsing...')
try:
    tree = parser.program_unit_f2003()
    errors = parser.getNumberOfSyntaxErrors()
    print(f'Parse tree: {type(tree)}')
    print(f'Parse errors: {errors}')
    print(f'Tree class name: {tree.__class__.__name__}')
    if hasattr(tree, 'children'):
        print(f'Tree has {len(tree.children) if tree.children else 0} children')
    if errors == 0:
        print('✓ SUCCESS: F2003 parser now works!')
    else:
        print('✗ Still has parse errors')
except Exception as e:
    print(f'✗ Exception: {e}')

# Print token info for reference  
print(f'\nPROGRAM token value: {lexer.PROGRAM}')