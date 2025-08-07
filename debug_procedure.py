#!/usr/bin/env python3

import sys
sys.path.append('./grammars')
from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

# Test the failing procedure pointer code
code = '''program test_proc
procedure(interface), pointer :: proc_ptr
end program test_proc
'''

input_stream = InputStream(code)
lexer = Fortran2003Lexer(input_stream)
parser = Fortran2003Parser(CommonTokenStream(lexer))

print("Testing procedure pointer parsing...")
print("Code:")
print(code)
print("\nTokens:")

# Reset lexer to show tokens
lexer.reset()
token = lexer.nextToken()
i = 0
while token.type != -1 and i < 15:
    token_name = lexer.symbolicNames[token.type] if token.type < len(lexer.symbolicNames) else f'UNKNOWN({token.type})'
    print(f'  {i}: {token_name} = "{token.text}"')
    token = lexer.nextToken()
    i += 1

# Now test parsing
lexer.reset()
parser = Fortran2003Parser(CommonTokenStream(lexer))

try:
    tree = parser.program_unit_f2003()
    errors = parser.getNumberOfSyntaxErrors()
    print(f'\nParse errors: {errors}')
    if errors > 0:
        print("Parse failed - procedure pointer not properly handled")
    else:
        print("✓ Parse succeeded")
except Exception as e:
    print(f'Exception: {e}')