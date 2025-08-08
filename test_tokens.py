#!/usr/bin/env python3

import sys
sys.path.append('build/Fortran2003')
sys.path.append('grammars')

from Fortran2003Lexer import Fortran2003Lexer
import antlr4

code = 'type, abstract :: shape_t'

lexer = Fortran2003Lexer(antlr4.InputStream(code))
tokens = []
token = lexer.nextToken()
while token.type != antlr4.Token.EOF:
    tokens.append((token.type, token.text, lexer.symbolicNames[token.type]))
    token = lexer.nextToken()

for token_type, text, name in tokens:
    print(f'{name}: "{text}"')