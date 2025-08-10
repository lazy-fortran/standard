parser grammar LazyFortran2025Parser;

options {
    tokenVocab = LazyFortran2025Lexer;
}

import Fortran2023Parser;

// LazyFortran2025 - Syntactic relaxations for modern Fortran
// Key features:
// 1. Optional program/module blocks
// 2. Optional contains keyword  
// 3. Type inference (accept undeclared variables)
// 4. Implicit none default (compiler feature)

// Since we're inheriting from a complex chain and ANTLR doesn't allow
// true overriding of imported rules in all cases, we work with what we have