// ============================================================================
// LFortran Lexer - LFortran Standard Extensions to Fortran 2028
// ============================================================================
//
// This lexer extends the Fortran 2028 lexer with LFortran-specific features:
//
// 1. LFortran generic syntax extensions:
//    - Caret inline-instantiation syntax
//
// 2. Type inference syntax:
//    - := walrus declaration operator
//
// 3. Global scope support:
//    - Bare statements at top level (handled in parser)
//
// Reference: LFortran compiler (https://lfortran.org)
// ============================================================================

lexer grammar LFortranLexer;

import Fortran2028Lexer;

// Caret for inline instantiation (J3 r4 alternative)
CARET
    : '^'
    ;

// ============================================================================
// TYPE INFERENCE TOKENS
// ============================================================================
// Walrus declaration syntax used by LFortran type inference:
//   x := expr
COLON_EQUAL
    : ':='
    ;
