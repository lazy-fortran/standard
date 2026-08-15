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

// ============================================================================
// TRAITS PROPOSAL TOKENS
// ============================================================================
// Trait conformance and type-level extension syntax:
//   type, sealed, implements(ITrait) :: my_type
//   implements ITrait :: my_type
//   initial :: init
//   integer | real(real64)
IMPLEMENTS_KW
    : I M P L E M E N T S
    ;

SEALED_KW
    : S E A L E D
    ;

INITIAL_KW
    : I N I T I A L
    ;

PIPE
    : '|'
    ;

// ============================================================================
// ARRAY CONTRACTS PROPOSAL TOKENS (issue #745)
// ============================================================================
// Explicit broadcasting operation:
//   x = x + broadcast(dt * v, over=particle)
// `broadcast` is reserved in LFortran so it is unambiguous in expressions.
// (The `shape(...)` attribute reuses the inherited SHAPE_INTRINSIC token and
// therefore does not reserve a new keyword.)
BROADCAST_KW
    : B R O A D C A S T
    ;
