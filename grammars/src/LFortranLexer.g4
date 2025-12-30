// ============================================================================
// LFortran Lexer - LFortran Standard Extensions to Fortran 2023
// ============================================================================
//
// This lexer extends the Fortran 2023 lexer with LFortran-specific features:
//
// 1. J3 Generics (Fortran 202Y preview):
//    - TEMPLATE, REQUIREMENT, REQUIRE, INSTANTIATE keywords
//    - DEFERRED keyword for type parameters
//
// 2. Global scope support:
//    - Bare statements at top level (handled in parser)
//
// Reference: LFortran compiler (https://lfortran.org)
// ============================================================================

lexer grammar LFortranLexer;

import Fortran2023Lexer;

// ============================================================================
// J3 GENERICS KEYWORDS (Fortran 202Y Preview)
// ============================================================================
// These keywords enable parameterized programming with explicit instantiation.
// Based on J3 papers 24-107r1 and related proposals.

// TEMPLATE construct - defines parameterized procedures
TEMPLATE_KW
    : T E M P L A T E
    ;

// REQUIREMENT construct - defines type constraints
REQUIREMENT_KW
    : R E Q U I R E M E N T
    ;

// REQUIRE statement - specifies required constraints
REQUIRE_KW
    : R E Q U I R E
    ;

// INSTANTIATE statement - explicit template instantiation
INSTANTIATE_KW
    : I N S T A N T I A T E
    ;

// DEFERRED keyword - for deferred type parameters in templates
DEFERRED_KW
    : D E F E R R E D
    ;

// ============================================================================
// END CONSTRUCT KEYWORDS
// ============================================================================

END_TEMPLATE
    : E N D WS* T E M P L A T E
    ;

END_REQUIREMENT
    : E N D WS* R E Q U I R E M E N T
    ;
