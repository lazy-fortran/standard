/*
 * Fortran2028Lexer.g4
 *
 * Fortran 2028 Working Draft lexer overlay.
 * Base: Fortran 2023 lexer
 * Source reference: J3 standing document 26-007 (Fortran 2028 Working Draft)
 *
 * This grammar defines lexical additions for F2028 template facilities:
 * - TEMPLATE / REQUIREMENT / REQUIRE(S) / INSTANTIATE
 * - DEFERRED / CONSTANT attribute keywords for deferred arguments
 * - Curly braces used in instantiation argument syntax
 */

lexer grammar Fortran2028Lexer;

import Fortran2023Lexer;

// ============================================================================
// F2028 TEMPLATE FACILITY TOKENS (Working Draft)
// ============================================================================

TEMPLATE_KW
    : T E M P L A T E
    ;

REQUIREMENT_KW
    : R E Q U I R E M E N T
    ;

REQUIRE_KW
    : R E Q U I R E
    ;

REQUIRES_KW
    : R E Q U I R E S
    ;

INSTANTIATE_KW
    : I N S T A N T I A T E
    ;

DEFERRED_KW
    : D E F E R R E D
    ;

CONSTANT_KW
    : C O N S T A N T
    ;

END_TEMPLATE_STMT
    : E N D WS* T E M P L A T E
    ;

END_REQUIREMENT_STMT
    : E N D WS* R E Q U I R E M E N T
    ;

// Delimiters added in template edits.
LBRACE
    : '{'
    ;

RBRACE
    : '}'
    ;
