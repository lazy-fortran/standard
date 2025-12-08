/*
 * LazyFortran2025Lexer.g4
 *
 * Lazy Fortran 2025 lexer.
 * Extends the Fortran 2023 token vocabulary with a tiny set of
 * Lazy‑only tokens used for optional trait‑like contracts in .lf
 * files. Core Fortran source continues to use the Fortran2023Lexer
 * unchanged; these tokens are only consumed by the lazy parser.
 */

lexer grammar LazyFortran2025Lexer;

import Fortran2023Lexer;

// ---------------------------------------------------------------------------
// Lazy‑only extension tokens
// ---------------------------------------------------------------------------

// Trait declaration keyword, aligned with Traits‑for‑Fortran terminology.
TRAIT : T R A I T ;

// Annotation prefix for opt‑in trait contracts, e.g. @AdditiveMonoid(T).
AT    : '@' ;

