/*
 * LazyFortran2025Lexer.g4
 *
 * Lazy Fortran 2025 lexer.
 * Reuses the Fortran 2023 token vocabulary without changes.
 */

lexer grammar LazyFortran2025Lexer;

import Fortran2023Lexer;

// No additional tokens are required at the lexer level.
// LazyFortran2025 relies on Fortran 2023 tokens and relaxes syntax
// only in the parser.

