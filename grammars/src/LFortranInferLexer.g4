// ============================================================================
// LFortranInfer Lexer - LFortran Infer Mode
// ============================================================================
//
// This lexer extends LFortran for infer mode (global scope / script mode).
// Infer mode enables script-style Fortran programming without traditional
// program/end program wrappers.
//
// Activation: `lfortran --infer` flag
//
// Features (all handled in parser, not lexer):
//   - Bare statements at top level
//   - Bare expressions for REPL evaluation
//   - Bare declarations without program units
//   - Mixed script-style and traditional program units
//
// Reference: LFortran compiler (https://lfortran.org)
// ============================================================================

lexer grammar LFortranInferLexer;

import LFortranLexer;

// ============================================================================ 
// TOKEN INHERITANCE
// ============================================================================
// All tokens are inherited from LFortranLexer (which includes):
//   - Fortran 2028 tokens (from Fortran2028Lexer)
//   - J3 Generics tokens (TEMPLATE, REQUIREMENT, REQUIRE, INSTANTIATE, DEFERRED)
//   - Type inference token (COLON_EQUAL for :=)
//   - No additional infer-specific tokens required here
// ============================================================================
