// ============================================================================
// LFortranInfer Lexer - LFortran Infer Mode (Global Scope)
// ============================================================================
//
// This lexer extends LFortran for infer/script mode.
// No additional tokens needed - global scope is handled in the parser.
//
// Reference: LFortran compiler (https://lfortran.org)
// ============================================================================

lexer grammar LFortranInferLexer;

import LFortranLexer;

// No additional tokens - all tokens inherited from LFortranLexer
