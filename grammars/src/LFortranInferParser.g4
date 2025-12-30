// ============================================================================
// LFortranInfer Parser - LFortran Infer Mode (Global Scope / Script Mode)
// ============================================================================
//
// This parser extends LFortran with global scope support for infer mode:
//
// Global scope features:
//   - Bare statements at top level without program/end program
//   - Bare expressions at top level (REPL mode)
//   - Bare declarations at top level
//   - Bare use/implicit statements at top level
//
// This enables script-style Fortran programming and interactive use.
//
// Reference: LFortran compiler (https://lfortran.org)
// ============================================================================

parser grammar LFortranInferParser;

options { tokenVocab = LFortranInferLexer; }

import LFortranParser;

// ============================================================================
// TOP-LEVEL PROGRAM STRUCTURE (Global Scope / Script Mode)
// ============================================================================
// LFortran infer mode allows bare statements, declarations, and expressions
// at the top level without requiring a program wrapper.

program_lfortran_infer
    : script_unit_list EOF
    ;

script_unit_list
    : script_unit+
    ;

// A script unit can be a standard program unit, J3 generic, OR bare code
script_unit
    : NEWLINE* program_unit_f2023 NEWLINE*   // Standard: program, module, etc.
    | NEWLINE* template_construct NEWLINE*   // J3 Generics: template definition
    | NEWLINE* requirement_construct NEWLINE* // J3 Generics: requirement definition
    | NEWLINE* instantiate_stmt NEWLINE*     // J3 Generics: instantiation
    | NEWLINE* use_stmt NEWLINE*             // Bare use statement (global scope)
    | NEWLINE* implicit_stmt NEWLINE*        // Bare implicit statement (global scope)
    | NEWLINE* declaration_construct NEWLINE* // Bare declaration (global scope)
    | NEWLINE* executable_construct NEWLINE* // Bare statement (global scope)
    | NEWLINE* expr_f2003 NEWLINE*           // Bare expression (global scope / REPL)
    ;
