/*
 * LazyFortran2025Parser.g4
 *
 * Parser extension for Lazy Fortran 2025 (.lf files).
 * Provides dual entry points:
 * - traditional_entry: strict Fortran 2023 program units
 * - lazy_entry: relaxed syntax without required PROGRAM or MODULE blocks
 *   and without a required CONTAINS section before procedures.
 */

parser grammar LazyFortran2025Parser;

options {
    tokenVocab = LazyFortran2025Lexer;
}

import Fortran2023Parser;

// ============================================================================
// ENTRY POINTS
// ============================================================================

// Entry point for traditional Fortran source (.f90, .f95, .f03, .f08, .f18,
// .f23). Uses the Fortran 2023 program unit structure unchanged.
traditional_entry
    : program_unit_f2023 EOF
    ;

// Entry point for LazyFortran2025 source (.lf). Allows top-level statements
// and procedures without explicit PROGRAM, MODULE, or CONTAINS.
lazy_entry
    : lazy_program EOF
    ;

// ============================================================================
// LAZY PROGRAM STRUCTURE
// ============================================================================

lazy_program
    : NEWLINE* lazy_element* NEWLINE*
    ;

// A lazy element is either a declaration/statement or a procedure definition.
lazy_element
    : lazy_declaration_or_statement
    | lazy_procedure_definition
    ;

// Allow specification items and executable statements to be interleaved at
// the top level in a lazy source file.
lazy_declaration_or_statement
    : lazy_specification_construct
    | lazy_executable_construct
    ;

// Specification constructs permitted at the top level in lazy mode.
lazy_specification_construct
    : use_stmt
    | import_stmt
    | implicit_stmt
    | contains_stmt
    | declaration_construct_f2018
    ;

// Lazy execution constructs include Fortran 2018 constructs and Fortran 2023
// enhanced executable statements such as conditional expressions.
lazy_executable_construct
    : executable_stmt_f2023
    | executable_construct_f2018
    ;

// Procedures that can appear at the top level without a CONTAINS section.
// These reuse the Fortran 2018 function and subroutine subprogram rules so
// that LazyFortran2025 remains compatible with the modern Fortran features.
lazy_procedure_definition
    : function_subprogram_f2018
    | subroutine_subprogram_f2018
    ;

