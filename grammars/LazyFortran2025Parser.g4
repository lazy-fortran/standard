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
    : lazy_use_stmt
    | import_stmt
    | implicit_stmt
    | contains_stmt
    | lazy_trait_definition
    | lazy_trait_annotation_stmt
    | declaration_construct_f2018
    | lazy_iso_int_kind_declaration
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
    : lazy_function_subprogram
    | lazy_subroutine_subprogram
    ;

// Lazy function and subroutine subprograms that preserve F2018 semantics while
// relaxing NEWLINE handling in the execution part so that procedure-only
// library-style .lf files parse naturally.
lazy_function_subprogram
    : function_stmt_f2018 specification_part_f2018? lazy_execution_part?
      internal_subprogram_part? end_function_stmt
    ;

lazy_subroutine_subprogram
    : subroutine_stmt_f2018 specification_part_f2018? lazy_execution_part?
      internal_subprogram_part? end_subroutine_stmt
    ;

lazy_execution_part
    : lazy_execution_construct*
    ;

lazy_execution_construct
    : NEWLINE* executable_construct_f2018 NEWLINE*
    ;

// ============================================================================
// OPTIONAL TRAIT‑LIKE CONTRACTS (Lazy‑only, PROVISIONAL)
// ============================================================================

// Lightweight trait definition treated as a specification construct in lazy
// mode. The body reuses existing Fortran 2018 declaration constructs so that
// traits can enumerate required procedures and types using familiar syntax.
lazy_trait_definition
    : trait_def_stmt
      lazy_trait_member*
      end_trait_stmt
    ;

trait_def_stmt
    : TRAIT IDENTIFIER lazy_trait_type_param_list? NEWLINE
    ;

lazy_trait_type_param_list
    : LPAREN lazy_trait_type_param (COMMA lazy_trait_type_param)* RPAREN
    ;

lazy_trait_type_param
    : IDENTIFIER
    ;

lazy_trait_member
    : declaration_construct_f2018
    | NEWLINE
    ;

end_trait_stmt
    : END TRAIT (IDENTIFIER)? NEWLINE
    ;

// Annotation statement that can appear alongside other specification items in
// a lazy program. Semantically it associates one or more traits (with optional
// type arguments) to the surrounding definition; the parser simply records the
// annotation as its own construct.
lazy_trait_annotation_stmt
    : AT IDENTIFIER lazy_trait_actual_arg_list? NEWLINE
    ;

lazy_trait_actual_arg_list
    : LPAREN lazy_trait_actual_arg (COMMA lazy_trait_actual_arg)* RPAREN
    ;

lazy_trait_actual_arg
    : expr_f2023
    ;

// Lazy use statement that accepts both traditional ONLY lists and enhanced
// F2008/F2018 intrinsic kind names like INT32 from ISO_FORTRAN_ENV.
lazy_use_stmt
    : USE IDENTIFIER NEWLINE
    | USE IDENTIFIER COMMA ONLY COLON lazy_only_list NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON ieee_module_name NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON ieee_module_name COMMA
      ONLY COLON ieee_only_list NEWLINE
    ;

lazy_only_list
    : lazy_only_name (COMMA lazy_only_name)*
    ;

lazy_only_name
    : IDENTIFIER
    | c_interop_type
    | INT8
    | INT16
    | INT32
    | INT64
    | REAL32
    | REAL64
    | REAL128
    ;

// Support INTEGER(INT32) style declarations in lazy mode without changing the
// core F2003/F2018 kind selector rules.
lazy_iso_int_kind_declaration
    : INTEGER LPAREN lazy_iso_int_kind RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    ;

lazy_iso_int_kind
    : INT8
    | INT16
    | INT32
    | INT64
    ;
