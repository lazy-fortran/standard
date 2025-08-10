parser grammar LazyFortran2025Parser;

options {
    tokenVocab = LazyFortran2025Lexer;
}

import Fortran2023Parser;

// LazyFortran2025 - Syntactic relaxations for modern Fortran
// 
// WORKAROUND: Dual entry points to bypass ANTLR4's import limitations
// - traditional_entry: For .f90, .f95, etc. (uses inherited rules)
// - lazy_entry: For .lf files (bypasses PROGRAM/MODULE requirements)
//
// Key features:
// 1. Optional program/module blocks (via lazy_entry)
// 2. Optional contains keyword  
// 3. Type inference (accept undeclared variables)
// 4. Implicit none default (compiler feature)

// ============================================================================
// DUAL ENTRY POINTS - The workaround for .lf files
// ============================================================================

// Entry point for .f90, .f95, .f03, .f08, .f18, .f23 files
traditional_entry
    : program_unit_f2023+ EOF
    ;

// Entry point for .lf files - NO PROGRAM/MODULE REQUIRED!
lazy_entry
    : lazy_code EOF
    ;

// ============================================================================
// LAZY CODE STRUCTURE - Direct statements without wrappers
// ============================================================================

lazy_code
    : lazy_item*
    ;

lazy_item
    : use_stmt NEWLINE?
    | implicit_stmt NEWLINE?
    | lazy_declaration NEWLINE?
    | lazy_executable NEWLINE?
    | lazy_procedure                    // No CONTAINS needed!
    | NEWLINE
    ;

// ============================================================================
// LAZY DECLARATIONS - Variables without types
// ============================================================================

lazy_declaration
    : type_declaration_stmt             // Traditional declaration (optional)
    | lazy_assignment                   // Variable created by assignment
    ;

// Assignment that creates a variable (type inference)
lazy_assignment
    : IDENTIFIER '=' expr_f2023
    | IDENTIFIER '(' expr_list ')' '=' expr_f2023
    ;

// ============================================================================
// LAZY EXECUTABLES - Direct statements
// ============================================================================

lazy_executable
    : lazy_assignment
    | call_stmt
    | print_stmt
    | if_construct
    | do_construct
    | select_case_construct
    | where_construct
    | forall_construct
    | stop_stmt
    | return_stmt
    ;

// ============================================================================
// LAZY PROCEDURES - No CONTAINS required
// ============================================================================

lazy_procedure
    : lazy_function
    | lazy_subroutine
    ;

lazy_function
    : function_prefix IDENTIFIER '(' dummy_arg_list? ')' result_clause? NEWLINE
      lazy_subprogram_body
      end_function_stmt
    ;

lazy_subroutine
    : SUBROUTINE IDENTIFIER '(' dummy_arg_list? ')' NEWLINE
      lazy_subprogram_body
      end_subroutine_stmt
    ;

lazy_subprogram_body
    : (lazy_declaration NEWLINE | lazy_executable NEWLINE | NEWLINE)*
    ;

function_prefix
    : type_spec? FUNCTION
    | PURE FUNCTION
    | ELEMENTAL FUNCTION
    | RECURSIVE FUNCTION
    ;

result_clause
    : RESULT '(' IDENTIFIER ')'
    ;

// ============================================================================
// EXPRESSION LIST for array indexing
// ============================================================================

expr_list
    : expr_f2023 (',' expr_f2023)*
    ;