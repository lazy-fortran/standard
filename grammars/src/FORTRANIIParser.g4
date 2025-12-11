/*
 * FORTRANIIParser.g4
 *
 * FORTRAN II (1958) - Parser for Procedural Programming Revolution
 * Introduces separately compiled subroutines and functions.
 *
 * This parser extends FORTRANParser (1957) with the six new FORTRAN II
 * statement/subprogram forms: SUBROUTINE, FUNCTION, CALL, RETURN, COMMON,
 * and END (as a formal terminator).
 *
 * Reference: IBM FORTRAN II for the IBM 704 Data Processing System
 *            (Form C28-6000-2, 1958)
 *            - Part I, Chapter 1: Introduction and Types of Statements
 *            - Part I, Chapter 2: Coding for FORTRAN II
 *            - Part I, Chapter 3: The New FORTRAN II Statements
 *            - Part I, Chapter 4: Expressions (inherited from FORTRAN I)
 *            - Appendix A: Summary of FORTRAN II Statements
 *
 * Manual available at: Computer History Museum archive
 *   https://archive.computerhistory.org/resources/text/Fortran/
 */
parser grammar FORTRANIIParser;

import FORTRANParser;  // Import FORTRAN I (1957) constructs (C28-6003)

options {
    tokenVocab = FORTRANIILexer;
}

// ============================================================================
// FORTRAN II (1958) PROGRAM STRUCTURE
// C28-6000-2 Part I, Chapter 1: Introduction
// ============================================================================
// FORTRAN II introduces separately compiled program units:
// - Main program: sequence of statements ending with END
// - Subroutine subprogram: SUBROUTINE definition with END
// - Function subprogram: FUNCTION definition with END
// ============================================================================

// ============================================================================
// PROGRAM UNIT RULES
// C28-6000-2 Part I, Chapter 1 and Chapter 3
// ============================================================================

// FORTRAN II program with separately compiled units
// C28-6000-2 Part I, Chapter 1: A source deck consists of one program unit
fortran_program
    : (main_program | subroutine_subprogram | function_subprogram) EOF
    ;

// Main program - C28-6000-2 Part I, Chapter 1
// A main program is a sequence of statements ending with END
main_program
    : statement_list end_stmt NEWLINE?
    ;

// Statement list - C28-6000-2 Part I, Chapter 2
// Allows empty lines (blank punch cards were valid)
statement_list
    : statement*
    ;

// Individual statement - C28-6000-2 Part I, Chapter 2
// Labels were punched in columns 1-5, statements in columns 7-72
statement
    : statement_label? statement_body NEWLINE?
    | NEWLINE  // Blank punch card (allowed)
    ;

// Statement label - C28-6000-2 Part I, Chapter 2
// 1-5 digit integer (1-99999), punched in columns 1-5
statement_label
    : LABEL
    ;

// ---------------------------------------------------------------------------
// Inherited numeric handling overrides
// ---------------------------------------------------------------------------
// FORTRANIILexer tokenizes many fixed-form numeric constants as LABEL tokens.
// To preserve FORTRAN I expression and label syntax without duplicating the
// full expression grammar, FORTRAN II widens the inherited rules to accept
// LABEL wherever an integer literal or label is expected.

label
    : INTEGER_LITERAL
    | LABEL
    ;

literal
    : INTEGER_LITERAL
    | LABEL
    | REAL_LITERAL
    ;

// Subroutine definition - C28-6000-2 Part I, Chapter 3, Section 3.2
// Defines a separately compiled subroutine subprogram
// Form: SUBROUTINE name (a1, a2, ..., an) or SUBROUTINE name
subroutine_subprogram
    : SUBROUTINE IDENTIFIER parameter_list? NEWLINE
      statement_list
      end_stmt NEWLINE?
    ;

// Function definition - C28-6000-2 Part I, Chapter 3, Section 3.3
// Defines a separately compiled function subprogram
// Form: [type] FUNCTION name (a1, a2, ..., an)
function_subprogram
    : type_spec? FUNCTION IDENTIFIER parameter_list NEWLINE
      statement_list
      end_stmt NEWLINE?
    ;

// Parameter list for subprograms - C28-6000-2 Part I, Chapter 3
// Dummy arguments for subroutines and functions
parameter_list
    : LPAREN (IDENTIFIER (COMMA IDENTIFIER)*)? RPAREN
    ;

// Type specification - C28-6000-2 Part I, Chapter 3, Section 3.3
// Optional type prefix for FUNCTION definitions
type_spec
    : INTEGER
    | REAL
    ;

// ============================================================================
// STRICT 1958 MODE ENTRY POINTS
// C28-6000-2: Historical mode enforcing blank COMMON only (no named blocks)
// ============================================================================

// Strict 1958 FORTRAN II program entry point
fortran_program_strict
    : (main_program_strict | subroutine_subprogram_strict
      | function_subprogram_strict) EOF
    ;

// Strict main program - C28-6000-2 Part I, Chapter 1
main_program_strict
    : statement_list_strict end_stmt NEWLINE?
    ;

// Strict statement list - C28-6000-2 Part I, Chapter 2
statement_list_strict
    : statement_strict*
    ;

// Strict individual statement - C28-6000-2 Part I, Chapter 2
statement_strict
    : statement_label? statement_body_strict NEWLINE?
    | NEWLINE
    ;

// Strict subroutine definition - C28-6000-2 Part I, Chapter 3, Section 3.2
subroutine_subprogram_strict
    : SUBROUTINE IDENTIFIER parameter_list? NEWLINE
      statement_list_strict
      end_stmt NEWLINE?
    ;

// Strict function definition - C28-6000-2 Part I, Chapter 3, Section 3.3
function_subprogram_strict
    : type_spec? FUNCTION IDENTIFIER parameter_list NEWLINE
      statement_list_strict
      end_stmt NEWLINE?
    ;

// ============================================================================
// STATEMENT TYPES
// C28-6000-2 Part I, Chapter 1 and Appendix A
// ============================================================================
// FORTRAN II includes all original FORTRAN I (1957) statements plus six new
// forms: SUBROUTINE, FUNCTION, CALL, RETURN, COMMON, and END.
// ============================================================================

// Relaxed mode statement body - allows named COMMON blocks
statement_body
    : common_stmt           // NEW in FORTRAN II (relaxed mode)
    | return_stmt           // NEW in FORTRAN II
    | call_stmt             // NEW in FORTRAN II
    | end_file_stmt         // Disambiguate END FILE before bare END
    | statement_body_fi     // All FORTRAN I (1957) statements
    ;

// Strict 1958 mode statement body - enforces blank COMMON only
statement_body_strict
    : common_stmt_strict    // NEW in FORTRAN II (strict blank COMMON)
    | return_stmt           // NEW in FORTRAN II
    | call_stmt             // NEW in FORTRAN II
    | end_file_stmt         // Disambiguate END FILE before bare END
    | statement_body_fi     // All FORTRAN I (1957) statements
    ;

// ============================================================================
// FORTRAN II NEW STATEMENTS
// C28-6000-2 Part I, Chapter 3
// ============================================================================

// CALL statement - C28-6000-2 Part I, Chapter 3, Section 3.1
// Form: CALL name or CALL name (a1, a2, ..., an)
call_stmt
    : CALL IDENTIFIER (LPAREN expr_list? RPAREN)?
    ;

// RETURN statement - C28-6000-2 Part I, Chapter 3, Section 3.4
return_stmt
    : RETURN
    ;

// COMMON statement - C28-6000-2 Part I, Chapter 3, Section 3.5
// Form (relaxed): COMMON list or COMMON /name/ list
// Form (strict):  COMMON list only
common_stmt
    : COMMON (SLASH IDENTIFIER SLASH)? variable_list
    ;

common_stmt_strict
    : COMMON variable_list
    ;

// END statement - C28-6000-2 Part I, Chapter 3, Section 3.6
end_stmt
    : END
    ;

// ============================================================================
// UTILITY RULES
// ============================================================================

// Variable list for COMMON statement
variable_list
    : variable (COMMA variable)*
    ;
