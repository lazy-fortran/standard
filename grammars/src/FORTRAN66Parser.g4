/*
 * FORTRAN66Parser.g4
 *
 * FORTRAN 66 (ANSI X3.9-1966) - First Official FORTRAN Standard
 * Merges FORTRAN IV (1962) data types with FORTRAN 66 standardization.
 *
 * Reference: ANSI X3.9-1966 "USA Standard FORTRAN"
 *            (American National Standards Institute, March 7, 1966)
 *
 * Standard Structure:
 *   Section 1: Scope and Purpose
 *   Section 2: Definitions
 *   Section 3: Program Form and Organization (3.1-3.5)
 *   Section 4: Data Types and Constants (4.1-4.6)
 *   Section 5: Variables, Arrays, and Subscripts (5.1-5.4)
 *   Section 6: Expressions (6.1-6.6)
 *   Section 7: Statements (7.1 Executable, 7.2 Non-executable)
 *   Section 8: Procedures (8.1-8.7)
 *
 * This parser extends FORTRANIIParser with FORTRAN IV features and
 * FORTRAN 66 standardization constructs.
 */

parser grammar FORTRAN66Parser;

import FORTRANIIParser;  // Import FORTRAN II (1958) constructs

options {
    tokenVocab = FORTRAN66Lexer;
}

// ============================================================================
// FORTRAN 66 (X3.9-1966) WITH MERGED FORTRAN IV FEATURES
// ============================================================================
//
// FORTRAN 66 (ANSI X3.9-1966) merges FORTRAN IV (1962) innovations with
// formal standardization:
// - LOGICAL data type with .TRUE./.FALSE. literals (Section 4.5)
// - DOUBLE PRECISION and COMPLEX data types (Sections 4.3, 4.4)
// - Logical expressions with .AND., .OR., .NOT. (Section 6.4)
// - Relational expressions with .LT., .EQ., .GT., etc. (Section 6.3)
// - Logical IF statement (Section 7.1.2.4)
// - Standardized program unit structure (Section 8)
// - BLOCK DATA subprogram for COMMON initialization (Section 8.4)
//
// Historical Context (1962-1966):
// - FORTRAN IV (1962) added data types and logical operations
// - X3.4.3 committee (1962-1966) standardized machine-independent FORTRAN
// - Combined innovations create foundation for portable FORTRAN
//
// ============================================================================

// ============================================================================
// TYPE SPECIFICATION - X3.9-1966 Section 7.2.5
// ============================================================================
// X3.9-1966 Section 7.2.5 defines type statements that explicitly declare
// the type of variables, arrays, and functions:
//   INTEGER   - Section 4.1: whole numbers
//   REAL      - Section 4.2: floating-point approximations
//   DOUBLE PRECISION - Section 4.3: extended precision floating-point
//   COMPLEX   - Section 4.4: ordered pairs of REAL values
//   LOGICAL   - Section 4.5: truth values (.TRUE. or .FALSE.)
// ============================================================================

type_spec
    : INTEGER                    // X3.9-1966 Section 4.1
    | REAL                       // X3.9-1966 Section 4.2
    | LOGICAL                    // X3.9-1966 Section 4.5
    | DOUBLE PRECISION           // X3.9-1966 Section 4.3
    | COMPLEX                    // X3.9-1966 Section 4.4
    ;

// ============================================================================
// IMPLICIT STATEMENTS - X3.9-1966 Section 7.2.5
// ============================================================================
// IMPLICIT statements override default implicit typing rules by mapping
// letter ranges to specific types. Each statement declares one or more
// type ranges via parenthesized letter lists separated by commas.
implicit_stmt
    : IMPLICIT implicit_spec (COMMA implicit_spec)*
    ;

implicit_spec
    : type_spec LPAREN implicit_range_list RPAREN
    ;

implicit_range_list
    : implicit_range (COMMA implicit_range)*
    ;

implicit_range
    : implicit_letter (MINUS implicit_letter)?
    ;

implicit_letter
    : IDENTIFIER
    | D
    ;

// ============================================================================       
// LOGICAL EXPRESSIONS - X3.9-1966 Section 6.4
// ============================================================================
// X3.9-1966 Section 6.4 defines logical expressions with five operators
// (in order of precedence, lowest to highest):
//   .NEQV./.EQV. - logical equivalence/non-equivalence (lowest precedence)
//   .OR.   - logical disjunction
//   .AND.  - logical conjunction
//   .NOT.  - logical negation (highest precedence, unary)
//
// Operator precedence: .NOT. > .AND. > .OR. > .EQV./.NEQV.
// Operators of equal precedence associate left to right.
//
// A logical expression evaluates to .TRUE. or .FALSE.
// ============================================================================

// Logical expression - X3.9-1966 Section 6.4
// Lowest precedence: .EQV. and .NEQV. equivalence operators
logical_expr
    : logical_or_expr (logical_equiv_op logical_or_expr)*
    ;

// Logical equivalence operators - X3.9-1966 Section 6.4
logical_equiv_op
    : DOT_EQV                     // X3.9-1966 Section 6.4 - logical equivalence
    | DOT_NEQV                    // X3.9-1966 Section 6.4 - logical non-equivalence
    ;

// Logical OR level - X3.9-1966 Section 6.4
// Higher precedence than .EQV./.NEQV.
logical_or_expr
    : logical_and_expr (DOT_OR logical_and_expr)*
    ;

// Logical AND level - X3.9-1966 Section 6.4
// Higher precedence than .OR.
logical_and_expr
    : logical_not_expr (DOT_AND logical_not_expr)*
    ;

// Logical NOT level - X3.9-1966 Section 6.4
// Highest precedence: .NOT. negation (unary)
logical_not_expr
    : DOT_NOT logical_primary
    | logical_primary
    ;

// Logical primary - X3.9-1966 Section 6.4
// Base cases: literals, relational expressions, variables, parenthesized
logical_primary
    : logical_literal
    | relational_expr
    | logical_variable
    | LPAREN logical_expr RPAREN
    ;

// Logical constants - X3.9-1966 Section 4.5.2
logical_literal
    : DOT_TRUE                   // X3.9-1966 Section 4.5.2
    | DOT_FALSE                  // X3.9-1966 Section 4.5.2
    ;

// Logical variable reference - X3.9-1966 Section 5.1
// Simple variable or array element
logical_variable
    : IDENTIFIER
    | IDENTIFIER LPAREN expr_list RPAREN  // Array element (Section 5.3)
    ;

// ============================================================================
// RELATIONAL EXPRESSIONS - X3.9-1966 Section 6.3
// ============================================================================
// X3.9-1966 Section 6.3 defines relational expressions that compare two
// arithmetic expressions and produce a logical value (.TRUE. or .FALSE.):
//   e1 .LT. e2  - less than
//   e1 .LE. e2  - less than or equal
//   e1 .EQ. e2  - equal
//   e1 .NE. e2  - not equal
//   e1 .GT. e2  - greater than
//   e1 .GE. e2  - greater than or equal
// ============================================================================

relational_expr
    : expr relational_op expr
    ;

relational_op
    : DOT_EQ                     // X3.9-1966 Section 6.3
    | DOT_NE                     // X3.9-1966 Section 6.3
    | DOT_LT                     // X3.9-1966 Section 6.3
    | DOT_LE                     // X3.9-1966 Section 6.3
    | DOT_GT                     // X3.9-1966 Section 6.3
    | DOT_GE                     // X3.9-1966 Section 6.3
    ;

// ============================================================================
// LOGICAL IF STATEMENT - X3.9-1966 Section 7.1.2.4
// ============================================================================
// X3.9-1966 Section 7.1.2.4 defines the logical IF statement:
//   IF (e) s
// where e is a logical expression and s is any executable statement
// except DO, another logical IF, or END.
//
// If e is .TRUE., statement s is executed; otherwise, control passes to
// the next executable statement.
//
// SEMANTIC CONSTRAINT (X3.9-1966 Section 7.1.2.4):
// The statement s must NOT be:
// - Another logical IF statement (no nesting)
// - A DO statement
// - Any non-executable statement (DIMENSION, COMMON, FORMAT, etc.)
//
// This rule enforces the grammatical restrictions by only allowing
// executable statements that are legal as the body of a logical IF.
// ============================================================================

logical_if_stmt
    : IF LPAREN logical_expr RPAREN logical_if_body
    ;

// ============================================================================
// LOGICAL IF STATEMENT BODY - X3.9-1966 Section 7.1.2.4
// ============================================================================
// Restricted statement list for logical IF bodies. Excludes:
// - logical_if_stmt (prevents nesting)
// - do_stmt (explicitly forbidden)
// - Non-executable statements (type declarations, DIMENSION, COMMON, FORMAT, etc.)
// ============================================================================

logical_if_body
    : statement_function_stmt      // Assignment function definition (allowed)
    | assignment_stmt              // v = e (allowed)
    | goto_stmt                    // GO TO n (allowed)
    | computed_goto_stmt           // GO TO (n1, n2, ...), i (allowed)
    | assign_stmt                  // ASSIGN i TO n (allowed)
    | assigned_goto_stmt           // GO TO n, (n1, ...) (allowed)
    | arithmetic_if_stmt           // IF (e) n1, n2, n3 (allowed)
    | if_stmt_sense_light          // IF (SENSE LIGHT i) (allowed)
    | if_stmt_sense_switch         // IF (SENSE SWITCH i) (allowed)
    | if_stmt_accumulator_overflow // IF ACCUMULATOR OVERFLOW (allowed)
    | if_stmt_quotient_overflow    // IF QUOTIENT OVERFLOW (allowed)
    | if_stmt_divide_check         // IF DIVIDE CHECK (allowed)
    // Explicitly EXCLUDED: do_stmt (X3.9-1966 Section 7.1.2.4)
    | continue_stmt                // CONTINUE (allowed)
    | stop_stmt                    // STOP [n] (allowed)
    | pause_stmt                   // PAUSE [n] (allowed)
    | read_tape_drum_stmt          // READ tape/drum (allowed)
    | read_stmt                    // READ forms (allowed)
    | write_tape_drum_stmt         // WRITE tape/drum (allowed)
    | write_stmt_basic             // WRITE output_list (allowed)
    | print_stmt                   // PRINT n, list (allowed)
    | punch_stmt                   // PUNCH n, list (allowed)
    | end_file_stmt                // END FILE i (allowed)
    | rewind_stmt                  // REWIND i (allowed)
    | backspace_stmt               // BACKSPACE i (allowed)
    // Explicitly EXCLUDED: logical_if_stmt (X3.9-1966 Section 7.1.2.4)
    // Explicitly EXCLUDED: format_stmt (non-executable)
    // Explicitly EXCLUDED: dimension_stmt (non-executable)
    // Explicitly EXCLUDED: equivalence_stmt (non-executable)
    // Explicitly EXCLUDED: frequency_stmt (non-executable)
    // Explicitly EXCLUDED: sense_light_stmt (non-executable)
    // Explicitly EXCLUDED: common_stmt (non-executable)
    | return_stmt                  // RETURN (allowed)
    | call_stmt                    // CALL name (args) (allowed)
    ;

// ============================================================================
// PROGRAM STRUCTURE - X3.9-1966 Section 8
// ============================================================================
// X3.9-1966 Section 8 defines program units:
//   8.1 Main Program - sequence of statements ending with END
//   8.2 FUNCTION Subprogram - returns a value via function name
//   8.3 SUBROUTINE Subprogram - called via CALL statement
//   8.4 BLOCK DATA Subprogram - initializes named COMMON blocks
//
// A FORTRAN program consists of exactly one main program and zero or more
// subprograms (FUNCTION, SUBROUTINE, or BLOCK DATA).
// ============================================================================

// FORTRAN 66 program - X3.9-1966 Section 8
// A complete program consists of one or more program units
fortran66_program
    : NEWLINE* program_unit+ NEWLINE* EOF
    ;

// Program unit - X3.9-1966 Section 8
// Main program must be distinguished from subprograms by the absence
// of SUBROUTINE, FUNCTION, or BLOCK DATA keywords.
program_unit
    : subprogram                // Check subprogram first (has leading keyword)
    | block_data_subprogram     // BLOCK DATA has leading keyword
    | main_program              // Main program: statements ending with END
    ;

// Main program - X3.9-1966 Section 8.1
// A main program is a sequence of statements ending with END.
// It does not have a leading SUBROUTINE, FUNCTION, or BLOCK DATA keyword.
//
// Statement ordering per X3.9-1966 Section 7:
// 1. Specification statements (type declarations, DIMENSION, COMMON,
//    EQUIVALENCE, EXTERNAL)
// 2. Statement functions (optional, after specifications)
// 3. DATA statements (optional, can mix with statement functions)
// 4. Executable statements (control flow, I/O, assignments)
main_program
    : specification_part
      statement_function_and_data_part?
      executable_part?
      end_stmt NEWLINE?
    ;

// Subprograms - X3.9-1966 Sections 8.2 and 8.3
subprogram
    : subroutine_subprogram
    | function_subprogram
    ;

// SUBROUTINE subprogram - X3.9-1966 Section 8.3
// Form: SUBROUTINE name [(dummy-arg-list)]
// Called via CALL statement; does not return a value through its name.
//
// Statement ordering per X3.9-1966 Section 7:
// 1. Specification statements (type declarations, DIMENSION, COMMON,
//    EQUIVALENCE, EXTERNAL)
// 2. Statement functions (optional, after specifications)
// 3. DATA statements (optional, can mix with statement functions)
// 4. Executable statements (control flow, I/O, assignments)
subroutine_subprogram
    : NEWLINE* SUBROUTINE IDENTIFIER parameter_list? NEWLINE
      specification_part
      statement_function_and_data_part?
      executable_part?
      end_stmt NEWLINE?
    ;

// FUNCTION subprogram - X3.9-1966 Section 8.2
// Form: [type] FUNCTION name (dummy-arg-list)
// Returns a value through the function name; called as part of an expression.
//
// Statement ordering per X3.9-1966 Section 7:
// 1. Specification statements (type declarations, DIMENSION, COMMON,
//    EQUIVALENCE, EXTERNAL)
// 2. Statement functions (optional, after specifications)
// 3. DATA statements (optional, can mix with statement functions)
// 4. Executable statements (control flow, I/O, assignments)
function_subprogram
    : NEWLINE* type_spec? FUNCTION IDENTIFIER parameter_list NEWLINE
      specification_part
      statement_function_and_data_part?
      executable_part?
      end_stmt NEWLINE?
    ;

// BLOCK DATA subprogram - X3.9-1966 Section 8.4
// Form: BLOCK DATA [name]
// Provides compile-time initialization for variables in named COMMON blocks.
// May contain only: DIMENSION, COMMON, EQUIVALENCE, type, and DATA statements.
block_data_subprogram
    : BLOCKDATA block_data_name? NEWLINE
      data_initialization_part?
      END
    ;

// Block data name - X3.9-1966 Section 8.4
// Optional name for BLOCK DATA subprogram
block_data_name
    : IDENTIFIER
    ;

// Data initialization part - X3.9-1966 Section 8.4
// Statements allowed in BLOCK DATA subprogram
data_initialization_part
    : data_initialization_statement+
    ;

data_initialization_statement
    : label? data_initialization_body NEWLINE?
    | NEWLINE  // Blank lines allowed
    ;

// Allowed statement types in BLOCK DATA - X3.9-1966 Section 8.4
data_initialization_body
    : common_stmt          // X3.9-1966 Section 7.2.2
    | dimension_stmt       // X3.9-1966 Section 7.2.1
    | equivalence_stmt     // X3.9-1966 Section 7.2.3
    | type_declaration     // X3.9-1966 Section 7.2.5
    | data_stmt            // X3.9-1966 Section 7.2.6
    ;


// ============================================================================
// PROGRAM STRUCTURE PARTS - X3.9-1966 Section 7
// ============================================================================
// X3.9-1966 Section 7 requires strict ordering of statement groups:
// 1. Specification part (type declarations, DIMENSION, COMMON, EQUIVALENCE, EXTERNAL)
// 2. Statement function and DATA part (statement functions, DATA statements)
// 3. Executable part (control flow, I/O, assignments, etc.)
// ============================================================================

// Specification part - X3.9-1966 Section 7.2
// Type statements, DIMENSION, COMMON, EQUIVALENCE, EXTERNAL declarations
// Must appear before statement functions and executable statements
specification_part
    : (specification_statement | format_statement_line)*
    ;

// Specification statements - X3.9-1966 Section 7.2
// These statements define program structure and types; must precede
// all other statements
specification_statement
    : label? specification_body NEWLINE?
    | NEWLINE
    ;

// Specification statement types - X3.9-1966 Section 7.2
specification_body
    : implicit_stmt             // X3.9-1966 Section 7.2.5
    | type_declaration          // X3.9-1966 Section 7.2.5
    | dimension_stmt            // X3.9-1966 Section 7.2.1
    | common_stmt               // X3.9-1966 Section 7.2.2
    | equivalence_stmt          // X3.9-1966 Section 7.2.3
    | external_stmt             // X3.9-1966 Section 7.2.4
    | intrinsic_stmt            // X3.9-1966 Section 8.7
    ;

// Statement function and DATA part - X3.9-1966 Section 7.2
// Statement functions and DATA statements can appear after specification part
// but before executable statements
statement_function_and_data_part
    : (statement_function_or_data_statement | format_statement_line)*
    ;

// Statement function or DATA statement - X3.9-1966 Section 7.2
// These statements initialize data or define statement functions
statement_function_or_data_statement
    : label? statement_function_or_data_body NEWLINE?
    | NEWLINE
    ;

// Statement function or DATA statement types - X3.9-1966 Section 7.2
statement_function_or_data_body
    : statement_function_stmt   // X3.9-1966 Section 7.2
    | data_stmt                 // X3.9-1966 Section 7.2.6
    ;

// Executable part - X3.9-1966 Section 7.1
// All executable statements (control flow, I/O, assignments)
// Must appear after all specification and statement function/DATA statements
executable_part
    : (executable_statement | format_statement_line)*
    ;

// Format statements may appear anywhere within program units
// (X3.9-1966 Section 7.2.7). They are non-executable definitions that can
// appear before, between, or after the ordered parts without affecting the
// ordering requirement for specification, statement function/data, and
// executable statements.
format_statement_line
    : label? format_stmt NEWLINE?
    ;

// Executable statement - X3.9-1966 Section 7.1
// Control flow, I/O, and assignment statements
executable_statement
    : label? executable_statement_body NEWLINE?
    | NEWLINE
    ;

// Executable statement types - X3.9-1966 Section 7.1
executable_statement_body
    : assignment_stmt           // X3.9-1966 Section 7.1.1.1, 7.1.1.2
    | assign_stmt               // X3.9-1966 Section 7.1.1.3
    | assigned_goto_stmt        // X3.9-1966 Section 7.1.2.1.2
    | computed_goto_stmt        // X3.9-1966 Section 7.1.2.2
    | goto_stmt                 // X3.9-1966 Section 7.1.2.1.1
    | arithmetic_if_stmt        // X3.9-1966 Section 7.1.2.3
    | logical_if_stmt           // X3.9-1966 Section 7.1.2.4
    | do_stmt                   // X3.9-1966 Section 7.1.2.8
    | continue_stmt             // X3.9-1966 Section 7.1.2.9
    | stop_stmt                 // X3.9-1966 Section 7.1.2.5
    | pause_stmt                // X3.9-1966 Section 7.1.2.6
    | call_stmt                 // X3.9-1966 Section 7.1.2.10
    | return_stmt               // X3.9-1966 Section 7.1.2.7
    | read_tape_drum_stmt       // Inherited: tape/drum I/O
    | read_stmt                 // X3.9-1966 Section 7.1.3.1
    | write_tape_drum_stmt      // Inherited: tape/drum I/O
    | write_stmt                // X3.9-1966 Section 7.1.3.1
    | print_stmt                // X3.9-1966 Section 7.1.3.2
    | punch_stmt                // X3.9-1966 Section 7.1.3.2
    | rewind_stmt               // X3.9-1966 Section 7.1.3.3
    | backspace_stmt            // X3.9-1966 Section 7.1.3.3
    | endfile_stmt              // X3.9-1966 Section 7.1.3.3
    ;

// Type declaration - X3.9-1966 Section 7.2.5
// Form: type v1, v2, ..., vn
// Explicitly declares the type of variables and arrays
type_declaration
    : type_spec variable_list
    ;

// Statement list - X3.9-1966 Section 3.2
// A sequence of statements (blank lines allowed as in punch cards)
statement_list
    : statement*
    ;

// Statement - X3.9-1966 Section 3.2
// Form: [label] statement-body
// Labels were punched in columns 1-5, statements in columns 7-72
statement
    : label? statement_body NEWLINE?
    | NEWLINE  // Blank line (blank punch card was valid)
    ;

// Statement label - X3.9-1966 Section 3.4
// 1 to 5 decimal digits (range 1-99999)
// Labels identify statements for control transfer and FORMAT references
label
    : LABEL
    ;

// ============================================================================
// STATEMENT TYPES - X3.9-1966 Section 7
// ============================================================================
// X3.9-1966 Section 7 classifies statements into:
//   7.1 Executable Statements (control flow, I/O, assignment)
//   7.2 Non-executable Statements (declarations, specifications)
//
// NOTE: end_stmt is NOT included here - it is handled separately as a
// program unit terminator (X3.9-1966 Section 8) to enable parsing
// multiple program units.
// ============================================================================

statement_body
    // Non-executable statements - X3.9-1966 Section 7.2
    : statement_function_stmt  // X3.9-1966 Section 7.2 (statement function definition)
    | dimension_stmt           // X3.9-1966 Section 7.2.1
    | common_stmt              // X3.9-1966 Section 7.2.2
    | equivalence_stmt         // X3.9-1966 Section 7.2.3
    | external_stmt            // X3.9-1966 Section 7.2.4
    | implicit_stmt            // X3.9-1966 Section 7.2.5
    | type_declaration         // X3.9-1966 Section 7.2.5
    | data_stmt                // X3.9-1966 Section 7.2.6
    | format_stmt              // X3.9-1966 Section 7.2.7
    | intrinsic_stmt           // X3.9-1966 Section 8.7
    // Executable statements - X3.9-1966 Section 7.1
    // Assignment statements - Section 7.1.1
    | assignment_stmt          // X3.9-1966 Section 7.1.1.1 (arith), 7.1.1.2 (logical)
    | assign_stmt              // X3.9-1966 Section 7.1.1.3 (ASSIGN k TO i)
    // Control statements - Section 7.1.2
    | assigned_goto_stmt       // X3.9-1966 Section 7.1.2.1.2 - precede goto
    | computed_goto_stmt       // X3.9-1966 Section 7.1.2.2 (computed GO TO)
    | goto_stmt                // X3.9-1966 Section 7.1.2.1.1 (unconditional GO TO)
    | arithmetic_if_stmt       // X3.9-1966 Section 7.1.2.3 (arithmetic IF)
    | logical_if_stmt          // X3.9-1966 Section 7.1.2.4 (logical IF)
    | do_stmt                  // X3.9-1966 Section 7.1.2.8 (DO)
    | continue_stmt            // X3.9-1966 Section 7.1.2.9 (CONTINUE)
    | stop_stmt                // X3.9-1966 Section 7.1.2.5 (STOP)
    | pause_stmt               // X3.9-1966 Section 7.1.2.6 (PAUSE)
    | call_stmt                // X3.9-1966 Section 7.1.2.10 (CALL)
    | return_stmt              // X3.9-1966 Section 7.1.2.7 (RETURN)
    // I/O statements - Section 7.1.3
    | read_stmt                // X3.9-1966 Section 7.1.3.1 (READ)
    | write_stmt               // X3.9-1966 Section 7.1.3.1 (WRITE)
    | print_stmt               // X3.9-1966 Section 7.1.3.2 (output)
    | punch_stmt               // X3.9-1966 Section 7.1.3.2 (output)
    | rewind_stmt              // X3.9-1966 Section 7.1.3.3 (auxiliary I/O)
    | backspace_stmt           // X3.9-1966 Section 7.1.3.3 (auxiliary I/O)
    | endfile_stmt             // X3.9-1966 Section 7.1.3.3 (auxiliary I/O)
    ;

// ============================================================================
// AUXILIARY I/O STATEMENTS - X3.9-1966 Section 7.1.3.3
// ============================================================================
// X3.9-1966 Section 7.1.3.3 defines auxiliary I/O statements for sequential
// file positioning:
//   REWIND u     - Position file to its initial point
//   BACKSPACE u  - Position file back one record
//   ENDFILE u    - Write an end-of-file record
// where u is an unsigned integer expression (unit identifier).
// ============================================================================

// REWIND statement - X3.9-1966 Section 7.1.3.3
// Positions the file to its initial point (beginning)
rewind_stmt
    : REWIND integer_expr
    ;

// BACKSPACE statement - X3.9-1966 Section 7.1.3.3
// Positions the file back one record
backspace_stmt
    : BACKSPACE integer_expr
    ;

// ENDFILE statement - X3.9-1966 Section 7.1.3.3
// Writes an end-of-file record on the specified unit
endfile_stmt
    : ENDFILE integer_expr
    ;

// ============================================================================
// WRITE STATEMENT - X3.9-1966 Section 7.1.3.1
// ============================================================================
// X3.9-1966 Section 7.1.3.1 defines the WRITE statement:
//   WRITE (u, f) list
// where:
//   u = unit number (integer expression)
//   f = FORMAT statement label
//   list = optional output list (expressions separated by commas)
//
// The WRITE statement writes one record to a sequential file on the
// specified unit using the format specified by the FORMAT label.
// If no output list is provided, the statement writes an empty record.
// ============================================================================

// WRITE statement - X3.9-1966 Section 7.1.3.1
// Form: WRITE (u, f) [list]
write_stmt
    : WRITE LPAREN integer_expr COMMA label RPAREN output_list?
    ;

// ============================================================================
// EXTERNAL AND INTRINSIC STATEMENTS - X3.9-1966 Sections 7.2.4 and 8.7
// ============================================================================
// X3.9-1966 Section 7.2.4 defines the EXTERNAL statement:
//   EXTERNAL name1, name2, ..., namen
// Identifies names as external procedure names (allows passing as arguments).
//
// X3.9-1966 Section 8.7 defines intrinsic functions and the INTRINSIC statement:
//   INTRINSIC name1, name2, ..., namen
// Identifies names as intrinsic (built-in) function names.
// ============================================================================

// EXTERNAL statement - X3.9-1966 Section 7.2.4
// Declares names that refer to external procedures
external_stmt
    : EXTERNAL identifier_list
    ;

// INTRINSIC statement - X3.9-1966 Section 8.7
// Declares names that refer to intrinsic (built-in) functions
intrinsic_stmt
    : INTRINSIC identifier_list
    ;

// Identifier list - used by EXTERNAL and INTRINSIC statements
identifier_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// Variable list - X3.9-1966 Section 5.1
// List of variables for type declarations
variable_list
    : variable (COMMA variable)*
    ;

// ============================================================================
// DATA STATEMENT - X3.9-1966 Section 7.2.6
// ============================================================================
// X3.9-1966 Section 7.2.6 defines the DATA statement for compile-time
// initialization of variables and arrays:
//   DATA nlist /clist/ [, nlist /clist/]...
// where:
//   nlist = list of variables, array elements, or implied DO loops
//   clist = list of constants (with optional repeat count)
//
// DATA statements may appear in main programs, subprograms, and BLOCK DATA.
//
// Examples:
//   DATA X, Y, Z /1.0, 2.0, 3.0/
//   DATA A /3*0.0/                     (repeat count: 3 zeros)
//   DATA (ARR(I), I=1,10) /10*1.0/     (implied DO)
// ============================================================================

// DATA statement - X3.9-1966 Section 7.2.6
data_stmt
    : DATA data_stmt_set (COMMA data_stmt_set)*
    ;

// DATA statement set - X3.9-1966 Section 7.2.6
// Object list followed by constant list in slashes
data_stmt_set
    : data_stmt_object_list SLASH data_stmt_value_list SLASH
    ;

// Object list - X3.9-1966 Section 7.2.6
// Variables, array elements, or implied DO loops to initialize
data_stmt_object_list
    : data_stmt_object (COMMA data_stmt_object)*
    ;

// Data object - X3.9-1966 Section 7.2.6
data_stmt_object
    : variable                              // Simple variable or array element
    | LPAREN data_implied_do RPAREN         // Implied DO loop
    ;

// Implied DO - X3.9-1966 Section 7.2.6
// Form: (object-list, i = e1, e2 [, e3])
data_implied_do
    : data_stmt_object_list COMMA IDENTIFIER EQUALS expr COMMA expr (COMMA expr)?
    ;

// Constant list - X3.9-1966 Section 7.2.6
data_stmt_value_list
    : data_stmt_value (COMMA data_stmt_value)*
    ;

// Constant value - X3.9-1966 Section 7.2.6
// Form: [r*]c where r is repeat count and c is constant
data_stmt_value
    : data_stmt_repeat? data_stmt_constant
    ;

// Repeat count - X3.9-1966 Section 7.2.6
// Unsigned integer followed by asterisk (e.g., 3* in 3*0.0)
// FORTRAN 77 (ISO 1539:1980 Section 9.3) inherits this syntax
data_stmt_repeat
    : unsigned_int MULTIPLY
    ;

// Unsigned integer - X3.9-1966 Section 4.1.2
// FORTRAN 77 (ISO 1539:1980) accepts the same syntax
unsigned_int
    : INTEGER_LITERAL
    | LABEL               // LABEL tokens match 1-5 digit integers
    ;

// Data constant - X3.9-1966 Section 7.2.6
// Optionally signed literal constant
// FORTRAN 77 (ISO 1539:1980 Section 9.3) extends to support repetition
data_stmt_constant
    : PLUS? literal
    | MINUS literal
    ;

// ============================================================================
// ASSIGN STATEMENT - X3.9-1966 Section 7.1.1.3
// ============================================================================
// X3.9-1966 Section 7.1.1.3 defines the ASSIGN statement:
//   ASSIGN k TO i
// where k is a statement label and i is an integer variable.
// This stores the label k in variable i for use with assigned GO TO.
//
// NON-COMPLIANT with ISO/IEC 1539-1:2018: ASSIGN is a deleted feature
// per Annex B.2. Retained for historical FORTRAN 66 accuracy only.
// See docs/fortran_66_audit.md for compliance details.
// ============================================================================

assign_stmt
    : ASSIGN label TO variable
    ;

// ============================================================================
// ASSIGNED GO TO STATEMENT - X3.9-1966 Section 7.1.2.1.2
// ============================================================================
// X3.9-1966 Section 7.1.2.1.2 defines the assigned GO TO statement:
//   GO TO i, (k1, k2, ..., km)
// where i is an integer variable containing a label (set by ASSIGN)
// and (k1, k2, ..., km) is a list of valid target labels.
// Branches to the label stored in variable i.
//
// NON-COMPLIANT with ISO/IEC 1539-1:2018: Assigned GO TO is a deleted
// feature per Annex B.2. Retained for historical FORTRAN 66 accuracy only.
// See docs/fortran_66_audit.md for compliance details.
// ============================================================================

assigned_goto_stmt
    : GOTO variable COMMA LPAREN label_list RPAREN
    ;

// ============================================================================
// DO STATEMENT - X3.9-1966 Section 7.1.2.8
// ============================================================================
// X3.9-1966 Section 7.1.2.8 defines the DO statement:
//   DO s i = e1, e2 [, e3]
// where:
//   s  = statement label of the terminal statement (range)
//   i  = integer DO variable
//   e1 = initial value expression
//   e2 = limit value expression
//   e3 = increment expression (defaults to 1)
//
// Note: FORTRAN II used ASSIGN keyword; FORTRAN 66 uses = (equals sign).
// ============================================================================

do_stmt
    : DO label variable EQUALS expr COMMA expr (COMMA expr)?
    ;

// ============================================================================
// CONSTANTS (LITERALS) - X3.9-1966 Section 4
// ============================================================================
// X3.9-1966 Section 4 defines constants for each data type:
//   4.1.2 Integer constants
//   4.2.2 Real constants
//   4.3.2 Double precision constants
//   4.4.2 Complex constants
//   4.5.2 Logical constants (.TRUE., .FALSE.)
//
// Note: LABEL tokens match 1-5 digit integers due to lexer precedence.
// ============================================================================

literal
    : INTEGER_LITERAL            // X3.9-1966 Section 4.1.2
    | LABEL                      // Accept LABEL as integer (token precedence)
    | REAL_LITERAL               // X3.9-1966 Section 4.2.2
    | logical_literal            // X3.9-1966 Section 4.5.2
    | complex_literal            // X3.9-1966 Section 4.4.2
    ;

// Complex constant - X3.9-1966 Section 4.4.2
// Form: (real-or-int-const, real-or-int-const)
// Examples: (1.0, 2.0), (3, -4), (-1.5E2, 2.5E-1)
//
// SEMANTIC CONSTRAINT (X3.9-1966 Section 4.4.2):
// Both parts MUST be literal constants (not expressions, variables, arrays).
// Valid:   (1.0, 2.0), (3, -4), (-1.5E2, +2.5E-1)
// Invalid: (X, Y), (1+2, 3), (A(1), B(2))
//
// Grammar enforcement: complex_part restricts to signed_real_literal or
// signed_int_literal, rejecting expressions and variables at token level.
complex_literal
    : LPAREN complex_part COMMA complex_part RPAREN
    ;

// Complex part - either signed real or signed integer literal
// Restricts to literal constants per X3.9-1966 Section 4.4.2
complex_part
    : signed_real_literal
    | signed_int_literal
    ;

// Signed real literal (optional sign)
signed_real_literal
    : PLUS? REAL_LITERAL
    | MINUS REAL_LITERAL
    ;

// Signed integer literal (optional sign)
signed_int_literal
    : PLUS? INTEGER_LITERAL
    | PLUS? LABEL               // LABEL also matches integers
    | MINUS INTEGER_LITERAL
    | MINUS LABEL               // LABEL also matches integers
    ;

// ============================================================================
// FORMAT STATEMENT - X3.9-1966 Section 7.2.7
// ============================================================================
// X3.9-1966 Section 7.2.7 defines the FORMAT statement and edit descriptors:
//   Iw       - Integer (w = field width)
//   Fw.d     - Real fixed-point (w = width, d = decimal places)
//   Ew.d     - Real exponential (w = width, d = decimal places)
//   Dw.d     - Double precision exponential
//   Gw.d     - General (fixed or exponential based on magnitude)
//   Aw       - Character/alphanumeric (added in FORTRAN IV)
//   Lw       - Logical
//   nX       - Skip n positions
//   nP       - Scale factor
//   nHc...c  - Hollerith (n characters follow)
//
// Note: Lexer tokenizes F10.2 as F10 (IDENTIFIER) followed by .2 (REAL_LITERAL).
// ============================================================================

format_item
    : format_repeat_count? format_descriptor_full
    | HOLLERITH                  // X3.9-1966 Section 7.2.7
    ;

format_repeat_count
    : INTEGER_LITERAL
    | LABEL
    ;

// Format descriptor with optional decimal specification
format_descriptor_full
    : IDENTIFIER format_decimal_part?
    ;

// Decimal part of format descriptor (e.g., .d or .dEe)
format_decimal_part
    : REAL_LITERAL              // Handles .2, .6E2, etc.
    ;

// ============================================================================
// FORTRAN 66 (X3.9-1966) HISTORICAL SIGNIFICANCE
// ============================================================================
//
// FORTRAN 66 (ANSI X3.9-1966, March 7, 1966) was revolutionary because:
//
// 1. **First Language Standard**: First formal programming language standard
//    - Established ANSI X3.9-1966 as the template for language standards
//    - Developed by X3.4.3 subcommittee (1962-1966)
//
// 2. **Machine Independence**: Removed vendor-specific dependencies
//    - Code became portable across different computer architectures
//    - Eliminated IBM-specific and machine-dependent features
//
// 3. **Program Structure**: Standardized program organization (Section 8)
//    - Main Program, FUNCTION, SUBROUTINE, BLOCK DATA subprograms
//    - Clear separation between executable and non-executable statements
//
// 4. **Scientific Computing Foundation**: Established reliable base
//    - Universities could teach standard FORTRAN curriculum
//    - Research code became portable between institutions
//
// 5. **Industry Adoption**: Vendors aligned implementations
//    - Reduced fragmentation in FORTRAN implementations
//    - Enabled commercial software industry growth
//
// This parser captures FORTRAN 66 standardization achievements while
// maintaining compatibility with FORTRAN IV data type innovations.
// It serves as the foundation for FORTRAN 77 structured programming.
//
// ============================================================================
