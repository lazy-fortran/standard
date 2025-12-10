/*
 * FORTRANIIParser.g4
 *
 * FORTRAN II (1958) - Parser for Procedural Programming Revolution
 * Introduces separately compiled subroutines and functions.
 *
 * This parser extends FORTRANParser (1957) with the six new FORTRAN II
 * statement/subprogram forms and redefines the complete statement set
 * to include CALL, RETURN, and COMMON.
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
//
// Historical context:
// - No file system: magnetic tapes and card decks were the "files"
// - Primary purpose: scientific computation and engineering calculations
// - Separate compilation enabled modular programming and code reuse
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
    : label? statement_body NEWLINE?
    | NEWLINE  // Blank punch card (allowed)
    ;

// Statement label - C28-6000-2 Part I, Chapter 2
// 1-5 digit integer (1-99999), punched in columns 1-5
label
    : LABEL
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
// These entry points use statement_body_strict and common_stmt_strict to
// enforce the original 1958 FORTRAN II specification where only blank COMMON
// was available. Named COMMON blocks (/name/) were introduced in FORTRAN 66.
//
// Use these rules for historical audits or when strict 1958 conformance is
// required. The default entry points (fortran_program, main_program, etc.)
// use relaxed mode that accepts named COMMON blocks for compatibility.
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
    : label? statement_body_strict NEWLINE?
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
// C28-6000-2 Part I, Chapter 1 and Appendix A: Summary of FORTRAN II Statements
// ============================================================================
// FORTRAN II includes all original FORTRAN I (1957) statements plus six new
// forms: SUBROUTINE, FUNCTION, CALL, RETURN, COMMON, and END.
//
// NOTE: end_stmt is NOT included here - it is handled separately as a
// program unit terminator in main_program, subroutine_subprogram, and
// function_subprogram rules.
// ============================================================================

// Relaxed mode statement body - allows named COMMON blocks
statement_body
    : statement_function_stmt  // Appendix A: Statement function definition
    | assignment_stmt      // Appendix A: v = e (assignment)
    | goto_stmt           // Appendix A: GO TO n
    | computed_goto_stmt  // Appendix A: GO TO (n1, n2, ...), i
    | arithmetic_if_stmt  // Appendix A: IF (e) n1, n2, n3
    | do_stmt            // Appendix A: DO n i = m1, m2 [, m3]
    | continue_stmt      // Appendix A: CONTINUE
    | stop_stmt          // Appendix A: STOP [n]
    | pause_stmt         // Appendix A: PAUSE [n]
    | read_stmt          // Appendix A: READ forms
    | write_stmt_basic   // C28-6003 Chap III: WRITE output_list (from FORTRAN I)
    | print_stmt         // Appendix A: PRINT n, list
    | punch_stmt         // Appendix A: PUNCH n, list
    | format_stmt        // Appendix A: FORMAT (specification)
    | dimension_stmt     // Appendix A: DIMENSION v, v, ...
    | equivalence_stmt   // Appendix A: EQUIVALENCE (a,b,...), ...
    | frequency_stmt     // Appendix A: FREQUENCY n (i1, i2, ...)
    | common_stmt        // Appendix A: COMMON list (NEW in FORTRAN II)
    | return_stmt        // Appendix A: RETURN (NEW in FORTRAN II)
    | call_stmt          // Appendix A: CALL name (args) (NEW in FORTRAN II)
    ;

// Strict 1958 mode statement body - enforces blank COMMON only
// Use statement_body_strict via strict entry points for historical audits.
statement_body_strict
    : statement_function_stmt  // Appendix A: Statement function definition
    | assignment_stmt      // Appendix A: v = e (assignment)
    | goto_stmt           // Appendix A: GO TO n
    | computed_goto_stmt  // Appendix A: GO TO (n1, n2, ...), i
    | arithmetic_if_stmt  // Appendix A: IF (e) n1, n2, n3
    | do_stmt            // Appendix A: DO n i = m1, m2 [, m3]
    | continue_stmt      // Appendix A: CONTINUE
    | stop_stmt          // Appendix A: STOP [n]
    | pause_stmt         // Appendix A: PAUSE [n]
    | read_stmt          // Appendix A: READ forms
    | write_stmt_basic   // C28-6003 Chap III: WRITE output_list (from FORTRAN I)
    | print_stmt         // Appendix A: PRINT n, list
    | punch_stmt         // Appendix A: PUNCH n, list
    | format_stmt        // Appendix A: FORMAT (specification)
    | dimension_stmt     // Appendix A: DIMENSION v, v, ...
    | equivalence_stmt   // Appendix A: EQUIVALENCE (a,b,...), ...
    | frequency_stmt     // Appendix A: FREQUENCY n (i1, i2, ...)
    | common_stmt_strict // 1958 strict: blank COMMON only
    | return_stmt        // Appendix A: RETURN (NEW in FORTRAN II)
    | call_stmt          // Appendix A: CALL name (args) (NEW in FORTRAN II)
    ;

// ============================================================================
// FORTRAN II NEW STATEMENTS
// C28-6000-2 Part I, Chapter 3: The New FORTRAN II Statements
// ============================================================================

// CALL statement - C28-6000-2 Part I, Chapter 3, Section 3.1
// Form: CALL name or CALL name (a1, a2, ..., an)
// Transfers control to a subroutine, passing arguments by reference
call_stmt
    : CALL IDENTIFIER (LPAREN expr_list? RPAREN)?
    ;


// ============================================================================
// STATEMENT FUNCTION DEFINITION
// C28-6000-2 Part I, Chapter 4: Expressions (inherited from FORTRAN I)
// ============================================================================
// Statement functions define inline functions within a program unit.
// Form: name(dummy-arg-list) = expression
//
// Note: Statement functions were present in FORTRAN I and continue in
// FORTRAN II. They must appear after specification statements and before
// executable statements (enforced semantically, not syntactically).
//
// Examples:
//   F(X) = X * X + 2.0 * X + 1.0
//   AREA(R) = 3.14159 * R * R
//   DIST(X1, Y1, X2, Y2) = SQRT((X2-X1)**2 + (Y2-Y1)**2)
// ============================================================================

statement_function_stmt
    : IDENTIFIER LPAREN statement_function_dummy_arg_list RPAREN EQUALS expr
    ;

// Dummy argument list for statement functions (identifiers only)
statement_function_dummy_arg_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;


// ============================================================================
// INHERITED FORTRAN I (1957) STATEMENTS
// C28-6000-2 Part I, Chapter 1: All original FORTRAN statements remain valid
// See also C28-6003 (FORTRAN I manual) Appendix B for statement forms
// ============================================================================

// Assignment statement - Appendix A: v = e
// The natural mathematical notation that made FORTRAN revolutionary
assignment_stmt
    : variable EQUALS expr
    ;

// Unconditional GO TO - Appendix A: GO TO n
// Transfers control to the statement with label n
goto_stmt
    : GOTO label
    ;

// Computed GO TO - Appendix A: GO TO (n1, n2, ..., nm), i
// Transfers to n1 if i=1, n2 if i=2, etc.
computed_goto_stmt
    : GOTO LPAREN label_list RPAREN COMMA expr
    ;

// Arithmetic IF - Appendix A: IF (e) n1, n2, n3
// Three-way branch: n1 if e<0, n2 if e=0, n3 if e>0
arithmetic_if_stmt
    : IF LPAREN expr RPAREN label COMMA label COMMA label
    ;

// DO statement - Appendix A: DO n i = m1, m2 [, m3]
// Counted loop from m1 to m2 with optional step m3, ends at label n
do_stmt
    : DO label variable ASSIGN expr COMMA expr (COMMA expr)?
    ;

// CONTINUE statement - Appendix A: CONTINUE
// DO loop termination and general jump target
continue_stmt : CONTINUE ;

// STOP statement - Appendix A: STOP [n]
// Program termination with optional numeric code
stop_stmt : STOP integer_expr? ;

// PAUSE statement - Appendix A: PAUSE [n]
// Operator intervention: halts and displays message to operator
pause_stmt : PAUSE integer_expr? ;

// RETURN statement - C28-6000-2 Part I, Chapter 3, Section 3.4
// Returns control from subroutine or function to caller (NEW in FORTRAN II)
return_stmt : RETURN ;

// END statement - C28-6000-2 Part I, Chapter 3, Section 3.6
// Terminates a program unit (formalized in FORTRAN II)
end_stmt : END ;


// ============================================================================
// I/O STATEMENTS
// C28-6000-2 Appendix A: Input/Output statements (inherited from FORTRAN I)
// ============================================================================

// READ statement - Appendix A: READ forms
// Input from punch cards or magnetic tape
read_stmt
    : READ integer_expr COMMA input_list                    // READ unit, list
    | READ integer_expr COMMA label COMMA input_list       // READ unit, format, list
    ;

// PRINT statement - Appendix A: PRINT n, list
// Output to line printer (primary 1957 output device)
print_stmt
    : PRINT integer_expr COMMA output_list
    ;

// PUNCH statement - Appendix A: PUNCH n, list
// Output to card punch (1957 data storage mechanism)
punch_stmt
    : PUNCH integer_expr COMMA output_list
    ;

// Note: FORMAT statement (format_stmt) is inherited from FORTRANParser.
// The 1957 FORTRAN grammar includes FORMAT statements per C28-6003 Appendix B
// row 16. FORMAT was part of the original IBM 704 FORTRAN specification.
// See FORTRANParser.g4 for format_stmt, format_specification, format_item,
// format_repeat_count, format_descriptor, and format_decimal_part rules.


// ============================================================================
// SPECIFICATION STATEMENTS
// C28-6000-2 Appendix A: Specification statements
// ============================================================================

// DIMENSION statement - Appendix A: DIMENSION v, v, ...
// Array dimension declarations (up to 3 dimensions in 1957)
dimension_stmt
    : DIMENSION array_declarator (COMMA array_declarator)*
    ;

// Array declarator with dimensions
array_declarator
    : IDENTIFIER LPAREN dimension_list RPAREN
    ;

// Dimension list (compile-time constants in original FORTRAN)
dimension_list
    : integer_expr (COMMA integer_expr)*
    ;

// EQUIVALENCE statement - Appendix A: EQUIVALENCE (a,b,...), ...
// Memory overlay: allows variables to share the same memory location
equivalence_stmt
    : EQUIVALENCE equivalence_set (COMMA equivalence_set)*
    ;

// Equivalence set (variables sharing memory)
equivalence_set
    : LPAREN variable COMMA variable (COMMA variable)* RPAREN
    ;

// FREQUENCY statement - Appendix A: FREQUENCY n (i1, i2, ...)
// Optimization hint for branch prediction (unique to 1957 FORTRAN)
// Removed in later standards
frequency_stmt
    : FREQUENCY label LPAREN integer_expr (COMMA integer_expr)* RPAREN
    ;

// COMMON statement - C28-6000-2 Part I, Chapter 3, Section 3.5
// Shared storage between program units (NEW in FORTRAN II)
//
// Historical context:
// The 1958 IBM 704 FORTRAN II manual (C28-6000-2) defines only blank COMMON:
//   "COMMON a1, a2, ..., an"
// where a1..an are variable names or array names. Named COMMON blocks
// (COMMON /BLOCK/ list) were introduced in FORTRAN 66.
//
// This grammar provides two modes (see issue #156):
// - common_stmt: Relaxed mode - accepts both blank and named COMMON for
//   practical compatibility with later standards.
// - common_stmt_strict: Strict 1958 mode - accepts only blank COMMON per
//   the historical C28-6000-2 specification.
//
// Form (relaxed): COMMON list or COMMON /name/ list
// Form (strict):  COMMON list only
common_stmt
    : COMMON (SLASH IDENTIFIER SLASH)? variable_list
    ;

// Strict COMMON statement - C28-6000-2 Part I, Chapter 3, Section 3.5
// Historical 1958 semantics: blank COMMON only, no named blocks.
// Use this rule (via strict entry points) for historical audits.
common_stmt_strict
    : COMMON variable_list
    ;


// ============================================================================
// EXPRESSION EVALUATION
// C28-6000-2 Part I, Chapter 4: Expressions (inherited from FORTRAN I)
// ============================================================================
// FORTRAN II implements proper operator precedence (highest to lowest):
//   1. ** (power) - RIGHT ASSOCIATIVE
//   2. unary +, - (unary operators)
//   3. *, / (multiplication, division) - left associative
//   4. binary +, - (addition, subtraction) - left associative
//
// Per C28-6000-2, expression syntax matches 1957 FORTRAN mathematical
// expressions. The hierarchy ensures:
// - 2**3**4 parses as 2**(3**4) per right-associativity of **
// - -2**2 parses as -(2**2) = -4, not (-2)**2 = 4
// - 2+3*4 parses as 2+(3*4) = 14, not (2+3)*4 = 20
//
// Note: FORTRAN II does not include relational_expr at the top of expr.
// Relational expressions (.EQ., .NE., etc.) were primarily used with
// arithmetic IF; FORTRAN 66 introduces logical types for these.
// ============================================================================

// Arithmetic expression - top of the precedence hierarchy
expr
    : additive_expr
    ;

// Addition and subtraction - lowest arithmetic precedence
additive_expr
    : additive_expr additive_op multiplicative_expr
    | multiplicative_expr
    ;

additive_op
    : PLUS | MINUS
    ;

// Multiplication and division
multiplicative_expr
    : multiplicative_expr multiplicative_op unary_expr
    | unary_expr
    ;

multiplicative_op
    : MULTIPLY | SLASH
    ;

// Unary operators (higher precedence than binary +/-)
unary_expr
    : unary_op unary_expr
    | power_expr
    ;

unary_op
    : PLUS | MINUS
    ;

// Exponentiation - highest precedence, RIGHT ASSOCIATIVE
// 2**3**4 = 2**(3**4), NOT (2**3)**4
power_expr
    : primary POWER power_expr    // Right associative: a**b**c = a**(b**c)
    | primary
    ;

// Primary expressions - atoms of the expression grammar
primary
    : literal
    | variable
    | LPAREN expr RPAREN
    ;

// Literals - C28-6000-2 Part I, Chapter 4
// Fixed-point (integer) and floating-point (real) constants
literal
    : INTEGER_LITERAL
    | LABEL              // LABEL tokens are valid integer literals in expressions
    | REAL_LITERAL
    ;

// Variables and array/function references - C28-6000-2 Part I, Chapter 4
// In FORTRAN II, SIN(X) and A(I) are syntactically identical;
// semantic analysis distinguishes functions from arrays
variable
    : IDENTIFIER (LPAREN expr_list RPAREN)?
    ;


// ============================================================================
// UTILITY RULES
// Helper patterns for labels, lists, and I/O
// ============================================================================

label_list
    : label (COMMA label)*
    ;

variable_list
    : variable (COMMA variable)*
    ;

input_list
    : variable (COMMA variable)*
    ;

output_list
    : expr (COMMA expr)*
    ;

expr_list
    : expr (COMMA expr)*
    ;

integer_expr
    : expr  // Expression that must evaluate to integer (semantic constraint)
    ;
