// FORTRAN II (1958) - Parser for Procedural Programming Revolution
// Introduces separately compiled subroutines and functions
parser grammar FORTRANIIParser;

import FORTRANParser;  // Import FORTRAN I (1957) constructs

options {
    tokenVocab = FORTRANIILexer;
}

// ====================================================================
// FORTRAN II (1958) NEW PARSER RULES
// ====================================================================
// - No file system: Magnetic tapes and card decks were the "files"
//
// MATHEMATICAL FOCUS (1957):
// - Primary purpose: Scientific computation and engineering calculations
// - Expression evaluation: Full precedence with parentheses override
// - Built-in functions: Mathematical functions (SIN, COS, SQRT, LOG, etc.)
// - Optimization: Compiler focused on mathematical expression efficiency
//
// ====================================================================

// ====================================================================
// FORTRAN II (1958) - SUBROUTINE AND FUNCTION DEFINITIONS
// ====================================================================

// FORTRAN II program with separately compiled units
fortran_program
    : (main_program | subroutine_subprogram | function_subprogram) EOF
    ;

// Main program (inherited from FORTRAN I)
// A main program is a sequence of statements ending with END.
main_program
    : statement_list end_stmt NEWLINE?
    ;

// Statement list allowing empty lines (punch cards could be blank)
statement_list
    : statement*
    ;

// Individual statement with optional label and newline handling
// Labels were punched in columns 1-5, statements in columns 7-72
statement
    : label? statement_body NEWLINE?
    | NEWLINE  // Blank punch card (allowed)
    ;

// Statement label (1957: 1-99999, punched in columns 1-5)
label
    : LABEL
    ;

// Subroutine definition (NEW in FORTRAN II)
subroutine_subprogram
    : SUBROUTINE IDENTIFIER parameter_list? NEWLINE
      statement_list
      end_stmt NEWLINE?
    ;

// Function definition (NEW in FORTRAN II)
function_subprogram
    : type_spec? FUNCTION IDENTIFIER parameter_list NEWLINE
      statement_list
      end_stmt NEWLINE?
    ;

// Parameter list for subprograms
parameter_list
    : LPAREN (IDENTIFIER (COMMA IDENTIFIER)*)? RPAREN
    ;

// Type specification for functions
type_spec
    : INTEGER
    | REAL
    ;

// All statement types available in 1957 FORTRAN
// This was a remarkably small language by today's standards!
// NOTE: end_stmt is NOT included here - it is handled separately as a
// program unit terminator in main_program, subroutine_subprogram, and
// function_subprogram rules.
statement_body
    : assignment_stmt      // Variable = Expression (mathematical notation!)
    | goto_stmt           // Unconditional jump to labeled statement
    | computed_goto_stmt  // Multi-way branch based on integer expression
    | arithmetic_if_stmt  // Three-way branch based on expression sign
    | do_stmt            // Counted loop with mandatory label
    | continue_stmt      // Loop termination and jump target
    | stop_stmt          // Program termination
    | pause_stmt         // Operator intervention (unique to 1957!)
    | read_stmt          // Input from cards/tape
    | print_stmt         // Output to line printer
    | punch_stmt         // Output to card punch (data storage!)
    | format_stmt        // I/O formatting specification
    | dimension_stmt     // Array dimension declarations
    | equivalence_stmt   // Variable memory overlay
    | frequency_stmt     // Optimization hints (unique to 1957!)
    | common_stmt        // Global variable declarations
    | return_stmt       // Return from subprogram
    | call_stmt         // NEW in FORTRAN II: Call subroutine
    ;

// CALL statement (NEW in FORTRAN II)
call_stmt
    : CALL IDENTIFIER (LPAREN expr_list? RPAREN)?
    ;


// Assignment statement (1957 revolutionary syntax!)
// This natural mathematical notation was groundbreaking in 1957
// Before FORTRAN, programmers wrote: STO A, ADD B, STO C
// FORTRAN allowed: C = A + B (revolutionary!)
assignment_stmt
    : variable EQUALS expr
    ;


// Unconditional GOTO (1957 primary control flow)
// Example: GO TO 100 (jump to statement labeled 100)
goto_stmt
    : GOTO label
    ;

// Computed GOTO (1957 multi-way branch)
// Example: GO TO (10, 20, 30), I
// If I=1 go to 10, I=2 go to 20, I=3 go to 30
computed_goto_stmt
    : GOTO LPAREN label_list RPAREN COMMA expr
    ;

// Arithmetic IF (1957 three-way branch - revolutionary!)
// Example: IF (X-Y) 10, 20, 30
// If X-Y < 0 go to 10, if X-Y = 0 go to 20, if X-Y > 0 go to 30
// This was the ONLY conditional statement in 1957!
arithmetic_if_stmt
    : IF LPAREN expr RPAREN label COMMA label COMMA label
    ;

// DO statement (1957 - counted loops with labels)
// Example: DO 100 I = 1, 10, 2 (loop I from 1 to 10 step 2, end at label 100)
// The label was MANDATORY - no END DO in 1957!
do_stmt
    : DO label variable ASSIGN expr COMMA expr (COMMA expr)?
    ;


// CONTINUE statement (DO loop termination and general jump target)
continue_stmt : CONTINUE ;

// STOP statement (program termination with optional numeric code)
stop_stmt : STOP integer_expr? ;

// PAUSE statement (unique to 1957 - operator intervention!)
// Halted program and displayed message to computer operator
// Operator could then examine memory, modify settings, and continue
// Example: PAUSE 1234 (display code 1234 and wait for operator)
pause_stmt : PAUSE integer_expr? ;

// RETURN statement (NEW in FORTRAN II for separate compilation)
return_stmt : RETURN ;

// END statement (end of main program or subprogram)
end_stmt : END ;


// READ statement (input from punch cards or magnetic tape)
// Example: READ 100, A, B, C (read using format 100)
// Example: READ 5, 200, X, Y (read from unit 5 using format 200)
read_stmt
    : READ integer_expr COMMA input_list                    // READ unit, list
    | READ integer_expr COMMA label COMMA input_list       // READ unit, format, list
    ;

// PRINT statement (output to line printer - primary 1957 output)
// Example: PRINT 100, A, B, C (print using format 100)
print_stmt
    : PRINT integer_expr COMMA output_list   // PRINT format, list
    ;

// PUNCH statement (output to card punch - 1957 data storage!)
// This was how FORTRAN programs created data files in 1957
// Example: PUNCH 200, X, Y, Z (punch variables to cards using format 200)
punch_stmt
    : PUNCH integer_expr COMMA output_list   // PUNCH format, list
    ;


// FORMAT statement (I/O formatting - revolutionary feature!)
// Example: 100 FORMAT (I5, F10.2, E15.6, 5HHELLO)
// Allowed precise control over input/output layout
format_stmt
    : FORMAT LPAREN format_specification RPAREN
    ;

// Format specification items
format_specification
    : format_item (COMMA format_item)*
    ;

// Individual format items
format_item
    : INTEGER_LITERAL? format_descriptor  // e.g., I5, F10.2, E15.6
    | HOLLERITH                          // e.g., 5HHELLO (literal text)
    ;

// Format descriptors (I, E, F, G, A, H, X, etc.)
format_descriptor
    : IDENTIFIER  // Format codes like I, E, F, G, A, H, X
    ;


// DIMENSION statement (array declarations - arrays were revolutionary!)
// Example: DIMENSION A(100), B(10,20), C(5,5,5)
// Multi-dimensional arrays in a high-level language were unprecedented in 1957
dimension_stmt
    : DIMENSION array_declarator (COMMA array_declarator)*
    ;

// Array declarator with dimensions
array_declarator
    : IDENTIFIER LPAREN dimension_list RPAREN
    ;

// Dimension list (compile-time constants only in 1957)
dimension_list
    : integer_expr (COMMA integer_expr)*
    ;

// EQUIVALENCE statement (memory overlay - memory was precious!)
// Example: EQUIVALENCE (A, B(1)), (X, Y, Z)
// Allowed variables to share the same memory location
equivalence_stmt
    : EQUIVALENCE equivalence_set (COMMA equivalence_set)*
    ;

// Equivalence set (variables sharing memory)
equivalence_set
    : LPAREN variable COMMA variable (COMMA variable)* RPAREN
    ;

// FREQUENCY statement (1957 optimization hint - unique to original FORTRAN!)
// Example: FREQUENCY 10 (25, 3, 1)
// Told compiler statement 10 executed 25 times out of 29 (optimization hint)
// This feature was removed in later FORTRAN versions
frequency_stmt
    : FREQUENCY label LPAREN integer_expr (COMMA integer_expr)* RPAREN
    ;

// COMMON statement (global variable storage)
// Example: COMMON A, B, C
// Example: COMMON /BLOCK1/ X, Y, Z
// Shared variables between main program and subprograms
common_stmt
    : COMMON (SLASH IDENTIFIER SLASH)? variable_list
    ;


// ====================================================================
// EXPRESSION EVALUATION - Proper Precedence Hierarchy
// ====================================================================
// FORTRAN II implements proper operator precedence (highest to lowest):
//   1. ** (power) - RIGHT ASSOCIATIVE
//   2. unary +, - (unary operators)
//   3. *, / (multiplication, division) - left associative
//   4. binary +, - (addition, subtraction) - left associative
//
// Per IBM FORTRAN II manual (Form C28-6000-2, 1958), expression syntax
// matches 1957 FORTRAN mathematical expressions. The hierarchy ensures:
// - 2**3**4 parses as 2**(3**4) per right-associativity of **
// - -2**2 parses as -(2**2) = -4, not (-2)**2 = 4
// - 2+3*4 parses as 2+(3*4) = 14, not (2+3)*4 = 20
//
// Note: FORTRAN II does not include relational_expr at the top of expr.
// Relational expressions (.EQ., .NE., etc.) were primarily used with
// arithmetic IF in early FORTRAN; FORTRAN 66 introduces logical types
// where relational expressions appear in logical_expr context instead.
// ====================================================================

// Arithmetic expression - top of the precedence hierarchy for FORTRAN II
// Does not include relational operators (those are added in FORTRAN 66
// within the logical expression context)
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
// 2**3**4 = 2**(3**4) = 2**81 = 2417851639229258349412352
// NOT (2**3)**4 = 8**4 = 4096
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

// Literals (integers and reals)
// Note: LABEL tokens (1-5 digit integers starting with 1-9) are lexically
// identical to integer literals in expression context, so we accept both.
literal
    : INTEGER_LITERAL
    | LABEL              // LABEL tokens are valid integer literals in expressions
    | REAL_LITERAL
    ;

// Variables and array/function references
// In FORTRAN II, SIN(X) and A(I) are syntactically identical;
// semantic analysis distinguishes functions from arrays
variable
    : IDENTIFIER (LPAREN expr_list RPAREN)?
    ;


// Various lists used throughout the grammar
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

// ====================================================================
// IMPLEMENTATION STATUS
// ====================================================================
//
// CURRENT STATUS: Complete FORTRAN II procedural programming implementation
// COMPILATION: Compiles with ANTLR4, integrates with FORTRAN I base
// FUNCTIONALITY: Full subroutine and function support with CALL statements
//
// HISTORICAL ACCURACY: Based on IBM 704 FORTRAN II Operator's Manual (1958)
// EDUCATIONAL VALUE: Complete record of procedural programming revolution
// RESEARCH VALUE: Foundation for modern subroutine-based programming
// ====================================================================