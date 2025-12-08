/*
 * FORTRANParser.g4
 *
 * FORTRAN I (1957) - The Original IBM 704 FORTRAN Parser
 * The world's first high-level programming language.
 *
 * This parser defines a historically grounded core subset of the original
 * 1957 IBM 704 FORTRAN, rather than a full reconstruction of the compiler.
 * It focuses on arithmetic expressions, labels, DO loops, arithmetic IF,
 * computed GOTO, FREQUENCY and basic I/O, and serves as the foundation for
 * all subsequent FORTRAN/Fortran standards in this repository.
 */

parser grammar FORTRANParser;

options {
    tokenVocab = FORTRANLexer;
}

// ============================================================================
// PROGRAM STRUCTURE: Top-level constructs
// ============================================================================

// Core program unit - can be extended by format-specific parsers
program_unit_core
    : (NEWLINE* statement)* NEWLINE* EOF
    ;

// ============================================================================
// STATEMENTS: Universal statement types (1957-2023)
// ============================================================================

statement
    : (label)? statement_body
    ;

statement_body
    : assignment_stmt
    | goto_stmt
    | computed_goto_stmt
    | if_stmt_arithmetic
    | do_stmt_basic
    | frequency_stmt
    | read_stmt_basic
    | write_stmt_basic
    | pause_stmt
    | dimension_stmt
    | equivalence_stmt
    | CONTINUE
    | STOP
    | END
    ;

// PAUSE statement (1957 operator intervention)
// PAUSE or PAUSE n (where n is an optional integer)
// Per IBM 704 FORTRAN manual Appendix B
pause_stmt
    : PAUSE (INTEGER_LITERAL)?
    ;

// DIMENSION statement (array declarations)
// Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B
// Syntax: DIMENSION v, v, v, ... where v is an array declarator
// Example: DIMENSION A(100), B(10,20), C(5,5,5)
dimension_stmt
    : DIMENSION array_declarator (COMMA array_declarator)*
    ;

// Array declarator specifies array name and dimensions
// In 1957 FORTRAN, dimensions were compile-time constants
array_declarator
    : IDENTIFIER LPAREN dimension_list RPAREN
    ;

// Dimension list contains one or more dimension bounds
dimension_list
    : expr (COMMA expr)*
    ;

// EQUIVALENCE statement (memory overlay)
// Per IBM 704 FORTRAN manual (Form C28-6003, Oct 1958) Appendix B
// Syntax: EQUIVALENCE (a,b,c,...), (d,e,f,...), ...
// Example: EQUIVALENCE (A, B(1)), (X, Y, Z)
// Allows variables to share the same memory location
equivalence_stmt
    : EQUIVALENCE equivalence_set (COMMA equivalence_set)*
    ;

// Equivalence set is a parenthesized list of variables sharing memory
equivalence_set
    : LPAREN variable (COMMA variable)+ RPAREN
    ;

// Assignment statement (universal since 1957)
assignment_stmt
    : variable EQUALS expr
    ;

// GOTO statement (universal since 1957)
goto_stmt
    : GOTO label
    ;

// Computed GOTO (1957 multi-way branch)
// GO TO (label1, label2, ...), expression
computed_goto_stmt
    : GOTO LPAREN label_list RPAREN COMMA expr
    ;

label_list
    : label (COMMA label)*
    ;

// Arithmetic IF statement (1957 original form)
// IF (expression) label1, label2, label3
// Goes to label1 if expr < 0, label2 if expr = 0, label3 if expr > 0
if_stmt_arithmetic
    : IF LPAREN expr RPAREN label COMMA label COMMA label
    ;

// Basic DO loop (1957 form)
// DO label variable = expr, expr [, expr]
do_stmt_basic
    : DO label variable EQUALS expr COMMA expr (COMMA expr)?
    ;

// FREQUENCY statement (1957 optimization hint)
// FREQUENCY label (n1, n2, ...)
frequency_stmt
    : FREQUENCY label LPAREN expr_list RPAREN
    ;

// Basic I/O statements (simplified universal forms)
read_stmt_basic
    : READ input_list
    ;

write_stmt_basic
    : WRITE output_list
    ;


// ============================================================================
// EXPRESSIONS: Proper precedence hierarchy (FORTRAN operator precedence)
// ============================================================================
// FORTRAN operator precedence (highest to lowest):
// 1. ** (power) - right associative
// 2. unary +, - (unary operators)  
// 3. *, / (multiplication, division) - left associative
// 4. binary +, - (addition, subtraction) - left associative
// 5. relational operators (.EQ., .NE., etc.) - left associative

expr
    : relational_expr
    ;

relational_expr
    : relational_expr relational_op additive_expr    # RelationalExpression
    | additive_expr                                  # RelationalPrimary
    ;

relational_op
    : EQ | NE | LT | LE | GT | GE
    ;

additive_expr
    : additive_expr additive_op multiplicative_expr  # AdditiveExpression
    | multiplicative_expr                            # AdditivePrimary
    ;

additive_op
    : PLUS | MINUS
    ;

multiplicative_expr
    : multiplicative_expr multiplicative_op unary_expr  # MultiplicativeExpression
    | unary_expr                                        # MultiplicativePrimary
    ;

multiplicative_op
    : MULTIPLY | SLASH
    ;

unary_expr
    : unary_op unary_expr           # UnaryExpression
    | power_expr                    # UnaryPrimary
    ;

unary_op
    : PLUS | MINUS
    ;

power_expr
    : primary POWER power_expr      # PowerExpression     // Right associative
    | primary                       # PowerPrimary
    ;

primary
    : literal
    | variable
    | LPAREN expr RPAREN
    ;

// ============================================================================
// LITERALS AND IDENTIFIERS: Basic data elements
// ============================================================================

literal
    : INTEGER_LITERAL
    | REAL_LITERAL
    ;

variable
    : IDENTIFIER (LPAREN expr_list RPAREN)?  // Simple variable or array element
    ;

// ============================================================================
// UTILITY RULES: Helper patterns
// ============================================================================

label
    : INTEGER_LITERAL
    ;

expr_list
    : (expr (COMMA expr)*)?
    ;

input_list
    : variable (COMMA variable)*
    ;

output_list
    : expr (COMMA expr)*
    ;
