/*
 * SharedCoreParser.g4
 * 
 * Parser grammar for shared FORTRAN language constructs
 * present across ALL standards from FORTRAN 1957 to LazyFortran 2023+
 * 
 * This foundational parser defines universal syntax patterns that
 * have remained consistent across the entire FORTRAN evolution.
 */

parser grammar SharedCoreParser;

options {
    tokenVocab = SharedCoreLexer;
}

// ============================================================================
// PROGRAM STRUCTURE: Top-level constructs
// ============================================================================

// Core program unit - can be extended by format-specific parsers
program_unit_core
    : statement_list EOF
    ;

statement_list
    : statement*
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
    | if_stmt_arithmetic
    | do_stmt_basic
    | read_stmt_basic
    | write_stmt_basic
    | call_stmt
    | CONTINUE
    | STOP
    | END
    ;

// Assignment statement (universal since 1957)
assignment_stmt
    : variable ASSIGN expr
    ;

// GOTO statement (universal since 1957)
goto_stmt
    : GOTO label
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
    : DO label variable ASSIGN expr COMMA expr (COMMA expr)?
    ;

// Basic I/O statements (simplified universal forms)
read_stmt_basic
    : READ input_list
    ;

write_stmt_basic
    : WRITE output_list
    ;

// CALL statement (universal since FORTRAN II, ~1958)
call_stmt
    : CALL IDENTIFIER (LPAREN expr_list RPAREN)?
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
    : MULTIPLY | DIVIDE
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