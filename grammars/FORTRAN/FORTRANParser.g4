// FORTRAN Parser - Original IBM 704 Language (1957)
parser grammar FORTRANParser;

import SharedCoreParser;  // Import universal constructs

options {
    tokenVocab = FORTRANLexer;
}

// ====================================================================
// FORTRAN 1957 Grammar Structure
// Based on IBM 704 FORTRAN Programmer's Reference Manual (1957)
// ====================================================================

// Program structure (1957 - no explicit PROGRAM statement)
fortran_program
    : statement_list EOF
    ;

statement_list
    : statement*
    ;

// Statement with optional label
statement
    : label? statement_body NEWLINE?
    | NEWLINE  // Empty line
    ;

label
    : LABEL
    ;

// 1957 Statement types
statement_body
    : assignment_stmt
    | goto_stmt
    | computed_goto_stmt
    | arithmetic_if_stmt
    | do_stmt
    | continue_stmt
    | stop_stmt
    | pause_stmt
    | read_stmt
    | print_stmt
    | punch_stmt
    | format_stmt
    | dimension_stmt
    | equivalence_stmt
    | frequency_stmt
    | common_stmt
    | end_stmt
    | return_stmt
    ;

// Assignment statement (1957)
assignment_stmt
    : variable ASSIGN expr
    ;

// GOTO statements (1957)
goto_stmt
    : GOTO label
    ;

// Computed GOTO (1957)
computed_goto_stmt
    : GOTO LPAREN label_list RPAREN COMMA expr
    ;

// Arithmetic IF (1957 - three-way branch)
// IF (expr) negative_label, zero_label, positive_label
arithmetic_if_stmt
    : IF LPAREN expr RPAREN label COMMA label COMMA label
    ;

// DO loop (1957 - always with label)
do_stmt
    : DO label variable ASSIGN expr COMMA expr (COMMA expr)?
    ;

// Control statements
continue_stmt : CONTINUE ;
stop_stmt : STOP integer_expr? ;  // Optional stop code
pause_stmt : PAUSE integer_expr? ; // Optional pause code (for operator intervention!)
return_stmt : RETURN ;
end_stmt : END ;

// I/O statements (1957)
read_stmt
    : READ integer_expr COMMA input_list     // READ tape_unit, list
    | READ integer_expr COMMA label COMMA input_list  // READ with format
    ;

print_stmt
    : PRINT integer_expr COMMA output_list   // PRINT format_label, list
    ;

punch_stmt
    : PUNCH integer_expr COMMA output_list   // PUNCH format_label, list
    ;

// FORMAT statement (1957)
format_stmt
    : FORMAT LPAREN format_specification RPAREN
    ;

format_specification
    : format_item (COMMA format_item)*
    ;

format_item
    : INTEGER_LITERAL? format_descriptor
    | HOLLERITH  // Hollerith constants for literal text
    ;

format_descriptor
    : IDENTIFIER  // Format descriptors like I, E, F, G, A, H, X
    ;

// Declaration statements (1957)
dimension_stmt
    : DIMENSION array_declarator (COMMA array_declarator)*
    ;

array_declarator
    : IDENTIFIER LPAREN dimension_list RPAREN
    ;

dimension_list
    : integer_expr (COMMA integer_expr)*
    ;

equivalence_stmt
    : EQUIVALENCE equivalence_set (COMMA equivalence_set)*
    ;

equivalence_set
    : LPAREN variable COMMA variable (COMMA variable)* RPAREN
    ;

// FREQUENCY statement (1957 optimization hint!)
frequency_stmt
    : FREQUENCY label LPAREN integer_expr (COMMA integer_expr)* RPAREN
    ;

common_stmt
    : COMMON variable_list
    ;

// Expressions (1957)
expr
    : expr POWER expr           # PowerExpr      // Right associative
    | expr (MULTIPLY | DIVIDE) expr  # MultDivExpr
    | expr (PLUS | MINUS) expr       # AddSubExpr
    | PLUS expr                       # UnaryPlusExpr
    | MINUS expr                      # UnaryMinusExpr
    | primary                         # PrimaryExpr
    ;

primary
    : literal
    | variable
    | function_reference  // Built-in functions only in 1957
    | LPAREN expr RPAREN
    ;

literal
    : INTEGER_LITERAL
    | REAL_LITERAL
    ;

variable
    : IDENTIFIER (LPAREN subscript_list RPAREN)?  // Array subscripting
    ;

subscript_list
    : expr (COMMA expr)*
    ;

// Function references (1957 - built-in functions only)
// User-defined functions used statement function syntax
function_reference
    : IDENTIFIER LPAREN expr_list RPAREN
    ;

// Lists
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
    : expr  // Must evaluate to integer
    ;