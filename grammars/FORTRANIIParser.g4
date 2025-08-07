// FORTRAN II (1958) - Parser for Procedural Programming Revolution
// Adds user-written subroutines and functions to FORTRAN I (1957)
parser grammar FORTRANIIParser;

import FORTRANParser;  // Import FORTRAN I (1957) constructs

options {
    tokenVocab = FORTRANIILexer;
}

// ====================================================================
// FORTRAN 1957 HISTORICAL GRAMMAR DOCUMENTATION [COMPREHENSIVE STUB]
// ====================================================================
//
// This is a HISTORICAL STUB documenting the original 1957 FORTRAN grammar
// structure as implemented for IBM 704 computers.
//
// HISTORICAL CONTEXT (1957):
// The original FORTRAN compiler was a revolutionary achievement that:
// - Translated mathematical formulas into efficient machine code
// - Proved high-level languages could match hand-coded assembly efficiency
// - Required 18 person-years to develop (1954-1957)
// - Fit in 32K of IBM 704 memory (including runtime system!)
// - Produced optimized code that often exceeded human assembly programmers
//
// PROGRAM STRUCTURE (1957 - No modules, no procedures):
// - Main program: Simple sequence of statements with optional labels
// - No PROGRAM statement: Programs started with first executable statement
// - No separate compilation: Entire program compiled as single unit
// - Subprograms: FUNCTION definitions embedded within main program
// - Memory layout: Single address space, no stack, limited recursion
//
// CONTROL FLOW PHILOSOPHY (1957):
// - Statement labels: Primary control flow mechanism (1-99999)
// - GOTO statements: Unrestricted jumping to any labeled statement
// - Arithmetic IF: Three-way branching based on expression sign
// - DO loops: Counted iteration with mandatory labels for termination
// - No structured programming: GOTO-based spaghetti code was normal!
//
// DATA MODEL (1957):
// - Variables: Maximum 6 characters, no declarations required (implicit typing)
// - Arrays: Multi-dimensional with compile-time bounds only
// - Types: INTEGER and REAL only (no CHARACTER, LOGICAL, COMPLEX)
// - Memory: All variables global, COMMON for sharing, EQUIVALENCE for overlaying
// - Constants: Numeric literals and Hollerith strings (nHcharacters)
//
// I/O PHILOSOPHY (1957):
// - Devices: Punch cards (input), line printer (output), card punch (storage)
// - Formatting: FORMAT statements with descriptors (I, E, F, G, A, H, X)
// - No interactive I/O: Pure batch processing environment
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
// PROGRAM STRUCTURE [DOCUMENTED STUB]
// ====================================================================

// FORTRAN 1957 program (no explicit PROGRAM statement in 1957)
// Just a sequence of statements, possibly with labels
// The first executable statement begins program execution
fortran_program
    : statement_list EOF
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

// ====================================================================
// STATEMENT TYPES [DOCUMENTED STUB]
// ====================================================================

// All statement types available in 1957 FORTRAN
// This was a remarkably small language by today's standards!
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
    | end_stmt          // End of program
    | return_stmt       // Return from subprogram
    ;

// ====================================================================
// ASSIGNMENT AND EXPRESSIONS [DOCUMENTED STUB]
// ====================================================================

// Assignment statement (1957 revolutionary syntax!)
// This natural mathematical notation was groundbreaking in 1957
// Before FORTRAN, programmers wrote: STO A, ADD B, STO C
// FORTRAN allowed: C = A + B (revolutionary!)
assignment_stmt
    : variable ASSIGN expr
    ;

// ====================================================================
// CONTROL FLOW STATEMENTS [DOCUMENTED STUB]
// ====================================================================

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

// ====================================================================
// CONTROL STATEMENTS [DOCUMENTED STUB]
// ====================================================================

// CONTINUE statement (DO loop termination and general jump target)
continue_stmt : CONTINUE ;

// STOP statement (program termination with optional numeric code)
stop_stmt : STOP integer_expr? ;

// PAUSE statement (unique to 1957 - operator intervention!)
// Halted program and displayed message to computer operator
// Operator could then examine memory, modify settings, and continue
// Example: PAUSE 1234 (display code 1234 and wait for operator)
pause_stmt : PAUSE integer_expr? ;

// RETURN statement (return from function or subroutine)
return_stmt : RETURN ;

// END statement (end of main program or subprogram)
end_stmt : END ;

// ====================================================================
// INPUT/OUTPUT STATEMENTS [DOCUMENTED STUB]
// ====================================================================

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

// ====================================================================
// FORMAT STATEMENTS [DOCUMENTED STUB]
// ====================================================================

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

// ====================================================================
// DECLARATION STATEMENTS [DOCUMENTED STUB]
// ====================================================================

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
// Shared variables between main program and subprograms
common_stmt
    : COMMON variable_list
    ;

// ====================================================================
// EXPRESSIONS AND PRIMARIES [DOCUMENTED STUB]
// ====================================================================

// Expression evaluation (1957 mathematical focus)
// Full operator precedence with parentheses override
// ** (exponentiation) was included from day one!
expr
    : expr POWER expr                    # PowerExpr        // ** (right associative)
    | expr (MULTIPLY | DIVIDE) expr      # MultDivExpr      // *, /
    | expr (PLUS | MINUS) expr           # AddSubExpr       // +, -
    | PLUS expr                          # UnaryPlusExpr    // +expr
    | MINUS expr                         # UnaryMinusExpr   // -expr
    | primary                            # PrimaryExpr      // Literals, variables, etc.
    ;

// Primary expressions
primary
    : literal                            // Numeric constants
    | variable                           // Variables and array elements
    | function_reference                 // Built-in function calls
    | LPAREN expr RPAREN                // Parenthesized expressions
    ;

// Literals (1957 supported integers and reals only)
literal
    : INTEGER_LITERAL                    // e.g., 123, -456
    | REAL_LITERAL                       // e.g., 3.14, 2.5E-3
    ;

// Variables and array references
// 1957 variables: max 6 characters, implicit typing (I-N integer, else real)
variable
    : IDENTIFIER (LPAREN subscript_list RPAREN)?  // Variable or array element
    ;

// Array subscript list
subscript_list
    : expr (COMMA expr)*
    ;

// Function references (1957 - built-in mathematical functions only)
// User-defined functions used statement function syntax (not covered in stub)
function_reference
    : IDENTIFIER LPAREN expr_list RPAREN
    ;

// ====================================================================
// UTILITY RULES [DOCUMENTED STUB]  
// ====================================================================

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
// STUB IMPLEMENTATION STATUS
// ====================================================================
//
// CURRENT STATUS: Comprehensive historical documentation, minimal parsing
// COMPILATION: Compiles with ANTLR4, integrates with SharedCoreParser
// FUNCTIONALITY: Basic syntax recognition only, no semantic validation
//
// HISTORICAL ACCURACY: Based on IBM 704 FORTRAN Preliminary Operator's Manual
// EDUCATIONAL VALUE: Complete record of 1957 programming language features
// RESEARCH VALUE: Foundation for computer science history studies
//
// PHASE 1 (CURRENT): Historical documentation and grammar stub
// PHASE 2 (FUTURE): Fixed-form parsing with column-sensitive lexing
// PHASE 3 (FUTURE): Semantic validation and historical code compatibility
//
// This stub provides the complete grammatical structure for FORTRAN II
// extension and serves as an educational resource for understanding the
// revolutionary impact of the world's first high-level programming language.
// ====================================================================