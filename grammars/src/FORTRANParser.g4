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
 *
 * Reference: IBM Reference Manual, FORTRAN Automatic Coding System for the
 *            IBM 704 Data Processing System (Form C28-6003, October 1958)
 *            - Chapter I: Introduction (program structure, coding format)
 *            - Chapter II: The FORTRAN Language (statements, expressions)
 *            - Chapter III: Input-Output
 *            - Appendix B: Table of FORTRAN Statements (32 statement types)
 *
 * Manual available at: Computer History Museum archive
 *   https://archive.computerhistory.org/resources/text/Fortran/102649787.05.01.acc.pdf
 */

parser grammar FORTRANParser;

options {
    tokenVocab = FORTRANLexer;
}

// ============================================================================
// PROGRAM STRUCTURE
// C28-6003 Chapter I.A: A FORTRAN source program consists of a sequence
// of statements, each on a separate card, ending with an END statement.
// ============================================================================

// Core program unit - can be extended by format-specific parsers
// C28-6003: No explicit PROGRAM statement in 1957; program = sequence of stmts
program_unit_core
    : (NEWLINE* statement)* NEWLINE* EOF
    ;

// ============================================================================
// STATEMENTS
// C28-6003 Appendix B: Table of FORTRAN Statements (32 types)
// Statements may have optional labels (columns 1-5 in card format)
// ============================================================================

// Statement with optional label
// C28-6003 Chapter I.B: Statement numbers in columns 1-5
statement
    : (label)? statement_body
    ;

// Statement body - all 1957 FORTRAN statement types
// C28-6003 Appendix B enumerates 32 statement types; this parser implements
// a subset focused on control flow, I/O, and declarations
statement_body
    : assignment_stmt               // Appendix B: arithmetic/subscripted
    | goto_stmt                     // Appendix B row 1: GO TO n
    | computed_goto_stmt            // Appendix B row 2: GO TO (n1,...), i
    | assign_stmt                   // Appendix B row 12: ASSIGN i TO n
    | assigned_goto_stmt            // Appendix B row 2: GO TO n, (n1,...)
    | if_stmt_arithmetic            // Appendix B row 3: IF (e) n1,n2,n3
    | if_stmt_two_way_positive      // Appendix B row 9: IF (e) n1, n2 (>= 0 branch n1)
    | if_stmt_two_way_zero          // Appendix B row 10: IF (e) n1, n2 (= 0 branch n1)
    | if_stmt_sense_light           // Appendix B row 8: IF (SENSE LIGHT i)
    | if_stmt_sense_switch          // Appendix B row 4: IF (SENSE SWITCH i)
    | if_stmt_accumulator_overflow  // Appendix B row 5: IF ACCUMULATOR OVERFLOW
    | if_stmt_quotient_overflow     // Appendix B row 6: IF QUOTIENT OVERFLOW
    | if_stmt_divide_check          // Appendix B row 7: IF DIVIDE CHECK
    | sense_light_stmt              // Appendix B row 11: SENSE LIGHT i
    | do_stmt_basic                 // Appendix B row 18: DO n i = m1,m2,m3
    | frequency_stmt                // Appendix B row 13: FREQUENCY n(i,j,...)
    | format_stmt                   // Appendix B row 16: FORMAT (specification)
    | statement_function_stmt       // Appendix B row 17: f(a,b,...) = expr
    | read_tape_drum_stmt            // Appendix B rows 21-24: READ tape/drum forms
    | read_stmt_basic               // Appendix B row 20, 24: READ n, list / READ n
    | write_tape_drum_stmt          // Appendix B rows 25-27: WRITE tape/drum forms
    | write_stmt_basic              // Simple WRITE output_list
    | print_stmt                    // Appendix B row 28: PRINT n, list
    | punch_stmt                    // Appendix B row 29: PUNCH n, list
    | end_file_stmt                 // C28-6003 Chapter III.F: END FILE i
    | rewind_stmt                   // C28-6003 Chapter III.F: REWIND i
    | backspace_stmt                // C28-6003 Chapter III.F: BACKSPACE i
    | pause_stmt                    // Appendix B row 31: PAUSE / PAUSE n
    | dimension_stmt                // Appendix B row 14: DIMENSION v,v,...
    | equivalence_stmt              // Appendix B row 15: EQUIVALENCE sets
    | CONTINUE                      // Appendix B row 19: CONTINUE
    | STOP                          // Appendix B row 30: STOP / STOP n
    | END                           // Appendix B row 32: END
    ;

// ============================================================================
// DECLARATION STATEMENTS
// C28-6003 Chapter II.D (Subscripted Variables) and Appendix B rows 14-17
// ============================================================================

// PAUSE statement - Appendix B row 31
// C28-6003: PAUSE or PAUSE n causes operator intervention
pause_stmt
    : PAUSE (INTEGER_LITERAL)?
    ;

// DIMENSION statement - Appendix B row 14
// C28-6003 Chapter II.D: DIMENSION v, v, v, ... where v is array declarator
// Example: DIMENSION A(100), B(10,20), C(5,5,5)
dimension_stmt
    : DIMENSION array_declarator (COMMA array_declarator)*
    ;

// Array declarator specifies array name and dimensions
// C28-6003 Chapter II.D: up to 3 dimensions, compile-time constant bounds
array_declarator
    : IDENTIFIER LPAREN dimension_list RPAREN
    ;

// Dimension list contains one or more dimension bounds
// C28-6003: In 1957, dimensions were unsigned integer constants
dimension_list
    : expr (COMMA expr)*
    ;

// EQUIVALENCE statement - Appendix B row 15
// C28-6003 Chapter II.F: EQUIVALENCE (a,b,c,...), (d,e,f,...), ...
// Example: EQUIVALENCE (A, B(1)), (X, Y, Z)
// Allows variables to share the same memory location
equivalence_stmt
    : EQUIVALENCE equivalence_set (COMMA equivalence_set)*
    ;

// Equivalence set is a parenthesized list of variables sharing memory
// C28-6003: At least two variables per set
equivalence_set
    : LPAREN variable (COMMA variable)+ RPAREN
    ;

// STATEMENT FUNCTION - Appendix B row 17
// C28-6003 Chapter II.E (Function Statements) and Appendix B row 17
// Statement functions are compile-time-substitutable functions defined as:
//   f(a1, a2, ..., an) = expr
// where:
// - f is the function name (typically 4-7 chars, often ending in F)
// - a1, a2, ..., an are formal parameters (identifiers)
// - expr is an arithmetic expression using the parameters
// Must appear before any executable statements in the program.
//
// Examples:
//   POLYF(X) = C0 + X*(C1 + X*C2)      ! Floating-point polynomial
//   MULTF(I,J) = I*J                   ! Integer multiplication
//   XMULTF(X,Y) = X*Y                  ! Fixed-point multiplication
//
// Note: STATEMENT FUNCTIONS are made OBSOLESCENT in Fortran 90
// (ISO/IEC 1539:1991 Annex B.2.2). Retained here for historical accuracy
// in the 1957 FORTRAN grammar.
statement_function_stmt
    : IDENTIFIER LPAREN identifier_list RPAREN EQUALS expr
    ;

// Identifier list for statement function arguments
identifier_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ============================================================================
// CONTROL FLOW STATEMENTS
// C28-6003 Chapter II.G (Control Statements) and Appendix B rows 1-12, 18-19
// ============================================================================

// Assignment statement - not in Appendix B but fundamental
// C28-6003 Chapter II.C: v = e where v is variable, e is expression
assignment_stmt
    : variable EQUALS expr
    ;

// Unconditional GO TO - Appendix B row 1
// C28-6003 Chapter II.G: GO TO n transfers to statement n
goto_stmt
    : GOTO label
    ;

// Computed GO TO - Appendix B row 2
// C28-6003 Chapter II.G: GO TO (n1, n2, ..., nm), i
// Transfers to n1 if i=1, n2 if i=2, etc.
computed_goto_stmt
    : GOTO LPAREN label_list RPAREN COMMA expr
    ;

// ASSIGN statement - Appendix B row 12
// C28-6003 Chapter II.G: ASSIGN i TO n
// Stores label i in integer variable n for later assigned GO TO
//
// NON-COMPLIANT: ASSIGN is a deleted feature per ISO/IEC 1539-1:2018
// Annex B.2. Retained for historical accuracy only.
// See docs/fortran_1957_audit.md for compliance details.
assign_stmt
    : ASSIGN label TO variable
    ;

// Assigned GO TO - Appendix B row 2 (variant)
// C28-6003 Chapter II.G: GO TO n, (l1, l2, ..., lm)
// Branches to label stored in variable n (set by ASSIGN)
//
// NON-COMPLIANT: Assigned GO TO is a deleted feature per ISO/IEC
// 1539-1:2018 Annex B.2. Retained for historical accuracy only.
// See docs/fortran_1957_audit.md for compliance details.
assigned_goto_stmt
    : GOTO variable COMMA LPAREN label_list RPAREN
    ;

// Label list for computed/assigned GO TO
label_list
    : label (COMMA label)*
    ;

// Arithmetic IF statement - Appendix B row 3
// C28-6003 Chapter II.G: IF (e) n1, n2, n3
// Branches to n1 if e<0, n2 if e=0, n3 if e>0
if_stmt_arithmetic
    : IF LPAREN expr RPAREN label COMMA label COMMA label
    ;

// ============================================================================
// TWO-LABEL IF STATEMENTS
// C28-6003 Appendix B rows 9-10: Two-branch IF statements
// ============================================================================

// IF (e) n1, n2 - Appendix B row 9
// C28-6003: Two-label arithmetic IF
// If e >= 0, branch to n1; if e < 0, branch to n2
if_stmt_two_way_positive
    : IF LPAREN expr RPAREN label COMMA label
    ;

// IF (e) n1, n2 - Appendix B row 10
// C28-6003: Two-label arithmetic IF (alternative form)
// If e = 0, branch to n1; if e != 0, branch to n2
if_stmt_two_way_zero
    : IF LPAREN expr RPAREN label COMMA label
    ;

// ============================================================================
// HARDWARE-SPECIFIC IF STATEMENTS (IBM 704)
// C28-6003 Appendix B rows 4-11: IF statements for hardware indicators
// These test IBM 704 console switches, sense lights, and overflow indicators
// ============================================================================

// IF (SENSE LIGHT i) n1, n2 - Appendix B row 8
// C28-6003: Tests sense light i; if on goes to n1, if off goes to n2
// The sense light is turned off after the test
if_stmt_sense_light
    : IF LPAREN SENSE LIGHT INTEGER_LITERAL RPAREN label COMMA label
    ;

// IF (SENSE SWITCH i) n1, n2 - Appendix B row 4
// C28-6003: Tests console sense switch i; if up goes to n1, if down goes to n2
if_stmt_sense_switch
    : IF LPAREN SENSE SWITCH INTEGER_LITERAL RPAREN label COMMA label
    ;

// IF ACCUMULATOR OVERFLOW n1, n2 - Appendix B row 5
// C28-6003: Tests and resets accumulator overflow indicator
// If on goes to n1, if off goes to n2
if_stmt_accumulator_overflow
    : IF ACCUMULATOR OVERFLOW label COMMA label
    ;

// IF QUOTIENT OVERFLOW n1, n2 - Appendix B row 6
// C28-6003: Tests and resets MQ (quotient) overflow indicator
// If on goes to n1, if off goes to n2
if_stmt_quotient_overflow
    : IF QUOTIENT OVERFLOW label COMMA label
    ;

// IF DIVIDE CHECK n1, n2 - Appendix B row 7
// C28-6003: Tests and resets divide-check indicator
// If on (divide by zero occurred) goes to n1, if off goes to n2
if_stmt_divide_check
    : IF DIVIDE CHECK label COMMA label
    ;

// SENSE LIGHT i - Appendix B row 11
// C28-6003: Sets sense light i on (can be tested by IF (SENSE LIGHT i))
// IBM 704 had four sense lights (1-4) on the console
sense_light_stmt
    : SENSE LIGHT INTEGER_LITERAL
    ;

// DO statement - Appendix B row 18
// C28-6003 Chapter II.G: DO n i = m1, m2 [, m3]
// Executes statements through n, incrementing i from m1 to m2 by m3
do_stmt_basic
    : DO label variable EQUALS expr COMMA expr (COMMA expr)?
    ;

// FREQUENCY statement - Appendix B row 13
// C28-6003 Chapter II.G: FREQUENCY n (i1, i2, ...)
// Optimization hint for branch prediction at arithmetic IF
frequency_stmt
    : FREQUENCY label LPAREN expr_list RPAREN
    ;

// ============================================================================
// I/O STATEMENTS
// C28-6003 Chapter III (Input-Output) and Appendix B rows 16, 20-29
// This grammar implements the full 1957 I/O statement family including
// tape/drum operations and file-control statements.
// ============================================================================

// READ statement - Appendix B rows 20, 24
// C28-6003 Chapter III.A: READ n, list - read using format n
// Row 20: READ n, list - formatted input with variable list
// Row 24: READ n - formatted input with FORMAT-implied list
// Simple form (READ list) also accepted for compatibility
read_stmt_basic
    : READ label COMMA input_list       // READ n, list (row 20, formatted)
    | READ label                        // READ n (row 24, format-only)
    | READ input_list                   // READ list (simple form)
    ;

// ============================================================================
// TAPE/DRUM READ STATEMENTS
// C28-6003 Chapter III.B-D and Appendix B rows 21-23
// ============================================================================
// Combined rule for READ TAPE/DRUM variants to handle backtracking correctly

// READ tape/drum statement - unified rule for all tape/drum READ forms
// Appendix B rows 21-23
// Uses io_device_keyword to match TAPE/DRUM/INPUT contextually
read_tape_drum_stmt
    : READ io_device_keyword io_device_keyword tape_unit
      COMMA label COMMA input_list                          // row 21: INPUT TAPE
    | READ io_device_keyword tape_unit
      COMMA drum_sector COMMA input_list                    // row 23: DRUM
    | READ io_device_keyword tape_unit COMMA input_list     // row 22: TAPE
    ;

// WRITE statement - simple form
// C28-6003 Chapter III: Basic WRITE output_list
write_stmt_basic
    : WRITE output_list
    ;

// ============================================================================
// TAPE/DRUM WRITE STATEMENTS
// C28-6003 Chapter III.B-D and Appendix B rows 25-27
// ============================================================================
// Combined rule for WRITE TAPE/DRUM variants to handle backtracking correctly

// WRITE tape/drum statement - unified rule for all tape/drum WRITE forms
// Appendix B rows 25-27
// Uses io_device_keyword to match TAPE/DRUM/OUTPUT contextually
write_tape_drum_stmt
    : WRITE io_device_keyword io_device_keyword tape_unit
      COMMA label COMMA output_list                          // row 25: OUTPUT TAPE
    | WRITE io_device_keyword drum_unit
      COMMA drum_sector COMMA output_list                    // row 27: DRUM
    | WRITE io_device_keyword tape_unit COMMA output_list    // row 26: TAPE
    ;

// PRINT statement - Appendix B row 28
// C28-6003 Chapter III.E: PRINT n, list - print to line printer using format n
// List may be omitted per manual row 28 variant
print_stmt
    : PRINT label (COMMA output_list)?
    ;

// PUNCH statement - Appendix B row 29
// C28-6003 Chapter III.E: PUNCH n, list - punch to cards using format n
// List may be omitted per manual row 29 variant
punch_stmt
    : PUNCH label (COMMA output_list)?
    ;

// ============================================================================
// FILE-CONTROL STATEMENTS
// C28-6003 Chapter III.F (File-control statements)
// These control tape positioning and end-of-file marking
// ============================================================================

// END FILE statement - C28-6003 Chapter III.F
// END FILE i - writes an end-of-file mark on tape unit i
// Note: FILE is matched as IDENTIFIER via io_device_keyword to avoid keyword conflicts
end_file_stmt
    : END io_device_keyword tape_unit
    ;

// REWIND statement - C28-6003 Chapter III.F
// REWIND i - rewinds tape unit i to the beginning
rewind_stmt
    : REWIND tape_unit
    ;

// BACKSPACE statement - C28-6003 Chapter III.F
// BACKSPACE i - repositions tape unit i back one logical record
backspace_stmt
    : BACKSPACE tape_unit
    ;

// ============================================================================
// I/O UNIT SPECIFIERS
// C28-6003 Chapter III: Tape and drum unit designators
// ============================================================================

// Tape unit number - typically a small integer (1-10 on IBM 704)
tape_unit
    : expr
    ;

// Drum unit number - typically a small integer
drum_unit
    : expr
    ;

// Drum sector address - integer expression for drum positioning
drum_sector
    : expr
    ;

// ============================================================================
// FORMAT STATEMENT
// C28-6003 Chapter III (Input-Output) and Appendix B row 16
// ============================================================================
// The FORMAT statement defines the layout for formatted I/O operations.
// Form: FORMAT (format-specification)
// Edit descriptors include:
//   Iw       - Integer (w = field width)
//   Fw.d     - Real fixed-point (w = width, d = decimal places)
//   Ew.d     - Real exponential (w = width, d = decimal places)
//   nX       - Skip n positions
//   nHtext   - Hollerith literal (n characters of text)
//
// FORMAT statements are non-executable and must have a statement label
// so they can be referenced by READ, PRINT, PUNCH statements.
// ============================================================================

format_stmt
    : FORMAT LPAREN format_specification RPAREN
    ;

// Format specification items - C28-6003 Chapter III
format_specification
    : format_item (COMMA format_item)*
    ;

// Individual format items - C28-6003 Chapter III
// Note: This grammar accepts format descriptors in several forms due to
// lexer tokenization behaviors:
// - IDENTIFIER (e.g., I5, F10) for simple descriptors
// - REAL_LITERAL (e.g., 2E15 as scientific notation) for E descriptors
//   with repeat counts where lexer greedily matches nEm as real literal
// - HOLLERITH (e.g., 5HHELLO) for literal text
format_item
    : format_repeat_count? format_descriptor  // e.g., 3I5, F10.2
    | format_e_descriptor                     // e.g., 2E15.6 (lexed as REAL_LITERAL)
    | HOLLERITH                               // e.g., 5HHELLO (literal text)
    ;

// Optional repeat count before format descriptor
format_repeat_count
    : INTEGER_LITERAL
    ;

// Format descriptor with optional decimal specification
// C28-6003: I, F, E descriptors with field width and decimal places
// Note: The lexer tokenizes F10.2 as IDENTIFIER (F10) followed by
// REAL_LITERAL (.2). This rule matches both forms.
format_descriptor
    : IDENTIFIER format_decimal_part?
    ;

// E format descriptor special handling
// C28-6003: Ew.d format for exponential notation
// When a repeat count precedes an E descriptor (e.g., 2E15.6), the lexer
// tokenizes "2E15" as a REAL_LITERAL (scientific notation) because it
// matches the pattern DIGIT+ EXPONENT. The ".6" is then tokenized as
// a separate REAL_LITERAL (.DIGIT+). This rule accepts both tokens
// to correctly parse E descriptors with repeat counts.
format_e_descriptor
    : REAL_LITERAL format_decimal_part?
    ;

// Decimal part of format descriptor (e.g., .d in Fw.d)
format_decimal_part
    : REAL_LITERAL
    ;


// ============================================================================
// EXPRESSIONS
// C28-6003 Chapter II.C (Expressions) - arithmetic expression syntax
// ============================================================================
// FORTRAN operator precedence (highest to lowest):
// 1. ** (power) - right associative
// 2. unary +, - (unary operators)
// 3. *, / (multiplication, division) - left associative
// 4. binary +, - (addition, subtraction) - left associative
// 5. relational operators (.EQ., .NE., etc.) - left associative

// Top-level expression
// C28-6003 Chapter II.C: Expressions combine constants, variables, operators
expr
    : relational_expr
    ;

// Relational expressions - C28-6003 Chapter II.C
// Used in IF statement conditions
relational_expr
    : relational_expr relational_op additive_expr    # RelationalExpression
    | additive_expr                                  # RelationalPrimary
    ;

// Relational operators - C28-6003 Chapter II.C
relational_op
    : EQ | NE | LT | LE | GT | GE
    ;

// Additive expressions - C28-6003 Chapter II.C
// Addition and subtraction, left associative
additive_expr
    : additive_expr additive_op multiplicative_expr  # AdditiveExpression
    | multiplicative_expr                            # AdditivePrimary
    ;

additive_op
    : PLUS | MINUS
    ;

// Multiplicative expressions - C28-6003 Chapter II.C
// Multiplication and division, left associative
multiplicative_expr
    : multiplicative_expr multiplicative_op unary_expr  # MultiplicativeExpression
    | unary_expr                                        # MultiplicativePrimary
    ;

multiplicative_op
    : MULTIPLY | SLASH
    ;

// Unary expressions - C28-6003 Chapter II.C
// Unary plus and minus
unary_expr
    : unary_op unary_expr           # UnaryExpression
    | power_expr                    # UnaryPrimary
    ;

unary_op
    : PLUS | MINUS
    ;

// Power expressions - C28-6003 Chapter II.C
// Exponentiation (**), right associative
power_expr
    : primary POWER power_expr      # PowerExpression     // Right associative
    | primary                       # PowerPrimary
    ;

// Primary expressions - C28-6003 Chapter II.C
// Literals, variables, and parenthesized expressions
primary
    : literal
    | variable
    | LPAREN expr RPAREN
    ;

// ============================================================================
// LITERALS AND VARIABLES
// C28-6003 Chapter II.A (Constants) and Chapter II.B (Variables)
// ============================================================================

// Numeric literals - C28-6003 Chapter II.A
// Fixed-point (integer) and floating-point (real) constants
literal
    : INTEGER_LITERAL               // C28-6003 Chapter II.A.1
    | REAL_LITERAL                  // C28-6003 Chapter II.A.2
    ;

// Variables - C28-6003 Chapter II.B
// Simple variable or subscripted variable (array element)
variable
    : IDENTIFIER (LPAREN expr_list RPAREN)?
    ;

// ============================================================================
// UTILITY RULES
// Helper patterns for labels, lists, and I/O
// ============================================================================

// Statement label - C28-6003 Chapter I.B
// 1-5 digit unsigned integer in columns 1-5
label
    : INTEGER_LITERAL
    ;

// Expression list for subscripts and function arguments
// C28-6003 Chapter II.B and II.E
expr_list
    : (expr (COMMA expr)*)?
    ;

// Input list for READ - C28-6003 Chapter III
// List of variables to receive input values
input_list
    : variable (COMMA variable)*
    ;

// Output list for WRITE - C28-6003 Chapter III
// List of expressions to output
output_list
    : expr (COMMA expr)*
    ;

// ============================================================================
// CONTEXTUAL KEYWORDS FOR TAPE/DRUM I/O
// C28-6003 Chapter III - IBM 704 specific I/O keywords
// ============================================================================
// These keywords are matched as IDENTIFIERs to avoid conflicts with later
// Fortran standards where INPUT, OUTPUT, TAPE, DRUM, and FILE can be used
// as regular identifiers. The grammar uses structural disambiguation (number
// of tokens and commas) rather than keyword-based predicates.

// I/O device keyword - matches INPUT, OUTPUT, TAPE, DRUM, or FILE as identifiers
// The grammar relies on alternative ordering and structural differences to
// disambiguate between tape and drum statements:
// - READ/WRITE DRUM uses 5 commas (unit, sector, list)
// - READ/WRITE TAPE uses 3 commas (unit, list)
// - READ/WRITE INPUT/OUTPUT TAPE uses 2 keywords + 5 tokens
io_device_keyword
    : IDENTIFIER
    ;
