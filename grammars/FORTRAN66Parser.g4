// FORTRAN 66 (1966) - First Official FORTRAN Standard with FORTRAN IV Features
// Merges FORTRAN IV (1962) data types with FORTRAN 66 standardization
parser grammar FORTRAN66Parser;

import FORTRANIIParser;  // Import FORTRAN II (1958) constructs

options {
    tokenVocab = FORTRAN66Lexer;
}

// ====================================================================
// FORTRAN 66 (1966) WITH MERGED FORTRAN IV FEATURES
// ====================================================================
//
// FORTRAN 66 (1966) merges FORTRAN IV (1962) innovations with standardization:
// - LOGICAL data type with .TRUE./.FALSE. literals (from FORTRAN IV)
// - DOUBLE PRECISION and COMPLEX data types (from FORTRAN IV) 
// - Logical expressions with .AND., .OR., .NOT. (from FORTRAN IV)
// - Logical IF statement (from FORTRAN IV)
// - Standardized program unit structure and BLOCK DATA
//
// Historical Context (1962-1966):
// - FORTRAN IV (1962) added data types and logical operations
// - FORTRAN 66 (1966) standardized and made machine-independent
// - Combined innovations create foundation for portable FORTRAN
//
// ====================================================================

// ====================================================================
// FORTRAN IV (1962) TYPE SYSTEM - merged into FORTRAN 66
// ====================================================================

// FORTRAN 66 type specification with FORTRAN IV data types
type_spec
    : INTEGER
    | REAL
    | LOGICAL                    // NEW in FORTRAN IV (1962)
    | DOUBLE PRECISION           // NEW in FORTRAN IV (1962)
    | COMPLEX                    // NEW in FORTRAN IV (1962)
    ;

// ====================================================================
// FORTRAN IV (1962) LOGICAL EXPRESSIONS - merged into FORTRAN 66
// ====================================================================

// Logical expressions (NEW in FORTRAN IV, 1962)
logical_expr
    : logical_term (DOT_OR logical_term)*
    ;

logical_term
    : logical_factor (DOT_AND logical_factor)*
    ;

logical_factor
    : DOT_NOT logical_primary
    | logical_primary
    ;

logical_primary
    : logical_literal
    | relational_expr
    | logical_variable
    | LPAREN logical_expr RPAREN
    ;

// Logical literals (NEW in FORTRAN IV, 1962)
logical_literal
    : DOT_TRUE
    | DOT_FALSE
    ;

// Logical variable reference
logical_variable
    : IDENTIFIER
    | IDENTIFIER LPAREN expr_list RPAREN  // Array element
    ;

// Relational expressions (enhanced in FORTRAN IV, 1962)
relational_expr
    : expr relational_op expr
    ;

relational_op
    : DOT_EQ
    | DOT_NE  
    | DOT_LT
    | DOT_LE
    | DOT_GT
    | DOT_GE
    ;

// Logical IF statement (NEW in FORTRAN IV, 1962)
logical_if_stmt
    : IF LPAREN logical_expr RPAREN statement_body
    ;

// ====================================================================
// FORTRAN 66 (1966) - STANDARDIZED PROGRAM STRUCTURE
// ====================================================================

// FORTRAN 66 program with standardized program units
// Per X3.9-1966, a FORTRAN program consists of one or more program units
// (main program, external functions, external subroutines, block data)
// compiled separately or together in sequence.
fortran66_program
    : NEWLINE* program_unit+ NEWLINE* EOF
    ;

// A program unit is a main program, subprogram, or block data unit
// Main program must be distinguished from subprograms; a main program
// ends with END and does not start with SUBROUTINE or FUNCTION.
program_unit
    : subprogram                // Check subprogram first (has leading keyword)
    | block_data_subprogram     // BLOCK DATA has leading keyword
    | main_program              // Main program: statements ending with END
    ;

// Main program (standardized from FORTRAN IV)
// A main program is a sequence of statements ending with END.
// It does not have a leading SUBROUTINE, FUNCTION, or BLOCK DATA keyword.
main_program
    : statement+ end_stmt NEWLINE?
    ;

// Subprograms (functions and subroutines from FORTRAN IV)
subprogram
    : subroutine_subprogram
    | function_subprogram
    ;

// Override subroutine_subprogram to use explicit END handling
// for proper multiple program unit support.
// Leading NEWLINE* allows blank lines between program units.
subroutine_subprogram
    : NEWLINE* SUBROUTINE IDENTIFIER parameter_list? NEWLINE
      statement*
      end_stmt NEWLINE?
    ;

// Override function_subprogram to use explicit END handling
// for proper multiple program unit support.
// Leading NEWLINE* allows blank lines between program units.
function_subprogram
    : NEWLINE* type_spec? FUNCTION IDENTIFIER parameter_list NEWLINE
      statement*
      end_stmt NEWLINE?
    ;

// BLOCK DATA subprogram (NEW standardized unit in FORTRAN 66)
// Used to initialize variables in named COMMON blocks
block_data_subprogram
    : BLOCKDATA block_data_name? NEWLINE
      data_initialization_part?
      END
    ;

// Block data name (optional)
block_data_name
    : IDENTIFIER
    ;

// Data initialization part (assignments in BLOCK DATA)
data_initialization_part
    : data_initialization_statement+
    ;

data_initialization_statement
    : label? data_initialization_body NEWLINE?
    | NEWLINE  // Blank lines allowed
    ;

data_initialization_body
    : common_stmt           // COMMON variable declarations
    | dimension_stmt       // Array dimension declarations  
    | equivalence_stmt     // Variable equivalences
    | type_declaration    // Type declarations
    ;


// Type declarations (standardized from FORTRAN IV)
type_declaration
    : type_spec variable_list
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

// Statement label (FORTRAN 66: 1-99999, punched in columns 1-5)
label
    : LABEL
    ;

// All statement types standardized in FORTRAN 66
// NOTE: end_stmt is NOT included here - it is handled separately as a
// program unit terminator to enable parsing multiple program units.
statement_body
    : assignment_stmt      // Variable = Expression
    | goto_stmt           // Unconditional jump to labeled statement
    | computed_goto_stmt  // Multi-way branch based on integer expression
    | arithmetic_if_stmt  // Three-way branch based on expression sign
    | logical_if_stmt     // Two-way branch based on logical expression
    | do_stmt            // Counted loop with mandatory label
    | continue_stmt      // Loop termination and jump target
    | stop_stmt          // Program termination
    | pause_stmt         // Operator intervention
    | read_stmt          // Input from cards/tape
    | print_stmt         // Output to line printer
    | punch_stmt         // Output to card punch
    | format_stmt        // I/O formatting specification
    | dimension_stmt     // Array dimension declarations
    | equivalence_stmt   // Variable memory overlay
    | common_stmt        // Global variable declarations
    | type_declaration  // Variable type declarations
    | external_stmt     // External procedure declaration (X3.9-1966 Section 7.2)
    | intrinsic_stmt    // Intrinsic function specification (X3.9-1966 Section 7.2)
    | rewind_stmt       // Sequential file positioning (X3.9-1966 Section 7.1.3.3)
    | backspace_stmt    // Sequential file positioning (X3.9-1966 Section 7.1.3.3)
    | endfile_stmt      // Sequential file positioning (X3.9-1966 Section 7.1.3.3)
    | return_stmt       // Return from subprogram
    | call_stmt         // Call subroutine
    ;

// ====================================================================
// FORTRAN 66 (1966) - AUXILIARY I/O STATEMENTS
// ====================================================================
// Per ANSI X3.9-1966 Section 7.1.3.3, auxiliary I/O statements control
// sequential file positioning. The syntax is: statement-keyword u
// where u is an unsigned integer expression identifying the I/O unit.

// REWIND statement - position file to beginning (X3.9-1966 Section 7.1.3.3)
// Example: REWIND 5
rewind_stmt
    : REWIND integer_expr
    ;

// BACKSPACE statement - position file back one record (X3.9-1966 Section 7.1.3.3)
// Example: BACKSPACE 5
backspace_stmt
    : BACKSPACE integer_expr
    ;

// ENDFILE statement - write end-of-file mark (X3.9-1966 Section 7.1.3.3)
// Example: ENDFILE 5
endfile_stmt
    : ENDFILE integer_expr
    ;

// ====================================================================
// FORTRAN 66 (1966) - EXTERNAL AND INTRINSIC STATEMENTS
// ====================================================================
// Per ANSI X3.9-1966 Section 7.2, these are non-executable declaration
// statements that classify procedure names:
// - EXTERNAL identifies user-defined external procedures
// - INTRINSIC identifies standard library intrinsic functions

// EXTERNAL statement (X3.9-1966 Section 7.2)
// Declares one or more external procedure names.
// Example: EXTERNAL F, G, MYFUNC
external_stmt
    : EXTERNAL identifier_list
    ;

// INTRINSIC statement (X3.9-1966 Section 7.2)
// Declares one or more intrinsic function names to use standard meanings.
// Example: INTRINSIC SIN, COS, SQRT, ABS
intrinsic_stmt
    : INTRINSIC identifier_list
    ;

// Identifier list for EXTERNAL/INTRINSIC statements
identifier_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// Variable list for declarations
variable_list
    : variable (COMMA variable)*
    ;

// ====================================================================
// FORTRAN 66 (1966) - DO STATEMENT OVERRIDE
// ====================================================================
// Override do_stmt from FORTRANIIParser to use EQUALS (=) instead of
// ASSIGN keyword. Per ANSI X3.9-1966 Section 7.1.2.8, the DO statement
// syntax is: DO s i = e1, e2 [, e3]
// where = is the equals sign, not the ASSIGN keyword.
do_stmt
    : DO label variable EQUALS expr COMMA expr (COMMA expr)?
    ;

// ====================================================================
// FORTRAN 66 (1966) - LITERAL OVERRIDE
// ====================================================================
// Override literal to accept LABEL tokens as integers. Due to lexer
// token precedence, LABEL is defined before INTEGER_LITERAL in
// FORTRANIILexer and matches first for 1-5 digit numbers like 100.
// This allows expressions like PRINT 100 to parse correctly.
literal
    : INTEGER_LITERAL
    | LABEL                     // Accept LABEL as integer (token precedence)
    | REAL_LITERAL
    | logical_literal           // From FORTRAN IV (1962)
    ;

// ====================================================================
// FORTRAN 66 (1966) - FORMAT DESCRIPTOR OVERRIDE
// ====================================================================
// Override format handling to properly parse FORTRAN 66 format specs
// like I5, F10.2, E15.6, etc. The lexer tokenizes F10.2 as F10 (IDENTIFIER)
// followed by .2 (REAL_LITERAL), so we need to handle this combination.
// Per ANSI X3.9-1966 Section 7.2.3, format descriptors include:
// - Iw (integer), Fw.d (float), Ew.d (exponential), Dw.d (double)
// - Gw.d (general), Aw (character), Lw (logical)
// - nX (skip), nP (scale factor), nHc...c (Hollerith)

format_item
    : format_repeat_count? format_descriptor_full
    | HOLLERITH
    ;

format_repeat_count
    : INTEGER_LITERAL
    | LABEL
    ;

// Full format descriptor including optional decimal and exponent parts
// Handles cases like F10, F10.2, E15.6E2, etc.
format_descriptor_full
    : IDENTIFIER format_decimal_part?
    ;

// Decimal part of format descriptor (.d or .dEe)
// The lexer produces .2 as REAL_LITERAL for .2
format_decimal_part
    : REAL_LITERAL              // Handles .2, .6E2, etc.
    ;

// ====================================================================
// FORTRAN 66 (1966) - HISTORICAL SIGNIFICANCE
// ====================================================================
//
// FORTRAN 66 (1966) was revolutionary because:
//
// 1. **First Language Standard**: First programming language standard worldwide
//    - Established ANSI X3.9-1966 as the foundation
//    - Created template for future language standardization
//
// 2. **Machine Independence**: Removed vendor-specific dependencies
//    - Code could run on different computer architectures
//    - Eliminated IBM-specific features from FORTRAN IV
//
// 3. **Program Structure**: Standardized program organization
//    - MAIN PROGRAM, SUBROUTINE, FUNCTION program units
//    - BLOCK DATA for common block initialization
//
// 4. **Scientific Computing Foundation**: Established reliable base
//    - Universities could teach standard FORTRAN
//    - Research code became portable between institutions
//
// 5. **Industry Adoption**: Vendors aligned with standard
//    - Reduced fragmentation in FORTRAN implementations
//    - Enabled software industry growth
//
// This parser captures FORTRAN 66's standardization achievements while
// maintaining full compatibility with FORTRAN IV's data type innovations.
// It serves as the foundation for FORTRAN 77's structured programming.
//
// ====================================================================