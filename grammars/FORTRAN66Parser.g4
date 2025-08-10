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
fortran66_program
    : (main_program | subprogram | block_data_subprogram) EOF
    ;

// Main program (standardized from FORTRAN IV)
main_program
    : statement_list
    ;

// Subprograms (functions and subroutines from FORTRAN IV)
subprogram
    : subroutine_subprogram
    | function_subprogram
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
    | end_stmt          // End of program
    | return_stmt       // Return from subprogram
    | call_stmt         // Call subroutine
    ;

// Variable list for declarations
variable_list
    : variable (COMMA variable)*
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