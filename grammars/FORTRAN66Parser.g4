// FORTRAN 66 (1966) - First Official FORTRAN Standard
// Standardized FORTRAN IV with machine-independent features
parser grammar FORTRAN66Parser;

import FORTRANIVParser;  // Import FORTRAN IV (1962) constructs

options {
    tokenVocab = FORTRAN66Lexer;
}

// ====================================================================
// FORTRAN 66 (1966) NEW PARSER RULES
// ====================================================================
//
// FORTRAN 66 (1966) HISTORICAL SIGNIFICANCE:
// - First programming language standard (ANSI X3.9-1966)
// - Based on FORTRAN IV but removed machine dependencies  
// - Established foundation for portable FORTRAN programming
// - Defined main program structure and BLOCK DATA unit
//
// Historical Context (1966):
// - American Standards Association committee work (1962-1966)
// - Addressed compatibility issues between vendors
// - Created two standards: FORTRAN and Basic FORTRAN
// - Became first high-level language (HLL) standard worldwide
//
// ====================================================================

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