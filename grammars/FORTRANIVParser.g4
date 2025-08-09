// FORTRAN IV (1962) - Parser for Data Type Revolution
// Adds LOGICAL, DOUBLE PRECISION, and COMPLEX data types
parser grammar FORTRANIVParser;

import FORTRANIIParser;  // Import FORTRAN II (1958) constructs

options {
    tokenVocab = FORTRANIVLexer;
}

// ====================================================================
// FORTRAN IV (1962) NEW PARSER RULES
// ====================================================================
//
// FORTRAN IV (1962) MAJOR INNOVATIONS:
// - LOGICAL data type with .TRUE./.FALSE. literals
// - DOUBLE PRECISION for higher precision floating point
// - COMPLEX data type for complex number arithmetic
// - Logical IF statement (in addition to arithmetic IF)
// - Enhanced Boolean expressions with .AND., .OR., .NOT.
// - Mixed-mode arithmetic with automatic type conversions
// - New intrinsic functions: DBLE, CMPLX, REAL, AIMAG, etc.
//
// Historical Context (1962):
// - Released for IBM 7030 "Stretch" supercomputer
// - Later ported to IBM 7090, 7094, and IBM 1401 (1966)
// - Became the foundation for FORTRAN 66 standard
// - Made FORTRAN truly portable across different computers
//
// ====================================================================

// ====================================================================
// FORTRAN IV (1962) - ENHANCED TYPE SYSTEM
// ====================================================================

// Override type specification to include FORTRAN IV data types
type_spec
    : INTEGER
    | REAL
    | LOGICAL                    // NEW in FORTRAN IV (1962)
    | DOUBLE PRECISION           // NEW in FORTRAN IV (1962)
    | COMPLEX                    // NEW in FORTRAN IV (1962)
    ;

// ====================================================================
// FORTRAN IV (1962) - ENHANCED EXPRESSIONS
// ====================================================================

// Override statement body to include new statement types
statement_body
    : assignment_stmt      // Variable = Expression (mathematical notation!)
    | goto_stmt           // Unconditional jump to labeled statement
    | computed_goto_stmt  // Multi-way branch based on integer expression
    | arithmetic_if_stmt  // Three-way branch based on expression sign
    | logical_if_stmt     // NEW: Two-way branch based on logical expression
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
    | call_stmt         // Call subroutine
    ;

// Logical IF statement (NEW in FORTRAN IV, 1962)
logical_if_stmt
    : IF LPAREN logical_expr RPAREN statement_body
    ;

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

// ====================================================================
// FORTRAN IV (1962) - ENHANCED ARITHMETIC EXPRESSIONS
// ====================================================================

// Override expression handling to support new data types
// Mixed-mode arithmetic: automatic conversions between types
expr
    : term ((PLUS | MINUS) term)*
    ;

term
    : factor ((MULTIPLY | SLASH) factor)*
    ;

factor
    : primary (POWER primary)*    // Right associative exponentiation
    ;

primary
    : INTEGER_LITERAL
    | REAL_LITERAL
    | double_precision_literal    // NEW in FORTRAN IV
    | complex_literal            // NEW in FORTRAN IV
    | logical_literal           // NEW in FORTRAN IV  
    | variable
    | function_call
    | LPAREN expr RPAREN
    | PLUS primary              // Unary plus
    | MINUS primary             // Unary minus
    ;

// New literal types (FORTRAN IV, 1962)
double_precision_literal
    : REAL_LITERAL D (PLUS | MINUS)? INTEGER_LITERAL  // e.g., 3.14D0
    | INTEGER_LITERAL D (PLUS | MINUS)? INTEGER_LITERAL // e.g., 1D+10
    ;

complex_literal
    : LPAREN expr COMMA expr RPAREN    // (real_part, imaginary_part)
    ;

// ====================================================================
// FORTRAN IV (1962) - ENHANCED BUILT-IN FUNCTIONS
// ====================================================================

// Function calls with new FORTRAN IV intrinsics
function_call
    : IDENTIFIER LPAREN expr_list? RPAREN
    ;

// Expression list for function arguments
expr_list
    : expr (COMMA expr)*
    ;

// ====================================================================
// FORTRAN IV (1962) - HISTORICAL SIGNIFICANCE
// ====================================================================
//
// FORTRAN IV (1962) was revolutionary because:
//
// 1. **Complete Type System**: Added logical, double precision, complex
//    - Enabled scientific computing with proper data types
//    - Supported Boolean algebra and logical operations
//    - Provided high-precision floating point computation
//
// 2. **Portable Language**: Removed IBM-specific dependencies  
//    - Could run on different computer architectures
//    - Became truly machine-independent programming language
//
// 3. **Foundation for Standards**: Became basis for FORTRAN 66
//    - First standardized version built on FORTRAN IV
//    - Established stable feature set for scientific programming
//
// 4. **Enhanced Expressions**: Mixed-mode arithmetic
//    - Automatic type conversions between INTEGER/REAL/DOUBLE/COMPLEX
//    - Sophisticated expression evaluation
//
// 5. **Scientific Focus**: Built-in mathematical functions
//    - DBLE() for double precision conversion
//    - CMPLX() for complex number creation
//    - REAL(), AIMAG() for complex number extraction
//
// This parser captures the essence of FORTRAN IV's data type revolution
// while maintaining full compatibility with FORTRAN II procedural features.
//
// ====================================================================