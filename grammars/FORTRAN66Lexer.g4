// FORTRAN 66 (X3.9-1966) - First FORTRAN Standard with FORTRAN IV Features
// Merges FORTRAN IV (1962) data types with FORTRAN 66 standardization
lexer grammar FORTRAN66Lexer;

import FORTRANIILexer;  // Import FORTRAN II (1958) constructs

// ====================================================================
// FORTRAN 66 (X3.9-1966) HISTORICAL OVERVIEW
// ====================================================================
//
// FORTRAN 66 (ANSI X3.9-1966) was a landmark achievement:
// - First programming language defined by formal standard (March 1966)
// - Based on FORTRAN IV but removed machine-dependent features
// - Defined two languages: FORTRAN and Basic FORTRAN
// - Established portable FORTRAN across different computer systems
//
// Historical Context:
// - Developed by American Standards Association committee (1962-1966)
// - Response to need for machine-independent FORTRAN
// - Created unified standard from various FORTRAN IV implementations
// - Served as foundation for all subsequent FORTRAN standards
//
// Key Achievement:
// - Formal standardization rather than new language features
// - Removed IBM-specific and machine-dependent constructs
// - Established consistent FORTRAN semantics across vendors
//

// ====================================================================
// FORTRAN IV (1962) DATA TYPES - merged into FORTRAN 66
// ====================================================================

// LOGICAL data type (added in FORTRAN IV, 1962)
LOGICAL         : L O G I C A L ;

// Double precision (added in FORTRAN IV, 1962)  
DOUBLE          : D O U B L E ;
PRECISION       : P R E C I S I O N ;

// Complex numbers (added in FORTRAN IV, 1962)
COMPLEX         : C O M P L E X ;

// Double precision exponent marker (added in FORTRAN IV, 1962)
D               : [Dd] ;

// ====================================================================
// FORTRAN IV (1962) LOGICAL LITERALS AND OPERATORS - merged into FORTRAN 66
// ====================================================================

// Logical literals (FORTRAN IV innovation)
DOT_TRUE        : '.' T R U E '.' ;
DOT_FALSE       : '.' F A L S E '.' ;

// Logical operators (FORTRAN IV innovation)
DOT_AND         : '.' A N D '.' ;
DOT_OR          : '.' O R '.' ;  
DOT_NOT         : '.' N O T '.' ;
DOT_EQV         : '.' E Q V '.' ;
DOT_NEQV        : '.' N E Q V '.' ;

// Relational operators (enhanced in FORTRAN IV)
DOT_EQ          : '.' E Q '.' ;
DOT_NE          : '.' N E '.' ;
DOT_LT          : '.' L T '.' ;
DOT_LE          : '.' L E '.' ;
DOT_GT          : '.' G T '.' ;
DOT_GE          : '.' G E '.' ;

// ====================================================================
// FORTRAN 66 STANDARDIZATION FEATURES
// ====================================================================

// Standard program unit keywords (formalized in FORTRAN 66)
BLOCK           : B L O C K ;          // BLOCK keyword
BLOCKDATA       : B L O C K D A T A ;  // BLOCK DATA program unit

// DATA statement keyword (ANSI X3.9-1966 Section 7.2)
DATA            : D A T A ;

// Standard procedure specifications (formalized in FORTRAN 66)
EXTERNAL        : E X T E R N A L ;    // External procedure declaration
INTRINSIC       : I N T R I N S I C ;  // Intrinsic function specification

// Auxiliary I/O statements (ANSI X3.9-1966 Section 7.1.3.3)
// Sequential file positioning statements
REWIND          : R E W I N D ;        // Position file to beginning
BACKSPACE       : B A C K S P A C E ;  // Position file back one record
ENDFILE         : E N D F I L E ;      // Write end-of-file mark

// Standardized statement labels (1-99999, no leading zeros)
// This was formalized in FORTRAN 66 to ensure portability

// ====================================================================
// FORTRAN 66 (1966) HISTORICAL SIGNIFICANCE
// ====================================================================
//
// FORTRAN 66 was revolutionary because:
// 1. First formal programming language standard (X3.9-1966)
// 2. Established portable FORTRAN across different computers
// 3. Removed machine-specific dependencies from FORTRAN IV
// 4. Created unified semantics for FORTRAN implementations
// 5. Foundation for all subsequent FORTRAN/Fortran standards
//
// This standard enabled truly portable scientific software and
// established FORTRAN as the dominant language for scientific computing.
// The standardization process served as a model for all future
// programming language standards.
//
// Note: FORTRAN 66 did not add major new language features beyond
// FORTRAN IV - its achievement was formal standardization and
// removal of machine dependencies.
//
// ====================================================================