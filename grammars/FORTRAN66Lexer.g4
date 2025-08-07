// FORTRAN 66 (X3.9-1966) - First FORTRAN Standard
// Formal standardization of FORTRAN IV with removed machine dependencies
lexer grammar FORTRAN66Lexer;

import FORTRANIVLexer;  // Import FORTRAN IV (1962) constructs

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
// FORTRAN 66 STANDARDIZATION FEATURES
// ====================================================================

// FORTRAN 66 (X3.9-1966) standardized and formalized:
// - Program unit structure (MAIN, SUBROUTINE, FUNCTION, BLOCK DATA)
// - Data types (INTEGER, REAL, DOUBLE PRECISION, COMPLEX, LOGICAL)
// - Control flow (standardized GOTO, IF, DO semantics)
// - I/O operations (standardized READ, WRITE, FORMAT)
// - Subprogram linkage (CALL, RETURN, COMMON, EXTERNAL)
// - Removed machine-dependent features from FORTRAN IV implementations

// Standard program unit keywords (formalized in FORTRAN 66)
BLOCKDATA       : B L O C K D A T A ;  // BLOCK DATA program unit
DATA            : D A T A ;            // Data initialization statement

// Standard procedure specifications (formalized in FORTRAN 66)
EXTERNAL        : E X T E R N A L ;    // External procedure declaration
INTRINSIC       : I N T R I N S I C ;  // Intrinsic function specification

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