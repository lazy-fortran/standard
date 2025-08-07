// FORTRAN IV (1962) - Data Type Revolution
// Added LOGICAL and DOUBLE PRECISION data types
lexer grammar FortranIVLexer;

import FORTRANLexer;  // Import FORTRAN II (1958) constructs

// ====================================================================
// FORTRAN IV (1962) HISTORICAL OVERVIEW
// ====================================================================
//
// FORTRAN IV (1962) was a major enhancement that added:
// - LOGICAL data type (.TRUE., .FALSE.)
// - DOUBLE PRECISION data type (higher precision reals)  
// - COMPLEX data type (complex numbers)
// - Logical IF statement (alternative to arithmetic IF)
// - Enhanced Boolean expressions
// - Removed machine-dependent features from FORTRAN II
//
// Historical Context:
// - Released in 1962 for IBM 7030 ("Stretch")
// - Later ported to IBM 7090, 7094, and IBM 1401 (1966)
// - Became the foundation for FORTRAN 66 standard
//

// ====================================================================
// FORTRAN IV (1962) NEW DATA TYPES
// ====================================================================

// LOGICAL data type (added in FORTRAN IV, 1962)
LOGICAL         : L O G I C A L ;

// Double precision (added in FORTRAN IV, 1962)  
DOUBLE          : D O U B L E ;
PRECISION       : P R E C I S I O N ;

// Complex numbers (added in FORTRAN IV, 1962)
COMPLEX         : C O M P L E X ;

// ====================================================================
// FORTRAN IV (1962) LOGICAL LITERALS AND OPERATORS
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
// FORTRAN IV (1962) HISTORICAL SIGNIFICANCE
// ====================================================================
//
// FORTRAN IV was crucial because:
// 1. Introduced proper data typing beyond INTEGER/REAL
// 2. Added logical operations and Boolean algebra
// 3. Provided foundation for structured programming
// 4. Became basis for first FORTRAN standard (FORTRAN 66)
// 5. Removed IBM-specific machine dependencies
//
// This version made FORTRAN truly portable across different computers
// and established it as the dominant scientific programming language.
//
// ====================================================================