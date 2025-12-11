/*
 * FORTRAN66Lexer.g4
 *
 * FORTRAN 66 (ANSI X3.9-1966) - First FORTRAN Standard
 * Merges FORTRAN IV (1962) data types with FORTRAN 66 standardization.
 *
 * Reference: ANSI X3.9-1966 "USA Standard FORTRAN"
 *            (American National Standards Institute, March 7, 1966)
 *
 * Standard Structure:
 *   Section 1: Scope and Purpose
 *   Section 2: Definitions
 *   Section 3: Program Form and Organization (3.1-3.5)
 *   Section 4: Data Types and Constants (4.1-4.6)
 *   Section 5: Variables, Arrays, and Subscripts (5.1-5.4)
 *   Section 6: Expressions (6.1-6.6)
 *   Section 7: Statements (7.1 Executable, 7.2 Non-executable)
 *   Section 8: Procedures (8.1-8.7)
 *
 * This lexer extends FORTRANIILexer with FORTRAN IV data types and
 * operators that were codified in the X3.9-1966 standard.
 */

lexer grammar FORTRAN66Lexer;

import FORTRANIILexer;  // Import FORTRAN II (1958) constructs

// ============================================================================
// FORTRAN 66 (X3.9-1966) HISTORICAL OVERVIEW
// ============================================================================
//
// FORTRAN 66 (ANSI X3.9-1966) was a landmark achievement:
// - First programming language defined by formal standard (March 7, 1966)
// - Based on FORTRAN IV but removed machine-dependent features
// - Defined two levels: FORTRAN and Basic FORTRAN
// - Established portable FORTRAN across different computer systems
//
// Historical Context:
// - Developed by American Standards Association X3.4.3 committee (1962-1966)
// - Response to need for machine-independent FORTRAN
// - Created unified standard from various FORTRAN IV implementations
// - Served as foundation for all subsequent FORTRAN standards
//
// Key Achievement:
// - Formal standardization rather than new language features
// - Removed IBM-specific and machine-dependent constructs
// - Established consistent FORTRAN semantics across vendors
//
// ============================================================================

// ============================================================================
// DATA TYPES - X3.9-1966 Section 4
// ============================================================================
// X3.9-1966 Section 4 defines six data types for FORTRAN:
//   4.1 Integer (from FORTRAN I) - X3.9-1966 Section 4.1
//   4.2 Real (from FORTRAN I) - X3.9-1966 Section 4.2
//   4.3 Double Precision (from FORTRAN IV, 1962) - X3.9-1966 Section 4.3
//   4.4 Complex (from FORTRAN IV, 1962) - X3.9-1966 Section 4.4
//   4.5 Logical (from FORTRAN IV, 1962) - X3.9-1966 Section 4.5
//   4.6 (reserved for future use in X3.9-1966)
// ============================================================================

// INTEGER data type - X3.9-1966 Section 4.1
// Used in explicit type declarations: INTEGER var1, var2
INTEGER         : I N T E G E R ;

// REAL data type - X3.9-1966 Section 4.2
// Used in explicit type declarations: REAL var1, var2
REAL            : R E A L ;

// IMPLICIT statement - X3.9-1966 Section 7.2.5
// Defines implicit typing rules: IMPLICIT INTEGER (A-H, O-Z)
IMPLICIT        : I M P L I C I T ;

// LOGICAL data type - X3.9-1966 Section 4.5
// Represents truth values (.TRUE. or .FALSE.)
LOGICAL         : L O G I C A L ;

// DOUBLE PRECISION data type - X3.9-1966 Section 4.3
// Extended precision floating-point (typically twice REAL precision)
DOUBLE          : D O U B L E ;
PRECISION       : P R E C I S I O N ;

// COMPLEX data type - X3.9-1966 Section 4.4
// Ordered pair of REAL values (real part, imaginary part)
COMPLEX         : C O M P L E X ;

// Double precision exponent marker - X3.9-1966 Section 4.3.2
// Used in DOUBLE PRECISION constants: 1.0D0, 3.14159D0
D               : [Dd] ;

// Exponent fragment override - X3.9-1966 Section 4.3.2
// Ensures D exponent notation is available starting with FORTRAN 66
fragment EXPONENT : [eEdD] [+-]? DIGIT+ ;

// ============================================================================
// LOGICAL CONSTANTS - X3.9-1966 Section 4.5.2
// ============================================================================
// X3.9-1966 Section 4.5.2 defines two logical constants:
//   .TRUE.  - represents the true value
//   .FALSE. - represents the false value
// ============================================================================

DOT_TRUE        : '.' T R U E '.' ;
DOT_FALSE       : '.' F A L S E '.' ;

// ============================================================================
// LOGICAL OPERATORS - X3.9-1966 Section 6.4
// ============================================================================
// X3.9-1966 Section 6.4 defines logical operators for logical expressions:
//   .NOT.  - logical negation (unary)
//   .AND.  - logical conjunction
//   .OR.   - logical disjunction
//   .EQV.  - logical equivalence (A .EQV. B = .NOT. (A .NEQV. B))
//   .NEQV. - logical non-equivalence (exclusive or)
// ============================================================================

DOT_AND         : '.' A N D '.' ;
DOT_OR          : '.' O R '.' ;
DOT_NOT         : '.' N O T '.' ;
DOT_EQV         : '.' E Q V '.' ;
DOT_NEQV        : '.' N E Q V '.' ;

// ============================================================================
// RELATIONAL OPERATORS - X3.9-1966 Section 6.3
// ============================================================================
// X3.9-1966 Section 6.3 defines relational operators that compare
// arithmetic expressions and produce logical values:
//   .LT. - less than
//   .LE. - less than or equal
//   .EQ. - equal
//   .NE. - not equal
//   .GT. - greater than
//   .GE. - greater than or equal
// ============================================================================

DOT_EQ          : '.' E Q '.' ;
DOT_NE          : '.' N E '.' ;
DOT_LT          : '.' L T '.' ;
DOT_LE          : '.' L E '.' ;
DOT_GT          : '.' G T '.' ;
DOT_GE          : '.' G E '.' ;

// ============================================================================
// PROGRAM UNIT KEYWORDS - X3.9-1966 Section 8
// ============================================================================
// X3.9-1966 Section 8 defines program unit structure:
//   8.1 Main Program
//   8.2 FUNCTION Subprogram
//   8.3 SUBROUTINE Subprogram
//   8.4 BLOCK DATA Subprogram (for COMMON block initialization)
// ============================================================================

// BLOCK DATA program unit - X3.9-1966 Section 8.4
// Provides compile-time initialization for named COMMON blocks
BLOCK           : B L O C K ;
BLOCKDATA       : B L O C K D A T A ;

// ============================================================================
// NON-EXECUTABLE STATEMENT KEYWORDS - X3.9-1966 Section 7.2
// ============================================================================
// X3.9-1966 Section 7.2 defines non-executable (specification) statements:
//   7.2.1 DIMENSION
//   7.2.2 COMMON
//   7.2.3 EQUIVALENCE
//   7.2.4 EXTERNAL
//   7.2.5 Type statements (INTEGER, REAL, DOUBLE PRECISION, COMPLEX, LOGICAL)
//   7.2.6 DATA
//   7.2.7 FORMAT
// ============================================================================

// DATA statement - X3.9-1966 Section 7.2.6
// Provides compile-time initialization of variables and arrays
DATA            : D A T A ;

// EXTERNAL statement - X3.9-1966 Section 7.2.4
// Declares names that refer to external procedures
EXTERNAL        : E X T E R N A L ;

// INTRINSIC statement - X3.9-1966 Section 8.7
// Declares names that refer to intrinsic (built-in) functions
// Note: INTRINSIC clarifies when a name should use intrinsic semantics
INTRINSIC       : I N T R I N S I C ;

// ============================================================================
// AUXILIARY I/O STATEMENT KEYWORDS - X3.9-1966 Section 7.1.3.3
// ============================================================================
// X3.9-1966 Section 7.1.3.3 defines auxiliary I/O statements for
// sequential file positioning:
//   REWIND u     - Position file to beginning
//   BACKSPACE u  - Position file back one record
//   ENDFILE u    - Write end-of-file mark
// where u is an unsigned integer expression (unit number)
// ============================================================================

REWIND          : R E W I N D ;
BACKSPACE       : B A C K S P A C E ;
ENDFILE         : E N D F I L E ;

// ============================================================================
// STATEMENT LABELS - X3.9-1966 Section 3.4
// ============================================================================
// X3.9-1966 Section 3.4 defines statement labels:
// - 1 to 5 decimal digits (range 1-99999)
// - Leading zeros are not significant
// - Labels identify statements for control transfer and FORMAT references
// Note: LABEL token is inherited from FORTRANIILexer
// ============================================================================

// ============================================================================
// DELIMITERS FOR COMPLEX CONSTANTS - X3.9-1966 Section 4.4.2
// ============================================================================
// Colon used in FORTRAN 77 for substring notation (inherited by F77)
// Added here for consistency with complex constant syntax even though
// complex constants are primarily a FORTRAN 66+ feature
COLON           : ':' ;

// ============================================================================
// IDENTIFIER - X3.9-1966 Section 2.3 "Symbolic Names"
// ============================================================================
// X3.9-1966 Section 2.3 mandates that symbolic names consist of 1 to 6
// alphanumeric characters, with the first character being alphabetic.
//
// NOTE: The identifier length restriction is inherited from FORTRANLexer.
// While X3.9-1966 Section 2.3 requires identifiers to be "1 to 6 characters",
// this is a semantic constraint that should be validated at the semantic analysis
// phase, not enforced by truncating lexer tokens. Lexer-level truncation breaks
// parsing of valid code where longer identifiers appear (e.g., statement labels
// like 100, or keywords that happen to be longer than 6 characters when written
// without whitespace in fixed-form FORTRAN).
//
// Compliance: STANDARD-COMPLIANT with X3.9-1966 Section 2.3 (via semantic validation)
//
// See FORTRAN77Lexer for identifier length extension to 31 characters.
// ============================================================================
// (IDENTIFIER rule inherited from FORTRANLexer - no local override)

// ============================================================================
// FORTRAN 66 (X3.9-1966) HISTORICAL SIGNIFICANCE
// ============================================================================
//
// FORTRAN 66 was revolutionary because:
// 1. First formal programming language standard (ANSI X3.9-1966, March 7, 1966)
// 2. Established portable FORTRAN across different computer architectures
// 3. Removed machine-specific dependencies from FORTRAN IV
// 4. Created unified semantics for FORTRAN implementations
// 5. Foundation for all subsequent FORTRAN/Fortran standards (F77, F90, etc.)
//
// This standard enabled truly portable scientific software and established
// FORTRAN as the dominant language for scientific computing. The X3.4.3
// subcommittee process served as a model for future language standards.
//
// Note: FORTRAN 66 did not add major new language features beyond FORTRAN IV;
// its achievement was formal standardization and removal of machine dependencies.
//
// ============================================================================
