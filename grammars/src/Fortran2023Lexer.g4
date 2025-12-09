/*
 * Fortran2023Lexer.g4
 *
 * Fortran 2023 (ISO/IEC 1539-1:2023) Lexer
 * Reference: J3/22-007 "Fortran 2023" (Working Draft), J3/23-007r1 (Draft Standard)
 *
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 *
 * ISO/IEC 1539-1:2023 References:
 * - Section 6: Lexical tokens and source form
 * - Section 7: Types (7.8 Enumeration types)
 * - Section 10: Expressions (10.1.5 Conditional expressions)
 * - Section 17: IEEE arithmetic intrinsic modules
 */

lexer grammar Fortran2023Lexer;

import Fortran2018Lexer;

// ============================================================================
// FORTRAN 2023 NEW TOKENS (ISO/IEC 1539-1:2023)
// ============================================================================
//
// Fortran 2023 is primarily a minor revision that corrects errors and
// omissions in Fortran 2018, but includes several new features:
//
// Major New Features (J3/22-007):
// - Section 7.8: Enumeration types (enhanced ENUM construct)
// - Section 10.1.5: Conditional expressions (ternary operator ? :)
// - Section 17: Enhanced IEEE arithmetic functions (IEEE_ARITHMETIC module)
// - Section 7.7 R772: BOZ constant improvements
// - Section 14.2.4: NAMELIST enhancements
// - Section 16.9.196: SYSTEM_CLOCK improvements
//
// ============================================================================

// ----------------------------------------------------------------------------
// Enumeration Types (NEW in F2023)
// ISO/IEC 1539-1:2023 Section 7.8: Enumeration types
// J3/22-007 R759-R763: enum-def, enum-def-stmt, enumerator-def-stmt,
// enumerator, end-enum-stmt
// R761: enumerator-def-stmt is ENUMERATOR [ :: ] enumerator-list
// ----------------------------------------------------------------------------

// Better enumeration support - enhanced from F2003 ENUM construct
ENUMERATOR       : E N U M E R A T O R ;

// ----------------------------------------------------------------------------
// Conditional Expressions (NEW in F2023)
// ISO/IEC 1539-1:2023 Section 10.1.5: Conditional expressions
// J3/22-007 R1020-R1024: conditional-expr, conditional-consequent-expr,
// cond-else-expr, conditional-test, consequent
// R1020: conditional-expr is ( conditional-test ? consequent ... : consequent )
// ----------------------------------------------------------------------------

// Conditional expression operator (ternary operator)
// ISO/IEC 1539-1:2023 Section 10.1.5: QUESTION is part of conditional-expr syntax
QUESTION         : '?' ;             // condition ? true_expr : false_expr

// ----------------------------------------------------------------------------
// Enhanced IEEE Arithmetic Functions (F2023)
// ISO/IEC 1539-1:2023 Section 17: IEEE arithmetic intrinsic modules
// J3/22-007 Section 17.11: IEEE_ARITHMETIC intrinsic module summary
// Section 17.11.20-17.11.23: IEEE_MAX_NUM, IEEE_MAX_NUM_MAG,
// IEEE_MIN_NUM, IEEE_MIN_NUM_MAG (NaN handling refinements)
// Note: IEEE_MAX/IEEE_MIN tokens represent simplified names for these functions
// ----------------------------------------------------------------------------

// New IEEE functions in F2023 (Section 17 of J3/22-007)
IEEE_MAX         : I E E E '_' M A X ;
IEEE_MIN         : I E E E '_' M I N ;
IEEE_MAX_MAG     : I E E E '_' M A X '_' M A G ;
IEEE_MIN_MAG     : I E E E '_' M I N '_' M A G ;

// ----------------------------------------------------------------------------
// Enhanced Intrinsic Constants (F2023)
// ISO/IEC 1539-1:2023 Section 16.10.2: ISO_FORTRAN_ENV intrinsic module
// J3/22-007 Section 16.10.2.14: LOGICAL_KINDS - array of available logical kinds
// J3/22-007 Section 16.10.2.2: CHARACTER_KINDS - array of available char kinds
// ----------------------------------------------------------------------------

// Enhanced functions and constants (Section 16.10.2 of J3/22-007)
LOGICAL_KINDS    : L O G I C A L '_' K I N D S ;
CHARACTER_KINDS  : C H A R A C T E R '_' K I N D S ;

// ----------------------------------------------------------------------------
// Degree-based Trigonometric Intrinsics (NEW in F2023)
// ISO/IEC 1539-1:2023 Section 16.9: Intrinsic procedures
// J3/22-007 Section 16.9.3, 16.9.17, 16.9.21, 16.9.22, 16.9.51, 16.9.170,
// 16.9.186
// ----------------------------------------------------------------------------

// Inverse trigonometric functions returning degrees
// ACOSD(X): Arc cosine in degrees (Section 16.9.3)
ACOSD            : A C O S D ;
// ASIND(X): Arc sine in degrees (Section 16.9.17)
ASIND            : A S I N D ;
// ATAND(X) or ATAND(Y,X): Arc tangent in degrees (Section 16.9.21)
ATAND            : A T A N D ;
// ATAN2D(Y,X): Arc tangent of Y/X in degrees (Section 16.9.22)
ATAN2D           : A T A N '2' D ;

// Trigonometric functions with argument in degrees
// COSD(X): Cosine with argument in degrees (Section 16.9.51)
COSD             : C O S D ;
// SIND(X): Sine with argument in degrees (Section 16.9.170)
SIND             : S I N D ;
// TAND(X): Tangent with argument in degrees (Section 16.9.186)
TAND             : T A N D ;

// ----------------------------------------------------------------------------
// Pi-Scaled Trigonometric Intrinsics (NEW in F2023)
// ISO/IEC 1539-1:2023 Section 16.9: Intrinsic procedures
// J3/22-007 Section 16.9.4, 16.9.5, 16.9.18, 16.9.23, 16.9.24, 16.9.52,
// 16.9.171, 16.9.187
// ----------------------------------------------------------------------------

// Inverse trigonometric functions returning result divided by pi
// ACOSPI(X): Arc cosine / pi (Section 16.9.4)
ACOSPI           : A C O S P I ;
// ASINPI(X): Arc sine / pi (Section 16.9.18)
ASINPI           : A S I N P I ;
// ATANPI(X) or ATANPI(Y,X): Arc tangent / pi (Section 16.9.23)
ATANPI           : A T A N P I ;
// ATAN2PI(Y,X): Arc tangent of Y/X / pi (Section 16.9.24)
ATAN2PI          : A T A N '2' P I ;

// Trigonometric functions with argument multiplied by pi
// COSPI(X): Cosine of pi * x (Section 16.9.52)
COSPI            : C O S P I ;
// SINPI(X): Sine of pi * x (Section 16.9.171)
SINPI            : S I N P I ;
// TANPI(X): Tangent of pi * x (Section 16.9.187)
TANPI            : T A N P I ;

// Fragments are inherited from Fortran2018Lexer - no need to duplicate

// ============================================================================
// FORTRAN 2023 STANDARD OVERVIEW (ISO/IEC 1539-1:2023)
// ============================================================================
//
// Fortran 2023 (ISO/IEC 1539-1:2023) was published in November 2023.
// Reference documents: J3/22-007, J3/23-007r1, J3/24-007
//
// Key New Features and Section References:
//
// 1. Section 7.8: Enumeration types (R759-R763)
//    - Enhanced ENUM construct with named type enumerations
//    - Interoperable enumerations (BIND(C)) and Fortran-specific enumerations
//
// 2. Section 10.1.5: Conditional expressions (R1020-R1024)
//    - Ternary operator syntax: ( condition ? true_expr : false_expr )
//    - Lazy evaluation semantics
//
// 3. Section 17: IEEE arithmetic intrinsic modules
//    - IEEE_MAX_NUM, IEEE_MIN_NUM with NaN handling refinements
//    - IEEE_MAX_NUM_MAG, IEEE_MIN_NUM_MAG
//
// 4. Section 7.7 R772: BOZ literal constants
//    - Enhanced array constructor support with explicit type-spec
//
// 5. Section 14.2.4: NAMELIST statement
//    - PUBLIC groups may contain PRIVATE variables
//
// 6. Section 16.9.196: SYSTEM_CLOCK intrinsic
//    - All integer arguments must have same kind
//
// 7. Section 7.3.2.1: TYPEOF/CLASSOF type specifiers
//    - Declare entities with same type as another entity
//
// This lexer captures F2023 tokens while maintaining full compatibility
// with F2018. Foundation for LazyFortran2025 type inference extensions.
//
// ============================================================================