/*
 * Fortran2023Lexer.g4
 * 
 * Fortran 2023 (ISO/IEC 1539-1:2023) - Latest ISO Standard
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

lexer grammar Fortran2023Lexer;

import Fortran2018Lexer;

// ============================================================================
// FORTRAN 2023 NEW TOKENS
// ============================================================================
//
// Fortran 2023 is primarily a minor revision that corrects errors and 
// omissions in Fortran 2018, but includes several new features:
//
// Major New Features:
// - Enumerated types (ENUM construct enhancements)
// - Conditional expressions (ternary operator ? :)
// - Enhanced IEEE arithmetic functions
// - BOZ constant improvements  
// - NAMELIST enhancements
// - SYSTEM_CLOCK improvements
//
// ============================================================================

// ============================================================================
// ENUMERATED TYPES (Enhanced in F2023)
// ============================================================================

// Better enumeration support - enhanced from F2003 ENUM construct
ENUMERATOR       : E N U M E R A T O R ;

// ============================================================================
// CONDITIONAL EXPRESSIONS (NEW in F2023)
// ============================================================================

// Conditional expression operator (ternary operator)
QUESTION         : '?' ;             // condition ? true_expr : false_expr

// ============================================================================
// ENHANCED IEEE ARITHMETIC (F2023)
// ============================================================================

// New IEEE functions in F2023
IEEE_MAX         : I E E E '_' M A X ;
IEEE_MIN         : I E E E '_' M I N ;
IEEE_MAX_MAG     : I E E E '_' M A X '_' M A G ;
IEEE_MIN_MAG     : I E E E '_' M I N '_' M A G ;

// ============================================================================
// ENHANCED INTRINSIC FUNCTIONS (F2023)
// ============================================================================

// Enhanced functions and constants
LOGICAL_KINDS    : L O G I C A L '_' K I N D S ;
CHARACTER_KINDS  : C H A R A C T E R '_' K I N D S ;

// Fragments are inherited from Fortran2018Lexer - no need to duplicate

// ============================================================================
// FORTRAN 2023 HISTORICAL SIGNIFICANCE
// ============================================================================
//
// Fortran 2023 (ISO/IEC 1539-1:2023) was published in November 2023:
//
// 1. **Minor Revision Focus**: Primarily corrects errors in F2018
// 2. **Enumerated Types**: Enhanced ENUM construct with better type safety
// 3. **Conditional Expressions**: Ternary operator support (? :)
// 4. **IEEE Improvements**: New IEEE_MAX, IEEE_MIN functions
// 5. **BOZ Constants**: Enhanced array constructor support
// 6. **NAMELIST**: PUBLIC groups may contain PRIVATE variables
// 7. **SYSTEM_CLOCK**: All arguments must have same kind
//
// This lexer captures F2023's incremental improvements while maintaining
// full compatibility with F2018's modern Fortran revolution.
// Foundation for LazyFortran2025 type inference extensions.
//
// ============================================================================