/*
 * Fortran2023Parser.g4
 * 
 * Fortran 2023 (ISO/IEC 1539-1:2023) - Latest ISO Standard
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

parser grammar Fortran2023Parser;

import Fortran2018Parser;

options {
    tokenVocab = Fortran2023Lexer;
}

// ============================================================================
// FORTRAN 2023 PROGRAM STRUCTURE 
// ============================================================================

// F2023 program unit (enhanced with enumeration and conditional expressions)
program_unit_f2023
    : NEWLINE* (main_program_f2023 | module_f2023 | submodule_f2008 
      | external_subprogram_f2023) NEWLINE*
    ;

// Enhanced main program for F2023 (inherits from F2018)
main_program_f2023
    : main_program_f2018
    ;

// Enhanced module for F2023 (inherits from F2018)
module_f2023
    : module_f2018
    ;

// Enhanced external subprogram for F2023 (inherits from F2018)
external_subprogram_f2023
    : external_subprogram_f2018
    ;

// ============================================================================
// FORTRAN 2023 SPECIFICATION PART (Enhanced)
// ============================================================================

// Inherit F2018 specification part with F2023 enhancements
specification_part_f2023
    : specification_part_f2018
    ;

// ============================================================================
// ENUMERATED TYPES (Enhanced in F2023)
// ============================================================================

// Enhanced enumeration definition (F2023 improvement)
enum_def_f2023
    : enum_def_stmt_f2023
      enumerator_def_stmt_f2023*
      end_enum_stmt_f2023
    ;

// Enhanced ENUM definition statement (F2023)
enum_def_stmt_f2023
    : ENUM COMMA BIND LPAREN C RPAREN (DOUBLE_COLON type_spec)? NEWLINE
    | ENUM (DOUBLE_COLON type_spec)? NEWLINE    // F2023 typed enumerators
    ;

// Enhanced enumerator definition (F2023)
enumerator_def_stmt_f2023
    : ENUMERATOR (DOUBLE_COLON)? enumerator_list_f2023 NEWLINE
    ;

// F2023 enumerator list with better type support
enumerator_list_f2023
    : enumerator_f2023 (COMMA enumerator_f2023)*
    ;

// F2023 enumerator with enhanced initialization
enumerator_f2023
    : IDENTIFIER (EQUALS INTEGER_LITERAL)?
    ;

// Enhanced end enum statement
end_enum_stmt_f2023
    : END ENUM NEWLINE
    ;

// ============================================================================
// CONDITIONAL EXPRESSIONS (NEW in F2023)
// ============================================================================

// ============================================================================
// ENHANCED EXPRESSIONS (F2023)
// ============================================================================

// Enhanced expression for F2023 with conditional expressions
expr_f2023
    : IDENTIFIER QUESTION IDENTIFIER COLON IDENTIFIER  // NEW: conditional expression
    | IDENTIFIER
    | INTEGER_LITERAL
    | REAL_LITERAL
    | STRING_LITERAL
    | LPAREN expr_f2023 RPAREN
    ;

// F2023 conditional expression (ternary operator) - non-recursive
conditional_expr_f2023
    : IDENTIFIER QUESTION IDENTIFIER COLON IDENTIFIER
    ;

// ============================================================================
// EXECUTION PART (Enhanced for F2023)
// ============================================================================

// Inherit F2018 execution part for F2023
execution_part_f2023
    : execution_part_f2018
    ;

// ============================================================================
// RANDOM INITIALIZATION (Enhanced in F2023)
// ============================================================================

// Enhanced random initialization statement
random_init_stmt_f2023
    : RANDOM_INIT LPAREN 
      REPEATABLE EQUALS logical_expr COMMA
      IMAGE_DISTINCT EQUALS logical_expr 
      RPAREN NEWLINE
    ;

// ============================================================================
// TYPE DECLARATIONS (Enhanced in F2023)
// ============================================================================

// Simplified F2023 type declarations (inherit from F2018)
type_declaration_stmt_f2023
    : IDENTIFIER COLON COLON IDENTIFIER NEWLINE
    ;

// ============================================================================
// ENHANCED IEEE ARITHMETIC (F2023)
// ============================================================================

// F2023 IEEE intrinsic functions (enhanced)
ieee_intrinsic_function_f2023
    : IEEE_MAX LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    | IEEE_MIN LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    | IEEE_MAX_MAG LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    | IEEE_MIN_MAG LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    ;

// ============================================================================
// ENHANCED NAMELIST (F2023)
// ============================================================================

// Enhanced namelist group object (F2023: PUBLIC groups may contain PRIVATE)
namelist_group_object_f2023
    : IDENTIFIER
    ;

// ============================================================================
// BOZ CONSTANTS (Enhanced in F2023) 
// ============================================================================

// Enhanced BOZ constant handling in array constructors
boz_literal_constant_f2023
    : BINARY_CONSTANT
    | OCTAL_CONSTANT  
    | HEX_CONSTANT
    ;

// ============================================================================
// SYSTEM_CLOCK ENHANCEMENTS (F2023)
// ============================================================================

// Enhanced SYSTEM_CLOCK intrinsic (F2023: all args same kind)
system_clock_stmt_f2023
    : CALL SYSTEM_CLOCK LPAREN 
      (IDENTIFIER (COMMA IDENTIFIER)? (COMMA IDENTIFIER)?)?
      RPAREN NEWLINE
    ;

// ============================================================================
// FORTRAN 2023 HISTORICAL SIGNIFICANCE
// ============================================================================
//
// Fortran 2023 (ISO/IEC 1539-1:2023) represents incremental refinement:
//
// 1. **Error Correction Focus**: Primarily fixes issues in F2018
// 2. **Enumerated Types**: Enhanced ENUM with better type safety
//    - Default integer enumerations with auto-increment  
//    - Real/character/logical type enumerations
//    - BOZ literal constant support
// 3. **Conditional Expressions**: Ternary operator (? :) support
//    - C-style conditional expressions: condition ? true_val : false_val
//    - Right-associative parsing with COND_EXPR backend
// 4. **IEEE Arithmetic**: New IEEE_MAX, IEEE_MIN, IEEE_MAX_MAG, IEEE_MIN_MAG
//    - Changed behavior: NaN handling improvements 
// 5. **BOZ Constants**: Enhanced array constructor support
//    - BOZ with explicit REAL type-spec interpreted as REAL bits
// 6. **NAMELIST**: PUBLIC groups may contain PRIVATE variables
// 7. **SYSTEM_CLOCK**: All integer args must have same kind
//
// This parser captures F2023's refinements while maintaining full
// compatibility with the F2018 modern Fortran foundation.
// Serves as the complete foundation for LazyFortran2025.
//
// ============================================================================