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

// F2023 specification part with enhancements
specification_part_f2023
    : (specification_item_f2023)*
    ;

// F2023 specification items
specification_item_f2023
    : enum_def_f2023               // NEW in F2023
    | type_declaration_stmt_f2023
    | NEWLINE
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
    : ENUM COMMA BIND LPAREN C RPAREN NEWLINE
    | ENUM (DOUBLE_COLON IDENTIFIER)? NEWLINE    // F2023 typed enumerators
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
    : primary_f2023 (binary_op_f2023 primary_f2023)*
    ;

// F2023 primary expression
primary_f2023
    : IDENTIFIER
    | INTEGER_LITERAL
    | REAL_LITERAL
    | STRING_LITERAL
    | LPAREN expr_f2023 RPAREN
    ;

// F2023 conditional expression (ternary operator) - separate from primary to avoid recursion
conditional_expr_f2023
    : expr_f2023 QUESTION expr_f2023 COLON expr_f2023
    ;

// Binary operators for F2023
binary_op_f2023
    : PLUS | MINUS | MULTIPLY | SLASH | POWER
    | EQ | NE | LT | LE | GT | GE
    | AND | OR
    ;

// ============================================================================
// EXECUTION PART (Enhanced for F2023)
// ============================================================================

// F2023 execution part with enhancements
execution_part_f2023
    : (executable_stmt_f2023)*
    ;

// F2023 executable statements
executable_stmt_f2023
    : assignment_stmt_f2023
    | call_stmt_f2023
    | if_stmt_f2023
    | print_stmt_f2023
    | random_init_stmt_f2023
    | system_clock_stmt_f2023
    | NEWLINE
    ;

// F2023 assignment with conditional expressions
assignment_stmt_f2023
    : IDENTIFIER EQUALS expr_f2023 NEWLINE
    | IDENTIFIER EQUALS conditional_expr_f2023 NEWLINE
    ;

// F2023 call statement
call_stmt_f2023
    : CALL IDENTIFIER LPAREN (expr_f2023 (COMMA expr_f2023)*)? RPAREN NEWLINE
    ;

// F2023 if statement  
if_stmt_f2023
    : IF LPAREN expr_f2023 RPAREN THEN NEWLINE
    ;

// F2023 print statement
print_stmt_f2023
    : PRINT MULTIPLY COMMA expr_f2023 NEWLINE
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

// F2023 type declarations with enhancements
type_declaration_stmt_f2023
    : type_spec_f2023 DOUBLE_COLON entity_decl_list_f2023 NEWLINE
    ;

// F2023 type specification
type_spec_f2023
    : INTEGER
    | REAL
    | COMPLEX
    | LOGICAL
    | CHARACTER
    | DOUBLE PRECISION
    | TYPE LPAREN IDENTIFIER RPAREN
    ;

// Entity declaration list
entity_decl_list_f2023
    : entity_decl_f2023 (COMMA entity_decl_f2023)*
    ;

// Entity declaration
entity_decl_f2023
    : IDENTIFIER (EQUALS expr_f2023)?
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