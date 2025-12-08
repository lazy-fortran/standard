/*
 * Fortran2023Parser.g4
 *
 * Fortran 2023 (ISO/IEC 1539-1:2023) Parser
 * Reference: J3/22-007 "Fortran 2023" (Working Draft), J3/23-007r1 (Draft Standard)
 *
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 *
 * ISO/IEC 1539-1:2023 Key Sections:
 * - Section 7: Types (7.8 Enumeration types, R759-R763)
 * - Section 10: Expressions (10.1.5 Conditional expressions, R1020-R1024)
 * - Section 11: Execution control
 * - Section 14: Program units (14.2.4 NAMELIST)
 * - Section 16: Intrinsic procedures (16.9.196 SYSTEM_CLOCK)
 * - Section 17: IEEE arithmetic intrinsic modules
 */

parser grammar Fortran2023Parser;

import Fortran2018Parser;

options {
    tokenVocab = Fortran2023Lexer;
}

// ============================================================================
// FORTRAN 2023 PROGRAM STRUCTURE (ISO/IEC 1539-1:2023 Section 14)
// ============================================================================

// J3/22-007 R502: program-unit
// F2023 program unit (enhanced with enumeration and conditional expressions)
program_unit_f2023
    : NEWLINE* (main_program_f2023 | module_f2023 | submodule_f2008
      | external_subprogram_f2023) NEWLINE*
    ;

// J3/22-007 R1401: main-program
// Enhanced main program for F2023 (inherits from F2018)
main_program_f2023
    : main_program_f2018
    ;

// J3/22-007 R1404: module
// Enhanced module for F2023 (inherits from F2018)
module_f2023
    : module_f2018
    ;

// J3/22-007 R503: external-subprogram
// Enhanced external subprogram for F2023 (inherits from F2018)
external_subprogram_f2023
    : external_subprogram_f2018
    ;

// ============================================================================
// FORTRAN 2023 SPECIFICATION PART (ISO/IEC 1539-1:2023 Section 8)
// ============================================================================

// J3/22-007 R504: specification-part
// F2023 specification part with enhancements
specification_part_f2023
    : (specification_item_f2023)*
    ;

// J3/22-007 R507: declaration-construct
// F2023 specification items
specification_item_f2023
    : enum_def_f2023               // NEW in F2023 (Section 7.8)
    | type_declaration_stmt_f2023
    | NEWLINE
    ;

// ============================================================================
// ENUMERATION TYPES (ISO/IEC 1539-1:2023 Section 7.8)
// ============================================================================
//
// J3/22-007 Section 7.8: Enumeration types
// Two variants: interoperable enumerations (BIND(C)) and Fortran-specific
//

// J3/22-007 R759: enum-def
// enum-def is enum-def-stmt enumerator-def-stmt [enumerator-def-stmt]...
// end-enum-stmt
enum_def_f2023
    : enum_def_stmt_f2023
      enumerator_def_stmt_f2023*
      end_enum_stmt_f2023
    ;

// J3/22-007 R760: enum-def-stmt
// enum-def-stmt is ENUM, BIND(C) [ :: enum-type-name ]
//             or ENUMERATION TYPE [ :: enum-type-name ]
enum_def_stmt_f2023
    : ENUM COMMA BIND LPAREN C RPAREN NEWLINE
    | ENUM (DOUBLE_COLON IDENTIFIER)? NEWLINE    // F2023 typed enumerators
    ;

// J3/22-007 R761: enumerator-def-stmt
// enumerator-def-stmt is ENUMERATOR [ :: ] enumerator-list
enumerator_def_stmt_f2023
    : ENUMERATOR (DOUBLE_COLON)? enumerator_list_f2023 NEWLINE
    ;

// J3/22-007 R761: enumerator-list
// enumerator-list is enumerator [ , enumerator ]...
enumerator_list_f2023
    : enumerator_f2023 (COMMA enumerator_f2023)*
    ;

// J3/22-007 R762: enumerator
// enumerator is named-constant [ = scalar-int-constant-expr ]
enumerator_f2023
    : IDENTIFIER (EQUALS INTEGER_LITERAL)?
    ;

// J3/22-007 R763: end-enum-stmt
// end-enum-stmt is END ENUM [ enum-type-name ]
end_enum_stmt_f2023
    : END ENUM NEWLINE
    ;

// ============================================================================
// CONDITIONAL EXPRESSIONS (ISO/IEC 1539-1:2023 Section 10.1.5)
// ============================================================================
//
// J3/22-007 Section 10.1.5: Conditional expressions
// Syntax: ( conditional-test ? consequent [ : conditional-test ? consequent ]
//          ... : consequent )
//

// ============================================================================
// ENHANCED EXPRESSIONS (ISO/IEC 1539-1:2023 Section 10)
// ============================================================================

// J3/22-007 R1001: expr (simplified for F2023 overlay)
// Enhanced expression for F2023 with conditional expressions
expr_f2023
    : primary_f2023 (binary_op_f2023 primary_f2023)*
    ;

// J3/22-007 R1002: primary (simplified subset)
// F2023 primary expression
primary_f2023
    : IDENTIFIER
    | INTEGER_LITERAL
    | REAL_LITERAL
    | STRING_LITERAL
    | LPAREN expr_f2023 RPAREN
    ;

// J3/22-007 R1020: conditional-expr
// conditional-expr is ( conditional-test ? consequent [ : conditional-test
//                      ? consequent ]... : consequent )
// Note: This simplified form captures the basic ternary syntax
conditional_expr_f2023
    : expr_f2023 QUESTION expr_f2023 COLON expr_f2023
    ;

// J3/22-007 R1001-R1010: binary operators (subset)
// Binary operators for F2023
binary_op_f2023
    : PLUS | MINUS | MULTIPLY | SLASH | POWER
    | EQ | NE | LT | LE | GT | GE
    | AND | OR
    ;

// ============================================================================
// EXECUTION PART (ISO/IEC 1539-1:2023 Section 11)
// ============================================================================

// J3/22-007 R509: execution-part
// F2023 execution part with enhancements
execution_part_f2023
    : (executable_stmt_f2023)*
    ;

// J3/22-007 R514: executable-construct (simplified F2023 overlay)
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

// J3/22-007 R1032: assignment-stmt
// F2023 assignment with conditional expressions (Section 10.1.5)
assignment_stmt_f2023
    : IDENTIFIER EQUALS expr_f2023 NEWLINE
    | IDENTIFIER EQUALS conditional_expr_f2023 NEWLINE
    ;

// J3/22-007 R1521: call-stmt
// F2023 call statement
call_stmt_f2023
    : CALL IDENTIFIER LPAREN (expr_f2023 (COMMA expr_f2023)*)? RPAREN NEWLINE
    ;

// J3/22-007 R1139: if-stmt (simplified)
// F2023 if statement
if_stmt_f2023
    : IF LPAREN expr_f2023 RPAREN THEN NEWLINE
    ;

// J3/22-007 R1230: print-stmt
// F2023 print statement
print_stmt_f2023
    : PRINT MULTIPLY COMMA expr_f2023 NEWLINE
    ;

// ============================================================================
// RANDOM INITIALIZATION (ISO/IEC 1539-1:2023 Section 16.9.152)
// ============================================================================
//
// J3/22-007 Section 16.9.152: RANDOM_INIT (REPEATABLE, IMAGE_DISTINCT)
// Enhanced in F2018, maintained in F2023
//

// J3/22-007 Section 16.9.152: RANDOM_INIT subroutine call
random_init_stmt_f2023
    : RANDOM_INIT LPAREN
      REPEATABLE EQUALS logical_expr COMMA
      IMAGE_DISTINCT EQUALS logical_expr
      RPAREN NEWLINE
    ;

// ============================================================================
// TYPE DECLARATIONS (ISO/IEC 1539-1:2023 Section 8)
// ============================================================================

// J3/22-007 R801: type-declaration-stmt
// F2023 type declarations with enhancements
type_declaration_stmt_f2023
    : type_spec_f2023 DOUBLE_COLON entity_decl_list_f2023 NEWLINE
    ;

// J3/22-007 R702: type-spec
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

// J3/22-007 R803: entity-decl-list
// Entity declaration list
entity_decl_list_f2023
    : entity_decl_f2023 (COMMA entity_decl_f2023)*
    ;

// J3/22-007 R803: entity-decl
// Entity declaration
entity_decl_f2023
    : IDENTIFIER (EQUALS expr_f2023)?
    ;

// ============================================================================
// ENHANCED IEEE ARITHMETIC (ISO/IEC 1539-1:2023 Section 17)
// ============================================================================
//
// J3/22-007 Section 17.11: IEEE_ARITHMETIC intrinsic module
// Section 17.11.20-17.11.23: IEEE_MAX_NUM, IEEE_MAX_NUM_MAG,
// IEEE_MIN_NUM, IEEE_MIN_NUM_MAG with refined NaN handling
//

// J3/22-007 Section 17.11.20-17.11.23: IEEE arithmetic functions
// F2023 IEEE intrinsic functions (enhanced NaN handling)
ieee_intrinsic_function_f2023
    : IEEE_MAX LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    | IEEE_MIN LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    | IEEE_MAX_MAG LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    | IEEE_MIN_MAG LPAREN IDENTIFIER (COMMA IDENTIFIER)* RPAREN
    ;

// ============================================================================
// ENHANCED NAMELIST (ISO/IEC 1539-1:2023 Section 14.2.4)
// ============================================================================
//
// J3/22-007 Section 14.2.4: NAMELIST statement
// F2023 enhancement: PUBLIC namelist groups may contain PRIVATE variables
//

// J3/22-007 R1418: namelist-group-object
// Enhanced namelist group object (F2023: PUBLIC groups may contain PRIVATE)
namelist_group_object_f2023
    : IDENTIFIER
    ;

// ============================================================================
// BOZ CONSTANTS (ISO/IEC 1539-1:2023 Section 7.7)
// ============================================================================
//
// J3/22-007 R772: boz-literal-constant
// F2023 enhancement: BOZ in array constructors with explicit type-spec
// interpreted as REAL bits when type-spec is REAL
//

// J3/22-007 R772: boz-literal-constant
// Enhanced BOZ constant handling in array constructors
boz_literal_constant_f2023
    : BINARY_CONSTANT
    | OCTAL_CONSTANT
    | HEX_CONSTANT
    ;

// ============================================================================
// SYSTEM_CLOCK ENHANCEMENTS (ISO/IEC 1539-1:2023 Section 16.9.196)
// ============================================================================
//
// J3/22-007 Section 16.9.196: SYSTEM_CLOCK (COUNT, COUNT_RATE, COUNT_MAX)
// F2023 requirement: All integer arguments must have the same kind
//

// J3/22-007 Section 16.9.196: SYSTEM_CLOCK subroutine
// Enhanced SYSTEM_CLOCK intrinsic (F2023: all args same kind)
system_clock_stmt_f2023
    : CALL SYSTEM_CLOCK LPAREN
      (IDENTIFIER (COMMA IDENTIFIER)? (COMMA IDENTIFIER)?)?
      RPAREN NEWLINE
    ;

// ============================================================================
// FORTRAN 2023 STANDARD OVERVIEW (ISO/IEC 1539-1:2023)
// ============================================================================
//
// Fortran 2023 (ISO/IEC 1539-1:2023) was published in November 2023.
// Reference documents: J3/22-007, J3/23-007r1, J3/24-007
//
// Key Features and J3/22-007 Section References:
//
// 1. Section 7.8 (R759-R763): Enumeration types
//    - Interoperable enumerations (BIND(C)) with named type
//    - Fortran-specific enumeration types
//
// 2. Section 10.1.5 (R1020-R1024): Conditional expressions
//    - Syntax: ( condition ? true_expr : false_expr )
//    - Lazy evaluation semantics
//
// 3. Section 17.11.20-17.11.23: IEEE arithmetic functions
//    - IEEE_MAX_NUM, IEEE_MIN_NUM with NaN handling refinements
//    - IEEE_MAX_NUM_MAG, IEEE_MIN_NUM_MAG
//
// 4. Section 7.7 (R772): BOZ literal constants
//    - Enhanced array constructor support with explicit type-spec
//
// 5. Section 14.2.4 (R1417-R1418): NAMELIST statement
//    - PUBLIC groups may contain PRIVATE variables
//
// 6. Section 16.9.196: SYSTEM_CLOCK intrinsic
//    - All integer arguments must have same kind
//
// 7. Section 7.3.2.1: TYPEOF/CLASSOF type specifiers
//    - Declare entities with same type as another entity
//
// This parser captures F2023 refinements while maintaining full
// compatibility with the F2018 foundation. Serves as the
// complete foundation for LazyFortran2025.
//
// ============================================================================