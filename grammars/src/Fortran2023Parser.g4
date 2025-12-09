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
    : ENUM COMMA BIND LPAREN C RPAREN (DOUBLE_COLON IDENTIFIER)? NEWLINE
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
    : END ENUM IDENTIFIER? NEWLINE
    ;

// Override F2008 end-enum-stmt to support F2023 optional type name
// ISO/IEC 1539-1:2023 R771: end-enum-type-stmt is END ENUM [ enum-type-name ]
// In F2023, END ENUM may include the optional enum-type-name for clarity
end_enum_stmt_f2008
    : END ENUM identifier_or_keyword? NEWLINE
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
// F2023 type specification (enhanced with TYPEOF/CLASSOF from Section 7.3.2.1)
type_spec_f2023
    : INTEGER
    | REAL
    | COMPLEX
    | LOGICAL
    | CHARACTER
    | DOUBLE PRECISION
    | TYPE LPAREN IDENTIFIER RPAREN
    | typeof_type_spec_f2023       // NEW in F2023 (R703)
    | classof_type_spec_f2023      // NEW in F2023 (R704)
    ;

// ============================================================================
// TYPEOF/CLASSOF TYPE SPECIFIERS (ISO/IEC 1539-1:2023 Section 7.3.2.1)
// ============================================================================
//
// J3/22-007 Section 7.3.2.1: TYPEOF and CLASSOF type specifiers
// These allow declaring variables with the same type as another entity.
//
// R703: typeof-type-spec is TYPEOF ( data-ref )
// R704: classof-type-spec is CLASSOF ( data-ref )
//
// TYPEOF: declares with same declared type as data-ref
// CLASSOF: declares with same dynamic type as data-ref (polymorphic)
//

// J3/22-007 R703: typeof-type-spec
// typeof-type-spec is TYPEOF ( data-ref )
// Declares entity with same declared type as data-ref
typeof_type_spec_f2023
    : TYPEOF LPAREN data_ref_f2023 RPAREN
    ;

// J3/22-007 R704: classof-type-spec
// classof-type-spec is CLASSOF ( data-ref )
// Declares entity with same dynamic type as data-ref (polymorphic context)
classof_type_spec_f2023
    : CLASSOF LPAREN data_ref_f2023 RPAREN
    ;

// J3/22-007 R611: data-ref (simplified for TYPEOF/CLASSOF context)
// data-ref is part-ref [ % part-ref ]...
// Simplified to handle common cases: variable names, array elements,
// structure components
data_ref_f2023
    : IDENTIFIER (LPAREN section_subscript_list_f2023 RPAREN)?
      (PERCENT data_ref_f2023)?
    ;

// J3/22-007 R619: section-subscript-list
// section-subscript-list is section-subscript [ , section-subscript ]...
section_subscript_list_f2023
    : section_subscript_f2023 (COMMA section_subscript_f2023)*
    ;

// J3/22-007 R620: section-subscript
// section-subscript is subscript | subscript-triplet | vector-subscript
// Simplified to handle common subscript patterns
section_subscript_f2023
    : expr_f2023                                    // Simple subscript
    | expr_f2023? COLON expr_f2023? (COLON expr_f2023)?  // Triplet
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

// ============================================================================
// IDENTIFIER OR KEYWORD OVERRIDE (F2023 Extension)
// ============================================================================
// F2023 adds IEEE_MAX, IEEE_MIN, etc. tokens that can be used as function names.
// Override F2008 identifier_or_keyword to include F2023-specific tokens.
identifier_or_keyword
    : IDENTIFIER
    | VALUE        // VALUE can be used as an identifier when not in C-binding context
    | NAME         // NAME can be used as an identifier
    | RESULT       // RESULT can be used as a variable name
    | SUM_INTRINSIC  // SUM can be used as a variable/result name
    | ID           // ID can be used as a variable name (common identifier)
    | DATA         // DATA can be used as a variable name (legacy keyword)
    | KIND         // KIND can be used as parameter name in type instantiation
    | LEN          // LEN can be used as parameter name in character declarations
    | TRIM_INTRINSIC  // TRIM can be used as function name
    | SIZE         // SIZE can be used as function name (F90 token)
    | SHAPE_INTRINSIC  // SHAPE can be used as a type name
    | STAT         // STAT can be used as variable name in ALLOCATE
    | ERRMSG       // ERRMSG can be used as variable name in ALLOCATE
    | SOURCE       // SOURCE can be used as variable name
    | MOLD         // MOLD can be used as variable name
    | UNIT         // UNIT can be used as variable name in I/O
    | IOSTAT       // IOSTAT can be used as variable name
    | FILE         // FILE can be used as variable name in I/O
    | ACCESS       // ACCESS can be used as variable name in I/O
    | FORM         // FORM can be used as variable name in I/O
    | STATUS       // STATUS can be used as variable name in I/O
    | BLANK        // BLANK can be used as variable name in I/O
    | POSITION     // POSITION can be used as variable name in I/O
    | ACTION       // ACTION can be used as variable name in I/O
    | DELIM        // DELIM can be used as variable name in I/O
    | PAD          // PAD can be used as variable name in I/O
    | RECL         // RECL can be used as variable name in I/O
    | IOMSG        // IOMSG can be used as variable name in I/O
    | ASYNCHRONOUS // ASYNCHRONOUS can be used as variable name in I/O
    // F2008-specific tokens inherited
    | IMAGES       // IMAGES can be used as variable name (SYNC IMAGES keyword)
    | ALL          // ALL can be used as variable name (SYNC ALL keyword)
    | MEMORY       // MEMORY can be used as variable name (SYNC MEMORY keyword)
    | CONCURRENT   // CONCURRENT can be used as variable name (DO CONCURRENT)
    | CONTIGUOUS   // CONTIGUOUS can be used as variable name
    | SUBMODULE    // SUBMODULE can be used as a name
    | BLOCK        // BLOCK can be used as variable name
    | ERROR_STOP   // ERROR_STOP is compound keyword
    // F2023-specific tokens
    | IEEE_MAX     // IEEE_MAX can be used as function name
    | IEEE_MIN     // IEEE_MIN can be used as function name
    | IEEE_MAX_MAG // IEEE_MAX_MAG can be used as function name
    | IEEE_MIN_MAG // IEEE_MIN_MAG can be used as function name
    | ENUMERATOR   // ENUMERATOR can be used as a name
    | PENDING      // PENDING can be used as enumerator name
    | ENUM         // ENUM can be used as a name
    // F2023 degree-based trigonometric intrinsics (Section 16.9)
    | ACOSD        // ACOSD can be used as function name
    | ASIND        // ASIND can be used as function name
    | ATAND        // ATAND can be used as function name
    | ATAN2D       // ATAN2D can be used as function name
    | COSD         // COSD can be used as function name
    | SIND         // SIND can be used as function name
    | TAND         // TAND can be used as function name
    // F2023 pi-scaled trigonometric intrinsics (Section 16.9)
    | ACOSPI       // ACOSPI can be used as function name
    | ASINPI       // ASINPI can be used as function name
    | ATANPI       // ATANPI can be used as function name
    | ATAN2PI      // ATAN2PI can be used as function name
    | COSPI        // COSPI can be used as function name
    | SINPI        // SINPI can be used as function name
    | TANPI        // TANPI can be used as function name
    // F2023 TYPEOF/CLASSOF type inference (Section 7.3.2.1)
    | TYPEOF       // TYPEOF can be used as identifier
    | CLASSOF      // CLASSOF can be used as identifier
    ;

// ============================================================================
// EXPRESSION OVERRIDE (F2023 Conditional Expressions)
// ============================================================================
// ISO/IEC 1539-1:2023 Section 10.1.5: Conditional expressions
// J3/22-007 R1020: conditional-expr is ( conditional-test ? consequent
//                  [ : conditional-test ? consequent ]... : consequent )
// Override F2003 expr to include conditional expression (ternary operator)
expr_f2003
    : expr_f2003 DOT_OR expr_f2003            // Logical OR (.or.)
    | expr_f2003 DOT_AND expr_f2003           // Logical AND (.and.)
    | DOT_NOT expr_f2003                      // Logical NOT (.not.)
    | expr_f2003 (GT | GT_OP) expr_f2003      // Greater than
    | expr_f2003 (LT | LT_OP) expr_f2003      // Less than
    | expr_f2003 (GE | GE_OP) expr_f2003      // Greater than or equal
    | expr_f2003 (LE | LE_OP) expr_f2003      // Less than or equal
    | expr_f2003 (EQ | EQ_OP) expr_f2003      // Equal
    | expr_f2003 (NE | NE_OP) expr_f2003      // Not equal
    | expr_f2003 CONCAT expr_f2003            // String concatenation (//)
    | expr_f2003 POWER expr_f2003             // Exponentiation
    | expr_f2003 (MULTIPLY | SLASH) expr_f2003  // Multiply/divide
    | expr_f2003 (PLUS | MINUS) expr_f2003    // Add/subtract
    | MINUS expr_f2003                        // Unary minus
    | PLUS expr_f2003                         // Unary plus
    | expr_f2003 QUESTION expr_f2003 COLON expr_f2003  // F2023 conditional (ternary)
    | LPAREN expr_f2003 QUESTION expr_f2003 COLON expr_f2003 RPAREN  // Parenthesized
    | expr_f90                                // Inherit F90 expressions
    | primary                                 // F2003 primary
    ;

// ============================================================================
// TYPE DECLARATION STATEMENT OVERRIDE (F2023 TYPEOF/CLASSOF)
// ============================================================================
// ISO/IEC 1539-1:2023 Section 7.3.2.1: TYPEOF and CLASSOF type specifiers
// Override F2003 type_declaration_stmt to add TYPEOF/CLASSOF support
//
// R703: typeof-type-spec is TYPEOF ( data-ref )
// R704: classof-type-spec is CLASSOF ( data-ref )

// Type declaration statement with F2023 TYPEOF/CLASSOF support
// Overrides F2003 type_declaration_stmt
type_declaration_stmt
    : INTEGER kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | REAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | LOGICAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | CHARACTER char_selector_extended? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | c_interop_type (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | TYPE LPAREN derived_type_spec RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    | CLASS LPAREN type_spec_or_star RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    // F2023 TYPEOF/CLASSOF type specifiers (Section 7.3.2.1)
    | TYPEOF LPAREN data_ref_f2023 RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    | CLASSOF LPAREN data_ref_f2023 RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    ;