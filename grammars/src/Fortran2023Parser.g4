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
// R1020: conditional-expr is ( conditional-test ? consequent
//        [ : conditional-test ? consequent ]... : consequent )
// R1021: conditional-test is scalar-logical-expr
// R1022: consequent is scalar-expr
// R1023: conditional-consequent-expr is conditional-test ? consequent
// R1024: cond-else-expr is : consequent
//
// The conditional expression MUST be parenthesized per ISO standard.
// Supports chained conditionals: ( cond1 ? val1 : cond2 ? val2 : default )
//

// J3/22-007 R1020: conditional-expr
// conditional-expr is ( conditional-test ? consequent
//                      [ : conditional-test ? consequent ]... : consequent )
// ISO/IEC 1539-1:2023 Section 10.1.5 requires parentheses around the
// entire expression. Chained conditionals are supported via the
// conditional_chain_f2023 rule.
conditional_expr_f2023
    : LPAREN conditional_chain_f2023 RPAREN
    ;

// J3/22-007 R1020: Chained conditional expression body
// Supports: cond1 ? val1 : cond2 ? val2 : ... : default
// The chain consists of one or more conditional-consequent pairs
// followed by a final else consequent.
conditional_chain_f2023
    : conditional_consequent_f2023 (conditional_consequent_f2023)* cond_else_f2023
    ;

// J3/22-007 R1023: conditional-consequent-expr
// conditional-consequent-expr is conditional-test ? consequent
conditional_consequent_f2023
    : conditional_test_f2023 QUESTION consequent_f2023 COLON
    ;

// J3/22-007 R1024: cond-else-expr (final else branch)
// cond-else-expr is : consequent (the colon is part of the preceding rule)
cond_else_f2023
    : consequent_f2023
    ;

// J3/22-007 R1021: conditional-test
// conditional-test is scalar-logical-expr
// Uses expr_f2003 since it can evaluate to a logical value
conditional_test_f2023
    : expr_f2003
    ;

// J3/22-007 R1022: consequent
// consequent is scalar-expr
// Uses expr_f2003 for the value expression
consequent_f2023
    : expr_f2003
    ;

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
    | string_intrinsic_stmt_f2023  // NEW in F2023 (Section 16.9.180, 16.9.197)
    | notify_wait_stmt_f2023       // NEW in F2023 (Section 11.6, R1179)
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
// STRING INTRINSICS (ISO/IEC 1539-1:2023 Section 16.9)
// ============================================================================
//
// J3/22-007 Section 16.9.180: SPLIT subroutine
// J3/22-007 Section 16.9.197: TOKENIZE subroutine
//
// These intrinsics provide string tokenization capabilities new in F2023.
//

// ISO/IEC 1539-1:2023 Section 16.9.180: SPLIT
// SPLIT(STRING, SET, TOKENS [, SEPARATOR])
// Splits STRING into tokens using characters in SET as delimiters.
// Arguments:
//   STRING (IN): Character scalar to split
//   SET (IN): Character scalar containing delimiter characters
//   TOKENS (OUT): Allocatable array of character strings containing tokens
//   SEPARATOR (OUT, optional): Allocatable array of character scalars
split_stmt_f2023
    : CALL SPLIT LPAREN
      expr_f2003 COMMA                           // STRING argument
      expr_f2003 COMMA                           // SET argument
      expr_f2003                                 // TOKENS argument
      (COMMA expr_f2003)?                        // SEPARATOR (optional)
      RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2023 Section 16.9.197: TOKENIZE
// TOKENIZE(STRING, SET [, QUOTE, BACK])
// Returns a token iterator for tokenizing STRING.
// Arguments:
//   STRING (IN): Character scalar to tokenize
//   SET (IN): Character scalar containing delimiter characters
//   QUOTE (IN, optional): Character scalar containing quote characters
//   BACK (IN, optional): Logical scalar for backward tokenization
tokenize_stmt_f2023
    : CALL TOKENIZE LPAREN
      expr_f2003 COMMA                           // STRING argument
      expr_f2003                                 // SET argument
      (COMMA tokenize_spec_f2023)*               // Optional QUOTE, BACK specs
      RPAREN NEWLINE
    ;

// Optional arguments for TOKENIZE (keyword or positional)
tokenize_spec_f2023
    : IDENTIFIER EQUALS expr_f2003              // Keyword argument (QUOTE=, BACK=)
    | expr_f2003                                // Positional argument
    ;

// ============================================================================
// STRING INTRINSICS INTEGRATION (F2023)
// ============================================================================
// Wire string intrinsics into executable_stmt_f2023 to allow usage in
// executable context.

// F2023 string intrinsic subroutine call
string_intrinsic_stmt_f2023
    : split_stmt_f2023
    | tokenize_stmt_f2023
    ;

// ============================================================================
// NOTIFY WAIT STATEMENT (ISO/IEC 1539-1:2023 Section 11.6)
// ============================================================================
//
// J3/22-007 Section 11.6: Coarray synchronization
// R1179: notify-wait-stmt is NOTIFY WAIT ( notify-variable
//        [ , event-wait-spec-list ] )
//
// The NOTIFY WAIT statement waits for a notification from another image.
// The notify-variable must be a coarray of NOTIFY_TYPE.
// The event-wait-spec-list allows UNTIL_COUNT=, STAT=, ERRMSG= specifiers.
//
// ISO/IEC 1539-1:2023 Section 16.5.9: NOTIFY_TYPE derived type
// NOTIFY_TYPE is provided by ISO_FORTRAN_ENV for coarray notification.
//

// J3/22-007 R1179: notify-wait-stmt
// notify-wait-stmt is NOTIFY WAIT ( notify-variable [ , event-wait-spec-list ] )
notify_wait_stmt_f2023
    : NOTIFY_WAIT LPAREN notify_variable_f2023
      (COMMA event_wait_spec_list)? RPAREN NEWLINE
    ;

// J3/22-007 R1179: notify-variable
// notify-variable is a coarray variable of type NOTIFY_TYPE
notify_variable_f2023
    : IDENTIFIER                   // Variable of NOTIFY_TYPE
    ;

// NOTIFY_TYPE declaration statement
// ISO/IEC 1539-1:2023 Section 16.5.9: NOTIFY_TYPE derived type
notify_type_declaration_stmt_f2023
    : TYPE LPAREN NOTIFY_TYPE RPAREN DOUBLE_COLON entity_decl_list NEWLINE
    ;

// ============================================================================
// DO CONCURRENT REDUCE LOCALITY SPECIFIER (ISO/IEC 1539-1:2023 Section 11.1.7)
// ============================================================================
//
// J3/22-007 Section 11.1.7.5: Additional semantics for DO CONCURRENT constructs
// R1130: locality-spec adds REDUCE ( reduce-operation : variable-name-list )
//
// The REDUCE locality specifier declares reduction variables for DO CONCURRENT.
// Each iteration has a separate reduction variable initialized to the identity
// value for the operation. At the end of the construct, the values are combined
// using the reduce-operation.
//
// reduce-operation is one of:
//   + | * | .AND. | .OR. | .EQV. | .NEQV. | MAX | MIN | IAND | IEOR | IOR
//

// ISO/IEC 1539-1:2023 R1129: concurrent-locality (F2023 override)
// Adds REDUCE locality specifier to F2018 locality specs
concurrent_locality
    : LOCAL LPAREN variable_name_list RPAREN
    | LOCAL_INIT LPAREN variable_name_list RPAREN
    | SHARED LPAREN variable_name_list RPAREN
    | DEFAULT LPAREN NONE RPAREN
    | reduce_locality_spec_f2023                  // NEW in F2023
    ;

// ISO/IEC 1539-1:2023 R1130: reduce-locality-spec
// reduce-locality-spec is REDUCE ( reduce-operation : variable-name-list )
reduce_locality_spec_f2023
    : REDUCE LPAREN reduce_operation_f2023 COLON variable_name_list RPAREN
    ;

// ISO/IEC 1539-1:2023: reduce-operation
// reduce-operation is + | * | .AND. | .OR. | .EQV. | .NEQV. | MAX | MIN |
//                     IAND | IEOR | IOR
// Note: MAX, MIN are parsed as IDENTIFIERs since they don't have dedicated tokens.
// IAND, IEOR, IOR have dedicated tokens from F95 lexer.
reduce_operation_f2023
    : PLUS                            // Sum reduction
    | MULTIPLY                        // Product reduction
    | DOT_AND                         // Logical AND reduction
    | DOT_OR                          // Logical OR reduction
    | DOT_EQV                         // Logical equivalence reduction
    | DOT_NEQV                        // Logical non-equivalence reduction
    | IDENTIFIER                      // MAX, MIN (and potentially other names)
    | IAND_INTRINSIC                  // Bitwise AND reduction
    | IEOR_INTRINSIC                  // Bitwise exclusive OR reduction
    | IOR_INTRINSIC                   // Bitwise inclusive OR reduction
    ;

// ============================================================================
// EXECUTABLE CONSTRUCT OVERRIDE (F2023 Extension)
// ============================================================================
// Override F2018 executable_construct to include F2023 NOTIFY WAIT statement

// ISO/IEC 1539-1:2023 R514: executable-construct
// Enhanced executable construct for F2023 (adds NOTIFY WAIT, includes I/O)
executable_construct_f2018
    : assignment_stmt                 // Inherit from F2003
    | call_stmt                       // Inherit from F2003
    | print_stmt                      // Inherit from F2003
    | stop_stmt_f2018                 // Enhanced in F2018 (R1160)
    | error_stop_stmt_f2018           // Enhanced in F2018 (R1161)
    | select_type_construct           // Inherit from F2003
    | select_rank_construct           // NEW in F2018 (R1148-R1151)
    | associate_construct             // Inherit from F2003
    | block_construct_f2008           // Inherit from F2008
    | allocate_stmt_f2008             // Inherit from F2008
    | collective_subroutine_call      // NEW in F2018 (Section 16.9.46-50)
    | team_construct                  // NEW in F2018 (teams: R1111-R1115, R1175-R1178)
    | event_construct                 // NEW in F2018 (events: R1170-R1173)
    | notify_wait_stmt_f2023          // NEW in F2023 (Section 11.6, R1179)
    | sync_construct                  // Inherit from F2008
    | wait_stmt                       // Inherit from F2003
    | flush_stmt                      // Inherit from F2003
    | open_stmt                       // I/O: OPEN (Section 12.5.6)
    | close_stmt                      // I/O: CLOSE (Section 12.5.7)
    | write_stmt                      // I/O: WRITE (Section 12.6.2)
    | read_stmt                       // I/O: READ (Section 12.6.1)
    | inquire_stmt                    // I/O: INQUIRE (Section 12.10.3)
    | if_construct                    // Inherit from F95
    | do_construct_f2018              // Enhanced in F2018 (R1119-R1132, DO CONCURRENT)
    | select_case_construct           // Inherit from F95
    | type_declaration_stmt_f2018     // F2018 allows mixed declarations
    | executable_construct_f2008      // Inherit F2008 constructs
    ;

// ============================================================================
// USE STATEMENT ONLY LIST OVERRIDE (F2023 Extension)
// ============================================================================
// Override F2018 only_item to include NOTIFY_TYPE for use in
// iso_fortran_env imports.

// ISO/IEC 1539-1:2023 R1412: only
// Enhanced to support NOTIFY_TYPE from iso_fortran_env
only_item_f2018
    : IDENTIFIER (POINTER_ASSIGN only_item_target_f2018)?
    | c_interop_type
    | OPERATOR LPAREN operator_token RPAREN
    | NOTIFY_TYPE                     // NEW in F2023 (Section 16.5.9)
    ;

// ============================================================================
// DERIVED TYPE SPEC OVERRIDE (F2023 Extension)
// ============================================================================
// Override F2003 derived_type_spec to include NOTIFY_TYPE for use in
// type declarations like type(notify_type) :: var

// ISO/IEC 1539-1:2023 R455: derived-type-spec
// Enhanced to support NOTIFY_TYPE from iso_fortran_env
derived_type_spec
    : IDENTIFIER                                        // Basic type name
    | IDENTIFIER LPAREN type_param_spec_list RPAREN     // Parameterized type
    | c_interop_type                                    // C interop types
    | NOTIFY_TYPE                                       // F2023 Section 16.5.9
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
    // F2023 string intrinsics (Section 16.9)
    | SPLIT        // SPLIT can be used as subroutine name
    | TOKENIZE     // TOKENIZE can be used as subroutine name
    // F2023 NOTIFY WAIT synchronization (Section 11.6)
    | NOTIFY_WAIT  // NOTIFY WAIT can be used as statement keyword
    | NOTIFY_TYPE  // NOTIFY_TYPE can be used as type name
    // F2023 I/O specifiers (Section 12.5.6.15)
    | LEADING_ZERO // LEADING_ZERO can be used as variable name in I/O
    ;

// ============================================================================
// EXPRESSION OVERRIDE (F2023 Conditional Expressions)
// ============================================================================
// ISO/IEC 1539-1:2023 Section 10.1.5: Conditional expressions
// J3/22-007 R1020: conditional-expr is ( conditional-test ? consequent
//                  [ : conditional-test ? consequent ]... : consequent )
// Override F2003 expr to include conditional expression in F2018 expression
// hierarchy. The conditional_expr_f2023 rule handles ISO-compliant syntax
// with mandatory parentheses and support for chained conditionals.
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
    | conditional_expr_f2023                  // F2023 conditional (Section 10.1.5)
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

// ============================================================================
// LEADING_ZERO I/O SPECIFIER OVERRIDE (ISO/IEC 1539-1:2023 Section 12.5.6.15)
// ============================================================================
//
// J3/22-007 Section 12.5.6.15: LEADING_ZERO= specifier
// R1213: io-control-spec includes LEADING_ZERO= scalar-default-char-expr
//
// The LEADING_ZERO specifier controls whether leading zeros are included
// in the output for I, B, O, and Z edit descriptors. Valid values:
// - 'YES': Include leading zeros
// - 'NO': Suppress leading zeros (default behavior)
// - 'PROCESSOR_DEFINED': Use processor-dependent default
//
// This specifier can appear in:
// - OPEN statements (connect-spec) to set default for a unit
// - READ/WRITE statements (io-control-spec) to override per-statement
// - INQUIRE statements (inquire-spec) to query current setting
//

// Override F2003 f2003_io_spec to add F2023 LEADING_ZERO specifier
f2003_io_spec
    : IDENTIFIER EQUALS primary
        // Regular identifier = value (file='test.dat')
    | UNIT EQUALS primary                        // unit=10, unit=*
    | FILE EQUALS primary                        // file='filename'
    | ACCESS EQUALS primary                      // access='stream' (F2003)
    | FORM EQUALS primary                        // form='unformatted'
    | STATUS EQUALS primary                      // status='new'
    | BLANK EQUALS primary                       // blank='null'
    | POSITION EQUALS primary                    // position='rewind'
    | ACTION EQUALS primary                      // action='read'
    | DELIM EQUALS primary                       // delim='apostrophe'
    | PAD EQUALS primary                         // pad='yes'
    | RECL EQUALS primary                        // recl=100
    | IOSTAT EQUALS primary                      // iostat=ios
    | IOMSG EQUALS primary                       // iomsg=msg (F2003)
    | ERR EQUALS primary                         // err=100
    | END EQUALS primary                         // end=200
    | EOR EQUALS primary                         // eor=300
    | ADVANCE EQUALS primary                     // advance='yes'
    | SIZE EQUALS primary                        // size=isize
    | REC EQUALS primary                         // rec=irec
    | ASYNCHRONOUS EQUALS primary                // asynchronous='yes' (F2003)
    | STREAM EQUALS primary                      // stream='yes' (F2003 R905)
    | PENDING EQUALS primary                     // pending=var (F2003 R923)
    | ID EQUALS primary                          // id=id_var (F2003)
    | FMT EQUALS primary                         // fmt=*, fmt=100, fmt='(DT)'
    | LEADING_ZERO EQUALS primary                // leading_zero='yes' (F2023)
    | primary                                    // Positional: *, 10, '(DT)', etc.
    ;

// ============================================================================
// INQUIRE STATEMENT OVERRIDE (ISO/IEC 1539-1:2023 Section 12.10.3)
// ============================================================================
//
// J3/22-007 Section 12.10.3: INQUIRE statement
// R1230: inquire-stmt is INQUIRE ( inquire-spec-list )
//                     or INQUIRE ( IOLENGTH = scalar-int-variable ) output-item-list
//
// F2023 adds LEADING_ZERO= specifier to inquire-spec (Section 12.5.6.15).
//
// Override to use f2003_io_spec_list which includes LEADING_ZERO.

// INQUIRE statement (ISO/IEC 1539-1:2023 R1230)
inquire_stmt
    : INQUIRE LPAREN f2003_io_spec_list RPAREN NEWLINE?
    ;