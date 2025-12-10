/*
 * Fortran2008Parser.g4
 *
 * Fortran 2008 - Enhanced Parallel Programming Revolution
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 *
 * Reference: ISO/IEC 1539-1:2010 (Fortran 2008 International Standard)
 *            J3/10-007 (Fortran 2008 final text)
 *
 * This parser implements syntax rules for Fortran 2008 as defined in
 * ISO/IEC 1539-1:2010, building on the Fortran 2003 parser with:
 * - Coarrays and image control (Sections 2.4.7, 5.3.6, 6.6, 8.5)
 * - Submodules and separate module procedures (Section 11.2.3)
 * - DO CONCURRENT construct (Section 8.1.6.6)
 * - BLOCK construct enhancements (Section 8.1.4)
 * - CONTIGUOUS attribute (Section 5.3.7)
 * - ERROR STOP statement (Section 8.4)
 * - New intrinsic procedures (Section 13.7)
 * - Enhanced ALLOCATE with coarray support (Section 6.7.1)
 *
 * INHERITANCE ARCHITECTURE (IN THIS REPO):
 * FORTRAN / FORTRANII / FORTRAN66 / FORTRAN77
 *   -> Fortran90Parser
 *   -> Fortran95Parser
 *   -> Fortran2003Parser
 *   -> Fortran2008Parser (this file)
 *   -> F2018+ standards
 */

parser grammar Fortran2008Parser;

import Fortran2003Parser;

options {
    tokenVocab = Fortran2008Lexer;
}

// ============================================================================
// FORTRAN 2008 PROGRAM STRUCTURE (ISO/IEC 1539-1:2010 Section 2.1, 11)
// ============================================================================
// ISO/IEC 1539-1:2010 Section 11 defines program units including the new
// submodule program unit added in F2008.
//
// Section 2.1: High level syntax
// - R201 (program) -> program-unit [program-unit]...
// - R202 (program-unit) -> main-program | external-subprogram | module
//                          | submodule | block-data
//
// F2008 additions:
// - Submodules (Section 11.2.3) for separate compilation of module procedures

// F2008 program unit (ISO/IEC 1539-1:2010 R202)
// Enhanced with submodule support
program_unit_f2008
    : NEWLINE* (main_program_f2008 | module_f2008 | submodule_f2008
      | external_subprogram_f2008) NEWLINE*
    ;

// Main program (ISO/IEC 1539-1:2010 Section 11.1, R1101)
// R1101: main-program -> [program-stmt] [specification-part]
//                        [execution-part] [internal-subprogram-part]
//                        end-program-stmt
// Enhanced with F2008 specification and execution parts
main_program_f2008
    : program_stmt specification_part_f2008? execution_part_f2008?
      internal_subprogram_part_f2003? end_program_stmt
    ;

// Module (ISO/IEC 1539-1:2010 Section 11.2.1, R1104)
// R1104: module -> module-stmt [specification-part] [module-subprogram-part]
//                  end-module-stmt
// Enhanced with F2008 specification part
module_f2008
    : module_stmt specification_part_f2008? module_subprogram_part? end_module_stmt
    ;

// External subprogram (ISO/IEC 1539-1:2010 Section 12.6, R1227)
// R1227: external-subprogram -> function-subprogram | subroutine-subprogram
external_subprogram_f2008
    : function_subprogram_f2008
    | subroutine_subprogram_f2008
    ;

// Function subprogram (ISO/IEC 1539-1:2010 Section 12.6.2.2, R1223)
// R1223: function-subprogram -> function-stmt [specification-part]
//                               [execution-part] [internal-subprogram-part]
//                               end-function-stmt
function_subprogram_f2008
    : function_stmt_f2008 specification_part_f2008? execution_part_f2008?
      internal_subprogram_part_f2003? end_function_stmt
    ;

// Subroutine subprogram (ISO/IEC 1539-1:2010 Section 12.6.2.3, R1231)
// R1231: subroutine-subprogram -> subroutine-stmt [specification-part]
//                                 [execution-part] [internal-subprogram-part]
//                                 end-subroutine-stmt
subroutine_subprogram_f2008
    : subroutine_stmt_f2008 specification_part_f2008? execution_part_f2008?
      internal_subprogram_part_f2003? end_subroutine_stmt
    ;

// Function statement (ISO/IEC 1539-1:2010 Section 12.6.2.2, R1224)
// R1224: function-stmt -> [prefix] FUNCTION function-name
//                         ( [dummy-arg-name-list] ) [suffix]
function_stmt_f2008
    : prefix_f2008? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// Subroutine statement (ISO/IEC 1539-1:2010 Section 12.6.2.3, R1232)
// R1232: subroutine-stmt -> [prefix] SUBROUTINE subroutine-name
//                           [( [dummy-arg-list] )] [proc-language-binding-spec]
subroutine_stmt_f2008
    : prefix_f2008? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)?
      binding_spec? NEWLINE
    ;

// ============================================================================
// PROCEDURE PREFIX (ISO/IEC 1539-1:2010 Section 12.6.2.2, R1225-R1226)
// ============================================================================
// F2008 extends F95 prefix to add MODULE, IMPURE, and NON_RECURSIVE keywords.
// This allows combined prefix specifications for procedure declarations.

// F2008 prefix (ISO/IEC 1539-1:2010 R1225)
// R1225: prefix -> prefix-spec [prefix-spec]...
// F2008 allows multiple prefix specifications in any order
prefix_f2008
    : prefix_spec_f2008+
    ;

// F2008 prefix specification (ISO/IEC 1539-1:2010 R1226)
// R1226: prefix-spec -> declaration-type-spec | ELEMENTAL | IMPURE |
//                       MODULE | PURE | RECURSIVE | NON_RECURSIVE
// F2008 adds MODULE (for separate module procedures), IMPURE (for non-pure
// elemental procedures), and NON_RECURSIVE (explicit non-recursive mark).
prefix_spec_f2008
    : RECURSIVE                     // F90 recursive procedures (Section 12.6.2.4)
    | PURE                          // F95 pure procedures (Section 12.7)
    | ELEMENTAL                     // F95 elemental procedures (Section 12.8)
    | IMPURE                        // F2008 IMPURE (Section 12.7)
    | MODULE                        // F2008 MODULE procedures (Section 12.6.2.5)
    | NON_RECURSIVE                 // F2008 NON_RECURSIVE (Section 12.6.2.2)
    | type_spec                     // Function return type (Section 5.1)
    ;

// ============================================================================
// SUBMODULES (ISO/IEC 1539-1:2010 Section 11.2.3)
// ============================================================================
// Submodules allow separate compilation of module procedures while hiding
// implementation details. A submodule extends a module or another submodule.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R1116: submodule -> submodule-stmt [specification-part]
//                       [module-subprogram-part] end-submodule-stmt
// - R1117: submodule-stmt -> SUBMODULE ( parent-identifier ) submodule-name
// - R1118: parent-identifier -> ancestor-module-name [: parent-submodule-name]
// - R1119: end-submodule-stmt -> END [SUBMODULE [submodule-name]]

// Submodule definition (ISO/IEC 1539-1:2010 R1116)
submodule_f2008
    : submodule_stmt specification_part_f2008? module_subprogram_part?
      end_submodule_stmt
    ;

// Submodule statement (ISO/IEC 1539-1:2010 R1117)
// R1117: submodule-stmt -> SUBMODULE ( parent-identifier ) submodule-name
submodule_stmt
    : SUBMODULE LPAREN parent_identifier RPAREN submodule_identifier NEWLINE
    ;

// End submodule statement (ISO/IEC 1539-1:2010 R1119)
// R1119: end-submodule-stmt -> END [SUBMODULE [submodule-name]]
end_submodule_stmt
    : END_SUBMODULE (submodule_identifier)? NEWLINE
    ;

// Parent identifier (ISO/IEC 1539-1:2010 R1118)
// R1118: parent-identifier -> ancestor-module-name [: parent-submodule-name]
// Extended to support nested submodule hierarchies
parent_identifier
    : IDENTIFIER (COLON IDENTIFIER)*
    ;

// Submodule name
submodule_identifier
    : IDENTIFIER
    ;

// ============================================================================
// SPECIFICATION PART (ISO/IEC 1539-1:2010 Section 2.1, R204)
// ============================================================================
// The specification part contains declarations and attributes.
// R204: specification-part -> [use-stmt]... [import-stmt]... [implicit-part]
//                             [declaration-construct]...
// F2008 adds CONTIGUOUS attribute (Section 5.3.7) and coarray declarations.

// F2008 specification part
specification_part_f2008
    : ((use_stmt | import_stmt | implicit_stmt | declaration_construct_f2008) NEWLINE*)*
    ;

// Declaration construct (ISO/IEC 1539-1:2010 R207)
// R207: declaration-construct -> derived-type-def | entry-stmt | enum-def
//       | format-stmt | interface-block | parameter-stmt
//       | procedure-declaration-stmt | other-specification-stmt
//       | type-declaration-stmt | stmt-function-stmt
// F2008 adds CONTIGUOUS statement (Section 5.3.7, R544)
declaration_construct_f2008
    : derived_type_def_f2003          // Derived types (Section 4.5)
    | enum_def_f2008                  // ENUM definitions (Section 4.6)
    | class_declaration_stmt          // CLASS declarations (Section 4.5.6)
    | procedure_declaration_stmt      // Procedure declarations (Section 12.4.3.2)
    | interface_block                 // Interface blocks (Section 12.4.3)
    | volatile_stmt                   // VOLATILE (Section 5.3.19)
    | protected_stmt                  // PROTECTED (Section 5.3.14)
    | contiguous_stmt                 // CONTIGUOUS (Section 5.3.7) - NEW in F2008
    | type_declaration_stmt_f2008     // Type declarations with F2008 kinds
    | public_stmt                     // PUBLIC (Section 5.3.10) - Inherited from F90
    | private_stmt                    // PRIVATE (Section 5.3.10) - Inherited from F90
    | namelist_stmt                   // NAMELIST (Section 5.4)
    | common_stmt                     // COMMON (Section 5.7.2)
    | parameter_stmt                  // PARAMETER (Section 5.4.11)
    | data_stmt                       // DATA (Section 5.4.7)
    | declaration_construct_f2003     // Inherit F2003 declarations
    ;

// ============================================================================
// ENUM DEFINITION (ISO/IEC 1539-1:2010 Section 4.6)
// ============================================================================
// F2003 introduced ENUM for C interoperability with BIND(C).
// F2023 extends with typed enumerators.

// Enum definition (ISO/IEC 1539-1:2010 R460)
enum_def_f2008
    : enum_def_stmt_f2008
      enumerator_def_stmt_f2008*
      end_enum_stmt_f2008
    ;

// Enum definition statement (ISO/IEC 1539-1:2010 R461)
// R461: enum-def-stmt -> ENUM, BIND(C) [ :: type-name ]
// Note: type-name can be a keyword-like identifier (e.g., "status")
enum_def_stmt_f2008
    : ENUM COMMA BIND LPAREN C RPAREN (DOUBLE_COLON identifier_or_keyword)? NEWLINE
    | ENUM (DOUBLE_COLON identifier_or_keyword)? NEWLINE
    ;

// Enumerator definition statement (ISO/IEC 1539-1:2010 R462)
// R462: enumerator-def-stmt -> ENUMERATOR [::] enumerator-list
// Enumerator names can also be keyword-like identifiers
enumerator_def_stmt_f2008
    : ENUMERATOR (DOUBLE_COLON)? enumerator_list_f2008 NEWLINE
    ;

// Enumerator list
enumerator_list_f2008
    : enumerator_f2008 (COMMA enumerator_f2008)*
    ;

// Enumerator (ISO/IEC 1539-1:2010 R463)
// R463: enumerator -> named-constant [ = scalar-int-constant-expr ]
enumerator_f2008
    : identifier_or_keyword (EQUALS INTEGER_LITERAL)?
    ;

// End enum statement (ISO/IEC 1539-1:2010 R464)
// R464: end-enum-stmt -> END ENUM
end_enum_stmt_f2008
    : END ENUM NEWLINE
    ;

// ============================================================================
// EXECUTION PART (ISO/IEC 1539-1:2010 Section 2.1, R208)
// ============================================================================
// The execution part contains executable constructs.
// R208: execution-part -> executable-construct [execution-part-construct]...
// R213: executable-construct -> action-stmt | associate-construct | block-construct
//                             | case-construct | critical-construct | do-construct
//                             | forall-construct | if-construct | select-type-construct
//                             | where-construct
// F2008 adds: ERROR STOP (Section 8.4), SYNC statements (Section 8.5),
// DO CONCURRENT (Section 8.1.6.6), enhanced BLOCK (Section 8.1.4)

// F2008 execution part
execution_part_f2008
    : execution_construct_f2008*
    ;

// Execution construct wrapper with newline handling
execution_construct_f2008
    : NEWLINE* executable_construct_f2008 NEWLINE*
    ;

// Executable construct (ISO/IEC 1539-1:2010 R213)
// Enhanced with F2008-specific constructs
executable_construct_f2008
    : assignment_stmt                 // Assignment (Section 7.2)
    | call_stmt                       // CALL (Section 12.5.1)
    | atomic_subroutine_call          // ATOMIC intrinsics (Section 13.7.19-20) - NEW
    | print_stmt                      // PRINT (Section 9.6.3)
    | stop_stmt                       // STOP (Section 8.4)
    | error_stop_stmt                 // ERROR STOP (Section 8.4) - NEW in F2008
    | critical_construct              // CRITICAL (Section 8.1.5) - NEW in F2008
    | lock_stmt                       // LOCK (Section 8.5.6) - NEW in F2008
    | unlock_stmt                     // UNLOCK (Section 8.5.6) - NEW in F2008
    | select_type_construct           // SELECT TYPE (Section 8.1.9)
    | associate_construct             // ASSOCIATE (Section 8.1.3)
    | block_construct_f2008           // BLOCK (Section 8.1.4) - Enhanced in F2008
    | allocate_stmt_f2008             // ALLOCATE (Section 6.7.1) - Enhanced in F2008
    | sync_construct                  // SYNC (Section 8.5) - NEW in F2008
    | wait_stmt                       // WAIT (Section 9.7.1)
    | flush_stmt                      // FLUSH (Section 9.7.2)
    | if_construct                    // IF (Section 8.1.7)
    | do_construct_f2008              // DO (Section 8.1.6) - Enhanced in F2008
    | select_case_construct           // SELECT CASE (Section 8.1.8)
    | where_construct                // WHERE construct (Section 7.5.3)
    | type_declaration_stmt_f2008     // F2008 allows mixed declarations
    | executable_construct            // Inherit F2003 constructs
    ;

// ============================================================================
// COARRAY SUPPORT (ISO/IEC 1539-1:2010 Sections 2.4.7, 5.3.6, 6.6)
// ============================================================================
// Coarrays implement a PGAS (Partitioned Global Address Space) parallel
// programming model. Each image has its own set of coarray data.
//
// Key coarray rules from ISO/IEC 1539-1:2010:
// - R509: coarray-spec -> deferred-coshape-spec-list | explicit-coshape-spec
// - R510: deferred-coshape-spec -> :
// - R511: explicit-coshape-spec -> [[lower-cobound:]upper-cobound,]...*
// - R624: image-selector -> [ cosubscript-list ]

// Coarray specification (ISO/IEC 1539-1:2010 R509)
// Used in declarations to specify coarray bounds
coarray_spec
    : LBRACKET cosubscript_list RBRACKET
    ;

// Cosubscript list (ISO/IEC 1539-1:2010 R625)
// R625: cosubscript-list -> cosubscript [, cosubscript]...
cosubscript_list
    : cosubscript (COMMA cosubscript)*
    | MULTIPLY                        // [*] - any image (deferred)
    ;

// Cosubscript (ISO/IEC 1539-1:2010 R625)
// Individual cosubscript in image selector
cosubscript
    : expr_f2003                      // Expression for specific image
    | expr_f2003 COLON                // lower bound:
    | COLON expr_f2003                // :upper bound
    | expr_f2003 COLON expr_f2003     // lower:upper
    | COLON                           // : (all images)
    ;

// ============================================================================
// IMAGE CONTROL STATEMENTS (ISO/IEC 1539-1:2010 Section 8.5)
// ============================================================================
// Image control statements synchronize execution between images.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R858: sync-all-stmt -> SYNC ALL [(sync-stat-list)]
// - R859: sync-images-stmt -> SYNC IMAGES (image-set [, sync-stat-list])
// - R862: sync-memory-stmt -> SYNC MEMORY [(sync-stat-list)]
// - R860: image-set -> int-expr | *
// - R863: sync-stat -> STAT = stat-variable | ERRMSG = errmsg-variable

// Sync constructs for coarray synchronization
sync_construct
    : sync_all_stmt
    | sync_images_stmt
    | sync_memory_stmt
    ;

// SYNC ALL statement (ISO/IEC 1539-1:2010 Section 8.5.3, R858)
// R858: sync-all-stmt -> SYNC ALL [(sync-stat-list)]
// Synchronizes all images in the current team
sync_all_stmt
    : SYNC ALL (LPAREN sync_stat_list? RPAREN)? NEWLINE
    ;

// SYNC IMAGES statement (ISO/IEC 1539-1:2010 Section 8.5.4, R859)
// R859: sync-images-stmt -> SYNC IMAGES (image-set [, sync-stat-list])
// Synchronizes specified images
sync_images_stmt
    : SYNC IMAGES LPAREN image_set (COMMA sync_stat_list)? RPAREN NEWLINE
    ;

// SYNC MEMORY statement (ISO/IEC 1539-1:2010 Section 8.5.5, R862)
// R862: sync-memory-stmt -> SYNC MEMORY [(sync-stat-list)]
// Memory fence for segment ordering
sync_memory_stmt
    : SYNC MEMORY (LPAREN sync_stat_list? RPAREN)? NEWLINE
    ;

// Image set (ISO/IEC 1539-1:2010 R860)
// R860: image-set -> int-expr | *
image_set
    : MULTIPLY                        // * (all images)
    | expr_f2003                      // Single image or scalar expression
    | LSQUARE expr_f2003 (COMMA expr_f2003)* RSQUARE
    | LBRACKET expr_f2003 (COMMA expr_f2003)* RBRACKET
    ;

// Sync stat list (ISO/IEC 1539-1:2010 R863)
sync_stat_list
    : sync_stat (COMMA sync_stat)*
    ;

// Sync stat (ISO/IEC 1539-1:2010 R863)
// R863: sync-stat -> STAT = stat-variable | ERRMSG = errmsg-variable
sync_stat
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    ;

// ============================================================================
// CRITICAL CONSTRUCT (ISO/IEC 1539-1:2010 Section 8.1.5)
// ============================================================================
// The CRITICAL construct limits execution of a block of statements to one
// image at a time across all images in the current team.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R818: critical-construct -> critical-stmt block end-critical-stmt
// - R819: critical-stmt -> [critical-construct-name :] CRITICAL [(sync-stat-list)]
// - R820: end-critical-stmt -> END CRITICAL [critical-construct-name]

// CRITICAL construct (ISO/IEC 1539-1:2010 R818)
// R818: critical-construct -> critical-stmt block end-critical-stmt
critical_construct
    : critical_stmt
      execution_part_f2008?
      end_critical_stmt
    ;

// CRITICAL statement (ISO/IEC 1539-1:2010 R819)
// R819: critical-stmt -> [critical-construct-name :] CRITICAL [(sync-stat-list)]
critical_stmt
    : (IDENTIFIER COLON)? CRITICAL (LPAREN sync_stat_list? RPAREN)? NEWLINE
    ;

// END CRITICAL statement (ISO/IEC 1539-1:2010 R820)
// R820: end-critical-stmt -> END CRITICAL [critical-construct-name]
end_critical_stmt
    : END_CRITICAL (IDENTIFIER)? NEWLINE
    ;

// ============================================================================
// LOCK/UNLOCK STATEMENTS (ISO/IEC 1539-1:2010 Section 8.5.6)
// ============================================================================
// LOCK and UNLOCK statements provide mutex-style synchronization for coarrays.
// A lock variable must be a scalar variable of type LOCK_TYPE from
// ISO_FORTRAN_ENV.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R859: lock-stmt -> LOCK ( lock-variable [, lock-stat-list] )
// - R860: unlock-stmt -> UNLOCK ( lock-variable [, sync-stat-list] )
// - R861: lock-stat -> ACQUIRED_LOCK = scalar-logical-variable | sync-stat
// - R866: lock-variable -> scalar-variable

// LOCK statement (ISO/IEC 1539-1:2010 R859)
// R859: lock-stmt -> LOCK ( lock-variable [, lock-stat-list] )
// Acquires a lock on a lock variable, optionally checking acquisition status
lock_stmt
    : LOCK LPAREN lock_variable (COMMA lock_stat_list)? RPAREN NEWLINE
    ;

// UNLOCK statement (ISO/IEC 1539-1:2010 R860)
// R860: unlock-stmt -> UNLOCK ( lock-variable [, sync-stat-list] )
// Releases a lock on a lock variable
unlock_stmt
    : UNLOCK LPAREN lock_variable (COMMA sync_stat_list)? RPAREN NEWLINE
    ;

// Lock variable (ISO/IEC 1539-1:2010 R866)
// R866: lock-variable -> scalar-variable
// Must be of type LOCK_TYPE from ISO_FORTRAN_ENV
lock_variable
    : variable_f90
    ;

// Lock stat list (ISO/IEC 1539-1:2010 R861)
// List of lock-specific and synchronization status specifiers
lock_stat_list
    : lock_stat (COMMA lock_stat)*
    ;

// Lock stat (ISO/IEC 1539-1:2010 R861)
// R861: lock-stat -> ACQUIRED_LOCK = scalar-logical-variable | sync-stat
lock_stat
    : ACQUIRED_LOCK EQUALS variable_f90  // Logical variable for acquisition status
    | sync_stat                          // STAT or ERRMSG
    ;

// ============================================================================
// COARRAY-AWARE DECLARATIONS (ISO/IEC 1539-1:2010 Section 5.3.6)
// ============================================================================
// Override F2003 rules to support coarray codimensions in declarations
// and variable references.

// Entity declaration with coarray support (ISO/IEC 1539-1:2010 R503)
// R503: entity-decl -> object-name [(array-spec)] [coarray-spec] [* char-length]
//                      [initialization] | function-name [* char-length]
// Enhanced to include optional coarray-spec
entity_decl
    : identifier_or_keyword
      (LPAREN array_spec RPAREN)?
      coarray_spec?
      (EQUALS expr_f2003)?
    ;

// Left-hand side expression with coarray image selector (ISO/IEC 1539-1:2010 R601)
// R601: designator -> ... | coindexed-named-object
// R624: image-selector -> [ cosubscript-list ]
// Enhanced to support image selectors on various LHS forms
lhs_expression
    : identifier_or_keyword                 // Simple variable
      coarray_spec?
    | identifier_or_keyword LPAREN (actual_arg_list | section_subscript_list)? RPAREN
      coarray_spec?                         // Array element or section
    | identifier_or_keyword PERCENT identifier_or_keyword
      coarray_spec?                         // Component
    | identifier_or_keyword PERCENT identifier_or_keyword
      LPAREN (actual_arg_list | section_subscript_list)? RPAREN
      coarray_spec?                         // Component array/section/method
    | identifier_or_keyword LPAREN (actual_arg_list | section_subscript_list)? RPAREN
      PERCENT identifier_or_keyword
      coarray_spec?                         // Array element or section component
    | identifier_or_keyword LPAREN
      (actual_arg_list | section_subscript_list)? RPAREN
      PERCENT identifier_or_keyword LPAREN
      (actual_arg_list | section_subscript_list)? RPAREN
      coarray_spec?                         // Array element/section component method
    ;

// ============================================================================
// SEPARATE MODULE PROCEDURES (ISO/IEC 1539-1:2010 Section 12.6.2.5)
// ============================================================================
// Separate module procedures allow the interface to be defined in a module
// while the implementation is in a submodule. This provides information
// hiding and separate compilation.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R1237: separate-module-subprogram -> mp-subprogram-stmt [specification-part]
//                                        [execution-part] [internal-subprogram-part]
//                                        end-mp-subprogram-stmt
// - R1238: mp-subprogram-stmt -> MODULE PROCEDURE procedure-name
// - The implementation uses MODULE FUNCTION and MODULE SUBROUTINE forms

// Module subprogram (ISO/IEC 1539-1:2010 R1108)
// R1108: module-subprogram -> function-subprogram | subroutine-subprogram
//                            | separate-module-subprogram
// Override to include F2008 separate module procedures
module_subprogram
    : function_subprogram_f2008
    | subroutine_subprogram_f2008
    | module_subroutine_subprogram_f2008
    | module_function_subprogram_f2008
    ;

// Separate module subroutine (ISO/IEC 1539-1:2010 Section 12.6.2.5, R1237)
// Implementation of a module procedure interface defined in parent module
module_subroutine_subprogram_f2008
    : module_subroutine_stmt_f2008 specification_part_f2008?
      execution_part_f2008? internal_subprogram_part_f2003? end_subroutine_stmt
    ;

// Separate module function (ISO/IEC 1539-1:2010 Section 12.6.2.5, R1237)
// Implementation of a module function interface defined in parent module
module_function_subprogram_f2008
    : module_function_stmt_f2008 specification_part_f2008?
      execution_part_f2008? internal_subprogram_part_f2003? end_function_stmt
    ;

// MODULE SUBROUTINE statement (ISO/IEC 1539-1:2010 R1238 variant)
// Defines implementation of a module procedure interface as subroutine
module_subroutine_stmt_f2008
    : MODULE SUBROUTINE IDENTIFIER
      (LPAREN dummy_arg_name_list? RPAREN)?
      binding_spec? NEWLINE
    ;

// MODULE FUNCTION statement (ISO/IEC 1539-1:2010 R1238 variant)
// Defines implementation of a module procedure interface as function
module_function_stmt_f2008
    : MODULE FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// ============================================================================
// DO CONCURRENT CONSTRUCT (ISO/IEC 1539-1:2010 Section 8.1.6.6)
// ============================================================================
// DO CONCURRENT provides explicit parallelization hints to the compiler.
// Iterations are independent and may execute in any order or concurrently.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R818: loop-control -> [,] do-variable = ... | [,] WHILE (...) |
//                         [,] CONCURRENT concurrent-header
// - R819: concurrent-header -> (concurrent-spec [, scalar-mask-expr])
// - R820: concurrent-spec -> concurrent-control-list [, concurrent-limit]
// - R821: concurrent-control -> index-name = concurrent-limit : concurrent-limit
//                               [: concurrent-step]

// DO construct with F2008 DO CONCURRENT support
do_construct_f2008
    : do_construct                    // Standard DO (Section 8.1.6)
    | do_concurrent_construct         // DO CONCURRENT (Section 8.1.6.6)
    ;

// DO CONCURRENT construct (ISO/IEC 1539-1:2010 Section 8.1.6.6)
// Enables loop parallelization with independent iterations
do_concurrent_construct
    : do_concurrent_stmt
      execution_part_f2008?
      end_do_stmt
    ;

// DO CONCURRENT statement (ISO/IEC 1539-1:2010 R818)
// R818: loop-control -> ... | [,] CONCURRENT concurrent-header
do_concurrent_stmt
    : (IDENTIFIER COLON)? DO CONCURRENT concurrent_header NEWLINE
    ;

// Concurrent header (ISO/IEC 1539-1:2010 R819)
// R819: concurrent-header -> (concurrent-spec [, scalar-mask-expr])
concurrent_header
    : LPAREN forall_triplet_spec_list (COMMA scalar_mask_expr)? RPAREN
    ;

// Concurrent control list (ISO/IEC 1539-1:2010 R820)
// R820: concurrent-spec -> concurrent-control-list [, concurrent-limit]
// Uses FORALL triplet syntax for index specifications
forall_triplet_spec_list
    : forall_triplet_spec (COMMA forall_triplet_spec)*
    ;

// Concurrent control (ISO/IEC 1539-1:2010 R821)
// R821: concurrent-control -> index-name = concurrent-limit : concurrent-limit
//                             [: concurrent-step]
forall_triplet_spec
    : IDENTIFIER EQUALS expr_f90 COLON expr_f90 (COLON expr_f90)?
    ;

// Scalar mask expression (ISO/IEC 1539-1:2010 R819)
// Optional logical mask to filter iterations
scalar_mask_expr
    : logical_expr
    ;

// ============================================================================
// BLOCK CONSTRUCT (ISO/IEC 1539-1:2010 Section 8.1.4)
// ============================================================================
// The BLOCK construct provides local scoping for variables.
// Variables declared within a BLOCK are local to that block.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R807: block-construct -> block-stmt [specification-part] [execution-part]
//                            end-block-stmt
// - R808: block-stmt -> [block-construct-name:] BLOCK
// - R809: end-block-stmt -> END BLOCK [block-construct-name]

// BLOCK construct (ISO/IEC 1539-1:2010 R807)
// Enhanced with F2008 specification and execution parts
block_construct_f2008
    : (IDENTIFIER COLON)? BLOCK NEWLINE
      specification_part_f2008?        // Local declarations
      execution_part_f2008?            // Executable statements
      END BLOCK (IDENTIFIER)? NEWLINE
    ;

// ============================================================================
// TYPE DECLARATIONS (ISO/IEC 1539-1:2010 Section 5.1)
// ============================================================================
// F2008 extends type declarations with new integer/real kinds from
// ISO_FORTRAN_ENV (Section 13.8.2) and the CONTIGUOUS attribute (Section 5.3.7).

// Type declaration with F2008 intrinsic kinds
type_declaration_stmt_f2008
    : type_declaration_stmt           // Inherit F2003 declarations
    | enhanced_intrinsic_declaration  // ISO_FORTRAN_ENV kinds
    ;

// Enhanced intrinsic declarations (ISO/IEC 1539-1:2010 Section 13.8.2)
// INT8, INT16, INT32, INT64: Integer kinds for specific bit sizes
// REAL32, REAL64, REAL128: Real kinds for IEEE floating-point sizes
// These are named constants from ISO_FORTRAN_ENV used as type specifiers
enhanced_intrinsic_declaration
    : INT8 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | INT16 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | INT32 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | INT64 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL32 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL64 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL128 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    ;

// CONTIGUOUS statement (ISO/IEC 1539-1:2010 Section 5.3.7, R544)
// R544: contiguous-stmt -> CONTIGUOUS [::] object-name-list
// Specifies that an array occupies a contiguous block of memory
contiguous_stmt
    : CONTIGUOUS DOUBLE_COLON object_name_list NEWLINE
    ;

// Attribute specification (ISO/IEC 1539-1:2010 R502)
// R502: attr-spec -> access-spec | ALLOCATABLE | ASYNCHRONOUS | CODIMENSION [...]
//                  | CONTIGUOUS | DIMENSION (array-spec) | EXTERNAL | ...
// Extended with CONTIGUOUS (Section 5.3.7) and CODIMENSION (Section 5.3.6)
// NEW in F2008
attr_spec
    : PUBLIC
    | PRIVATE
    | ALLOCATABLE
    | POINTER
    | INTENT LPAREN intent_spec RPAREN
    | OPTIONAL
    | TARGET
    | VOLATILE
    | PROTECTED
    | PARAMETER
    | VALUE
    | CONTIGUOUS                      // NEW in F2008 (Section 5.3.7)
    | CODIMENSION coarray_spec?       // NEW in F2008 (Section 5.3.6)
    ;

// ============================================================================
// ALLOCATE STATEMENT (ISO/IEC 1539-1:2010 Section 6.7.1)
// ============================================================================
// ALLOCATE statement with coarray support for allocating coarray codimensions.
//
// Key rules from ISO/IEC 1539-1:2010:
// - R626: allocate-stmt -> ALLOCATE ([type-spec::] allocation-list [, alloc-opt-list])
// - R628: allocation -> allocate-object [(allocate-shape-spec-list)]
//                       [allocate-coarray-spec]
// - R636: allocate-coarray-spec -> [allocate-coshape-spec-list,] [lower-bound-expr:]

// ALLOCATE statement with coarray support (ISO/IEC 1539-1:2010 R626)
allocate_stmt_f2008
    : ALLOCATE LPAREN type_spec DOUBLE_COLON allocation_list_f2008
      (COMMA alloc_opt_list)? RPAREN NEWLINE
    | ALLOCATE LPAREN allocation_list_f2008
      (COMMA alloc_opt_list)? RPAREN NEWLINE
    ;

// Allocation list (ISO/IEC 1539-1:2010 R627)
allocation_list_f2008
    : allocation_f2008 (COMMA allocation_f2008)*
    ;

// Allocation with coarray codimension (ISO/IEC 1539-1:2010 R628)
// R628: allocation -> allocate-object [(allocate-shape-spec-list)]
//                     [allocate-coarray-spec]
allocation_f2008
    : IDENTIFIER coarray_spec? (LPAREN allocate_shape_spec_list RPAREN)?
    | derived_type_spec DOUBLE_COLON IDENTIFIER coarray_spec?
      (LPAREN allocate_shape_spec_list RPAREN)?
    ;

// ============================================================================
// ERROR STOP STATEMENT (ISO/IEC 1539-1:2010 Section 8.4)
// ============================================================================
// ERROR STOP initiates error termination of execution, indicating
// abnormal program termination (unlike STOP which is normal termination).
//
// Key rule from ISO/IEC 1539-1:2010:
// - R856: error-stop-stmt -> ERROR STOP [stop-code]
// - R857: stop-code -> scalar-default-char-expr | scalar-int-expr

// ERROR STOP statement (ISO/IEC 1539-1:2010 R856)
error_stop_stmt
    : ERROR_STOP (INTEGER_LITERAL | string_literal)? NEWLINE
    ;

// ============================================================================
// INTRINSIC FUNCTIONS (ISO/IEC 1539-1:2010 Section 13.7)
// ============================================================================
// F2008 adds several new intrinsic procedures for mathematical functions,
// array operations, and image inquiry.

// F2008 intrinsic function calls
// Extended to include new F2008 intrinsics
intrinsic_function_call_f2008
    : intrinsic_function_call         // Inherit F2003 intrinsics
    | bessel_function_call            // Bessel functions (Section 13.7.22-27)
    | math_function_call              // Error/gamma functions (Section 13.7)
    | array_function_call             // Array functions (Section 13.7)
    | image_function_call             // Image intrinsics (Section 13.7)
    | compiler_inquiry_function_call  // Compiler inquiry (Section 13.7.41-42)
    | bit_shift_function_call         // Bit shift intrinsics (Section 13.7.158-160)
    | bit_mask_function_call          // Bit mask intrinsics (Section 13.7.110-111)
    | bit_reduction_function_call     // Bit reduction (Section 13.7.79-80, 94)
    ;

// Bessel function calls (ISO/IEC 1539-1:2010 Section 13.7.22-27)
// Mathematical functions for cylindrical coordinate problems
bessel_function_call
    : BESSEL_J0 LPAREN actual_arg_list RPAREN    // Section 13.7.22
    | BESSEL_J1 LPAREN actual_arg_list RPAREN    // Section 13.7.23
    | BESSEL_JN LPAREN actual_arg_list RPAREN    // Section 13.7.24
    | BESSEL_Y0 LPAREN actual_arg_list RPAREN    // Section 13.7.25
    | BESSEL_Y1 LPAREN actual_arg_list RPAREN    // Section 13.7.26
    | BESSEL_YN LPAREN actual_arg_list RPAREN    // Section 13.7.27
    ;

// Mathematical function calls (ISO/IEC 1539-1:2010 Section 13.7)
// Error function, gamma function, and hypotenuse
math_function_call
    : ERF LPAREN actual_arg_list RPAREN          // Section 13.7.52
    | ERFC LPAREN actual_arg_list RPAREN         // Section 13.7.53
    | GAMMA LPAREN actual_arg_list RPAREN        // Section 13.7.61
    | LOG_GAMMA LPAREN actual_arg_list RPAREN    // Section 13.7.108
    | HYPOT LPAREN actual_arg_list RPAREN        // Section 13.7.77
    ;

// Array function calls (ISO/IEC 1539-1:2010 Section 13.7)
// Array reduction and location functions
array_function_call
    : NORM2 LPAREN actual_arg_list RPAREN        // Section 13.7.119
    | PARITY LPAREN actual_arg_list RPAREN       // Section 13.7.127
    | FINDLOC LPAREN actual_arg_list RPAREN      // Section 13.7.58
    ;

// Image inquiry function calls (ISO/IEC 1539-1:2010 Section 13.7)
// Coarray image inquiry and storage inquiry
image_function_call
    : THIS_IMAGE LPAREN actual_arg_list? RPAREN      // Section 13.7.165
    | NUM_IMAGES LPAREN actual_arg_list? RPAREN      // Section 13.7.121
    | STORAGE_SIZE LPAREN actual_arg_list RPAREN     // Section 13.7.163
    | IS_CONTIGUOUS LPAREN actual_arg_list RPAREN    // Section 13.7.87
    ;

// Compiler inquiry function calls (ISO/IEC 1539-1:2010 Section 13.7.41-42)
// Compiler inquiry functions return CHARACTER values describing compilation
compiler_inquiry_function_call
    : COMPILER_VERSION LPAREN actual_arg_list? RPAREN  // Section 13.7.42
    | COMPILER_OPTIONS LPAREN actual_arg_list? RPAREN  // Section 13.7.41
    ;

// Bit shift intrinsic function calls (ISO/IEC 1539-1:2010 Section 13.7.158-160)
// Logical and arithmetic bit shift operations
bit_shift_function_call
    : SHIFTA LPAREN actual_arg_list RPAREN       // Section 13.7.158
    | SHIFTL LPAREN actual_arg_list RPAREN       // Section 13.7.159
    | SHIFTR LPAREN actual_arg_list RPAREN       // Section 13.7.160
    ;

// Bit mask intrinsic function calls (ISO/IEC 1539-1:2010 Section 13.7.110-111)
// Bit mask generation functions
bit_mask_function_call
    : MASKL LPAREN actual_arg_list RPAREN        // Section 13.7.110
    | MASKR LPAREN actual_arg_list RPAREN        // Section 13.7.111
    ;

// Bitwise reduction function calls (ISO/IEC 1539-1:2010 Section 13.7.79-80, 94)
// Bitwise reduction operations across array elements
// - IALL(ARRAY[,DIM][,MASK]): Bitwise AND reduction (Section 13.7.79)
// - IANY(ARRAY[,DIM][,MASK]): Bitwise OR reduction (Section 13.7.80)
// - IPARITY(ARRAY[,DIM][,MASK]): Bitwise XOR reduction (Section 13.7.94)
bit_reduction_function_call
    : IALL LPAREN actual_arg_list RPAREN         // Section 13.7.79
    | IANY LPAREN actual_arg_list RPAREN         // Section 13.7.80
    | IPARITY LPAREN actual_arg_list RPAREN      // Section 13.7.94
    ;

// ============================================================================
// ATOMIC INTRINSIC SUBROUTINES (ISO/IEC 1539-1:2010 Section 13.7.19-13.7.20)
// ============================================================================
// Atomic intrinsic subroutines for coarray programming.
// These are SUBROUTINES (not functions) and must be invoked via CALL.
//
// Key rules from ISO/IEC 1539-1:2010:
// - ATOMIC_DEFINE(ATOM, VALUE [, STAT]): Section 13.7.19
//   Atomically defines ATOM with VALUE. ATOM must be a coarray of type
//   integer(ATOMIC_INT_KIND) or logical(ATOMIC_LOGICAL_KIND).
// - ATOMIC_REF(VALUE, ATOM [, STAT]): Section 13.7.20
//   Atomically references ATOM and assigns to VALUE. ATOM must be a coarray.

// Atomic subroutine call statement (ISO/IEC 1539-1:2010 Section 13.7.19-20)
// Invoked as: CALL ATOMIC_DEFINE(atom, value) or CALL ATOMIC_REF(value, atom)
atomic_subroutine_call
    : CALL ATOMIC_DEFINE LPAREN actual_arg_list RPAREN NEWLINE  // Section 13.7.19
    | CALL ATOMIC_REF LPAREN actual_arg_list RPAREN NEWLINE     // Section 13.7.20
    ;

// ============================================================================
// PRIMARY EXPRESSIONS (ISO/IEC 1539-1:2010 Section 7.1.1)
// ============================================================================
// Primary expressions are the basic building blocks of Fortran expressions.
//
// Key rule from ISO/IEC 1539-1:2010:
// - R701: primary -> designator | literal-constant | array-constructor |
//                    structure-constructor | function-reference |
//                    type-param-inquiry | type-param-name | (expr)

// Primary expression (ISO/IEC 1539-1:2010 R701)
// Override F2003 to include F2008 intrinsic functions
primary
    : identifier_or_keyword (PERCENT identifier_or_keyword)*
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN
    | identifier_or_keyword DOUBLE_QUOTE_STRING
    | identifier_or_keyword SINGLE_QUOTE_STRING
    | intrinsic_function_call_f2008
    | ieee_constant
    | INTEGER_LITERAL
    | LABEL
    | REAL_LITERAL
    | SINGLE_QUOTE_STRING
    | DOUBLE_QUOTE_STRING
    | '*'                // List-directed format
    | array_constructor
    | LPAREN primary RPAREN
    ;

// Variable with coarray image selector (ISO/IEC 1539-1:2010 R601, R624)
// R624: image-selector -> [ cosubscript-list ]
variable_f2008
    : IDENTIFIER coarray_spec? (PERCENT IDENTIFIER)*
    | IDENTIFIER (LPAREN actual_arg_list RPAREN)? coarray_spec?
    ;

// ============================================================================
// RULE OVERRIDES
// ============================================================================
// Override F2003 top-level rules to use F2008 enhanced versions.
// This ensures F2008 features (coarrays, DO CONCURRENT, submodules, etc.)
// are available when using the F2008 parser.

// Specification part override
// Routes to F2008 specification part with CONTIGUOUS support
specification_part
    : specification_part_f2008
    ;

// Execution part override
// Routes to F2008 execution part with SYNC, ERROR STOP, DO CONCURRENT
execution_part
    : execution_part_f2008
    ;

// Program unit override
// Routes to F2008 program unit with submodule support
program_unit
    : program_unit_f2008
    ;

// ============================================================================
// WHERE CONSTRUCT ENHANCEMENTS (ISO/IEC 1539-1:2010 Section 8.1.4.3, R807)
// ============================================================================
// Fortran 2008 extends ELSEWHERE with MASKED keyword for clarity.
// MASKED ELSEWHERE explicitly indicates a condition-guarded ELSEWHERE clause.

// ELSEWHERE statement override (ISO/IEC 1539-1:2010 R807)
// Original (F90): ELSEWHERE [(scalar-mask-expr)] [construct-name]
// Enhanced (F2008): ELSEWHERE [(scalar-mask-expr)] [construct-name]
//                  | MASKED ELSEWHERE [(scalar-mask-expr)] [construct-name]
// Both forms are valid in F2008; MASKED is optional for clarity.
elsewhere_stmt
    : ELSEWHERE (LPAREN logical_expr_f90 RPAREN)? (IDENTIFIER)?
    | MASKED ELSEWHERE (LPAREN logical_expr_f90 RPAREN)? (IDENTIFIER)?
    ;

// ============================================================================
// IDENTIFIER OR KEYWORD OVERRIDE (F2008 Extension)
// ============================================================================
// F2008 adds several new keywords that can also be used as identifiers.
// Override F2003 identifier_or_keyword to include F2008-specific tokens.
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
    // F2008-specific tokens that can be used as identifiers
    | IMAGES       // IMAGES can be used as variable name (SYNC IMAGES keyword)
    | ALL          // ALL can be used as variable name (SYNC ALL keyword)
    | MEMORY       // MEMORY can be used as variable name (SYNC MEMORY keyword)
    | CONCURRENT   // CONCURRENT can be used as variable name (DO CONCURRENT)
    | CONTIGUOUS   // CONTIGUOUS can be used as variable name
    | CODIMENSION  // CODIMENSION can be used as variable name
    | SUBMODULE    // SUBMODULE can be used as a name
    | BLOCK        // BLOCK can be used as variable name
    | ERROR_STOP   // ERROR_STOP is compound keyword, not typically used as name
    | LOCK         // LOCK can be used as variable name
    | UNLOCK       // UNLOCK can be used as variable name
    // F2008 bit manipulation intrinsics (Section 13.7.110-111, 13.7.158-160)
    | SHIFTA       // SHIFTA can be used as variable name
    | SHIFTL       // SHIFTL can be used as variable name
    | SHIFTR       // SHIFTR can be used as variable name
    | MASKL        // MASKL can be used as variable name
    | MASKR        // MASKR can be used as variable name
    // F2008 bitwise reduction intrinsics (Section 13.7.79-80, 13.7.94)
    | IALL         // IALL can be used as variable name
    | IANY         // IANY can be used as variable name
    | IPARITY      // IPARITY can be used as variable name
    // F2008 atomic intrinsics (Section 13.7.19-20)
    | ATOMIC_DEFINE  // ATOMIC_DEFINE can be used as variable name
    | ATOMIC_REF     // ATOMIC_REF can be used as variable name
    // F2008 WHERE construct enhancements (Section 8.1.4.3)
    | MASKED         // MASKED can be used as variable name (MASKED ELSEWHERE keyword)
    // F2008 mathematical intrinsics (Section 13.7.77)
    | HYPOT          // HYPOT can be used as variable name
    ;
