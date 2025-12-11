/*
 * Fortran2018Parser.g4
 *
 * Fortran 2018 (ISO/IEC 1539-1:2018) Parser
 * Reference: J3/18-007r1 "Fortran 2018" (Working Draft)
 *
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 *
 * ISO/IEC 1539-1:2018 Key Sections:
 * - Section 8: Type declaration statements and attributes
 * - Section 9: Use association and data objects
 * - Section 10: Expressions and assignment
 * - Section 11: Execution control
 * - Section 14: Program units
 * - Section 15: Procedures
 * - Section 16: Intrinsic procedures
 */

parser grammar Fortran2018Parser;

import Fortran2008Parser;

options {
    tokenVocab = Fortran2018Lexer;
}

// ============================================================================
// FORTRAN 2018 PROGRAM STRUCTURE (ISO/IEC 1539-1:2018 Section 14)
// ============================================================================

// ISO/IEC 1539-1:2018 R502: program-unit
// F2018 program unit (enhanced with teams and events)
program_unit_f2018
    : NEWLINE* (main_program_f2018 | module_f2018 | submodule_f2008
      | external_subprogram_f2018) NEWLINE*
    ;

// ISO/IEC 1539-1:2018 R1401: main-program
// Enhanced main program for F2018
main_program_f2018
    : program_stmt specification_part_f2018? execution_part_f2018?
      internal_subprogram_part_f2003? end_program_stmt
    ;

// ISO/IEC 1539-1:2018 R1404: module
// Enhanced module for F2018
module_f2018
    : module_stmt specification_part_f2018?
      module_subprogram_part_f2018? end_module_stmt
    ;

// ISO/IEC 1539-1:2018 R503: external-subprogram
// Enhanced external subprogram for F2018
external_subprogram_f2018
    : function_subprogram_f2018
    | subroutine_subprogram_f2018
    ;

// ISO/IEC 1539-1:2018 R1529: function-subprogram
// Enhanced function subprogram for F2018
function_subprogram_f2018
    : function_stmt_f2018 specification_part_f2018? execution_part_f2018?
      internal_subprogram_part_f2003? end_function_stmt
    ;

// ISO/IEC 1539-1:2018 R1534: subroutine-subprogram
// Enhanced subroutine subprogram for F2018
subroutine_subprogram_f2018
    : subroutine_stmt_f2018 specification_part_f2018? execution_part_f2018?
      internal_subprogram_part_f2003? end_subroutine_stmt
    ;

// ISO/IEC 1539-1:2018 R1407: module-subprogram-part
module_subprogram_part_f2018
    : contains_stmt NEWLINE* (module_subprogram_f2018 NEWLINE*)*
    ;

// ISO/IEC 1539-1:2018 R1408: module-subprogram
module_subprogram_f2018
    : function_subprogram_f2018
    | subroutine_subprogram_f2018
    | module_subroutine_subprogram_f2008
    | module_function_subprogram_f2008
    ;

// ISO/IEC 1539-1:2018 R1530: function-stmt
// Enhanced function statement for F2018
function_stmt_f2018
    : prefix? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1535: subroutine-stmt
// Enhanced subroutine statement for F2018
subroutine_stmt_f2018
    : prefix? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)?
      binding_spec? NEWLINE
    ;

// ============================================================================
// ENHANCED SPECIFICATION PART (ISO/IEC 1539-1:2018 Section 8)
// ============================================================================

// ISO/IEC 1539-1:2018 R504: specification-part
specification_part_f2018
    : ((use_stmt | import_stmt | implicit_stmt | declaration_construct_f2018)
      NEWLINE*)*
    ;

// ISO/IEC 1539-1:2018 R507: declaration-construct
// Enhanced declaration construct for F2018
declaration_construct_f2018
    : derived_type_def_f2003          // Inherit F2003 types
    | class_declaration_stmt          // Inherit F2003 CLASS
    | procedure_declaration_stmt      // Inherit F2003 procedures
    | interface_block                 // Inherit F2003 interfaces
    | volatile_stmt                   // Inherit F2003 VOLATILE
    | protected_stmt                  // Inherit F2003 PROTECTED
    | contiguous_stmt                 // Inherit F2008 CONTIGUOUS
    | event_declaration_stmt          // NEW in F2018 (Section 16.5.5)
    | team_declaration_stmt           // NEW in F2018 (Section 16.5.6)
    | type_declaration_stmt_f2018     // Enhanced for F2018
    | declaration_construct_f2008     // Inherit F2008 declarations
    ;

// ISO/IEC 1539-1:2018 R1409: use-stmt
// Enhanced USE statement for F2018, including intrinsic modules such as
// ISO_FORTRAN_ENV in addition to the IEEE intrinsic modules from F2003.
// The module_name_f2018 rule allows both IDENTIFIER and IEEE module tokens.
use_stmt
    : USE module_name_f2018 NEWLINE
    | USE module_name_f2018 COMMA ONLY COLON only_list NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON intrinsic_module_name NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON intrinsic_module_name COMMA ONLY COLON
      only_list NEWLINE
    ;

// Module name for USE statement - includes IEEE tokens which lexer matches first
module_name_f2018
    : IDENTIFIER
    | ieee_module_name
    ;

// Intrinsic module names (e.g. ISO_FORTRAN_ENV, IEEE intrinsic modules)
intrinsic_module_name
    : ieee_module_name
    | IDENTIFIER
    ;

// ISO/IEC 1539-1:2018 R1412: only (elements of only-list)
only_list
    : only_item_f2018 (COMMA only_item_f2018)*
    ;

// ISO/IEC 1539-1:2018 R1412: only
only_item_f2018
    : IDENTIFIER (POINTER_ASSIGN only_item_target_f2018)?
    | c_interop_type
    | OPERATOR LPAREN operator_token RPAREN
    ;

// ISO/IEC 1539-1:2018 R1413: only-use-name (rename target)
only_item_target_f2018
    : IDENTIFIER
    | INT8
    | INT16
    | INT32
    | INT64
    | REAL32
    | REAL64
    | REAL128
    ;

// ============================================================================
// ENHANCED EXECUTION PART (ISO/IEC 1539-1:2018 Section 11)
// ============================================================================

// ISO/IEC 1539-1:2018 R509: execution-part
execution_part_f2018
    : execution_construct_f2018*
    ;

// ISO/IEC 1539-1:2018 R510: execution-part-construct
execution_construct_f2018
    : NEWLINE* executable_construct_f2018 NEWLINE*
    ;

// ISO/IEC 1539-1:2018 R514: executable-construct
// Enhanced executable construct for F2018
executable_construct_f2018
    : assignment_stmt                 // Inherit from F2003
    | collective_subroutine_call      // NEW in F2018 (must be BEFORE call_stmt)
    | call_stmt                       // Inherit from F2003
    | print_stmt                      // Inherit from F2003
    | stop_stmt_f2018                 // Enhanced in F2018 (R1160)
    | error_stop_stmt_f2018           // Enhanced in F2018 (R1161)
    | select_type_construct           // Inherit from F2003
    | select_rank_construct           // NEW in F2018 (R1148-R1151)
    | associate_construct             // Inherit from F2003
    | block_construct_f2008           // Inherit from F2008
    | allocate_stmt_f2008             // Inherit from F2008
    | team_construct                  // NEW in F2018 (teams: R1111-R1115, R1175-R1178)
    | event_construct                 // NEW in F2018 (events: R1170-R1173)
    | sync_construct                  // Inherit from F2008
    | wait_stmt                       // Inherit from F2003
    | flush_stmt                      // Inherit from F2003
    | if_construct                    // Inherit from F95
    | do_construct_f2018              // Enhanced in F2018 (R1119-R1132, DO CONCURRENT)
    | select_case_construct           // Inherit from F95
    | type_declaration_stmt_f2018     // F2018 allows mixed declarations
    | executable_construct_f2008      // Inherit F2008 constructs
    ;

// ============================================================================
// SELECT RANK CONSTRUCT (ISO/IEC 1539-1:2018 Section 11.1.10)
// ============================================================================

// ISO/IEC 1539-1:2018 R1148: select-rank-construct
select_rank_construct
    : select_rank_stmt
      rank_construct*
      end_select_rank_stmt
    ;

// ISO/IEC 1539-1:2018 R1149: select-rank-stmt
// R1149: [select-construct-name :] SELECT RANK ( [associate-name =>] selector )
select_rank_stmt
    : (IDENTIFIER COLON)? SELECT RANK_KEYWORD LPAREN
      (IDENTIFIER POINTER_ASSIGN)? expr_f90 RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1148: select-rank-construct
// Helper rule grouping case statement and its block
rank_construct
    : rank_case_stmt
      execution_part_f2018?
    ;

// ISO/IEC 1539-1:2018 R1150: select-rank-case-stmt
// R1150: RANK ( scalar-int-constant-expr ), RANK(*) or RANK DEFAULT
rank_case_stmt
    : RANK_KEYWORD LPAREN rank_value RPAREN NEWLINE   // RANK (selector) or RANK (*)
    | RANK_KEYWORD DEFAULT NEWLINE                    // RANK DEFAULT
    ;

// Rank selector value for R1150 (scalar-int-constant-expr or *)
rank_value
    : expr_f90                     // Specific rank selector: scalar-int-constant-expr
    | MULTIPLY                     // Assumed-rank case: RANK (*)
    ;

// ISO/IEC 1539-1:2018 R1151: end-select-rank-stmt
// Extended to support optional RANK keyword for modern compiler extensions
end_select_rank_stmt
    : END_SELECT (RANK_KEYWORD)? (IDENTIFIER)? NEWLINE
    ;

// ============================================================================
// COLLECTIVE COARRAY OPERATIONS (ISO/IEC 1539-1:2018 Section 16.9.46-50)
// ============================================================================

// ISO/IEC 1539-1:2018 Section 16.9.46-50: Collective subroutines
// CO_BROADCAST, CO_MAX, CO_MIN, CO_REDUCE, CO_SUM
collective_subroutine_call
    : co_sum_stmt
    | co_min_stmt
    | co_max_stmt
    | co_reduce_stmt
    | co_broadcast_stmt
    ;

// ISO/IEC 1539-1:2018 Section 16.9.50: CO_SUM
co_sum_stmt
    : CALL CO_SUM LPAREN collective_arg_list RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 Section 16.9.48: CO_MIN
co_min_stmt
    : CALL CO_MIN LPAREN collective_arg_list RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 Section 16.9.47: CO_MAX
co_max_stmt
    : CALL CO_MAX LPAREN collective_arg_list RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 Section 16.9.49: CO_REDUCE
co_reduce_stmt
    : CALL CO_REDUCE LPAREN variable_f2008 COMMA operation_spec
      (COMMA collective_stat_list)? RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 Section 16.9.46: CO_BROADCAST
co_broadcast_stmt
    : CALL CO_BROADCAST LPAREN variable_f2008 COMMA source_image_spec
      (COMMA collective_stat_list)? RPAREN NEWLINE
    ;

// Collective argument list with optional stat specifiers
collective_arg_list
    : variable_f2008 (COMMA collective_stat_list)?
    ;

// Collective stat specifiers (STAT=, ERRMSG=, RESULT_IMAGE=)
collective_stat_list
    : collective_stat (COMMA collective_stat)*
    ;

// ISO/IEC 1539-1:2018 Section 16.9.46-50: stat-variable, errmsg-variable
// RESULT_IMAGE accepts an expression (typically an integer image number)
collective_stat
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    | RESULT_IMAGE EQUALS expr_f90
    ;

// ISO/IEC 1539-1:2018 Section 16.9.49: OPERATION argument for CO_REDUCE
operation_spec
    : IDENTIFIER                    // Operation procedure name
    ;

// ISO/IEC 1539-1:2018 Section 16.9.46: SOURCE_IMAGE argument for CO_BROADCAST
source_image_spec
    : SOURCE_IMAGE EQUALS expr_f90
    ;

// ============================================================================
// TEAM SUPPORT (ISO/IEC 1539-1:2018 Section 11.6)
// ============================================================================

// ISO/IEC 1539-1:2018 R1111: change-team-construct
// ISO/IEC 1539-1:2018 R1175: form-team-stmt
team_construct
    : change_team_construct
    | form_team_stmt
    ;

// ISO/IEC 1539-1:2018 R1111: change-team-construct
change_team_construct
    : change_team_stmt
      execution_part_f2018?
      end_team_stmt
    ;

// ISO/IEC 1539-1:2018 R1112: change-team-stmt
change_team_stmt
    : (IDENTIFIER COLON)? CHANGE_TEAM LPAREN team_value
      (COMMA coarray_association_list)?
      (COMMA sync_stat_list)? RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1114: end-change-team-stmt (END TEAM ...)
end_team_stmt
    : END_TEAM (LPAREN sync_stat_list? RPAREN)? (IDENTIFIER)? NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1175: form-team-stmt
form_team_stmt
    : FORM_TEAM LPAREN team_number_expr COMMA team_variable
      (COMMA form_team_stat_list)? RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1115: team-value
team_value
    : expr_f90                     // Team expression
    ;

// ISO/IEC 1539-1:2018 R1176: team-number
team_number_expr
    : expr_f90                     // Team number
    ;

// ISO/IEC 1539-1:2018 R1177: team-variable (of TEAM_TYPE)
team_variable
    : IDENTIFIER                   // Variable of TEAM_TYPE
    ;

// ISO/IEC 1539-1:2018 R1113: coarray-association (list)
coarray_association_list
    : coarray_association (COMMA coarray_association)*
    ;

// ISO/IEC 1539-1:2018 R1113: coarray-association
coarray_association
    : IDENTIFIER LBRACKET cosubscript_list RBRACKET ARROW variable_f2008
    ;

// ISO/IEC 1539-1:2018 R1178: form-team-spec-list
form_team_stat_list
    : form_team_stat (COMMA form_team_stat)*
    ;

// ISO/IEC 1539-1:2018 R1178: form-team-spec
form_team_stat
    : NEW_INDEX EQUALS expr_f90
    | STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    ;

// ISO/IEC 1539-1:2018 Section 16.5.6: TEAM_TYPE declaration
// Team declaration
team_declaration_stmt
    : TYPE LPAREN TEAM_TYPE RPAREN DOUBLE_COLON entity_decl_list NEWLINE
    ;

// ============================================================================
// EVENT SUPPORT (ISO/IEC 1539-1:2018 Section 11.6.8)
// ============================================================================

// ISO/IEC 1539-1:2018 R1170-R1173: Event statements (with sync-stat R1165)
event_construct
    : event_post_stmt
    | event_wait_stmt
    | event_query_stmt
    ;

// ISO/IEC 1539-1:2018 R1170: event-post-stmt
event_post_stmt
    : EVENT_POST LPAREN event_variable (COMMA event_stat_list)? RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1172: event-wait-stmt
event_wait_stmt
    : EVENT_WAIT LPAREN event_variable (COMMA event_wait_spec_list)? RPAREN
      NEWLINE
    ;

// ISO/IEC 1539-1:2018 Section 16.9.72: EVENT_QUERY subroutine
event_query_stmt
    : EVENT_QUERY LPAREN event_variable COMMA COUNT EQUALS variable_f90
      (COMMA event_stat_list)? RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1171: event-variable
event_variable
    : IDENTIFIER                   // Variable of EVENT_TYPE
    ;

// ISO/IEC 1539-1:2018 R1165: sync-stat used in event statements (STAT= or ERRMSG=)
event_stat_list
    : event_stat (COMMA event_stat)*
    ;

// ISO/IEC 1539-1:2018 R1165: sync-stat (STAT= or ERRMSG=)
event_stat
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    ;

// ISO/IEC 1539-1:2018 R1173: event-wait-spec-list (sequence of event-wait-spec)
event_wait_spec_list
    : event_wait_spec (COMMA event_wait_spec)*
    ;

// ISO/IEC 1539-1:2018 R1173: event-wait-spec
event_wait_spec
    : UNTIL_COUNT EQUALS expr_f90
    | event_stat
    ;

// ISO/IEC 1539-1:2018 Section 16.5.5: EVENT_TYPE declaration
// Event declaration
event_declaration_stmt
    : TYPE LPAREN EVENT_TYPE RPAREN DOUBLE_COLON entity_decl_list NEWLINE
    ;

// ============================================================================
// ENHANCED DO CONSTRUCTS (ISO/IEC 1539-1:2018 Section 11.1.7)
// ============================================================================

// ISO/IEC 1539-1:2018 R1119: do-construct
// Includes DO CONCURRENT via loop-control R1123
do_construct_f2018
    : do_construct                    // Inherit standard DO from F95
    | do_concurrent_construct_f2018   // Enhanced in F2018
    ;

// DO CONCURRENT factored out of do-construct (R1119)
// Uses loop-control R1123 and adds locality specifiers
do_concurrent_construct_f2018
    : do_concurrent_stmt_f2018
      execution_part_f2018?
      end_do_stmt
    ;

// Derived from R1120 do-stmt with loop-control using CONCURRENT (R1123),
// plus concurrent-header (R1125) and concurrent-locality (R1129)
do_concurrent_stmt_f2018
    : (IDENTIFIER COLON)? DO CONCURRENT concurrent_header_f2018
      concurrent_locality_list? NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1125: concurrent-header
// concurrent-header is
// ( [integer-type-spec ::] concurrent-control-list [, scalar-mask-expr] )
concurrent_header_f2018
    : LPAREN (INTEGER (kind_selector)? DOUBLE_COLON)?
      forall_triplet_spec_list (COMMA scalar_mask_expr)? RPAREN
    ;

// List wrapper for R1129 concurrent-locality
concurrent_locality_list
    : concurrent_locality+
    ;

// ISO/IEC 1539-1:2018 R1129: concurrent-locality
// R1130: locality-spec is LOCAL (variable-name-list)
//        or LOCAL_INIT (variable-name-list)
//        or SHARED (variable-name-list)
//        or DEFAULT (NONE)
concurrent_locality
    : LOCAL LPAREN variable_name_list RPAREN
    | LOCAL_INIT LPAREN variable_name_list RPAREN
    | SHARED LPAREN variable_name_list RPAREN
    | DEFAULT LPAREN NONE RPAREN
    ;

// Variable-name list used in locality-spec (R1130)
variable_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ============================================================================
// ENHANCED STOP STATEMENTS (ISO/IEC 1539-1:2018 Section 11.4)
// ============================================================================

// ISO/IEC 1539-1:2018 R1160: stop-stmt with optional QUIET= specifier
stop_stmt_f2018
    : STOP (stop_code)? (COMMA QUIET EQUALS logical_expr)? NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1161: error-stop-stmt with optional QUIET= specifier
error_stop_stmt_f2018
    : ERROR_STOP (stop_code)? (COMMA QUIET EQUALS logical_expr)? NEWLINE
    ;

// ISO/IEC 1539-1:2018 R1162: stop-code
stop_code
    : INTEGER_LITERAL
    | string_literal
    | expr_f90                    // F2018 allows expressions
    ;

// ============================================================================
// ENHANCED TYPE DECLARATIONS (ISO/IEC 1539-1:2018 Section 8)
// ============================================================================

// ISO/IEC 1539-1:2018 R801: type-declaration-stmt
type_declaration_stmt_f2018
    : assumed_rank_declaration        // NEW in F2018 (R825: assumed-rank-spec)
    | type_declaration_stmt           // Inherit F2003 declarations
    | enhanced_intrinsic_declaration  // Inherit F2008 enhanced types
    ;

// ISO/IEC 1539-1:2018 R825: assumed-rank-spec is ..
// ISO/IEC 1539-1:2018 Section 8.5.8.7: Assumed-rank entity
assumed_rank_declaration
    : type_spec COMMA DIMENSION LPAREN DOT_DOT RPAREN DOUBLE_COLON
      entity_decl_list NEWLINE
    ;

// ============================================================================
// ENHANCED INTRINSIC FUNCTIONS (ISO/IEC 1539-1:2018 Section 16.9)
// ============================================================================

// Override intrinsic function calls to include F2018 functions
intrinsic_function_call_f2018
    : intrinsic_function_call_f2008   // Inherit F2008 intrinsics
    | image_status_function_call      // NEW in F2018 (16.9.73, 16.9.81, 16.9.182)
    | collective_function_call        // NEW in F2018 (16.9.52, 16.9.187)
    | random_init_call                // NEW in F2018 (16.9.152)
    | enhanced_math_function_call     // NEW in F2018 (16.9.140, 16.9.161)
    ;

// ISO/IEC 1539-1:2018 Section 16.9.81: IMAGE_STATUS
// ISO/IEC 1539-1:2018 Section 16.9.73: FAILED_IMAGES
// ISO/IEC 1539-1:2018 Section 16.9.182: STOPPED_IMAGES
image_status_function_call
    : IMAGE_STATUS LPAREN expr_f90 (COMMA TEAM EQUALS team_value)? RPAREN
    | FAILED_IMAGES LPAREN (TEAM EQUALS team_value)? RPAREN
    | STOPPED_IMAGES LPAREN (TEAM EQUALS team_value)? RPAREN
    ;

// ISO/IEC 1539-1:2018 Section 16.9.52: COSHAPE
// ISO/IEC 1539-1:2018 Section 16.9.78: GET_TEAM
// ISO/IEC 1539-1:2018 Section 16.9.187: TEAM_NUMBER
collective_function_call
    : COSHAPE LPAREN expr_f90 (COMMA KIND EQUALS expr_f90)? RPAREN
    | GET_TEAM LPAREN expr_f90 (COMMA TEAM EQUALS team_value)? RPAREN
    | TEAM_NUMBER LPAREN (TEAM EQUALS team_value)? RPAREN
    ;

// ISO/IEC 1539-1:2018 Section 16.9.152: RANDOM_INIT
random_init_call
    : CALL RANDOM_INIT LPAREN REPEATABLE EQUALS logical_expr COMMA
      IMAGE_DISTINCT EQUALS logical_expr RPAREN NEWLINE
    ;

// ISO/IEC 1539-1:2018 Section 16.9.140: OUT_OF_RANGE
// ISO/IEC 1539-1:2018 Section 16.9.161: REDUCE
enhanced_math_function_call
    : OUT_OF_RANGE LPAREN actual_arg_list RPAREN
    | REDUCE LPAREN array_expr COMMA operation_spec
      (COMMA DIM EQUALS expr_f90)? (COMMA MASK EQUALS logical_expr)? RPAREN
    ;

// Array expression for REDUCE and other array intrinsics
array_expr
    : expr_f90                       // Array expression
    ;

// ISO/IEC 1539-1:2018 R1023: primary (override to include F2018 intrinsics)
// Override F2008 primary to include F2018 intrinsic function calls
primary
    : identifier_or_keyword (PERCENT identifier_or_keyword)*
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN
    | identifier_or_keyword DOUBLE_QUOTE_STRING
    | identifier_or_keyword SINGLE_QUOTE_STRING
    | intrinsic_function_call_f2018
    | ieee_constant
    | INTEGER_LITERAL
    | LABEL
    | REAL_LITERAL
    | SINGLE_QUOTE_STRING
    | DOUBLE_QUOTE_STRING
    | '*'
    | array_constructor
    | LPAREN primary RPAREN
    ;

// ============================================================================
// UTILITY RULES - Override F2008 rules for F2018 entry points
// ============================================================================

// ISO/IEC 1539-1:2018 R504: specification-part override
specification_part
    : specification_part_f2018
    ;

// ISO/IEC 1539-1:2018 R509: execution-part override
execution_part
    : execution_part_f2018
    ;

// ISO/IEC 1539-1:2018 R502: program-unit override
program_unit
    : program_unit_f2018
    ;
