/*
 * Fortran2018Parser.g4
 * 
 * Fortran 2018 - Modern Fortran Revolution
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

parser grammar Fortran2018Parser;

import Fortran2008Parser;

options {
    tokenVocab = Fortran2018Lexer;
}

// ============================================================================
// FORTRAN 2018 PROGRAM STRUCTURE
// ============================================================================

// F2018 program unit (enhanced with teams and events)
program_unit_f2018
    : NEWLINE* (main_program_f2018 | module_f2018 | submodule_f2008 
      | external_subprogram_f2018) NEWLINE*
    ;

// Enhanced main program for F2018
main_program_f2018
    : program_stmt specification_part_f2018? execution_part_f2018? 
      internal_subprogram_part? end_program_stmt
    ;

// Enhanced module for F2018
module_f2018
    : module_stmt specification_part_f2018? module_subprogram_part? 
      end_module_stmt
    ;

// Enhanced external subprogram for F2018
external_subprogram_f2018
    : function_subprogram_f2018
    | subroutine_subprogram_f2018
    ;

// Enhanced function subprogram for F2018
function_subprogram_f2018
    : function_stmt_f2018 specification_part_f2018? execution_part_f2018? 
      internal_subprogram_part? end_function_stmt
    ;

// Enhanced subroutine subprogram for F2018
subroutine_subprogram_f2018
    : subroutine_stmt_f2018 specification_part_f2018? execution_part_f2018? 
      internal_subprogram_part? end_subroutine_stmt
    ;

// Enhanced function statement for F2018
function_stmt_f2018
    : prefix? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// Enhanced subroutine statement for F2018
subroutine_stmt_f2018
    : prefix? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? 
      binding_spec? NEWLINE
    ;

// ============================================================================
// ENHANCED SPECIFICATION PART (F2018)
// ============================================================================

specification_part_f2018
    : ((use_stmt | import_stmt | implicit_stmt | declaration_construct_f2018) 
      NEWLINE*)*
    ;

// Enhanced declaration construct for F2018
declaration_construct_f2018
    : derived_type_def_f2003          // Inherit F2003 types
    | class_declaration_stmt          // Inherit F2003 CLASS  
    | procedure_declaration_stmt      // Inherit F2003 procedures
    | interface_block                 // Inherit F2003 interfaces
    | volatile_stmt                   // Inherit F2003 VOLATILE
    | protected_stmt                  // Inherit F2003 PROTECTED
    | contiguous_stmt                 // Inherit F2008 CONTIGUOUS
    | event_declaration_stmt          // NEW in F2018
    | team_declaration_stmt           // NEW in F2018
    | type_declaration_stmt_f2018     // Enhanced for F2018
    | declaration_construct_f2008     // Inherit F2008 declarations
    ;

// ============================================================================
// ENHANCED EXECUTION PART (F2018)
// ============================================================================

execution_part_f2018
    : executable_construct_f2018*
    ;

// Enhanced executable construct for F2018
executable_construct_f2018
    : assignment_stmt                 // Inherit from F2003
    | call_stmt                       // Inherit from F2003
    | print_stmt                      // Inherit from F2003
    | stop_stmt_f2018                 // Enhanced in F2018
    | error_stop_stmt_f2018           // Enhanced in F2018
    | select_type_construct           // Inherit from F2003
    | select_rank_construct           // NEW in F2018
    | associate_construct             // Inherit from F2003
    | block_construct_f2008           // Inherit from F2008
    | allocate_stmt_f2008             // Inherit from F2008
    | collective_subroutine_call      // NEW in F2018
    | team_construct                  // NEW in F2018
    | event_construct                 // NEW in F2018
    | sync_construct                  // Inherit from F2008
    | wait_stmt                       // Inherit from F2003
    | flush_stmt                      // Inherit from F2003
    | if_construct                    // Inherit from F95
    | do_construct_f2018              // Enhanced in F2018
    | select_case_construct           // Inherit from F95
    | type_declaration_stmt_f2018     // F2018 allows mixed declarations
    | executable_construct_f2008      // Inherit F2008 constructs
    ;

// ============================================================================
// SELECT RANK CONSTRUCT (NEW in F2018)
// ============================================================================

select_rank_construct
    : select_rank_stmt
      (rank_case_stmt execution_part_f2018?)*
      end_select_rank_stmt
    ;

select_rank_stmt
    : (IDENTIFIER COLON)? SELECT RANK_KEYWORD LPAREN expr_f90 RPAREN NEWLINE
    ;

rank_case_stmt
    : RANK_KEYWORD LPAREN rank_value RPAREN NEWLINE   // RANK (n) or RANK (*)
    | RANK_KEYWORD DEFAULT NEWLINE                    // RANK DEFAULT
    ;

rank_value
    : INTEGER_LITERAL              // Specific rank: RANK (n)
    | '*'                          // Assumed-rank case: RANK (*)
    ;

end_select_rank_stmt
    : END SELECT (IDENTIFIER)? NEWLINE
    ;

// ============================================================================
// COLLECTIVE COARRAY OPERATIONS (NEW in F2018)
// ============================================================================

collective_subroutine_call
    : co_sum_stmt
    | co_min_stmt
    | co_max_stmt
    | co_reduce_stmt
    | co_broadcast_stmt
    ;

co_sum_stmt
    : CALL CO_SUM LPAREN collective_arg_list RPAREN NEWLINE
    ;

co_min_stmt
    : CALL CO_MIN LPAREN collective_arg_list RPAREN NEWLINE
    ;

co_max_stmt
    : CALL CO_MAX LPAREN collective_arg_list RPAREN NEWLINE
    ;

co_reduce_stmt
    : CALL CO_REDUCE LPAREN variable_f2008 COMMA operation_spec 
      (COMMA collective_stat_list)? RPAREN NEWLINE
    ;

co_broadcast_stmt
    : CALL CO_BROADCAST LPAREN variable_f2008 COMMA source_image_spec 
      (COMMA collective_stat_list)? RPAREN NEWLINE
    ;

collective_arg_list
    : variable_f2008 (COMMA collective_stat_list)?
    ;

collective_stat_list
    : collective_stat (COMMA collective_stat)*
    ;

collective_stat
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    | RESULT_IMAGE EQUALS variable_f90
    ;

operation_spec
    : IDENTIFIER                    // Operation procedure name
    ;

source_image_spec
    : SOURCE_IMAGE EQUALS expr_f90
    ;

// ============================================================================
// TEAM SUPPORT (NEW in F2018)
// ============================================================================

team_construct
    : change_team_construct
    | form_team_stmt
    ;

change_team_construct
    : change_team_stmt
      execution_part_f2018?
      end_team_stmt
    ;

change_team_stmt
    : (IDENTIFIER COLON)? CHANGE_TEAM LPAREN team_value 
      (COMMA coarray_association_list)? 
      (COMMA sync_stat_list)? RPAREN NEWLINE
    ;

end_team_stmt
    : END_TEAM (LPAREN sync_stat_list? RPAREN)? (IDENTIFIER)? NEWLINE
    ;

form_team_stmt
    : FORM_TEAM LPAREN team_number_expr COMMA team_variable 
      (COMMA form_team_stat_list)? RPAREN NEWLINE
    ;

team_value
    : expr_f90                     // Team expression
    ;

team_number_expr
    : expr_f90                     // Team number
    ;

team_variable
    : IDENTIFIER                   // Variable of TEAM_TYPE
    ;

coarray_association_list
    : coarray_association (COMMA coarray_association)*
    ;

coarray_association
    : IDENTIFIER LBRACKET cosubscript_list RBRACKET ARROW variable_f2008
    ;

form_team_stat_list
    : form_team_stat (COMMA form_team_stat)*
    ;

form_team_stat
    : NEW_INDEX EQUALS expr_f90
    | STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    ;

// Team declaration
team_declaration_stmt
    : TYPE LPAREN TEAM_TYPE RPAREN DOUBLE_COLON entity_decl_list NEWLINE
    ;

// ============================================================================
// EVENT SUPPORT (NEW in F2018)
// ============================================================================

event_construct
    : event_post_stmt
    | event_wait_stmt
    | event_query_stmt
    ;

event_post_stmt
    : EVENT_POST LPAREN event_variable (COMMA event_stat_list)? RPAREN NEWLINE
    ;

event_wait_stmt
    : EVENT_WAIT LPAREN event_variable (COMMA event_wait_spec_list)? RPAREN 
      NEWLINE
    ;

event_query_stmt
    : EVENT_QUERY LPAREN event_variable COMMA COUNT EQUALS variable_f90 
      (COMMA event_stat_list)? RPAREN NEWLINE
    ;

event_variable
    : IDENTIFIER                   // Variable of EVENT_TYPE
    ;

event_stat_list
    : event_stat (COMMA event_stat)*
    ;

event_stat
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    ;

event_wait_spec_list
    : event_wait_spec (COMMA event_wait_spec)*
    ;

event_wait_spec
    : UNTIL_COUNT EQUALS expr_f90
    | event_stat
    ;

// Event declaration
event_declaration_stmt
    : TYPE LPAREN EVENT_TYPE RPAREN DOUBLE_COLON entity_decl_list NEWLINE
    ;

// ============================================================================
// ENHANCED DO CONSTRUCTS (F2018)
// ============================================================================

do_construct_f2018
    : do_construct                    // Inherit standard DO from F95
    | do_concurrent_construct_f2018   // Enhanced in F2018
    ;

// Enhanced DO CONCURRENT with locality specifiers
do_concurrent_construct_f2018
    : do_concurrent_stmt_f2018
      execution_part_f2018?
      end_do_stmt
    ;

do_concurrent_stmt_f2018
    : (IDENTIFIER COLON)? DO_CONCURRENT concurrent_header_f2018 NEWLINE
    ;

concurrent_header_f2018
    : LPAREN forall_triplet_spec_list 
      (COMMA concurrent_locality_list)? 
      (COMMA scalar_mask_expr)? RPAREN
    ;

concurrent_locality_list
    : concurrent_locality (COMMA concurrent_locality)*
    ;

concurrent_locality
    : locality_spec DOUBLE_COLON variable_name_list
    ;

locality_spec
    : LOCAL_INIT
    | LOCAL
    | SHARED
    | DEFAULT LPAREN NONE RPAREN
    ;

variable_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ============================================================================
// ENHANCED STOP STATEMENTS (F2018)
// ============================================================================

stop_stmt_f2018
    : STOP (stop_code)? (COMMA QUIET EQUALS logical_expr)? NEWLINE
    ;

error_stop_stmt_f2018
    : ERROR_STOP (stop_code)? (COMMA QUIET EQUALS logical_expr)? NEWLINE
    ;

stop_code
    : INTEGER_LITERAL
    | string_literal
    | expr_f90                    // F2018 allows expressions
    ;

// ============================================================================
// ENHANCED TYPE DECLARATIONS (F2018)
// ============================================================================

type_declaration_stmt_f2018
    : type_declaration_stmt           // Inherit F2003 declarations
    | enhanced_intrinsic_declaration  // Inherit F2008 enhanced types
    | assumed_rank_declaration        // NEW in F2018
    ;

assumed_rank_declaration
    : type_spec COMMA DIMENSION LPAREN DOT_DOT RPAREN DOUBLE_COLON 
      entity_decl_list NEWLINE
    ;

// ============================================================================
// ENHANCED INTRINSIC FUNCTIONS (F2018)
// ============================================================================

// Override intrinsic function calls to include F2018 functions
intrinsic_function_call_f2018
    : intrinsic_function_call_f2008   // Inherit F2008 intrinsics
    | image_status_function_call      // NEW in F2018
    | collective_function_call        // NEW in F2018
    | random_init_call                // NEW in F2018
    | enhanced_math_function_call     // NEW in F2018
    ;

image_status_function_call
    : IMAGE_STATUS LPAREN expr_f90 (COMMA TEAM EQUALS team_value)? RPAREN
    | FAILED_IMAGES LPAREN (TEAM EQUALS team_value)? RPAREN
    | STOPPED_IMAGES LPAREN (TEAM EQUALS team_value)? RPAREN
    ;

collective_function_call
    : COSHAPE LPAREN expr_f90 (COMMA KIND EQUALS expr_f90)? RPAREN
    | TEAM_NUMBER LPAREN (TEAM EQUALS team_value)? RPAREN
    ;

random_init_call
    : CALL RANDOM_INIT LPAREN REPEATABLE EQUALS logical_expr COMMA 
      IMAGE_DISTINCT EQUALS logical_expr RPAREN NEWLINE
    ;

enhanced_math_function_call
    : OUT_OF_RANGE LPAREN actual_arg_list RPAREN
    | REDUCE LPAREN array_expr COMMA operation_spec 
      (COMMA DIM EQUALS expr_f90)? (COMMA MASK EQUALS logical_expr)? RPAREN
    ;

array_expr
    : expr_f90                       // Array expression
    ;

// ============================================================================
// UTILITY RULES
// ============================================================================

// Override F2008 rules to use F2018 enhanced versions where applicable
specification_part
    : specification_part_f2018
    ;

execution_part  
    : execution_part_f2018
    ;

program_unit
    : program_unit_f2018
    ;
