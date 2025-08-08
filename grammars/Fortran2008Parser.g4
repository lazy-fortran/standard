/*
 * Fortran2008Parser.g4
 * 
 * Fortran 2008 - Enhanced Parallel Programming Revolution
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

parser grammar Fortran2008Parser;

import Fortran2003Parser;

options {
    tokenVocab = Fortran2008Lexer;
}

// ============================================================================
// FORTRAN 2008 PROGRAM STRUCTURE
// ============================================================================

// F2008 program unit (enhanced with coarray and submodule support)
program_unit_f2008
    : NEWLINE* (main_program_f2008 | module_f2008 | submodule_f2008 | external_subprogram_f2008) NEWLINE*
    ;

// Enhanced main program for F2008
main_program_f2008
    : program_stmt specification_part_f2008? execution_part_f2008? 
      internal_subprogram_part? end_program_stmt
    ;

// Enhanced module for F2008
module_f2008
    : module_stmt specification_part_f2008? module_subprogram_part? end_module_stmt
    ;

// Enhanced external subprogram for F2008
external_subprogram_f2008
    : function_subprogram_f2008
    | subroutine_subprogram_f2008
    ;

// Enhanced function subprogram for F2008
function_subprogram_f2008
    : function_stmt_f2008 specification_part_f2008? execution_part_f2008? 
      internal_subprogram_part? end_function_stmt
    ;

// Enhanced subroutine subprogram for F2008
subroutine_subprogram_f2008
    : subroutine_stmt_f2008 specification_part_f2008? execution_part_f2008? 
      internal_subprogram_part? end_subroutine_stmt
    ;

// Enhanced function statement for F2008
function_stmt_f2008
    : prefix? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// Enhanced subroutine statement for F2008
subroutine_stmt_f2008
    : prefix? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? 
      binding_spec? NEWLINE
    ;

// ============================================================================
// SUBMODULES (NEW in F2008)
// ============================================================================

// Submodule definition
submodule_f2008
    : submodule_stmt specification_part_f2008? module_subprogram_part? 
      end_submodule_stmt
    ;

submodule_stmt
    : SUBMODULE LPAREN parent_identifier RPAREN submodule_identifier NEWLINE
    ;

end_submodule_stmt
    : END_SUBMODULE (submodule_identifier)? NEWLINE
    ;

parent_identifier
    : IDENTIFIER (COLON IDENTIFIER)?  // parent_module_name[:parent_submodule_name]
    ;

submodule_identifier
    : IDENTIFIER
    ;

// ============================================================================
// ENHANCED SPECIFICATION PART (F2008)
// ============================================================================

specification_part_f2008
    : ((use_stmt | import_stmt | implicit_stmt | declaration_construct_f2008) NEWLINE*)*
    ;

// Enhanced declaration construct for F2008
declaration_construct_f2008
    : derived_type_def_f2003          // Inherit F2003 types
    | class_declaration_stmt          // Inherit F2003 CLASS  
    | procedure_declaration_stmt      // Inherit F2003 procedures
    | interface_block                 // Inherit F2003 interfaces
    | volatile_stmt                   // Inherit F2003 VOLATILE
    | protected_stmt                  // Inherit F2003 PROTECTED
    | contiguous_stmt                 // NEW in F2008
    | type_declaration_stmt_f2008     // Enhanced for F2008
    | declaration_construct_f2003     // Inherit F2003 declarations
    ;

// ============================================================================
// ENHANCED EXECUTION PART (F2008)
// ============================================================================

execution_part_f2008
    : executable_construct_f2008*
    ;

// Enhanced executable construct for F2008
executable_construct_f2008
    : assignment_stmt                 // Inherit from F2003
    | call_stmt                       // Inherit from F2003
    | print_stmt                      // Inherit from F2003
    | stop_stmt                       // Inherit from F2003
    | error_stop_stmt                 // NEW in F2008
    | select_type_construct           // Inherit from F2003
    | associate_construct             // Inherit from F2003
    | block_construct_f2008           // Enhanced in F2008
    | allocate_stmt_f2008             // Enhanced in F2008
    | sync_construct                  // NEW in F2008
    | wait_stmt                       // Inherit from F2003
    | flush_stmt                      // Inherit from F2003
    | if_construct                    // Inherit from F95
    | do_construct_f2008              // Enhanced in F2008
    | select_case_construct           // Inherit from F95
    | type_declaration_stmt_f2008     // F2008 allows mixed declarations
    | executable_construct_f2003      // Inherit F2003 constructs
    ;

// ============================================================================
// COARRAY SUPPORT (NEW in F2008)
// ============================================================================

// Coarray specification in variable declarations
coarray_spec
    : LBRACKET cosubscript_list RBRACKET
    ;

cosubscript_list
    : cosubscript (COMMA cosubscript)*
    | MULTIPLY                        // [*] - any image
    ;

cosubscript
    : expr_f90                        // Expression for specific image
    | expr_f90 COLON                  // lower bound:
    | COLON expr_f90                  // :upper bound  
    | expr_f90 COLON expr_f90         // lower:upper
    | COLON                           // : (all images)
    ;

// Sync constructs for coarray synchronization
sync_construct
    : sync_all_stmt
    | sync_images_stmt
    | sync_memory_stmt
    ;

sync_all_stmt
    : SYNC_ALL (LPAREN sync_stat_list? RPAREN)? NEWLINE
    ;

sync_images_stmt
    : SYNC_IMAGES LPAREN image_set (COMMA sync_stat_list)? RPAREN NEWLINE
    ;

sync_memory_stmt  
    : SYNC_MEMORY (LPAREN sync_stat_list? RPAREN)? NEWLINE
    ;

image_set
    : MULTIPLY                        // * (all images)
    | expr_f90                        // Single image or array
    ;

sync_stat_list
    : sync_stat (COMMA sync_stat)*
    ;

sync_stat
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90
    ;

// ============================================================================
// ENHANCED DO CONSTRUCTS (F2008)
// ============================================================================

do_construct_f2008
    : do_construct                    // Inherit standard DO from F95
    | do_concurrent_construct         // NEW in F2008
    ;

// DO CONCURRENT construct for explicit parallelization
do_concurrent_construct
    : do_concurrent_stmt
      execution_part_f2008?
      end_do_stmt
    ;

do_concurrent_stmt
    : (IDENTIFIER COLON)? DO_CONCURRENT concurrent_header NEWLINE
    ;

concurrent_header
    : LPAREN forall_triplet_spec_list (COMMA scalar_mask_expr)? RPAREN
    ;

forall_triplet_spec_list
    : forall_triplet_spec (COMMA forall_triplet_spec)*
    ;

forall_triplet_spec
    : IDENTIFIER EQUALS expr_f90 COLON expr_f90 (COLON expr_f90)?
    ;

scalar_mask_expr
    : logical_expr
    ;

// ============================================================================
// ENHANCED BLOCK CONSTRUCT (F2008)
// ============================================================================

block_construct_f2008
    : (IDENTIFIER COLON)? BLOCK NEWLINE
      specification_part_f2008?        // Enhanced specification part
      execution_part_f2008?            // Enhanced execution part  
      END BLOCK (IDENTIFIER)? NEWLINE
    ;

// ============================================================================  
// ENHANCED TYPE DECLARATIONS (F2008)
// ============================================================================

type_declaration_stmt_f2008
    : type_declaration_stmt           // Inherit F2003 declarations
    | enhanced_intrinsic_declaration  // NEW F2008 intrinsic types
    ;

enhanced_intrinsic_declaration
    : INT8 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | INT16 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | INT32 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | INT64 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL32 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL64 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL128 (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    ;

// CONTIGUOUS attribute statement
contiguous_stmt
    : CONTIGUOUS DOUBLE_COLON object_name_list NEWLINE
    ;

// ============================================================================
// ENHANCED ALLOCATE STATEMENT (F2008)
// ============================================================================

allocate_stmt_f2008
    : ALLOCATE LPAREN type_spec DOUBLE_COLON allocation_list_f2008
      (COMMA alloc_opt_list)? RPAREN NEWLINE
    | ALLOCATE LPAREN allocation_list_f2008 
      (COMMA alloc_opt_list)? RPAREN NEWLINE
    ;

allocation_list_f2008
    : allocation_f2008 (COMMA allocation_f2008)*
    ;

allocation_f2008
    : IDENTIFIER coarray_spec? (LPAREN allocate_shape_spec_list RPAREN)?
    | derived_type_spec DOUBLE_COLON IDENTIFIER coarray_spec?
      (LPAREN allocate_shape_spec_list RPAREN)?
    ;

// ============================================================================
// ERROR HANDLING ENHANCEMENTS (F2008)
// ============================================================================

error_stop_stmt
    : ERROR_STOP (INTEGER_LITERAL | string_literal)? NEWLINE
    ;

// ============================================================================
// ENHANCED INTRINSIC FUNCTIONS (F2008)
// ============================================================================

// Override intrinsic function calls to include F2008 functions
intrinsic_function_call_f2008
    : intrinsic_function_call         // Inherit F2003 intrinsics
    | bessel_function_call            // NEW in F2008
    | math_function_call              // NEW in F2008
    | array_function_call             // NEW in F2008
    | image_function_call             // NEW in F2008
    ;

bessel_function_call
    : BESSEL_J0 LPAREN actual_arg_list RPAREN
    | BESSEL_J1 LPAREN actual_arg_list RPAREN
    | BESSEL_JN LPAREN actual_arg_list RPAREN
    | BESSEL_Y0 LPAREN actual_arg_list RPAREN
    | BESSEL_Y1 LPAREN actual_arg_list RPAREN
    | BESSEL_YN LPAREN actual_arg_list RPAREN
    ;

math_function_call
    : ERF LPAREN actual_arg_list RPAREN
    | ERFC LPAREN actual_arg_list RPAREN
    | GAMMA LPAREN actual_arg_list RPAREN
    | LOG_GAMMA LPAREN actual_arg_list RPAREN
    ;

array_function_call
    : NORM2 LPAREN actual_arg_list RPAREN
    | PARITY LPAREN actual_arg_list RPAREN
    | FINDLOC LPAREN actual_arg_list RPAREN
    ;

image_function_call
    : THIS_IMAGE LPAREN actual_arg_list? RPAREN
    | NUM_IMAGES LPAREN actual_arg_list? RPAREN
    | STORAGE_SIZE LPAREN actual_arg_list RPAREN
    ;

// ============================================================================
// ENHANCED PRIMARY EXPRESSIONS (F2008)
// ============================================================================

// Override primary to include F2008 enhancements
primary_f2008
    : primary                         // Inherit F2003 primary expressions
    | intrinsic_function_call_f2008   // Enhanced intrinsic functions
    ;

// Enhanced variable reference with coarray support
variable_f2008
    : IDENTIFIER coarray_spec? (PERCENT IDENTIFIER)*
    | IDENTIFIER (LPAREN actual_arg_list RPAREN)? coarray_spec?
    ;

// ============================================================================
// UTILITY RULES
// ============================================================================

// Override F2003 rules to use F2008 enhanced versions where applicable
specification_part
    : specification_part_f2008
    ;

execution_part  
    : execution_part_f2008
    ;

program_unit
    : program_unit_f2008
    ;