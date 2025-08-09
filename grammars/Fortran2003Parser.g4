/*
 * Fortran2003Parser.g4
 * 
 * Fortran 2003 - Object-Oriented Programming Revolution  
 * Unified parser supporting both fixed-form (.f, .for) and free-form (.f90+)
 * 
 * This is a simplified implementation focusing on the major F2003 features
 */

parser grammar Fortran2003Parser;

import Fortran95Parser;

options {
    tokenVocab = Fortran2003Lexer;
}

// ============================================================================
// FORTRAN 2003 PROGRAM STRUCTURE
// ============================================================================

// Keywords that can be used as identifiers in certain contexts
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
    ;

// F2003 program unit (enhanced with OOP features)
program_unit_f2003
    : NEWLINE* (main_program_f2003 | module_f2003 | external_subprogram_f2003) NEWLINE*
    ;

// Enhanced main program for F2003
main_program_f2003
    : program_stmt specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_program_stmt
    ;

// F2003 program statement with newline support
program_stmt
    : PROGRAM IDENTIFIER NEWLINE*
    ;

// F2003 end program statement with newline support
end_program_stmt
    : END (PROGRAM (IDENTIFIER)?)? NEWLINE*
    ;

// Override F90 main_program to use F2003 specification part
main_program
    : program_stmt specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_program_stmt
    ;

// Enhanced module for F2003
module_f2003
    : module_stmt NEWLINE* specification_part_f2003? NEWLINE* module_subprogram_part? NEWLINE* end_module_stmt
    ;

// Override F90 specification_part to use F2003 enhanced version
specification_part
    : specification_part_f2003
    ;

// Override F90 suffix to handle intrinsics as identifiers
suffix
    : RESULT LPAREN identifier_or_keyword RPAREN
    ;

// Override F90 module to use F2003 specification part - be explicit about structure
module
    : module_stmt NEWLINE* 
      specification_part_f2003? NEWLINE* 
      ( contains_stmt NEWLINE* (module_subprogram NEWLINE*)* )? 
      end_module_stmt
    ;

// Override F90 module_stmt to handle newlines
module_stmt  
    : MODULE IDENTIFIER NEWLINE*
    ;

// Override F90 end_module_stmt to handle newlines
end_module_stmt
    : END_MODULE (IDENTIFIER)? NEWLINE*
    ;

// Enhanced external subprogram for F2003
external_subprogram_f2003
    : function_subprogram_f2003
    | subroutine_subprogram_f2003
    ;

// Enhanced function subprogram for F2003
function_subprogram_f2003
    : function_stmt_f2003 specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_function_stmt
    ;

// Enhanced subroutine subprogram for F2003
subroutine_subprogram_f2003
    : subroutine_stmt_f2003 specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_subroutine_stmt
    ;

// Enhanced function statement for F2003
function_stmt_f2003
    : prefix? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// Enhanced subroutine statement for F2003
subroutine_stmt_f2003
    : prefix? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? binding_spec? NEWLINE
    ;

// Enhanced internal subprogram part for F2003
internal_subprogram_part_f2003
    : contains_stmt internal_subprogram+
    ;

// Override module subprogram to use F2003-enhanced versions
module_subprogram
    : function_subprogram_f2003
    | subroutine_subprogram_f2003
    ;

// Override interface body to use F2003 specification part (for IMPORT support)
interface_body
    : function_stmt_interface specification_part_f2003? end_function_stmt_interface
    | subroutine_stmt_interface specification_part_f2003? end_subroutine_stmt_interface
    ;

// Interface-specific statement overrides with NEWLINE support
subroutine_stmt_interface
    : (prefix)? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? binding_spec? NEWLINE
    ;

function_stmt_interface
    : (prefix)? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      (suffix)? binding_spec? NEWLINE
    ;

end_subroutine_stmt_interface
    : END (SUBROUTINE (IDENTIFIER)?)? NEWLINE
    ;

end_function_stmt_interface
    : END (FUNCTION (IDENTIFIER)?)? NEWLINE
    ;

// Override F90 end statements to handle NEWLINE properly
end_subroutine_stmt
    : END (SUBROUTINE (IDENTIFIER)?)? NEWLINE?
    ;

end_function_stmt
    : END (FUNCTION (IDENTIFIER)?)? NEWLINE?
    ;

// Override interface_stmt and end_interface_stmt for NEWLINE support
interface_stmt
    : INTERFACE (generic_spec)? NEWLINE
    | ABSTRACT_INTERFACE (generic_spec)? NEWLINE
    ;

end_interface_stmt
    : END_INTERFACE (generic_spec)? NEWLINE
    ;

// Override interface_block to handle NEWLINEs between specifications
interface_block
    : interface_stmt (NEWLINE* interface_specification)* NEWLINE* end_interface_stmt
    ;

// Enhanced specification part for F2003 - non-greedy to avoid consuming module-level contains
specification_part_f2003
    : specification_element_f2003*
    ;

specification_element_f2003
    : NEWLINE* (use_stmt | import_stmt | implicit_stmt | declaration_construct_f2003) NEWLINE?
    ;

// Enhanced declaration construct for F2003  
declaration_construct_f2003
    : derived_type_def_f2003        // Try TYPE definitions first (highest priority)
    | interface_block              // F90 interface blocks (before generic declarations)
    | class_declaration_stmt        // CLASS declarations before TYPE variables
    | procedure_declaration_stmt    // PROCEDURE declarations  
    | volatile_stmt                 // Standalone "volatile :: vars"
    | protected_stmt               // Standalone "protected :: vars"
    | type_declaration_stmt         // TYPE variables last (more specific first)
    | declaration_construct        // Inherit F95 declarations
    ;

// Enhanced execution part for F2003
execution_part_f2003
    : execution_construct_f2003*
    ;

// Execution construct with newline handling  
execution_construct_f2003
    : NEWLINE* executable_construct_f2003_inner NEWLINE*
    ;

// Enhanced executable construct for F2003
executable_construct_f2003_inner
    : assignment_stmt
    | call_stmt
    | print_stmt
    | stop_stmt
    | select_type_construct
    | associate_construct
    | block_construct
    | allocate_stmt_f2003
    | wait_stmt
    | flush_stmt
    | open_stmt
    | close_stmt
    | write_stmt
    | read_stmt
    | if_construct
    | do_construct
    | select_case_construct
    | type_declaration_stmt   // F2003 allows mixed declarations and executable statements
    | executable_construct    // Inherit F95 constructs
    ;

// ============================================================================
// FORTRAN 2003 NEW CONSTRUCTS - Object-Oriented Programming
// ============================================================================

// Enhanced derived type definition with OOP features (F2003)
derived_type_def_f2003
    : derived_type_stmt_f2003 NEWLINE*
      type_param_def_stmt*
      private_or_sequence*
      component_part?
      type_bound_procedure_part?
      end_type_stmt_f2003
    ;

// F2003 enhanced type statement with OOP attributes (following LLVM Flang structure)
derived_type_stmt_f2003
    : TYPE (COMMA type_attr_spec_list DOUBLE_COLON | DOUBLE_COLON)? type_name
      (LPAREN type_param_name_list RPAREN)?
    ;

// F2003 end type statement 
end_type_stmt_f2003
    : END_TYPE type_name? NEWLINE?
    ;

// Parent type specification for inheritance
parent_type_name
    : IDENTIFIER
    ;

// Type-bound procedure part (following reference grammar)
type_bound_procedure_part
    : contains_stmt binding_private_stmt? type_bound_proc_binding*
    ;

binding_private_stmt
    : PRIVATE NEWLINE
    ;

type_bound_proc_binding
    : type_bound_procedure_stmt NEWLINE?
    | type_bound_generic_stmt NEWLINE?
    | final_procedure_stmt NEWLINE?
    ;

// Override F90 contains_stmt to handle NEWLINE properly in F2003
contains_stmt
    : CONTAINS NEWLINE?
    ;

// Component part (general version for other contexts)
component_part
    : component_def_stmt*
    ;

component_def_stmt
    : data_component_def_stmt NEWLINE?   // Data component declarations
    | proc_component_def_stmt NEWLINE?   // Procedure pointer components
    ;

data_component_def_stmt
    : type_declaration_stmt          // Regular type declarations for components
    ;

private_or_sequence
    : private_components_stmt
    | sequence_stmt
    ;

private_components_stmt
    : PRIVATE NEWLINE
    ;

sequence_stmt
    : SEQUENCE NEWLINE
    ;

// Removed private_sequence_stmt - replaced by private_or_sequence

// Type-bound procedure statement - simplified and more explicit
type_bound_procedure_stmt
    : PROCEDURE DOUBLE_COLON type_bound_proc_decl_list
    | PROCEDURE COMMA binding_attr_list DOUBLE_COLON type_bound_proc_decl_list
    | PROCEDURE LPAREN IDENTIFIER RPAREN DOUBLE_COLON type_bound_proc_decl_list
    | PROCEDURE LPAREN IDENTIFIER RPAREN COMMA binding_attr_list DOUBLE_COLON type_bound_proc_decl_list
    ;

binding_attr_list
    : binding_attr (COMMA binding_attr)*
    ;

binding_attr
    : access_spec           // PUBLIC or PRIVATE  
    | DEFERRED
    | NOPASS
    | PASS (LPAREN IDENTIFIER RPAREN)?
    | NON_OVERRIDABLE
    ;

type_bound_proc_decl_list
    : type_bound_proc_decl (COMMA type_bound_proc_decl)*
    ;

type_bound_proc_decl
    : binding_name (POINTER_ASSIGN procedure_name)?
    ;

binding_name
    : IDENTIFIER
    ;

procedure_name
    : IDENTIFIER
    ;

access_spec
    : PUBLIC
    | PRIVATE
    ;

// Remove old proc_binding rules - replaced by type_bound_proc_decl

// Generic type-bound procedures
type_bound_generic_stmt
    : GENERIC (COMMA (PUBLIC | PRIVATE))? DOUBLE_COLON generic_spec
      POINTER_ASSIGN generic_binding_list NEWLINE
    ;

generic_binding_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// FINAL procedures (destructors)
final_procedure_stmt
    : FINAL DOUBLE_COLON? final_subroutine_name_list NEWLINE
    ;

final_subroutine_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// Procedure attribute specifications
proc_attr_spec_list
    : proc_attr_spec (COMMA proc_attr_spec)*
    ;

proc_attr_spec
    : PUBLIC
    | PRIVATE  
    | NOPASS
    | PASS (LPAREN IDENTIFIER RPAREN)?
    | DEFERRED
    | POINTER    // F2003 procedure pointers
    ;

// Type attribute specifications
type_attr_spec_list
    : type_attr_spec (COMMA type_attr_spec)*
    ;

type_attr_spec
    : PUBLIC
    | PRIVATE
    | ABSTRACT
    | EXTENDS LPAREN IDENTIFIER RPAREN
    | BIND LPAREN C RPAREN             // BIND(C) for derived types
    ;

// Type parameter definitions
type_param_def_stmt_list
    : type_param_def_stmt+
    ;

type_param_def_stmt
    : INTEGER COMMA type_param_attr_spec DOUBLE_COLON 
      type_param_decl_list NEWLINE
    ;

// Type parameter declaration with optional default value
type_param_decl_list
    : type_param_decl (COMMA type_param_decl)*
    ;

type_param_decl
    : IDENTIFIER (EQUALS default_init_expr)?
    ;

// Default initialization expression for type parameters
default_init_expr
    : IDENTIFIER LPAREN expr_f90 RPAREN  // Function call like kind(0.0)
    | expr_f90  // Simple expression
    ;

type_param_attr_spec
    : KIND
    | LEN
    ;

type_param_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ============================================================================
// ASSOCIATE CONSTRUCT
// ============================================================================

associate_construct
    : (IDENTIFIER COLON)? ASSOCIATE LPAREN association_list RPAREN NEWLINE
      execution_part_f2003?
      END ASSOCIATE (IDENTIFIER)? NEWLINE?
    ;

association_list
    : association (COMMA association)*
    ;

association
    : identifier_or_keyword POINTER_ASSIGN selector
    ;

selector
    : expr_f2003   // Any F2003 expression including component references
    ;

// ============================================================================
// BLOCK CONSTRUCT  
// ============================================================================

block_construct
    : (IDENTIFIER COLON)? BLOCK NEWLINE
      specification_part_f2003?
      execution_part_f2003?
      END BLOCK (IDENTIFIER)? NEWLINE?
    ;

// ============================================================================
// PROCEDURE POINTERS
// ============================================================================

procedure_declaration_stmt
    : PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN 
      (COMMA proc_attr_spec_list)? DOUBLE_COLON 
      procedure_entity_decl_list NEWLINE
    ;

procedure_entity_decl_list
    : procedure_entity_decl (COMMA procedure_entity_decl)*
    ;

procedure_entity_decl
    : IDENTIFIER (POINTER_ASSIGN proc_target)?
    ;

proc_target
    : IDENTIFIER                    // Target procedure name
    | IDENTIFIER LPAREN RPAREN     // null() or other function call
    ;

// Procedure pointer components (different syntax from regular procedure declarations)
// Must have either parentheses or POINTER attribute to be a component
proc_component_def_stmt
    : PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN COMMA
      proc_component_attr_spec_list DOUBLE_COLON proc_decl_list NEWLINE
    | PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN DOUBLE_COLON proc_decl_list NEWLINE
    ;

proc_component_attr_spec_list
    : proc_component_attr_spec (COMMA proc_component_attr_spec)*
    ;

proc_component_attr_spec
    : PUBLIC
    | PRIVATE
    | NOPASS
    | PASS (LPAREN IDENTIFIER RPAREN)?
    | POINTER
    | DEFERRED      // F2003 deferred type-bound procedures
    ;

proc_decl_list
    : proc_decl (COMMA proc_decl)*
    ;

proc_decl
    : IDENTIFIER (POINTER_ASSIGN IDENTIFIER)?  // proc_name [=> init_target]
    ;

// ============================================================================
// CLASS DECLARATIONS & POLYMORPHISM
// ============================================================================

class_declaration_stmt
    : CLASS LPAREN type_spec_or_star RPAREN (COMMA attr_spec_list)? 
      DOUBLE_COLON entity_decl_list
    ;

type_spec_or_star
    : SHAPE_INTRINSIC      // CLASS(SHAPE) - derived type named SHAPE (most specific first)
    | '*'                  // CLASS(*) - unlimited polymorphic
    | type_spec            // CLASS(INTEGER) - intrinsic types
    | IDENTIFIER           // CLASS(type_name) - derived types
    ;

// SELECT TYPE construct for runtime type selection
select_type_construct
    : select_type_stmt NEWLINE*
      (type_guard_stmt execution_part_f2003?)*
      end_select_type_stmt
    ;

select_type_stmt
    : (IDENTIFIER COLON)? SELECT_TYPE LPAREN (IDENTIFIER POINTER_ASSIGN)? selector_expr RPAREN
    ;

selector_expr
    : primary
    ;

type_guard_stmt
    : TYPE_IS LPAREN type_spec_or_derived RPAREN (IDENTIFIER)? NEWLINE
    | CLASS_IS LPAREN type_spec_or_derived RPAREN (IDENTIFIER)? NEWLINE
    | CLASS_DEFAULT (IDENTIFIER)? NEWLINE
    ;

type_spec_or_derived
    : type_spec
    | IDENTIFIER   // User-defined type name
    ;

end_select_type_stmt
    : END_SELECT (IDENTIFIER)? NEWLINE
    ;

// ============================================================================
// IMPORT STATEMENT
// ============================================================================

import_stmt
    : IMPORT (DOUBLE_COLON import_name_list)? NEWLINE
    ;

import_name_list
    : import_name (COMMA import_name)*
    ;

import_name
    : IDENTIFIER
    | c_interop_type  // Allow C interop types in IMPORT statements
    ;

// ============================================================================
// ENHANCED ALLOCATE STATEMENT
// ============================================================================

allocate_stmt_f2003
    : ALLOCATE LPAREN allocation_list
      (COMMA alloc_opt_list)? RPAREN NEWLINE
    ;

allocation_list
    : allocation (COMMA allocation)*
    ;

allocation
    : type_spec_allocation
    | identifier_or_keyword (LPAREN allocate_shape_spec_list RPAREN)?
    ;

// Type specification in ALLOCATE for PDTs
type_spec_allocation
    : derived_type_spec DOUBLE_COLON identifier_or_keyword 
      (LPAREN allocate_shape_spec_list RPAREN)?
    ;

allocate_shape_spec_list
    : allocate_shape_spec (COMMA allocate_shape_spec)*
    ;

allocate_shape_spec
    : expr_f90 (COLON expr_f90)?
    ;

alloc_opt_list
    : alloc_opt (COMMA alloc_opt)*
    ;

alloc_opt
    : STAT EQUALS identifier_or_keyword
    | ERRMSG EQUALS identifier_or_keyword  
    | SOURCE EQUALS expr_f2003
    | MOLD EQUALS expr_f2003
    ;

// ============================================================================
// ENHANCED I/O STATEMENTS
// ============================================================================

wait_stmt
    : WAIT LPAREN wait_spec_list RPAREN NEWLINE
    ;

wait_spec_list
    : wait_spec (COMMA wait_spec)*
    ;

wait_spec
    : UNIT EQUALS primary                     // unit=value
    | ID EQUALS primary                       // id=async_id
    | IOSTAT EQUALS primary                   // iostat=variable
    | IOMSG EQUALS primary                    // iomsg=variable
    | ERR EQUALS primary                      // err=label
    | END EQUALS primary                      // end=label (wait for endfile)
    | EOR EQUALS primary                      // eor=label (wait for end-of-record)
    | primary                                 // Positional unit specification
    ;

flush_stmt
    : FLUSH (LPAREN flush_spec_list RPAREN)? NEWLINE
    ;

flush_spec_list
    : flush_spec (COMMA flush_spec)*
    ;

flush_spec
    : UNIT EQUALS primary                     // unit=value (most common)
    | IOSTAT EQUALS primary                   // iostat=variable  
    | IOMSG EQUALS primary                    // iomsg=variable
    | ERR EQUALS primary                      // err=label
    | primary                                 // Positional unit specification
    ;

// Override F90 io_control_spec to use EQUALS instead of ASSIGN (F2003 syntax)
io_control_spec
    : UNIT EQUALS primary                    // unit=10, unit=*
    | FMT EQUALS primary                     // fmt=*, fmt=100, fmt=format_var
    | IOSTAT EQUALS primary                  // iostat=ios_var
    | ERR EQUALS primary                     // err=100
    | END EQUALS primary                     // end=200  
    | EOR EQUALS primary                     // eor=300
    | ADVANCE EQUALS primary                 // advance='yes'/'no'
    | SIZE EQUALS primary                    // size=size_var
    | REC EQUALS primary                     // rec=record_num
    | primary                                // Positional unit
    ;

io_control_spec_list
    : io_control_spec (COMMA io_control_spec)*
    ;

// F2003 I/O statements - completely override parent rules
open_stmt
    : OPEN LPAREN f2003_io_spec_list RPAREN NEWLINE?
    ;

close_stmt
    : CLOSE LPAREN f2003_io_spec_list RPAREN NEWLINE?
    ;

write_stmt
    : WRITE LPAREN f2003_io_spec_list RPAREN (output_item_list)? NEWLINE?
    ;

read_stmt
    : READ LPAREN f2003_io_spec_list RPAREN (input_item_list)? NEWLINE?
    ;

// Unified F2003 I/O specification list - handles all I/O statements
f2003_io_spec_list
    : f2003_io_spec (COMMA f2003_io_spec)*
    ;

f2003_io_spec
    : IDENTIFIER EQUALS primary                  // Regular identifier = value (file='test.dat')
    | UNIT EQUALS primary                        // unit=10, unit=*
    | FILE EQUALS primary                        // file='filename'
    | ACCESS EQUALS primary                      // access='stream'
    | FORM EQUALS primary                        // form='unformatted'
    | STATUS EQUALS primary                      // status='new'
    | BLANK EQUALS primary                       // blank='null'  
    | POSITION EQUALS primary                    // position='rewind'
    | ACTION EQUALS primary                      // action='read'
    | DELIM EQUALS primary                       // delim='apostrophe'
    | PAD EQUALS primary                         // pad='yes'
    | RECL EQUALS primary                        // recl=100
    | IOSTAT EQUALS primary                      // iostat=ios
    | IOMSG EQUALS primary                       // iomsg=msg
    | ERR EQUALS primary                         // err=100
    | END EQUALS primary                         // end=200
    | EOR EQUALS primary                         // eor=300
    | ADVANCE EQUALS primary                     // advance='yes'
    | SIZE EQUALS primary                        // size=isize
    | REC EQUALS primary                         // rec=irec
    | ASYNCHRONOUS EQUALS primary                // asynchronous='yes' (F2003)
    | STREAM EQUALS primary                      // stream='yes' (F2003)
    | PENDING EQUALS primary                     // pending=pending_var (F2003)
    | ID EQUALS primary                          // id=id_var (F2003)
    | FMT EQUALS primary                         // fmt=*, fmt=100
    | primary                                    // Positional specifier: *, 10, etc.
    ;

output_item_list
    : output_item (COMMA output_item)*
    ;

output_item
    : expr_f2003
    | io_implied_do
    ;

input_item_list
    : input_item (COMMA input_item)*
    ;

input_item
    : identifier_or_keyword
    | io_implied_do
    ;

io_implied_do
    : LPAREN io_implied_do_object_list COMMA identifier_or_keyword EQUALS 
      expr_f2003 COMMA expr_f2003 (COMMA expr_f2003)? RPAREN
    ;

io_implied_do_object_list
    : io_implied_do_object (COMMA io_implied_do_object)*
    ;

io_implied_do_object
    : input_item
    | output_item
    ;

// PRINT statement (legacy I/O support)
print_stmt
    : PRINT '*' COMMA actual_arg_list                // print *, args
    | PRINT primary COMMA actual_arg_list            // print format, args
    | PRINT '*'                                      // print *
    ;

// STOP statement
stop_stmt
    : STOP (INTEGER_LITERAL | string_literal)?
    ;

// DEALLOCATE statement
deallocate_stmt
    : DEALLOCATE LPAREN allocation_list RPAREN NEWLINE
    ;

// ============================================================================
// VOLATILE AND PROTECTED ATTRIBUTES
// ============================================================================

volatile_stmt
    : VOLATILE DOUBLE_COLON object_name_list NEWLINE
    ;

protected_stmt
    : PROTECTED DOUBLE_COLON object_name_list NEWLINE
    ;

object_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ============================================================================
// SIMPLIFIED UTILITY RULES
// ============================================================================

use_stmt
    : USE IDENTIFIER NEWLINE
    | USE IDENTIFIER COMMA ONLY COLON only_list NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON ieee_module_name NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON ieee_module_name COMMA 
      ONLY COLON ieee_only_list NEWLINE
    ;

// IEEE intrinsic modules
ieee_module_name
    : IEEE_EXCEPTIONS
    | IEEE_ARITHMETIC  
    | IEEE_FEATURES
    ;

// IEEE-specific only list for intrinsic modules
ieee_only_list
    : ieee_entity (COMMA ieee_entity)*
    ;

ieee_entity
    : ieee_exception_type
    | ieee_special_value
    | ieee_rounding_mode
    | ieee_feature_name
    | IDENTIFIER  // Other IEEE procedures/constants
    ;

ieee_exception_type
    : IEEE_OVERFLOW
    | IEEE_UNDERFLOW
    | IEEE_DIVIDE_BY_ZERO
    | IEEE_INVALID
    | IEEE_INEXACT
    ;

ieee_special_value
    : IEEE_POSITIVE_INF
    | IEEE_NEGATIVE_INF
    | IEEE_QUIET_NAN
    | IEEE_SIGNALING_NAN
    ;

ieee_rounding_mode
    : IEEE_NEAREST
    | IEEE_TO_ZERO
    | IEEE_UP
    | IEEE_DOWN
    ;

ieee_feature_name
    : IEEE_DATATYPE
    | IEEE_DENORMAL
    | IEEE_DIVIDE
    | IEEE_HALTING
    | IEEE_INEXACT_FLAG
    | IEEE_INF
    | IEEE_INVALID_FLAG
    | IEEE_NAN
    | IEEE_ROUNDING
    | IEEE_SQRT
    | IEEE_UNDERFLOW_FLAG
    ;

implicit_stmt
    : IMPLICIT NONE NEWLINE?
    | IMPLICIT implicit_spec_list NEWLINE?
    ;

implicit_spec_list
    : implicit_spec (COMMA implicit_spec)*
    ;

implicit_spec
    : type_spec LPAREN letter_spec_list RPAREN
    ;

type_spec
    : INTEGER
    | REAL
    | COMPLEX
    | CHARACTER
    | LOGICAL
    | c_interop_type
    ;

letter_spec_list
    : letter_spec (COMMA letter_spec)*
    ;

letter_spec
    : IDENTIFIER
    | IDENTIFIER MINUS IDENTIFIER
    ;

only_list
    : only_name (COMMA only_name)*
    ;

only_name
    : IDENTIFIER
    | c_interop_type
    ;

declaration_construct
    : derived_type_def_f2003
    | class_declaration_stmt
    | procedure_declaration_stmt
    | type_declaration_stmt
    | volatile_stmt
    | protected_stmt
    ;

type_declaration_stmt
    : INTEGER kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON 
      entity_decl_list NEWLINE
    | REAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON 
      entity_decl_list NEWLINE
    | LOGICAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON 
      entity_decl_list NEWLINE
    | CHARACTER char_selector? (COMMA attr_spec_list)? DOUBLE_COLON 
      entity_decl_list NEWLINE
    | c_interop_type (COMMA attr_spec_list)? DOUBLE_COLON 
      entity_decl_list NEWLINE
    | TYPE LPAREN derived_type_spec RPAREN (COMMA attr_spec_list)? 
      DOUBLE_COLON entity_decl_list NEWLINE
    | CLASS LPAREN type_spec_or_star RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    ;

// Kind selector for parameterized types
kind_selector
    : LPAREN kind_param RPAREN              // (k) - kind parameter
    | LPAREN KIND EQUALS kind_param RPAREN  // (kind=k) - explicit kind
    ;

kind_param
    : IDENTIFIER
    | INTEGER_LITERAL    // For numeric kinds like 4, 8
    | LABEL              // Statement labels are also parsed as integers
    | c_interop_type
    ;

// Character selector (enhanced for F2003)
char_selector
    : LPAREN char_length_spec RPAREN              // (len) or (len=n)
    ;

char_length_spec
    : c_interop_type                               // C interop: (c_char)
    | expr_f2003                                   // Simple length: (50)
    | LEN EQUALS expr_f2003                       // Explicit length: (len=50)  
    ;

// Derived type specification (enhanced for F2003)
derived_type_spec
    : IDENTIFIER                                          // Basic type name
    | IDENTIFIER LPAREN type_param_spec_list RPAREN      // Parameterized type
    | c_interop_type                                      // C interop types like c_int, c_ptr
    ;

// Type parameter specification list for instantiation
type_param_spec_list
    : type_param_spec (COMMA type_param_spec)*
    ;

type_param_spec
    : identifier_or_keyword EQUALS type_param_value    // kind=real64, n=:, m=*
    | type_param_value                                  // positional parameter
    ;

type_param_value
    : expr_f90        // Expression like 8, real64, or 100
    | COLON           // Deferred parameter (:)
    | '*'             // Assumed parameter (*)
    ;

attr_spec_list
    : attr_spec (COMMA attr_spec)*
    ;

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
    | VALUE        // F2003 C interoperability attribute
    ;

intent_spec
    : IN
    | OUT
    | INOUT
    ;

entity_decl_list
    : entity_decl (COMMA entity_decl)*
    ;

entity_decl
    : identifier_or_keyword (LPAREN array_spec RPAREN)? (EQUALS expr_f2003)?
        // F2003 entity declaration with initialization - use F2003 expressions for array constructors
    ;

// Override F90 entity declaration to support keywords as identifiers
entity_decl_f90
    : identifier_or_keyword (LPAREN array_spec_f90 RPAREN)? (MULTIPLY char_length)?
      (ASSIGN expr_f90)?
    ;

// Override F90 module_subprogram_part to handle NEWLINEs properly
module_subprogram_part
    : contains_stmt NEWLINE* (module_subprogram NEWLINE*)*
    ;

// Array specification (simplified)
array_spec
    : array_spec_element (COMMA array_spec_element)*
    ;

array_spec_element
    : expr_f90 (COLON expr_f90)?    // lower:upper or just upper
    | expr_f90 COLON                // lower:
    | COLON                         // :
    ;

execution_part
    : executable_construct*
    ;

executable_construct
    : assignment_stmt
    | call_stmt
    | print_stmt
    | stop_stmt
    | associate_construct
    | block_construct
    | allocate_stmt_f2003
    | deallocate_stmt
    | wait_stmt
    | flush_stmt
    | if_construct
    | do_construct
    | select_case_construct
    ;

// Enhanced assignment statement for F2003 - support array access and components
assignment_stmt
    : lhs_expression EQUALS expr_f2003
    | lhs_expression POINTER_ASSIGN primary
        // Procedure pointer assignment
    ;

// Enhanced F2003 expression that includes intrinsic functions  
expr_f2003
    : expr_f2003 (GT | GT_OP) expr_f2003      // Greater than
    | expr_f2003 (LT | LT_OP) expr_f2003      // Less than  
    | expr_f2003 (GE | GE_OP) expr_f2003      // Greater than or equal
    | expr_f2003 (LE | LE_OP) expr_f2003      // Less than or equal
    | expr_f2003 (EQ | EQ_OP) expr_f2003      // Equal
    | expr_f2003 (NE | NE_OP) expr_f2003      // Not equal
    | expr_f2003 CONCAT expr_f2003            // String concatenation (//)
    | expr_f2003 POWER expr_f2003             // Exponentiation (highest precedence)
    | expr_f2003 (MULTIPLY | DIVIDE) expr_f2003  // Multiplication/division
    | expr_f2003 (PLUS | MINUS) expr_f2003    // Addition/subtraction
    | MINUS expr_f2003                        // Unary minus
    | PLUS expr_f2003                         // Unary plus
    | expr_f90                                // Inherit F90 expressions  
    | primary                                 // Use F2003 primary (includes intrinsic_function_call)
    ;

// Left-hand side expression (variable, array element, component)
lhs_expression
    : identifier_or_keyword                                          // Simple variable
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN         // Array element (args optional for empty calls)
    | identifier_or_keyword PERCENT identifier_or_keyword          // Component
    | identifier_or_keyword PERCENT identifier_or_keyword LPAREN actual_arg_list? RPAREN  // Component array/method
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN PERCENT identifier_or_keyword  // Array element's component: shapes(i)%draw
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN PERCENT identifier_or_keyword LPAREN actual_arg_list? RPAREN  // Array element's component method: shapes(i)%draw()
    ;

call_stmt
    : CALL lhs_expression                                            // Enhanced for F2003 - support component calls
    | CALL identifier_or_keyword (LPAREN actual_arg_list? RPAREN)?  // Traditional procedure calls
    ;

actual_arg_list
    : actual_arg (COMMA actual_arg)*
    ;

// Argument can be positional or named (keyword argument)
actual_arg
    : identifier_or_keyword EQUALS expr_f2003    // Named argument: kind=real64
    | expr_f2003                                 // Positional argument
    ;

if_construct
    : IF LPAREN logical_expr RPAREN THEN NEWLINE
      execution_part_f2003?
      (ELSE NEWLINE execution_part_f2003?)?
      END IF NEWLINE?
    ;

logical_expr
    : expr_f2003
    ;

do_construct
    : DO NEWLINE
      execution_part_f2003?
      END DO NEWLINE?
    | DO identifier_or_keyword EQUALS expr_f2003 COMMA expr_f2003 (COMMA expr_f2003)? NEWLINE
      do_body_f2003
      END DO NEWLINE?
    ;

do_body_f2003
    : (NEWLINE | assignment_stmt | call_stmt | print_stmt | allocate_stmt_f2003 | deallocate_stmt | if_construct | do_construct | block_construct)*
    ;

select_case_construct
    : SELECT CASE LPAREN primary RPAREN NEWLINE
      case_construct+
      END SELECT NEWLINE?
    ;

case_construct
    : CASE LPAREN case_value_list RPAREN NEWLINE
      execution_part_f2003?
    | CASE DEFAULT NEWLINE
      execution_part_f2003?
    ;

case_value_list
    : primary (COMMA primary)*
    ;

// ============================================================================
// PRIMARY EXPRESSIONS
// ============================================================================

primary
    : identifier_or_keyword (PERCENT identifier_or_keyword)*
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN
    | identifier_or_keyword DOUBLE_QUOTE_STRING      // Prefixed string: c_char_"Hello"
    | identifier_or_keyword SINGLE_QUOTE_STRING      // Prefixed string: c_char_'Hello'
    | intrinsic_function_call
    | ieee_constant
    | INTEGER_LITERAL
    | LABEL              // Accept LABEL as integer literal (token precedence issue)
    | REAL_LITERAL
    | SINGLE_QUOTE_STRING
    | DOUBLE_QUOTE_STRING
    | '*'                // Asterisk for I/O format specifiers (*, list-directed)
    | array_constructor  // Array constructor [elem1, elem2, ...]
    | LPAREN primary RPAREN
    ;

// F90 Array constructor syntax
array_constructor
    : LSQUARE array_constructor_elements? RSQUARE
    ;

array_constructor_elements
    : array_constructor_element (COMMA array_constructor_element)*
    ;

array_constructor_element
    : expr_f2003                           // Regular expression
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN  // Constructor call: type_name(args)
    ;

// IEEE constants that can appear in expressions
ieee_constant
    : ieee_special_value
    | ieee_exception_type
    | ieee_rounding_mode
    | ieee_feature_name
    ;

// Handle intrinsic function calls
intrinsic_function_call
    : SELECTED_REAL_KIND LPAREN actual_arg_list RPAREN
    | SELECTED_INT_KIND LPAREN actual_arg_list RPAREN
    | KIND LPAREN actual_arg_list RPAREN
    | SIZE LPAREN actual_arg_list RPAREN        // SIZE intrinsic function
    | REAL LPAREN actual_arg_list RPAREN        // REAL type conversion
    | INTEGER LPAREN actual_arg_list RPAREN     // INTEGER type conversion
    | LOGICAL LPAREN actual_arg_list RPAREN     // LOGICAL type conversion
    | CHARACTER LPAREN actual_arg_list RPAREN   // CHARACTER type conversion
    | COMPLEX LPAREN actual_arg_list RPAREN     // COMPLEX type conversion
    | SUM_INTRINSIC LPAREN actual_arg_list RPAREN  // SUM function
    | ieee_function_call
    ;

// IEEE arithmetic function calls
ieee_function_call
    : ieee_inquiry_function LPAREN actual_arg_list RPAREN
    | ieee_value_function LPAREN actual_arg_list RPAREN
    | IDENTIFIER LPAREN actual_arg_list RPAREN  // General IEEE procedures
    ;

// IEEE inquiry functions
ieee_inquiry_function
    : IDENTIFIER  // ieee_is_nan, ieee_is_finite, ieee_is_normal, etc.
    ;

// IEEE value functions
ieee_value_function
    : IDENTIFIER  // ieee_value, ieee_copy_sign, ieee_next_after, etc.
    ;

// Override F90 function_reference to handle intrinsic functions
function_reference_f90
    : IDENTIFIER LPAREN actual_arg_spec_list? RPAREN
    | SELECTED_REAL_KIND LPAREN actual_arg_spec_list? RPAREN  
    | SELECTED_INT_KIND LPAREN actual_arg_spec_list? RPAREN
    | KIND LPAREN actual_arg_spec_list? RPAREN
    ;

// Override F90 literal_f90 to accept LABEL as integer literal
literal_f90
    : INTEGER_LITERAL_KIND          // Integer with kind (123_int32)
    | INTEGER_LITERAL               // Traditional integer
    | LABEL                         // Accept LABEL as integer (token precedence issue)
    | REAL_LITERAL_KIND             // Real with kind (3.14_real64)  
    | REAL_LITERAL                  // Traditional real
    | DOUBLE_QUOTE_STRING           // Double-quoted string (F90)
    | SINGLE_QUOTE_STRING           // Single-quoted string
    | logical_literal_f90           // Enhanced logical literals
    | boz_literal_constant          // Binary/octal/hex literals (F90)
    ;

// BIND(C) specification for C interoperability (simplified like external grammar)
binding_spec
    : BIND LPAREN C RPAREN                                               // BIND(C)
    | BIND LPAREN C COMMA NAME EQUALS string_literal RPAREN             // BIND(C, NAME="func")
    ;

// String literal for BIND(C) name
string_literal
    : DOUBLE_QUOTE_STRING
    | SINGLE_QUOTE_STRING
    ;

// C interoperability types
c_interop_type
    : C_INT
    | C_SHORT
    | C_LONG
    | C_LONG_LONG
    | C_SIGNED_CHAR
    | C_SIZE_T
    | C_INT8_T
    | C_INT16_T
    | C_INT32_T
    | C_INT64_T
    | C_INT_LEAST8_T
    | C_INT_LEAST16_T
    | C_INT_LEAST32_T
    | C_INT_LEAST64_T
    | C_INT_FAST8_T
    | C_INT_FAST16_T
    | C_INT_FAST32_T
    | C_INT_FAST64_T
    | C_INTMAX_T
    | C_INTPTR_T
    | C_FLOAT
    | C_DOUBLE
    | C_LONG_DOUBLE
    | C_FLOAT_COMPLEX
    | C_DOUBLE_COMPLEX
    | C_LONG_DOUBLE_COMPLEX
    | C_BOOL
    | C_CHAR
    | C_PTR
    | C_FUNPTR
    | C_NULL_PTR
    | C_NULL_FUNPTR
    ;

// Type name
type_name
    : identifier_or_keyword
    ;

