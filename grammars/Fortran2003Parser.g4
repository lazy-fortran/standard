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
    ;

// F2003 program unit (enhanced with OOP features)
program_unit_f2003
    : NEWLINE* (main_program_f2003 | module_f2003 | external_subprogram_f2003) NEWLINE*
    ;

// Enhanced main program for F2003
main_program_f2003
    : program_stmt specification_part_f2003? execution_part_f2003? internal_subprogram_part_f2003? end_program_stmt
    ;

// Enhanced module for F2003
module_f2003
    : module_stmt_f2003 specification_part_f2003? module_subprogram_part? end_module_stmt_f2003
    ;

// F2003 module statement with newline support
module_stmt_f2003
    : MODULE IDENTIFIER NEWLINE*
    ;

// F2003 end module statement with newline support  
end_module_stmt_f2003
    : END_MODULE (IDENTIFIER)? NEWLINE*
    ;

// Enhanced external subprogram for F2003
external_subprogram_f2003
    : function_subprogram_f2003
    | subroutine_subprogram_f2003
    ;

// Enhanced function subprogram for F2003
function_subprogram_f2003
    : function_stmt_f2003 specification_part_f2003? execution_part_f2003? internal_subprogram_part_f2003? end_function_stmt
    ;

// Enhanced subroutine subprogram for F2003
subroutine_subprogram_f2003
    : subroutine_stmt_f2003 specification_part_f2003? execution_part_f2003? internal_subprogram_part_f2003? end_subroutine_stmt
    ;

// Enhanced function statement for F2003
function_stmt_f2003
    : prefix? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN suffix? NEWLINE
    ;

// Enhanced subroutine statement for F2003
subroutine_stmt_f2003
    : prefix? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? BIND LPAREN C (COMMA NAME ASSIGN STRING_LITERAL)? RPAREN NEWLINE
    | prefix? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? NEWLINE
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
    : (prefix)? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? NEWLINE
    ;

function_stmt_interface
    : (prefix)? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN (suffix)? NEWLINE
    ;

end_subroutine_stmt_interface
    : END (SUBROUTINE (IDENTIFIER)?)? NEWLINE
    ;

end_function_stmt_interface
    : END (FUNCTION (IDENTIFIER)?)? NEWLINE
    ;

// Override interface_stmt and end_interface_stmt for NEWLINE support
interface_stmt
    : INTERFACE (generic_spec)? NEWLINE
    ;

end_interface_stmt
    : END_INTERFACE (generic_spec)? NEWLINE
    ;

// Override interface_block to handle NEWLINEs between specifications
interface_block
    : interface_stmt (NEWLINE* interface_specification)* NEWLINE* end_interface_stmt
    ;

// Enhanced specification part for F2003
specification_part_f2003
    : ((use_stmt | import_stmt | implicit_stmt | declaration_construct_f2003) NEWLINE*)*
    ;

// Enhanced declaration construct for F2003
declaration_construct_f2003
    : derived_type_def_f2003
    | type_declaration_stmt         // Try this first - handles "integer, protected ::" 
    | class_declaration_stmt
    | procedure_declaration_stmt
    | interface_block              // F90 interface blocks
    | volatile_stmt                 // Standalone "volatile :: vars"
    | protected_stmt               // Standalone "protected :: vars"
    | declaration_construct        // Inherit F95 declarations
    ;

// Enhanced execution part for F2003
execution_part_f2003
    : executable_construct_f2003*
    ;

// Enhanced executable construct for F2003
executable_construct_f2003
    : assignment_stmt
    | call_stmt
    | print_stmt
    | select_type_construct
    | associate_construct
    | block_construct
    | allocate_stmt_f2003
    | wait_stmt
    | flush_stmt
    | if_construct
    | do_construct
    | select_case_construct
    | executable_construct    // Inherit F95 constructs
    ;

// ============================================================================
// FORTRAN 2003 NEW CONSTRUCTS - Object-Oriented Programming
// ============================================================================

// Enhanced derived type definition with OOP features (F2003)
derived_type_def_f2003
    : derived_type_stmt_f2003 NEWLINE*
      type_param_def_stmt*
      (NEWLINE* component_def_stmt)*
      type_bound_procedure_part?
      end_type_stmt_f2003
    ;

// F2003 enhanced type statement with OOP attributes  
derived_type_stmt_f2003
    : TYPE (COMMA type_attr_spec_list)? DOUBLE_COLON type_name (LPAREN type_param_name_list RPAREN)?
    ;

// F2003 end type statement 
end_type_stmt_f2003
    : END_TYPE type_name? NEWLINE?
    ;

// Parent type specification for inheritance
parent_type_name
    : IDENTIFIER
    ;

// Type-bound procedure part
type_bound_procedure_part
    : CONTAINS NEWLINE?
      (NEWLINE* type_bound_proc_binding)*
    ;

// Override F90 contains_stmt to handle NEWLINE properly in F2003
contains_stmt
    : CONTAINS NEWLINE?
    ;

// Component definition statement list (simplified)
component_def_stmt_list
    : component_def_stmt+
    ;

component_def_stmt
    : type_declaration_stmt          // F2003 component declarations (reuse existing rule)
    | private_sequence_stmt          // PRIVATE or SEQUENCE (inherited from F90)
    ;

private_sequence_stmt
    : PRIVATE NEWLINE?
    | SEQUENCE NEWLINE?
    ;

// Type-bound procedure bindings
type_bound_proc_binding_list
    : type_bound_proc_binding+
    ;

type_bound_proc_binding
    : type_bound_procedure_stmt
    | type_bound_generic_stmt
    | final_procedure_stmt
    ;

// Type-bound procedure statement  
type_bound_procedure_stmt
    : PROCEDURE (LPAREN IDENTIFIER RPAREN)?
      (COMMA proc_attr_spec_list)? DOUBLE_COLON?
      proc_binding_list NEWLINE
    ;

proc_binding_list
    : proc_binding (COMMA proc_binding)*
    ;

proc_binding
    : IDENTIFIER (ARROW IDENTIFIER)?
    ;

// Generic type-bound procedures
type_bound_generic_stmt
    : GENERIC (COMMA (PUBLIC | PRIVATE))? DOUBLE_COLON generic_spec ARROW generic_binding_list NEWLINE
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
    | BIND LPAREN C RPAREN
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
      execution_part_f2003
      END ASSOCIATE (IDENTIFIER)? NEWLINE
    ;

association_list
    : association (COMMA association)*
    ;

association
    : IDENTIFIER ARROW selector
    ;

selector
    : expr_f90   // Any expression, not just primary
    ;

// ============================================================================
// BLOCK CONSTRUCT  
// ============================================================================

block_construct
    : IDENTIFIER? COLON? BLOCK NEWLINE
      specification_part?
      execution_part
      END BLOCK IDENTIFIER? NEWLINE
    ;

// ============================================================================
// PROCEDURE POINTERS
// ============================================================================

procedure_declaration_stmt
    : PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN 
      (COMMA proc_attr_spec_list)? DOUBLE_COLON 
      IDENTIFIER NEWLINE
    ;

// ============================================================================
// CLASS DECLARATIONS & POLYMORPHISM
// ============================================================================

class_declaration_stmt
    : CLASS LPAREN type_spec_or_star RPAREN (COMMA attr_spec_list)? 
      DOUBLE_COLON entity_decl_list NEWLINE
    ;

type_spec_or_star
    : IDENTIFIER           // CLASS(type_name)
    | '*'                  // CLASS(*) - unlimited polymorphic
    ;

// SELECT TYPE construct for runtime type selection
select_type_construct
    : select_type_stmt NEWLINE*
      (type_guard_stmt execution_part_f2003?)*
      end_select_type_stmt
    ;

select_type_stmt
    : (IDENTIFIER COLON)? SELECT_TYPE LPAREN (IDENTIFIER ARROW)? selector_expr RPAREN
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
    : IDENTIFIER (COMMA IDENTIFIER)*
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
    : IDENTIFIER (LPAREN allocate_shape_spec_list RPAREN)?
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
    : STAT EQUALS variable_f90
    | ERRMSG EQUALS variable_f90  
    | SOURCE EQUALS expr_f90
    | MOLD EQUALS expr_f90
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
    : UNIT ASSIGN primary
    | ID ASSIGN primary
    ;

flush_stmt
    : FLUSH (LPAREN flush_spec_list RPAREN)? NEWLINE
    ;

flush_spec_list
    : flush_spec (COMMA flush_spec)*
    ;

flush_spec
    : UNIT ASSIGN primary
    ;

// PRINT statement (legacy I/O support)
print_stmt
    : PRINT '*' COMMA actual_arg_list NEWLINE         // print *, args
    | PRINT primary COMMA actual_arg_list NEWLINE     // print format, args
    | PRINT '*' NEWLINE                               // print *
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

specification_part
    : (use_stmt | import_stmt | declaration_construct)*
    ;

use_stmt
    : USE IDENTIFIER NEWLINE
    | USE IDENTIFIER COMMA ONLY COLON only_list NEWLINE
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
    ;

letter_spec_list
    : letter_spec (COMMA letter_spec)*
    ;

letter_spec
    : IDENTIFIER
    | IDENTIFIER MINUS IDENTIFIER
    ;

only_list
    : IDENTIFIER (COMMA IDENTIFIER)*
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
    : INTEGER kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | REAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | CHARACTER char_selector? (COMMA attr_spec_list)? DOUBLE_COLON entity_decl_list NEWLINE
    | TYPE LPAREN derived_type_spec RPAREN (COMMA attr_spec_list)? 
      DOUBLE_COLON entity_decl_list NEWLINE
    ;

// Kind selector for parameterized types
kind_selector
    : LPAREN IDENTIFIER RPAREN              // (k) - kind parameter
    | LPAREN KIND EQUALS IDENTIFIER RPAREN  // (kind=k) - explicit kind
    ;

// Character selector (simplified)
char_selector
    : LPAREN IDENTIFIER RPAREN              // (len) - length parameter  
    | LPAREN LEN EQUALS IDENTIFIER RPAREN   // (len=n) - explicit length
    ;

// Derived type specification (enhanced for F2003)
derived_type_spec
    : IDENTIFIER                                          // Basic type name
    | IDENTIFIER LPAREN type_param_spec_list RPAREN      // Parameterized type
    ;

// Type parameter specification list for instantiation
type_param_spec_list
    : type_param_spec (COMMA type_param_spec)*
    ;

type_param_spec
    : IDENTIFIER EQUALS expr_f90    // kind=real64, rows=3
    | expr_f90                      // positional parameter
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
    : identifier_or_keyword (LPAREN array_spec RPAREN)? (EQUALS expr_f90)?  // F2003 entity declaration with initialization
    ;

// Override F90 entity declaration to support keywords as identifiers
entity_decl_f90
    : identifier_or_keyword (LPAREN array_spec_f90 RPAREN)? (MULTIPLY char_length)? (ASSIGN expr_f90)?
    ;

// Override F90 module_subprogram_part to handle NEWLINEs properly
module_subprogram_part
    : contains_stmt NEWLINE* (module_subprogram NEWLINE*)+ 
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
    | associate_construct
    | block_construct
    | allocate_stmt_f2003
    | wait_stmt
    | flush_stmt
    | if_construct
    | do_construct
    | select_case_construct
    ;

// Simplified constructs (inherit complex ones from F95)
assignment_stmt
    : IDENTIFIER ASSIGN primary NEWLINE
    | IDENTIFIER PERCENT IDENTIFIER ASSIGN primary NEWLINE
    ;

call_stmt
    : CALL IDENTIFIER (LPAREN actual_arg_list? RPAREN)? NEWLINE
    ;

actual_arg_list
    : primary (COMMA primary)*
    ;

if_construct
    : IF LPAREN logical_expr RPAREN THEN NEWLINE
      execution_part
      (ELSE NEWLINE execution_part)?
      END IF NEWLINE
    ;

logical_expr
    : primary
    ;

do_construct
    : DO NEWLINE
      execution_part
      END DO NEWLINE
    | DO IDENTIFIER ASSIGN primary COMMA primary NEWLINE
      execution_part
      END DO NEWLINE
    ;

select_case_construct
    : SELECT CASE LPAREN primary RPAREN NEWLINE
      case_construct+
      END SELECT NEWLINE
    ;

case_construct
    : CASE LPAREN case_value_list RPAREN NEWLINE
      execution_part
    | CASE DEFAULT NEWLINE
      execution_part
    ;

case_value_list
    : primary (COMMA primary)*
    ;

// ============================================================================
// PRIMARY EXPRESSIONS
// ============================================================================

primary
    : IDENTIFIER (PERCENT IDENTIFIER)*
    | IDENTIFIER LPAREN actual_arg_list? RPAREN
    | intrinsic_function_call
    | INTEGER_LITERAL
    | REAL_LITERAL
    | STRING_LITERAL
    | SINGLE_QUOTE_STRING
    | DOUBLE_QUOTE_STRING  
    | LPAREN primary RPAREN
    ;

// Handle intrinsic function calls
intrinsic_function_call
    : SELECTED_REAL_KIND LPAREN actual_arg_list RPAREN
    | SELECTED_INT_KIND LPAREN actual_arg_list RPAREN
    | KIND LPAREN actual_arg_list RPAREN
    ;

// Override F90 function_reference to handle intrinsic functions
function_reference_f90
    : IDENTIFIER LPAREN actual_arg_spec_list? RPAREN
    | SELECTED_REAL_KIND LPAREN actual_arg_spec_list? RPAREN  
    | SELECTED_INT_KIND LPAREN actual_arg_spec_list? RPAREN
    | KIND LPAREN actual_arg_spec_list? RPAREN
    ;

// Type name
type_name
    : IDENTIFIER
    ;

