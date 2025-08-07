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
// FORTRAN 2003 NEW CONSTRUCTS - Object-Oriented Programming
// ============================================================================

// Enhanced derived type definition with OOP features
derived_type_def_f2003
    : type_attr_spec_list? TYPE (LPAREN type_param_name_list RPAREN)? 
      (COMMA type_attr_spec_list)? COLON_COLON? type_name 
      (LPAREN parent_type_name RPAREN)? NEWLINE
      type_param_def_stmt_list?
      component_def_stmt_list?
      type_bound_procedure_part?
      END TYPE type_name? NEWLINE
    ;

// Parent type specification for inheritance
parent_type_name
    : IDENTIFIER
    ;

// Type-bound procedure part
type_bound_procedure_part
    : CONTAINS NEWLINE
      type_bound_proc_binding_list
    ;

// Component definition statement list (simplified)
component_def_stmt_list
    : component_def_stmt+
    ;

component_def_stmt
    : INTEGER (COMMA (PUBLIC | PRIVATE))? COLON_COLON IDENTIFIER NEWLINE
    | REAL (COMMA (PUBLIC | PRIVATE))? COLON_COLON IDENTIFIER NEWLINE
    | CHARACTER (COMMA (PUBLIC | PRIVATE))? COLON_COLON IDENTIFIER NEWLINE
    ;

// Type-bound procedure bindings
type_bound_proc_binding_list
    : type_bound_proc_binding+
    ;

type_bound_proc_binding
    : type_bound_procedure_stmt
    ;

// Type-bound procedure statement  
type_bound_procedure_stmt
    : PROCEDURE (LPAREN IDENTIFIER RPAREN)?
      (COMMA proc_attr_spec_list)? COLON_COLON?
      IDENTIFIER (ARROW IDENTIFIER)? NEWLINE
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
    ;

// Type attribute specifications
type_attr_spec_list
    : type_attr_spec (COMMA type_attr_spec)*
    ;

type_attr_spec
    : PUBLIC
    | PRIVATE
    | ABSTRACT
    | BIND LPAREN C RPAREN
    ;

// Type parameter definitions
type_param_def_stmt_list
    : type_param_def_stmt+
    ;

type_param_def_stmt
    : INTEGER COMMA type_param_attr_spec COLON_COLON 
      type_param_name_list NEWLINE
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
    : IDENTIFIER? COLON? ASSOCIATE LPAREN association_list RPAREN NEWLINE
      execution_part
      END ASSOCIATE IDENTIFIER? NEWLINE
    ;

association_list
    : association (COMMA association)*
    ;

association
    : IDENTIFIER ARROW selector
    ;

selector
    : primary
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
    : PROCEDURE LPAREN IDENTIFIER RPAREN 
      (COMMA proc_attr_spec_list)? COLON_COLON 
      IDENTIFIER NEWLINE
    ;

// ============================================================================
// CLASS DECLARATIONS
// ============================================================================

class_declaration_stmt
    : CLASS LPAREN IDENTIFIER RPAREN (COMMA attr_spec_list)? 
      COLON_COLON IDENTIFIER NEWLINE
    ;

// ============================================================================
// IMPORT STATEMENT
// ============================================================================

import_stmt
    : IMPORT (COLON_COLON import_name_list)? NEWLINE
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
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

alloc_opt_list
    : alloc_opt (COMMA alloc_opt)*
    ;

alloc_opt
    : STAT ASSIGN IDENTIFIER
    | SOURCE ASSIGN primary
    | MOLD ASSIGN primary
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

// ============================================================================
// VOLATILE AND PROTECTED ATTRIBUTES
// ============================================================================

volatile_stmt
    : VOLATILE COLON_COLON object_name_list NEWLINE
    ;

protected_stmt
    : PROTECTED COLON_COLON object_name_list NEWLINE
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
    : INTEGER (COMMA attr_spec_list)? COLON_COLON entity_decl_list NEWLINE
    | REAL (COMMA attr_spec_list)? COLON_COLON entity_decl_list NEWLINE
    | CHARACTER (COMMA attr_spec_list)? COLON_COLON entity_decl_list NEWLINE
    | TYPE LPAREN IDENTIFIER RPAREN (COMMA attr_spec_list)? 
      COLON_COLON entity_decl_list NEWLINE
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
    ;

intent_spec
    : IN
    | OUT
    | INOUT
    ;

entity_decl_list
    : IDENTIFIER (COMMA IDENTIFIER)*
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
    | INTEGER_LITERAL
    | REAL_LITERAL
    | STRING_LITERAL
    | LPAREN primary RPAREN
    ;

// Type name
type_name
    : IDENTIFIER
    ;

