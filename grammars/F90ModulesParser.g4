// Fortran 90 Module System and Interface Blocks
// Delegate grammar for MODULE, USE, and INTERFACE constructs
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90ModulesParser;

// ====================================================================
// MODULE SYSTEM (F90 MAJOR INNOVATION)
// ====================================================================
// ISO/IEC 1539:1991 Section 11 - Program Units
// Modules provide a mechanism for data sharing and procedure grouping.

// Module definition (revolutionary F90 feature)
module
    : module_stmt specification_part? module_subprogram_part? end_module_stmt
    ;

module_stmt
    : MODULE IDENTIFIER NEWLINE*
    ;

end_module_stmt
    : END_MODULE (IDENTIFIER)? NEWLINE*
    ;

// Module subprogram section
module_subprogram_part
    : contains_stmt NEWLINE* (module_subprogram NEWLINE*)*
    ;

module_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;

contains_stmt
    : CONTAINS NEWLINE?
    ;

// USE statement (F90 module import)
// ISO/IEC 1539:1991 Section 11.3
use_stmt
    : USE module_name (COMMA rename_list | COMMA ONLY COLON only_list)?
    ;

module_name
    : IDENTIFIER
    ;

// Module renaming and selective import
rename_list
    : rename (COMMA rename)*
    ;

rename
    : IDENTIFIER POINTER_ASSIGN IDENTIFIER       // local_name => module_name
    ;

only_list
    : only_item (COMMA only_item)*
    ;

only_item
    : IDENTIFIER (POINTER_ASSIGN IDENTIFIER)?    // local_name => module_name
    | OPERATOR LPAREN operator_token RPAREN
    ;

operator_token
    : PLUS | MINUS | MULTIPLY | SLASH | POWER
    | EQ_OP | NE_OP | LT_OP | LE_OP | GT_OP | GE_OP
    | DOT_EQ | DOT_NE | DOT_LT | DOT_LE | DOT_GT | DOT_GE
    | DOT_AND | DOT_OR | DOT_NOT | DOT_EQV | DOT_NEQV
    ;

// ====================================================================
// INTERFACE BLOCKS (F90 MAJOR INNOVATION)
// ====================================================================
// ISO/IEC 1539:1991 Section 12.3
// Interfaces provide explicit procedure declarations and generic overloading.

// Interface block (explicit interfaces and generic procedures)
interface_block
    : interface_stmt interface_specification* end_interface_stmt
    ;

interface_stmt
    : INTERFACE (generic_spec)?
    ;

generic_spec
    : IDENTIFIER                            // Generic procedure name
    | OPERATOR LPAREN operator_token RPAREN // Operator overloading
    | ASSIGNMENT LPAREN EQUALS RPAREN       // Assignment overloading
    ;

interface_specification
    : interface_body
    | procedure_stmt
    ;

interface_body
    : function_stmt specification_part? end_function_stmt
    | subroutine_stmt specification_part? end_subroutine_stmt
    ;

end_interface_stmt
    : END_INTERFACE (generic_spec)?
    ;

// ====================================================================
// IMPORT STATEMENT (F90)
// ====================================================================
// ISO/IEC 1539:1991 Section 12.4.1

import_stmt
    : IMPORT (DOUBLE_COLON import_name_list)?
    ;

import_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;
