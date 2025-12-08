// Fortran 90 Module System and Interface Blocks
// Reference: ISO/IEC 1539:1991 Section 11.3 (Modules) and Section 12.3 (Interfaces)
// Delegate grammar for MODULE, USE, and INTERFACE constructs
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90ModulesParser;

// ====================================================================
// MODULE SYSTEM - ISO/IEC 1539:1991 Section 11.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 11.3 defines the module system:
// - R1104 (module) -> module-stmt [specification-part]
//                     [module-subprogram-part] end-module-stmt
// - R1105 (module-stmt) -> MODULE module-name
// - R1106 (end-module-stmt) -> END [MODULE [module-name]]
// - R1107 (use-stmt) -> USE module-name [, rename-list] |
//                       USE module-name, ONLY: [only-list]
//
// Modules provide a mechanism for data sharing and procedure grouping.

// Module definition - ISO/IEC 1539:1991 Section 11.3, R1104
module
    : module_stmt specification_part? module_subprogram_part? end_module_stmt
    ;

// MODULE statement - ISO/IEC 1539:1991 Section 11.3, R1105
module_stmt
    : MODULE IDENTIFIER NEWLINE*
    ;

// END MODULE statement - ISO/IEC 1539:1991 Section 11.3, R1106
end_module_stmt
    : END_MODULE (IDENTIFIER)? NEWLINE*
    ;

// Module subprogram part - ISO/IEC 1539:1991 Section 11.3, R1108
module_subprogram_part
    : contains_stmt NEWLINE* (module_subprogram NEWLINE*)*
    ;

// Module subprogram - ISO/IEC 1539:1991 Section 11.3, R1109
module_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;

// CONTAINS statement - ISO/IEC 1539:1991 Section 12.5.1
contains_stmt
    : CONTAINS NEWLINE?
    ;

// ====================================================================
// USE STATEMENT - ISO/IEC 1539:1991 Section 11.3.2
// ====================================================================
//
// ISO/IEC 1539:1991 Section 11.3.2 defines module use:
// - R1107 (use-stmt) -> USE module-name [, rename-list] |
//                       USE module-name, ONLY: [only-list]
// - R1110 (rename) -> local-name => use-name
// - R1111 (only) -> generic-spec | only-use-name | rename

// USE statement - ISO/IEC 1539:1991 Section 11.3.2, R1107
use_stmt
    : USE module_name (COMMA rename_list | COMMA ONLY COLON only_list)?
    ;

// Module name - ISO/IEC 1539:1991 Section 11.3
module_name
    : IDENTIFIER
    ;

// Rename list - ISO/IEC 1539:1991 Section 11.3.2, R1107
rename_list
    : rename (COMMA rename)*
    ;

// Rename - ISO/IEC 1539:1991 Section 11.3.2, R1110
rename
    : IDENTIFIER POINTER_ASSIGN IDENTIFIER       // local_name => module_name
    ;

// Only list - ISO/IEC 1539:1991 Section 11.3.2, R1107
only_list
    : only_item (COMMA only_item)*
    ;

// Only item - ISO/IEC 1539:1991 Section 11.3.2, R1111
only_item
    : IDENTIFIER (POINTER_ASSIGN IDENTIFIER)?    // local_name => module_name
    | OPERATOR LPAREN operator_token RPAREN
    ;

// Operator token - ISO/IEC 1539:1991 Section 7.1.2
operator_token
    : PLUS | MINUS | MULTIPLY | SLASH | POWER
    | EQ_OP | NE_OP | LT_OP | LE_OP | GT_OP | GE_OP
    | DOT_EQ | DOT_NE | DOT_LT | DOT_LE | DOT_GT | DOT_GE
    | DOT_AND | DOT_OR | DOT_NOT | DOT_EQV | DOT_NEQV
    ;

// ====================================================================
// INTERFACE BLOCKS - ISO/IEC 1539:1991 Section 12.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 12.3 defines interface blocks:
// - R1201 (interface-block) -> interface-stmt [interface-specification]...
//                              end-interface-stmt
// - R1202 (interface-stmt) -> INTERFACE [generic-spec]
// - R1203 (generic-spec) -> generic-name | OPERATOR(defined-operator) |
//                           ASSIGNMENT(=)
// - R1204 (interface-specification) -> interface-body | procedure-stmt
// - R1205 (interface-body) -> function-stmt ... end-function-stmt |
//                             subroutine-stmt ... end-subroutine-stmt
// - R1206 (end-interface-stmt) -> END INTERFACE [generic-spec]
//
// Interfaces provide explicit procedure declarations and generic overloading.

// Interface block - ISO/IEC 1539:1991 Section 12.3, R1201
interface_block
    : interface_stmt interface_specification* end_interface_stmt
    ;

// INTERFACE statement - ISO/IEC 1539:1991 Section 12.3, R1202
interface_stmt
    : INTERFACE (generic_spec)?
    ;

// Generic spec - ISO/IEC 1539:1991 Section 12.3, R1203
generic_spec
    : IDENTIFIER                            // Generic procedure name
    | OPERATOR LPAREN operator_token RPAREN // Operator overloading
    | ASSIGNMENT LPAREN EQUALS RPAREN       // Assignment overloading
    ;

// Interface specification - ISO/IEC 1539:1991 Section 12.3, R1204
interface_specification
    : interface_body
    | procedure_stmt
    ;

// Interface body - ISO/IEC 1539:1991 Section 12.3, R1205
interface_body
    : function_stmt specification_part? end_function_stmt
    | subroutine_stmt specification_part? end_subroutine_stmt
    ;

// END INTERFACE statement - ISO/IEC 1539:1991 Section 12.3, R1206
end_interface_stmt
    : END_INTERFACE (generic_spec)?
    ;

// ====================================================================
// IMPORT STATEMENT - ISO/IEC 1539:1991 Section 12.3.2.2
// ====================================================================
//
// NOTE: Full IMPORT statement is a Fortran 2003 feature.
// F90 relies on implicit host association (Section 14.6.1.3).
// This rule provides forward compatibility.

// IMPORT statement (F2003, included for forward compatibility)
import_stmt
    : IMPORT (DOUBLE_COLON import_name_list)?
    ;

// Import name list
import_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;
