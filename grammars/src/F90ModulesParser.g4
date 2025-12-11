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
    : module_stmt specification_part? module_subprogram_part_f90? end_module_stmt
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
// Renamed to _f90 for consistent versioning with procedure rules
module_subprogram_part_f90
    : contains_stmt NEWLINE* (module_subprogram_f90 NEWLINE*)*
    ;

// Module subprogram - ISO/IEC 1539:1991 Section 11.3, R1109
// Renamed to _f90 to use the F90-specific subprogram rules
module_subprogram_f90
    : function_subprogram_f90
    | subroutine_subprogram_f90
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
    // User-defined dotted operators
    // ISO/IEC 1539:1991 Section 7.1.2 (R703-R704)
    | DOP
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
// and ISO/IEC 1539-1:1997 Section 12.2 (Fortran 95: Characteristics of procedures)
//
// IMPORTANT SEMANTIC CONSTRAINT (ISO/IEC 1539-1:1997 Section 12.2):
// When an explicit interface is provided, the interface must match the
// corresponding procedure definition in characteristics:
//
// SECTION 12.2.1.1 - Dummy argument characteristics (REQUIRED to match):
//   - Type and type parameters
//   - Rank and shape
//   - INTENT, OPTIONAL, TARGET, POINTER attributes
//   - For array bounds: exact dependence on entities in bounds expressions
//
// SECTION 12.2.2 - Function result characteristics (REQUIRED to match):
//   - Type and type parameters
//   - Rank and shape
//   - For nonconstant bounds: bounds expressions in interface MUST have
//     same dependence on entities as in procedure definition
//   - For nonconstant character length: length expressions must be equivalent
//
// NOTE: This grammar accepts all syntactic forms. Semantic validation
// (bounds matching, expression equivalence checking) is deferred to
// a semantic analysis phase and is NOT enforced by the parser.
// See issue #415 for tracking semantic analyzer implementation.
//
// STANDARD-COMPLIANT: Grammar is syntactically correct per ISO/IEC 1539-1:1997
// Section 12.3 (Interface blocks). Semantic constraints per Section 12.2 are
// documented but not enforced by the parser.
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
// and ISO/IEC 1539-1:1997 Section 12.2 (Fortran 95: Procedure characteristics)
//
// SEMANTIC CONSTRAINT (ISO/IEC 1539-1:1997 Section 12.2):
// The interface body declares procedure characteristics that MUST match
// the actual procedure definition (Section 12.2.1.1 and 12.2.2):
//
// For FUNCTION interfaces (Section 12.2.2):
//   - Result type and type parameters must match
//   - Result rank and shape must match
//   - For nonconstant bounds: expressions must depend on same entities
//     Example: interface says REAL :: f(N), definition must also say f(N)
//             NOT f(2*N) or f(N+1) - the bounds expression MUST match exactly
//
// For SUBROUTINE interfaces (Section 12.2.1.1):
//   - Each dummy argument type/rank/shape must match (Section 12.2.1.1)
//   - Array bounds dependence on entities must match between interface and definition
//
// NOTE: Current grammar is SYNTACTICALLY correct. This rule accepts any
// valid function/subroutine declaration syntax. Semantic validation of
// characteristic matching is deferred to semantic analysis phase.
// See issue #415 for semantic analyzer implementation requirements.
//
// STANDARD-COMPLIANT: Grammar syntax matches ISO/IEC 1539-1:1997 Section 12.3.
// Semantic constraints per Section 12.2 are documented here and in issue #415.
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
