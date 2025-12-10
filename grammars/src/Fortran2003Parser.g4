// Fortran 2003 (2004) Parser - Object-Oriented Programming Revolution
// Building on F95 with OOP, PDTs, C interoperability, and enhanced I/O
// Reference: ISO/IEC 1539-1:2004 (Fortran 2003)
//            J3/03-007 (Fortran 2003 draft/final text)
parser grammar Fortran2003Parser;

import Fortran95Parser;

options {
    tokenVocab = Fortran2003Lexer;
}

// ====================================================================
// FORTRAN 2003 PARSER OVERVIEW
// ====================================================================
//
// This parser implements syntax rules for Fortran 2003 as defined in:
//   ISO/IEC 1539-1:2004 (Fortran 2003 International Standard)
//   J3/03-007 (Fortran 2003 draft/final text)
//
// Fortran 2003 (ISO/IEC 1539-1:2004) introduces major enhancements over
// Fortran 95, including full object-oriented programming support, C
// interoperability, enhanced I/O, and parameterized derived types.
//
// This parser inherits unified format support (fixed/free) from F95
// and adds F2003-specific language constructs.
//
// MAJOR F2003 ENHANCEMENTS IMPLEMENTED (with ISO section refs):
// - Object-oriented programming: type extension, type-bound procedures,
//   polymorphism, CLASS, SELECT TYPE (Section 4.5, 12.4)
// - Parameterized derived types (Section 4.5.3)
// - Procedure pointers and ABSTRACT INTERFACE (Section 12.3.2.3)
// - C interoperability: BIND(C), ISO_C_BINDING (Section 15)
// - IEEE arithmetic modules (Section 14)
// - Enhanced ALLOCATE with SOURCE/MOLD (Section 6.3.1)
// - ASSOCIATE and BLOCK constructs (Section 8.1.3, 8.1.4)
// - Enhanced I/O: WAIT, FLUSH, stream I/O (Section 9)
// - IMPORT statement (Section 12.3.2.1)
// - VOLATILE and PROTECTED attributes (Section 5.1.2)
//
// INHERITANCE ARCHITECTURE (IN THIS REPO):
// FORTRAN / FORTRANII / FORTRAN66 / FORTRAN77
//   -> Fortran90Parser
//   -> Fortran95Parser
//   -> Fortran2003Parser
//   -> F2008+ standards
//
// ====================================================================

// ====================================================================
// FORTRAN 2003 PROGRAM STRUCTURE (ISO/IEC 1539-1:2004 Section 2.1, 11)
// ====================================================================
//
// Section 11 of ISO/IEC 1539-1:2004 defines the structure of Fortran
// program units: main programs, external subprograms, modules, and
// block data program units.
//
// ISO/IEC 1539-1:2004 Section 2.1: High level syntax
// - R201 (program) -> program-unit [program-unit]...
// - R202 (program-unit) -> main-program | external-subprogram | module
//                          | block-data
//
// ISO/IEC 1539-1:2004 Section 3.2: Statement classification
// Fortran statements are classified as executable or nonexecutable.
// F2003 continues the F95 statement taxonomy with additions for OOP.
//
// ====================================================================

// Keywords that can be used as identifiers in certain contexts
// ISO/IEC 1539-1:2004 Section 3.2.1: Names
// Names in Fortran consist of 1-63 alphanumeric characters.
// Keywords are not reserved; they can be used as names when context
// allows disambiguation.
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

// ====================================================================
// F2003 PROGRAM UNIT (ISO/IEC 1539-1:2004 Section 11.1)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 11.1 defines program units.
// - R202 (program-unit) -> main-program | external-subprogram | module
//                          | block-data
// - R1101 (main-program) -> [program-stmt] [specification-part]
//                           [execution-part] [internal-subprogram-part]
//                           end-program-stmt

// F2003 program unit (ISO/IEC 1539-1:2004 R202)
program_unit_f2003
    : NEWLINE* (main_program_f2003 | module_f2003 | external_subprogram_f2003) NEWLINE*
    ;

// Main program (ISO/IEC 1539-1:2004 Section 11.1, R1101)
main_program_f2003
    : program_stmt specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_program_stmt
    ;

// Program statement (ISO/IEC 1539-1:2004 R1102)
// R1102: program-stmt -> PROGRAM program-name
program_stmt
    : PROGRAM IDENTIFIER NEWLINE*
    ;

// End program statement (ISO/IEC 1539-1:2004 R1103)
// R1103: end-program-stmt -> END [PROGRAM [program-name]]
end_program_stmt
    : END (PROGRAM (IDENTIFIER)?)? NEWLINE*
    ;

// Override F90 main_program to use F2003 specification part
main_program
    : program_stmt specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_program_stmt
    ;

// ====================================================================
// F2003 MODULE (ISO/IEC 1539-1:2004 Section 11.2)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 11.2 defines modules.
// - R1104 (module) -> module-stmt [specification-part]
//                     [module-subprogram-part] end-module-stmt
// - R1105 (module-stmt) -> MODULE module-name
// - R1106 (end-module-stmt) -> END [MODULE [module-name]]

// Module (ISO/IEC 1539-1:2004 R1104)
module_f2003
    : module_stmt NEWLINE* specification_part_f2003? NEWLINE*
      module_subprogram_part? NEWLINE* end_module_stmt
    ;

// Override F90 specification_part to use F2003 enhanced version
specification_part
    : specification_part_f2003
    ;

// Function result suffix (ISO/IEC 1539-1:2004 R1219)
// R1219: suffix -> proc-language-binding-spec [RESULT (result-name)]
//                | RESULT (result-name) [proc-language-binding-spec]
suffix
    : RESULT LPAREN identifier_or_keyword RPAREN
    ;

// Override F90 module to use F2003 specification part
module
    : module_stmt NEWLINE*
      specification_part_f2003? NEWLINE*
      ( contains_stmt NEWLINE* (module_subprogram NEWLINE*)* )?
      end_module_stmt
    ;

// Module statement (ISO/IEC 1539-1:2004 R1105)
module_stmt
    : MODULE IDENTIFIER NEWLINE*
    ;

// End module statement (ISO/IEC 1539-1:2004 R1106)
end_module_stmt
    : END_MODULE (IDENTIFIER)? NEWLINE*
    ;

// ====================================================================
// F2003 EXTERNAL SUBPROGRAMS (ISO/IEC 1539-1:2004 Section 12.5)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 12.5 defines subprograms.
// - R1223 (external-subprogram) -> function-subprogram | subroutine-subprogram
// - R1216 (function-subprogram) -> function-stmt [specification-part]
//                                  [execution-part] [internal-subprogram-part]
//                                  end-function-stmt
// - R1220 (subroutine-subprogram) -> subroutine-stmt [specification-part]
//                                    [execution-part] [internal-subprogram-part]
//                                    end-subroutine-stmt

// External subprogram (ISO/IEC 1539-1:2004 R1223)
external_subprogram_f2003
    : function_subprogram_f2003
    | subroutine_subprogram_f2003
    ;

// Function subprogram (ISO/IEC 1539-1:2004 R1216)
function_subprogram_f2003
    : function_stmt_f2003 specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_function_stmt
    ;

// Subroutine subprogram (ISO/IEC 1539-1:2004 R1220)
subroutine_subprogram_f2003
    : subroutine_stmt_f2003 specification_part_f2003? execution_part_f2003?
      internal_subprogram_part_f2003? end_subroutine_stmt
    ;

// Function statement (ISO/IEC 1539-1:2004 R1217)
// R1217: function-stmt -> [prefix] FUNCTION function-name
//                         ( [dummy-arg-name-list] ) [suffix]
// F2003 adds binding-spec for C interoperability (Section 15)
function_stmt_f2003
    : prefix? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

// Subroutine statement (ISO/IEC 1539-1:2004 R1221)
// R1221: subroutine-stmt -> [prefix] SUBROUTINE subroutine-name
//                           [( [dummy-arg-list] )] [proc-language-binding-spec]
// F2003 adds binding-spec for C interoperability (Section 15)
subroutine_stmt_f2003
    : prefix? SUBROUTINE IDENTIFIER
      (LPAREN dummy_arg_name_list? RPAREN)? binding_spec? NEWLINE
    ;

// Internal subprogram part (ISO/IEC 1539-1:2004 R1213)
// R1213: internal-subprogram-part -> contains-stmt [internal-subprogram]...
internal_subprogram_part_f2003
    : contains_stmt internal_subprogram_f2003+
    ;

// Internal subprogram (ISO/IEC 1539-1:2004 R1214)
// R1214: internal-subprogram -> function-subprogram | subroutine-subprogram
internal_subprogram_f2003
    : function_subprogram_f2003
    | subroutine_subprogram_f2003
    ;

// Module subprogram (ISO/IEC 1539-1:2004 R1107)
// R1107: module-subprogram -> function-subprogram | subroutine-subprogram
module_subprogram
    : function_subprogram_f2003
    | subroutine_subprogram_f2003
    ;

// ====================================================================
// F2003 INTERFACE BLOCKS (ISO/IEC 1539-1:2004 Section 12.3.2)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 12.3.2 defines interface blocks.
// - R1201 (interface-block) -> interface-stmt [interface-specification]...
//                              end-interface-stmt
// - R1203 (interface-body) -> function-stmt [specification-part]
//                             end-function-stmt
//                           | subroutine-stmt [specification-part]
//                             end-subroutine-stmt
// F2003 adds ABSTRACT INTERFACE (Section 12.3.2.3)

// Interface body (ISO/IEC 1539-1:2004 R1203)
// Uses F2003 specification part for IMPORT support (Section 12.3.2.1)
interface_body
    : function_stmt_interface specification_part_f2003? end_function_stmt_interface
    | subroutine_stmt_interface specification_part_f2003? end_subroutine_stmt_interface
    ;

// Interface-specific subroutine statement
subroutine_stmt_interface
    : (prefix)? SUBROUTINE IDENTIFIER
      (LPAREN dummy_arg_name_list? RPAREN)? binding_spec? NEWLINE
    ;

// Interface-specific function statement
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

// End subroutine statement (ISO/IEC 1539-1:2004 R1222)
end_subroutine_stmt
    : END (SUBROUTINE (IDENTIFIER)?)? NEWLINE?
    ;

// End function statement (ISO/IEC 1539-1:2004 R1218)
end_function_stmt
    : END (FUNCTION (IDENTIFIER)?)? NEWLINE?
    ;

// Interface statement (ISO/IEC 1539-1:2004 R1202)
// R1202: interface-stmt -> INTERFACE [generic-spec] | ABSTRACT INTERFACE
// ABSTRACT INTERFACE is new in F2003 (Section 12.3.2.3)
interface_stmt
    : INTERFACE (generic_spec)? NEWLINE
    | ABSTRACT_INTERFACE (generic_spec)? NEWLINE
    ;

// End interface statement (ISO/IEC 1539-1:2004 R1207)
end_interface_stmt
    : END_INTERFACE (generic_spec)? NEWLINE
    ;

// Interface block (ISO/IEC 1539-1:2004 R1201)
interface_block
    : interface_stmt (NEWLINE* interface_specification)* NEWLINE* end_interface_stmt
    ;

// ====================================================================
// F2003 SPECIFICATION PART (ISO/IEC 1539-1:2004 Section 2.1)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 2.1 defines specification part.
// - R204 (specification-part) -> [use-stmt]... [import-stmt]... [implicit-part]
//                                [declaration-construct]...
// F2003 adds IMPORT statement (Section 12.3.2.1)

// Specification part (ISO/IEC 1539-1:2004 R204)
specification_part_f2003
    : specification_element_f2003*
    ;

specification_element_f2003
    : NEWLINE* (use_stmt
    | import_stmt
    | implicit_stmt
    | declaration_construct_f2003) NEWLINE?
    ;

// Declaration construct (ISO/IEC 1539-1:2004 R207)
// R207: declaration-construct -> derived-type-def | entry-stmt | enum-def
//                              | format-stmt | interface-block | parameter-stmt
//                              | procedure-declaration-stmt | specification-stmt
//                              | type-declaration-stmt | stmt-function-stmt
// F2003 adds derived-type-def with OOP (Section 4.5), procedure-declaration-stmt
// (Section 12.3.2.3), enum-def (Section 4.6)
declaration_construct_f2003
    : derived_type_def_f2003        // TYPE definitions (Section 4.5)
    | interface_block               // Interface blocks (Section 12.3.2)
    | class_declaration_stmt        // CLASS declarations (Section 4.5.6)
    | procedure_declaration_stmt    // PROCEDURE declarations (Section 12.3.2.3)
    | volatile_stmt                 // VOLATILE attribute (Section 5.1.2.16)
    | protected_stmt                // PROTECTED attribute (Section 5.1.2.10)
    | type_declaration_stmt         // Type declarations (Section 5.1)
    | declaration_construct         // Inherit F95 declarations
    ;

// ====================================================================
// F2003 EXECUTION PART (ISO/IEC 1539-1:2004 Section 2.1)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 2.1 defines execution part.
// - R208 (execution-part) -> executable-construct [execution-part-construct]...
// - R213 (executable-construct) -> action-stmt | associate-construct
//                                | block-construct | case-construct
//                                | critical-construct | do-construct
//                                | forall-construct | if-construct
//                                | select-type-construct | where-construct
// F2003 adds ASSOCIATE (Section 8.1.3), BLOCK (Section 8.1.4),
// SELECT TYPE (Section 8.1.5)

// Execution part (ISO/IEC 1539-1:2004 R208)
execution_part_f2003
    : execution_construct_f2003*
    ;

// Execution construct with newline handling
execution_construct_f2003
    : NEWLINE* executable_construct_f2003_inner NEWLINE*
    ;

// Executable construct (ISO/IEC 1539-1:2004 R213)
executable_construct_f2003_inner
    : assignment_stmt
    | call_stmt
    | print_stmt
    | stop_stmt
    | select_type_construct         // SELECT TYPE (Section 8.1.5)
    | associate_construct           // ASSOCIATE (Section 8.1.3)
    | block_construct               // BLOCK (Section 8.1.4)
    | allocate_stmt_f2003           // Enhanced ALLOCATE (Section 6.3.1)
    | wait_stmt                     // WAIT (Section 9.6)
    | flush_stmt                    // FLUSH (Section 9.7)
    | open_stmt
    | close_stmt
    | write_stmt
    | read_stmt
    | if_construct
    | do_construct
    | select_case_construct
    | type_declaration_stmt
        // F2003 allows mixed declarations and executable statements
    | executable_construct          // Inherit F95 constructs
    ;

// ====================================================================
// FORTRAN 2003 DERIVED TYPES AND OOP (ISO/IEC 1539-1:2004 Section 4.5)
// ====================================================================
//
// ISO/IEC 1539-1:2004 Section 4.5 defines derived types with OOP features.
// F2003 introduces major OOP enhancements:
// - Type extension via EXTENDS (Section 4.5.6)
// - Type-bound procedures (Section 4.5.4)
// - FINAL procedures for finalization (Section 4.5.5)
// - ABSTRACT types (Section 4.5.6)
// - Parameterized derived types (Section 4.5.3)
//
// Key rules:
// - R422 (derived-type-def) -> derived-type-stmt [type-param-def-stmt]...
//                              [private-or-sequence]... [component-part]
//                              [type-bound-procedure-part] end-type-stmt
// - R423 (derived-type-stmt) -> TYPE [[, type-attr-spec-list] ::]
//                               type-name [(type-param-name-list)]
// - R445 (type-bound-procedure-part) -> contains-stmt [binding-private-stmt]
//                                       [type-bound-proc-binding]...

// Derived type definition (ISO/IEC 1539-1:2004 R422)
derived_type_def_f2003
    : derived_type_stmt_f2003 NEWLINE*
      type_param_def_stmt*
      private_or_sequence*
      component_part?
      type_bound_procedure_part?
      end_type_stmt_f2003
    ;

// Derived type statement (ISO/IEC 1539-1:2004 R423)
// R423: derived-type-stmt -> TYPE [[, type-attr-spec-list] ::] type-name
//                            [(type-param-name-list)]
derived_type_stmt_f2003
    : TYPE (COMMA type_attr_spec_list DOUBLE_COLON | DOUBLE_COLON)? type_name
      (LPAREN type_param_name_list RPAREN)?
    ;

// End type statement (ISO/IEC 1539-1:2004 R430)
// R430: end-type-stmt -> END TYPE [type-name]
end_type_stmt_f2003
    : END_TYPE type_name? NEWLINE?
    ;

// Parent type name for EXTENDS (ISO/IEC 1539-1:2004 Section 4.5.6)
parent_type_name
    : IDENTIFIER
    ;

// Type-bound procedure part (ISO/IEC 1539-1:2004 R445)
// R445: type-bound-procedure-part -> contains-stmt [binding-private-stmt]
//                                    [type-bound-proc-binding]...
type_bound_procedure_part
    : contains_stmt binding_private_stmt? type_bound_proc_binding*
    ;

// Binding private statement (ISO/IEC 1539-1:2004 R446)
// R446: binding-private-stmt -> PRIVATE
binding_private_stmt
    : PRIVATE NEWLINE
    ;

// Type-bound procedure binding (ISO/IEC 1539-1:2004 R447)
// R447: type-bound-proc-binding -> type-bound-procedure-stmt
//                                | type-bound-generic-stmt
//                                | final-procedure-stmt
type_bound_proc_binding
    : type_bound_procedure_stmt NEWLINE?
    | type_bound_generic_stmt NEWLINE?
    | final_procedure_stmt NEWLINE?
    ;

// Contains statement (ISO/IEC 1539-1:2004 R1108)
// R1108: contains-stmt -> CONTAINS
contains_stmt
    : CONTAINS NEWLINE?
    ;

// Component part (ISO/IEC 1539-1:2004 R431)
// R431: component-part -> [component-def-stmt]...
component_part
    : component_def_stmt*
    ;

// Component definition statement (ISO/IEC 1539-1:2004 R432)
// R432: component-def-stmt -> data-component-def-stmt | proc-component-def-stmt
component_def_stmt
    : data_component_def_stmt NEWLINE?   // Data components (R433)
    | proc_component_def_stmt NEWLINE?   // Procedure components (R438)
    ;

// Data component definition statement (ISO/IEC 1539-1:2004 R433)
data_component_def_stmt
    : type_declaration_stmt
    ;

// Private or sequence (ISO/IEC 1539-1:2004 R424, R425)
private_or_sequence
    : private_components_stmt
    | sequence_stmt
    ;

// Private components statement (ISO/IEC 1539-1:2004 R424)
// R424: private-components-stmt -> PRIVATE
private_components_stmt
    : PRIVATE NEWLINE
    ;

// Sequence statement (ISO/IEC 1539-1:2004 R425)
// R425: sequence-stmt -> SEQUENCE
sequence_stmt
    : SEQUENCE NEWLINE
    ;

// Type-bound procedure statement (ISO/IEC 1539-1:2004 R448)
// R448: type-bound-procedure-stmt -> PROCEDURE [[, binding-attr-list] ::]
//                                    type-bound-proc-decl-list
//                                  | PROCEDURE (interface-name), binding-attr-list
//                                    :: binding-name-list
type_bound_procedure_stmt
    : PROCEDURE DOUBLE_COLON type_bound_proc_decl_list
    | PROCEDURE COMMA binding_attr_list DOUBLE_COLON type_bound_proc_decl_list
    | PROCEDURE LPAREN IDENTIFIER RPAREN DOUBLE_COLON type_bound_proc_decl_list
    | PROCEDURE LPAREN IDENTIFIER RPAREN COMMA binding_attr_list
      DOUBLE_COLON type_bound_proc_decl_list
    ;

// Binding attribute list (ISO/IEC 1539-1:2004 R449)
binding_attr_list
    : binding_attr (COMMA binding_attr)*
    ;

// Binding attribute (ISO/IEC 1539-1:2004 R449)
// R449: binding-attr -> PASS [(arg-name)] | NOPASS | NON_OVERRIDABLE
//                     | DEFERRED | access-spec
binding_attr
    : access_spec           // PUBLIC or PRIVATE
    | DEFERRED              // Deferred binding (Section 4.5.4)
    | NOPASS                // No passed-object dummy argument
    | PASS (LPAREN IDENTIFIER RPAREN)?  // Explicit passed-object
    | NON_OVERRIDABLE       // Cannot be overridden
    ;

// Type-bound procedure declaration list (ISO/IEC 1539-1:2004 R450)
type_bound_proc_decl_list
    : type_bound_proc_decl (COMMA type_bound_proc_decl)*
    ;

// Type-bound procedure declaration (ISO/IEC 1539-1:2004 R450)
// R450: type-bound-proc-decl -> binding-name [=> procedure-name]
type_bound_proc_decl
    : binding_name (POINTER_ASSIGN procedure_name)?
    ;

// Binding name
binding_name
    : IDENTIFIER
    ;

// Procedure name
procedure_name
    : IDENTIFIER
    ;

// Access specifier (ISO/IEC 1539-1:2004 R508)
// R508: access-spec -> PUBLIC | PRIVATE
access_spec
    : PUBLIC
    | PRIVATE
    ;

// Type-bound generic statement (ISO/IEC 1539-1:2004 R451)
// R451: type-bound-generic-stmt -> GENERIC [, access-spec] ::
//                                  generic-spec => binding-name-list
type_bound_generic_stmt
    : GENERIC (COMMA (PUBLIC | PRIVATE))? DOUBLE_COLON generic_spec
      POINTER_ASSIGN generic_binding_list NEWLINE
    ;

// Generic specification (ISO/IEC 1539-1:2004 R1206)
// R1206: generic-spec -> generic-name | OPERATOR (defined-operator)
//                      | ASSIGNMENT (=) | dtio-generic-spec
// F2003 adds dtio-generic-spec for defined derived-type I/O (Section 9.5.3.7)
// R1208: dtio-generic-spec -> READ (FORMATTED) | READ (UNFORMATTED)
//                           | WRITE (FORMATTED) | WRITE (UNFORMATTED)
generic_spec
    : IDENTIFIER                            // Generic procedure name
    | OPERATOR LPAREN operator_token RPAREN // Operator overloading
    | ASSIGNMENT LPAREN ASSIGN RPAREN       // Assignment overloading
    | READ LPAREN identifier_or_keyword RPAREN   // READ(FORMATTED) (R1208)
    | WRITE LPAREN identifier_or_keyword RPAREN  // WRITE(UNFORMATTED) (R1208)
    ;

// Generic binding list
generic_binding_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// FINAL procedure statement (ISO/IEC 1539-1:2004 R452)
// R452: final-procedure-stmt -> FINAL [::] final-subroutine-name-list
// FINAL procedures provide type finalization (Section 4.5.5)
final_procedure_stmt
    : FINAL DOUBLE_COLON? final_subroutine_name_list NEWLINE
    ;

final_subroutine_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// Procedure attribute specification (ISO/IEC 1539-1:2004 R1211)
// R1211: proc-attr-spec -> access-spec | proc-language-binding-spec
//                        | INTENT (intent-spec) | OPTIONAL | POINTER | SAVE
proc_attr_spec_list
    : proc_attr_spec (COMMA proc_attr_spec)*
    ;

proc_attr_spec
    : PUBLIC
    | PRIVATE
    | NOPASS
    | PASS (LPAREN IDENTIFIER RPAREN)?
    | DEFERRED
    | POINTER    // F2003 procedure pointers (Section 12.3.2.3)
    ;

// Type attribute specification (ISO/IEC 1539-1:2004 R424)
// R424: type-attr-spec -> access-spec | EXTENDS (parent-type-name)
//                       | ABSTRACT | BIND (C)
type_attr_spec_list
    : type_attr_spec (COMMA type_attr_spec)*
    ;

type_attr_spec
    : PUBLIC
    | PRIVATE
    | ABSTRACT              // Abstract type (Section 4.5.6)
    | EXTENDS LPAREN IDENTIFIER RPAREN  // Type extension (Section 4.5.6)
    | BIND LPAREN C RPAREN  // C interoperability (Section 15.3.3)
    ;

// ====================================================================
// PARAMETERIZED DERIVED TYPES (ISO/IEC 1539-1:2004 Section 4.5.3)
// ====================================================================
//
// F2003 introduces parameterized derived types (PDTs) with kind and len
// type parameters.
// - R426 (type-param-def-stmt) -> INTEGER, type-param-attr-spec ::
//                                 type-param-decl-list
// - R427 (type-param-decl) -> type-param-name [= scalar-int-initialization-expr]
// - R428 (type-param-attr-spec) -> KIND | LEN

// Type parameter definition statement list
type_param_def_stmt_list
    : type_param_def_stmt+
    ;

// Type parameter definition statement (ISO/IEC 1539-1:2004 R426)
type_param_def_stmt
    : INTEGER COMMA type_param_attr_spec DOUBLE_COLON
      type_param_decl_list NEWLINE
    ;

// Type parameter declaration list (ISO/IEC 1539-1:2004 R427)
type_param_decl_list
    : type_param_decl (COMMA type_param_decl)*
    ;

// Type parameter declaration (ISO/IEC 1539-1:2004 R427)
// R427: type-param-decl -> type-param-name [= scalar-int-initialization-expr]
type_param_decl
    : IDENTIFIER (EQUALS default_init_expr)?
    ;

// Default initialization expression
default_init_expr
    : IDENTIFIER LPAREN expr_f90 RPAREN  // Function call like kind(0.0)
    | expr_f90  // Simple expression
    ;

// Type parameter attribute specification (ISO/IEC 1539-1:2004 R428)
// R428: type-param-attr-spec -> KIND | LEN
type_param_attr_spec
    : KIND
    | LEN
    ;

// Type parameter name list
type_param_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ====================================================================
// ASSOCIATE CONSTRUCT (ISO/IEC 1539-1:2004 Section 8.1.3)
// ====================================================================
//
// F2003 introduces the ASSOCIATE construct for creating local aliases.
// - R816 (associate-construct) -> associate-stmt block end-associate-stmt
// - R817 (associate-stmt) -> [associate-construct-name :] ASSOCIATE
//                            ( association-list )
// - R818 (association) -> associate-name => selector

// Associate construct (ISO/IEC 1539-1:2004 R816)
associate_construct
    : (IDENTIFIER COLON)? ASSOCIATE LPAREN association_list RPAREN NEWLINE
      execution_part_f2003?
      END ASSOCIATE (IDENTIFIER)? NEWLINE?
    ;

// Association list (ISO/IEC 1539-1:2004 R817)
association_list
    : association (COMMA association)*
    ;

// Association (ISO/IEC 1539-1:2004 R818)
// R818: association -> associate-name => selector
association
    : identifier_or_keyword POINTER_ASSIGN selector
    ;

// Selector (ISO/IEC 1539-1:2004 R819)
// R819: selector -> expr | variable
selector
    : expr_f2003
    ;

// ====================================================================
// BLOCK CONSTRUCT (ISO/IEC 1539-1:2004 Section 8.1.4)
// ====================================================================
//
// F2003 introduces the BLOCK construct for local scoping.
// - R807 (block-construct) -> block-stmt [specification-part]
//                             [execution-part] end-block-stmt
// - R808 (block-stmt) -> [block-construct-name :] BLOCK
// - R809 (end-block-stmt) -> END BLOCK [block-construct-name]

// Block construct (ISO/IEC 1539-1:2004 R807)
block_construct
    : (IDENTIFIER COLON)? BLOCK NEWLINE
      specification_part_f2003?
      execution_part_f2003?
      END BLOCK (IDENTIFIER)? NEWLINE?
    ;

// ====================================================================
// PROCEDURE POINTERS (ISO/IEC 1539-1:2004 Section 12.3.2.3)
// ====================================================================
//
// F2003 introduces procedure pointers and procedure pointer components.
// - R1210 (procedure-declaration-stmt) -> PROCEDURE ([proc-interface])
//                                         [[, proc-attr-spec]... ::]
//                                         proc-decl-list
// - R438 (proc-component-def-stmt) -> PROCEDURE ( [proc-interface] )
//                                     , proc-component-attr-spec-list
//                                     :: proc-decl-list

// Procedure declaration statement (ISO/IEC 1539-1:2004 R1210)
procedure_declaration_stmt
    : PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN
      (COMMA proc_attr_spec_list)? DOUBLE_COLON
      procedure_entity_decl_list NEWLINE
    ;

// Procedure entity declaration list (ISO/IEC 1539-1:2004 R1212)
procedure_entity_decl_list
    : procedure_entity_decl (COMMA procedure_entity_decl)*
    ;

// Procedure entity declaration (ISO/IEC 1539-1:2004 R1212)
// R1212: proc-decl -> procedure-entity-name [=> proc-pointer-init]
procedure_entity_decl
    : IDENTIFIER (POINTER_ASSIGN proc_target)?
    ;

// Procedure pointer target (procedure or null)
proc_target
    : IDENTIFIER                    // Target procedure name
    | IDENTIFIER LPAREN RPAREN      // Procedure call (including function pointers)
    | NULL_INTRINSIC                // Null intrinsic without parentheses
    | NULL_INTRINSIC LPAREN RPAREN  // NULL() disassociates pointer
    ;

// Procedure component definition statement (ISO/IEC 1539-1:2004 R438)
// R438: proc-component-def-stmt -> PROCEDURE ( [proc-interface] )
//                                  , proc-component-attr-spec-list
//                                  :: proc-decl-list
proc_component_def_stmt
    : PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN COMMA
      proc_component_attr_spec_list DOUBLE_COLON proc_decl_list NEWLINE
    | PROCEDURE LPAREN (IDENTIFIER | INTERFACE) RPAREN
      DOUBLE_COLON proc_decl_list NEWLINE
    ;

// Procedure component attribute specification (ISO/IEC 1539-1:2004 R439)
// R439: proc-component-attr-spec -> POINTER | PASS [(arg-name)] | NOPASS
//                                 | access-spec
proc_component_attr_spec_list
    : proc_component_attr_spec (COMMA proc_component_attr_spec)*
    ;

proc_component_attr_spec
    : PUBLIC
    | PRIVATE
    | NOPASS
    | PASS (LPAREN IDENTIFIER RPAREN)?
    | POINTER
    | DEFERRED      // For abstract type-bound procedures
    ;

// Procedure declaration list (ISO/IEC 1539-1:2004 R440)
// R440: proc-decl -> procedure-entity-name [=> null-init]
proc_decl_list
    : proc_decl (COMMA proc_decl)*
    ;

proc_decl
    : IDENTIFIER (POINTER_ASSIGN IDENTIFIER)?
    ;

// ====================================================================
// CLASS DECLARATIONS & POLYMORPHISM (ISO/IEC 1539-1:2004 Section 4.5.6)
// ====================================================================
//
// F2003 introduces polymorphism with CLASS declarations.
// - R503 (declaration-type-spec) -> intrinsic-type-spec | TYPE (derived-type-spec)
//                                 | CLASS (derived-type-spec) | CLASS (*)
// CLASS(*) represents unlimited polymorphic (any type)
// CLASS(type-name) represents declared type or any extension

// Class declaration statement (ISO/IEC 1539-1:2004 Section 5.1)
class_declaration_stmt
    : CLASS LPAREN type_spec_or_star RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list
    ;

// Type specification or star for CLASS (ISO/IEC 1539-1:2004 R503)
// CLASS(*) = unlimited polymorphic
// CLASS(type-name) = polymorphic of declared type or extension
type_spec_or_star
    : SHAPE_INTRINSIC
        // CLASS(SHAPE) - derived type named SHAPE (most specific first)
    | '*'                  // CLASS(*) - unlimited polymorphic
    | type_spec            // CLASS(INTEGER) - intrinsic types
    | IDENTIFIER           // CLASS(type_name) - derived types
    ;

// ====================================================================
// SELECT TYPE CONSTRUCT (ISO/IEC 1539-1:2004 Section 8.1.5)
// ====================================================================
//
// F2003 introduces SELECT TYPE for runtime type selection.
// - R821 (select-type-construct) -> select-type-stmt [type-guard-stmt block]...
//                                   end-select-type-stmt
// - R822 (select-type-stmt) -> [select-construct-name :] SELECT TYPE
//                              ( [associate-name =>] selector )
// - R823 (type-guard-stmt) -> TYPE IS (type-spec) [select-construct-name]
//                           | CLASS IS (derived-type-spec) [select-construct-name]
//                           | CLASS DEFAULT [select-construct-name]

// Select type construct (ISO/IEC 1539-1:2004 R821)
select_type_construct
    : select_type_stmt NEWLINE*
      (type_guard_stmt execution_part_f2003?)*
      end_select_type_stmt
    ;

// Select type statement (ISO/IEC 1539-1:2004 R822)
select_type_stmt
    : (IDENTIFIER COLON)? SELECT TYPE LPAREN
      (IDENTIFIER POINTER_ASSIGN)? selector_expr RPAREN
    ;

selector_expr
    : primary
    ;

// Type guard statement (ISO/IEC 1539-1:2004 R823)
// R823: type-guard-stmt -> TYPE IS (type-spec) [select-construct-name]
//                        | CLASS IS (derived-type-spec) [select-construct-name]
//                        | CLASS DEFAULT [select-construct-name]
//
// DESIGN DECISION (Issue #184): The IS keyword in type guards is parsed
// as an IDENTIFIER token rather than a dedicated keyword. This is a
// deliberate grammar simplification that:
//   1. Works correctly for all case variations (is, IS, Is) since ANTLR
//      lexer rules are case-insensitive by design
//   2. Handles varied whitespace between TYPE/CLASS and IS naturally
//   3. Allows "is" to remain a valid variable name in other contexts
//   4. Follows the principle of minimal keyword reservation
//
// The grammar expects the IDENTIFIER to have text matching /[iI][sS]/
// but does not enforce this at the parser level.
type_guard_stmt
    : TYPE IDENTIFIER LPAREN type_spec_or_derived RPAREN (IDENTIFIER)? NEWLINE
    | CLASS IDENTIFIER LPAREN type_spec_or_derived RPAREN (IDENTIFIER)? NEWLINE
    | CLASS DEFAULT (IDENTIFIER)? NEWLINE
    ;

// Type specification or derived type (for type guards)
type_spec_or_derived
    : type_spec
    | IDENTIFIER   // User-defined type name
    ;

// End select type statement (ISO/IEC 1539-1:2004 R824)
// R824: end-select-type-stmt -> END SELECT [select-construct-name]
end_select_type_stmt
    : END_SELECT (IDENTIFIER)? NEWLINE
    ;

// ====================================================================
// IMPORT STATEMENT (ISO/IEC 1539-1:2004 Section 12.3.2.1)
// ====================================================================
//
// F2003 introduces the IMPORT statement for interface bodies.
// - R1209 (import-stmt) -> IMPORT [[::] import-name-list]
// IMPORT allows host-associated entities to be accessible in
// interface bodies.

// Import statement (ISO/IEC 1539-1:2004 R1209)
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

// ====================================================================
// ENHANCED ALLOCATE STATEMENT (ISO/IEC 1539-1:2004 Section 6.3.1)
// ====================================================================
//
// F2003 enhances ALLOCATE with SOURCE and MOLD specifiers.
// - R623 (allocate-stmt) -> ALLOCATE ( [type-spec ::] allocation-list
//                           [, alloc-opt-list] )
// - R627 (alloc-opt) -> STAT = stat-variable | ERRMSG = errmsg-variable
//                     | SOURCE = source-expr | MOLD = source-expr
// SOURCE= clones data from an existing object
// MOLD= clones only the type (without data)

// Allocate statement (ISO/IEC 1539-1:2004 R623)
allocate_stmt_f2003
    : ALLOCATE LPAREN allocation_list
      (COMMA alloc_opt_list)? RPAREN NEWLINE
    ;

// Allocation list (ISO/IEC 1539-1:2004 R624)
allocation_list
    : allocation (COMMA allocation)*
    ;

// Allocation (ISO/IEC 1539-1:2004 R624)
// R624: allocation -> allocate-object [(allocate-shape-spec-list)]
allocation
    : type_spec_allocation
    | identifier_or_keyword (LPAREN allocate_shape_spec_list RPAREN)?
    ;

// Type specification allocation (for PDTs)
// R623: [type-spec ::] in ALLOCATE statement
type_spec_allocation
    : derived_type_spec DOUBLE_COLON identifier_or_keyword
      (LPAREN allocate_shape_spec_list RPAREN)?
    ;

// Allocate shape specification list (ISO/IEC 1539-1:2004 R625)
allocate_shape_spec_list
    : allocate_shape_spec (COMMA allocate_shape_spec)*
    ;

// Allocate shape specification (ISO/IEC 1539-1:2004 R625)
// R625: allocate-shape-spec -> [lower-bound-expr :] upper-bound-expr
allocate_shape_spec
    : expr_f90 (COLON expr_f90)?
    ;

// Allocation option list (ISO/IEC 1539-1:2004 R627)
alloc_opt_list
    : alloc_opt (COMMA alloc_opt)*
    ;

// Allocation option (ISO/IEC 1539-1:2004 R627)
// R627: alloc-opt -> STAT = stat-variable | ERRMSG = errmsg-variable
//                  | SOURCE = source-expr | MOLD = source-expr
alloc_opt
    : STAT EQUALS identifier_or_keyword
    | ERRMSG EQUALS identifier_or_keyword
    | SOURCE EQUALS expr_f2003          // Clone data (Section 6.3.1)
    | MOLD EQUALS expr_f2003            // Clone type only (Section 6.3.1)
    ;

// ====================================================================
// ENHANCED I/O STATEMENTS (ISO/IEC 1539-1:2004 Section 9)
// ====================================================================
//
// F2003 significantly enhances I/O with:
// - Stream I/O (Section 9.2.2.4)
// - Asynchronous I/O (Section 9.5.1)
// - WAIT statement (Section 9.6)
// - FLUSH statement (Section 9.7)
// - IOMSG= specifier (Section 9.10)
// - Defined derived-type I/O (Section 9.5.3.7)
//
// Key rules:
// - R909 (wait-stmt) -> WAIT (wait-spec-list)
// - R922 (flush-stmt) -> FLUSH (flush-spec-list)

// WAIT statement (ISO/IEC 1539-1:2004 R909)
// R909: wait-stmt -> WAIT (wait-spec-list)
// WAIT completes asynchronous I/O operations
wait_stmt
    : WAIT LPAREN wait_spec_list RPAREN NEWLINE
    ;

// Wait specification list (ISO/IEC 1539-1:2004 R910)
wait_spec_list
    : wait_spec (COMMA wait_spec)*
    ;

// Wait specification (ISO/IEC 1539-1:2004 R910)
// R910: wait-spec -> [UNIT =] file-unit-number | IOSTAT = scalar-int-variable
//                  | IOMSG = iomsg-variable | ERR = label | END = label
//                  | EOR = label | ID = scalar-int-expr
wait_spec
    : UNIT EQUALS primary                     // unit=value
    | ID EQUALS primary                       // id=async_id
    | IOSTAT EQUALS primary                   // iostat=variable
    | IOMSG EQUALS primary                    // iomsg=variable (F2003)
    | ERR EQUALS primary                      // err=label
    | END EQUALS primary                      // end=label
    | EOR EQUALS primary                      // eor=label
    | primary                                 // Positional unit
    ;

// FLUSH statement (ISO/IEC 1539-1:2004 R922)
// R922: flush-stmt -> FLUSH (flush-spec-list) | FLUSH file-unit-number
// FLUSH ensures all pending I/O is completed
flush_stmt
    : FLUSH (LPAREN flush_spec_list RPAREN)? NEWLINE
    ;

// Flush specification list (ISO/IEC 1539-1:2004 R923)
flush_spec_list
    : flush_spec (COMMA flush_spec)*
    ;

// Flush specification (ISO/IEC 1539-1:2004 R923)
// R923: flush-spec -> [UNIT =] file-unit-number | IOSTAT = scalar-int-variable
//                   | IOMSG = iomsg-variable | ERR = label
flush_spec
    : UNIT EQUALS primary                     // unit=value
    | IOSTAT EQUALS primary                   // iostat=variable
    | IOMSG EQUALS primary                    // iomsg=variable (F2003)
    | ERR EQUALS primary                      // err=label
    | primary                                 // Positional unit
    ;

// I/O control specification (ISO/IEC 1539-1:2004 R913)
// R913: io-control-spec -> [UNIT =] io-unit | [FMT =] format
//                        | [NML =] namelist-group-name | ADVANCE = ...
io_control_spec
    : UNIT EQUALS primary                    // unit=10, unit=*
    | FMT EQUALS primary                     // fmt=*, fmt=100
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

// ====================================================================
// F2003 I/O STATEMENTS (ISO/IEC 1539-1:2004 Section 9.5)
// ====================================================================
//
// F2003 I/O statements with enhanced specifiers:
// - R904 (open-stmt) -> OPEN (connect-spec-list)
// - R907 (close-stmt) -> CLOSE (close-spec-list)
// - R910 (read-stmt) -> READ (io-control-spec-list) [input-item-list]
// - R911 (write-stmt) -> WRITE (io-control-spec-list) [output-item-list]
//
// F2003 additions: ASYNCHRONOUS=, STREAM=, ID=, IOMSG=, PENDING=

// OPEN statement (ISO/IEC 1539-1:2004 R904)
open_stmt
    : OPEN LPAREN f2003_io_spec_list RPAREN NEWLINE?
    ;

// CLOSE statement (ISO/IEC 1539-1:2004 R907)
close_stmt
    : CLOSE LPAREN f2003_io_spec_list RPAREN NEWLINE?
    ;

// WRITE statement (ISO/IEC 1539-1:2004 R911)
write_stmt
    : WRITE LPAREN f2003_io_spec_list RPAREN (output_item_list)? NEWLINE?
    ;

// READ statement (ISO/IEC 1539-1:2004 R910)
read_stmt
    : READ LPAREN f2003_io_spec_list RPAREN (input_item_list)? NEWLINE?
    ;

// Unified F2003 I/O specification list
//
// FORMAT STRINGS AND DT EDIT DESCRIPTORS (ISO/IEC 1539-1:2004 Section 10.2.2)
// ---------------------------------------------------------------------------
// Format specifications (fmt=... or positional string literals) are parsed
// as opaque character literals. The grammar does NOT structurally parse the
// format-specification content, including DT edit descriptors.
//
// DT edit descriptors for defined derived-type I/O (Section 10.2.2.4) have:
//   DT [ char-literal-constant ] [ ( v-list ) ]
// Examples:
//   write(*, '(DT)') obj
//   write(*, '(DT"mytype")') obj
//   write(*, '(DT(10,2))') obj
//   write(*, '(DT"fmt"(5,3,1))') obj
//
// The DT descriptor syntax is accepted as part of the format string character
// literal, but is not validated or parsed into structured nodes. This is an
// intentional design decision documented in docs/fortran_2003_audit.md.
// See also: issue #185, generic_spec rule for READ(FORMATTED)/WRITE(UNFORMATTED).
f2003_io_spec_list
    : f2003_io_spec (COMMA f2003_io_spec)*
    ;

// F2003 I/O specification (ISO/IEC 1539-1:2004 R905, R913)
// Covers OPEN connect-spec, CLOSE close-spec, READ/WRITE io-control-spec
f2003_io_spec
    : IDENTIFIER EQUALS primary
        // Regular identifier = value (file='test.dat')
    | UNIT EQUALS primary                        // unit=10, unit=*
    | FILE EQUALS primary                        // file='filename'
    | ACCESS EQUALS primary                      // access='stream' (F2003)
    | FORM EQUALS primary                        // form='unformatted'
    | STATUS EQUALS primary                      // status='new'
    | BLANK EQUALS primary                       // blank='null'
    | POSITION EQUALS primary                    // position='rewind'
    | ACTION EQUALS primary                      // action='read'
    | DELIM EQUALS primary                       // delim='apostrophe'
    | PAD EQUALS primary                         // pad='yes'
    | RECL EQUALS primary                        // recl=100
    | IOSTAT EQUALS primary                      // iostat=ios
    | IOMSG EQUALS primary                       // iomsg=msg (F2003)
    | ERR EQUALS primary                         // err=100
    | END EQUALS primary                         // end=200
    | EOR EQUALS primary                         // eor=300
    | ADVANCE EQUALS primary                     // advance='yes'
    | SIZE EQUALS primary                        // size=isize
    | REC EQUALS primary                         // rec=irec
    | ASYNCHRONOUS EQUALS primary                // asynchronous='yes' (F2003)
    | STREAM EQUALS primary                      // stream='yes' (F2003 R905)
    | PENDING EQUALS primary                     // pending=var (F2003 R923)
    | ID EQUALS primary                          // id=id_var (F2003)
    | FMT EQUALS primary                         // fmt=*, fmt=100, fmt='(DT)'
    | primary                                    // Positional: *, 10, '(DT)', etc.
    ;

// Output item list (ISO/IEC 1539-1:2004 R915)
output_item_list
    : output_item (COMMA output_item)*
    ;

// Output item (ISO/IEC 1539-1:2004 R915)
// R915: output-item -> expr | io-implied-do
output_item
    : expr_f2003
    | io_implied_do
    ;

// Input item list (ISO/IEC 1539-1:2004 R914)
input_item_list
    : input_item (COMMA input_item)*
    ;

// Input item (ISO/IEC 1539-1:2004 R914)
// R914: input-item -> variable | io-implied-do
input_item
    : identifier_or_keyword
    | io_implied_do
    ;

// I/O implied do (ISO/IEC 1539-1:2004 R916)
// R916: io-implied-do -> (io-implied-do-object-list, io-implied-do-control)
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

// PRINT statement (ISO/IEC 1539-1:2004 R912)
// R912: print-stmt -> PRINT format [, output-item-list]
print_stmt
    : PRINT '*' COMMA actual_arg_list                // print *, args
    | PRINT primary COMMA actual_arg_list            // print format, args
    | PRINT '*'                                      // print *
    ;

// STOP statement (ISO/IEC 1539-1:2004 R849)
// R849: stop-stmt -> STOP [stop-code]
stop_stmt
    : STOP (INTEGER_LITERAL | string_literal)?
    ;

// DEALLOCATE statement (ISO/IEC 1539-1:2004 R631)
// R631: deallocate-stmt -> DEALLOCATE (allocate-object-list [, dealloc-opt-list])
deallocate_stmt
    : DEALLOCATE LPAREN allocation_list RPAREN NEWLINE
    ;

// MOVE_ALLOC intrinsic subroutine (ISO/IEC 1539-1:2004 Section 13.7.80)
// R1219: CALL MOVE_ALLOC(FROM=from, TO=to)
// Atomically deallocates TO, moves allocation from FROM to TO, deallocates FROM
move_alloc_stmt
    : CALL MOVE_ALLOC LPAREN move_alloc_args RPAREN NEWLINE
    ;

// Move_alloc arguments - typically FROM and TO (named or positional)
move_alloc_args
    : actual_arg_list
    ;

// ====================================================================
// VOLATILE AND PROTECTED ATTRIBUTES (ISO/IEC 1539-1:2004 Section 5.1.2)
// ====================================================================
//
// F2003 introduces VOLATILE and PROTECTED attributes.
// - VOLATILE (Section 5.1.2.16): Variable may be modified by means
//   other than the executing program (e.g., asynchronous I/O, signals)
// - PROTECTED (Section 5.1.2.10): Module variable is read-only outside
//   its defining module

// VOLATILE statement (ISO/IEC 1539-1:2004 R548)
// R548: volatile-stmt -> VOLATILE [::] object-name-list
volatile_stmt
    : VOLATILE DOUBLE_COLON object_name_list NEWLINE
    ;

// PROTECTED statement (ISO/IEC 1539-1:2004 Section 5.1.2.10)
protected_stmt
    : PROTECTED DOUBLE_COLON object_name_list NEWLINE
    ;

object_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ====================================================================
// USE STATEMENT (ISO/IEC 1539-1:2004 Section 11.2.1)
// ====================================================================
//
// F2003 enhances USE for intrinsic modules.
// - R1109 (use-stmt) -> USE [[, module-nature] ::] module-name [, rename-list]
//                     | USE [[, module-nature] ::] module-name, ONLY: [only-list]
// - R1110 (module-nature) -> INTRINSIC | NON_INTRINSIC
//
// The INTRINSIC module nature is used for IEEE modules (Section 14)
// and ISO_C_BINDING (Section 15.2).

// USE statement (ISO/IEC 1539-1:2004 R1109)
use_stmt
    : USE IDENTIFIER NEWLINE
    | USE IDENTIFIER COMMA ONLY COLON only_list NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON ieee_module_name NEWLINE
    | USE COMMA INTRINSIC DOUBLE_COLON ieee_module_name COMMA
      ONLY COLON ieee_only_list NEWLINE
    | USE COMMA NON_INTRINSIC DOUBLE_COLON IDENTIFIER NEWLINE
    | USE COMMA NON_INTRINSIC DOUBLE_COLON IDENTIFIER COMMA
      ONLY COLON only_list NEWLINE
    ;

// ====================================================================
// IEEE INTRINSIC MODULES (ISO/IEC 1539-1:2004 Section 14)
// ====================================================================
//
// F2003 introduces three IEEE intrinsic modules:
// - IEEE_EXCEPTIONS (Section 14.2): Exception handling
// - IEEE_ARITHMETIC (Section 14.3): Arithmetic functions and values
// - IEEE_FEATURES (Section 14.4): Feature inquiry
//
// These modules provide IEEE 754 floating-point support.

// IEEE module names (ISO/IEC 1539-1:2004 Section 14)
ieee_module_name
    : IEEE_EXCEPTIONS      // Exception handling (Section 14.2)
    | IEEE_ARITHMETIC      // Arithmetic functions (Section 14.3)
    | IEEE_FEATURES        // Feature inquiry (Section 14.4)
    ;

// IEEE-specific only list for USE statements
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

// IEEE exception types (ISO/IEC 1539-1:2004 Section 14.2)
// Derived type IEEE_FLAG_TYPE values
ieee_exception_type
    : IEEE_OVERFLOW        // Floating-point overflow
    | IEEE_UNDERFLOW       // Floating-point underflow
    | IEEE_DIVIDE_BY_ZERO  // Division by zero
    | IEEE_INVALID         // Invalid operation
    | IEEE_INEXACT         // Inexact result
    ;

// IEEE special values (ISO/IEC 1539-1:2004 Section 14.3)
// Derived type IEEE_CLASS_TYPE values
ieee_special_value
    : IEEE_POSITIVE_INF    // Positive infinity
    | IEEE_NEGATIVE_INF    // Negative infinity
    | IEEE_QUIET_NAN       // Quiet NaN
    | IEEE_SIGNALING_NAN   // Signaling NaN
    ;

// IEEE rounding modes (ISO/IEC 1539-1:2004 Section 14.3)
// Derived type IEEE_ROUND_TYPE values
ieee_rounding_mode
    : IEEE_NEAREST         // Round to nearest
    | IEEE_TO_ZERO         // Round toward zero
    | IEEE_UP              // Round toward +infinity
    | IEEE_DOWN            // Round toward -infinity
    ;

// IEEE feature names (ISO/IEC 1539-1:2004 Section 14.4)
// Derived type IEEE_FEATURES_TYPE values
ieee_feature_name
    : IEEE_DATATYPE        // IEEE data type support
    | IEEE_DENORMAL        // Denormal numbers support
    | IEEE_DIVIDE          // Division operation support
    | IEEE_HALTING         // Halting mode support
    | IEEE_INEXACT_FLAG    // Inexact flag support
    | IEEE_INF             // Infinity support
    | IEEE_INVALID_FLAG    // Invalid flag support
    | IEEE_NAN             // NaN support
    | IEEE_ROUNDING        // Rounding mode support
    | IEEE_SQRT            // Square root support
    | IEEE_UNDERFLOW_FLAG  // Underflow flag support
    ;

// ====================================================================
// IMPLICIT STATEMENT (ISO/IEC 1539-1:2004 Section 5.3)
// ====================================================================
//
// - R549 (implicit-stmt) -> IMPLICIT implicit-spec-list | IMPLICIT NONE
// - R550 (implicit-spec) -> declaration-type-spec (letter-spec-list)

// IMPLICIT statement (ISO/IEC 1539-1:2004 R549)
implicit_stmt
    : IMPLICIT NONE NEWLINE?
    | IMPLICIT implicit_spec_list NEWLINE?
    ;

implicit_spec_list
    : implicit_spec (COMMA implicit_spec)*
    ;

// IMPLICIT specification (ISO/IEC 1539-1:2004 R550)
implicit_spec
    : type_spec LPAREN letter_spec_list RPAREN
    ;

// Type specification (ISO/IEC 1539-1:2004 R502)
// R502: type-spec -> intrinsic-type-spec | derived-type-spec
type_spec
    : INTEGER
    | REAL
    | COMPLEX
    | CHARACTER
    | LOGICAL
    | c_interop_type    // C interoperability types (Section 15)
    ;

// Letter specification (ISO/IEC 1539-1:2004 R551)
letter_spec_list
    : letter_spec (COMMA letter_spec)*
    ;

letter_spec
    : IDENTIFIER
    | IDENTIFIER MINUS IDENTIFIER
    ;

// Only list for USE statement (ISO/IEC 1539-1:2004 R1112)
only_list
    : only_name (COMMA only_name)*
    ;

only_name
    : IDENTIFIER
    | c_interop_type
    ;

// Declaration construct (fallback)
declaration_construct
    : derived_type_def_f2003
    | class_declaration_stmt
    | procedure_declaration_stmt
    | type_declaration_stmt
    | volatile_stmt
    | protected_stmt
    ;

// ====================================================================
// TYPE DECLARATION STATEMENT (ISO/IEC 1539-1:2004 Section 5.1)
// ====================================================================
//
// - R501 (type-declaration-stmt) -> declaration-type-spec [[, attr-spec]... ::]
//                                   entity-decl-list
// F2003 adds CLASS declarations (Section 4.5.6) and C interop types (Section 15)

// Type declaration statement (ISO/IEC 1539-1:2004 R501)
type_declaration_stmt
    : INTEGER kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | REAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | LOGICAL kind_selector? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | CHARACTER char_selector_extended? (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | c_interop_type (COMMA attr_spec_list)? DOUBLE_COLON
      entity_decl_list NEWLINE
    | TYPE LPAREN derived_type_spec RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    | CLASS LPAREN type_spec_or_star RPAREN (COMMA attr_spec_list)?
      DOUBLE_COLON entity_decl_list NEWLINE
    ;

// Kind selector (ISO/IEC 1539-1:2004 R507)
// R507: kind-selector -> ( [KIND =] scalar-int-initialization-expr )
kind_selector
    : LPAREN kind_param RPAREN              // (k) - kind parameter
    | LPAREN KIND EQUALS kind_param RPAREN  // (kind=k) - explicit kind
    ;

kind_param
    : IDENTIFIER
    | INTEGER_LITERAL    // For numeric kinds like 4, 8
    | LABEL              // Statement labels parsed as integers
    | c_interop_type
    ;

// Character selector extended (supports both F90+ and F77 styles)
// F77 style: CHARACTER*n (asterisk length notation)
// F90+ style: CHARACTER(len) or CHARACTER(len=n, kind=k)
// ISO/IEC 1539-1:2004 R508: char-selector -> length-selector | (LEN = ..., KIND = ...)
char_selector_extended
    : char_selector              // Modern F90+ style: (len) or (len=n)
    | character_length_f77       // F77 legacy style: *n or *(n)
    ;

char_selector
    : LPAREN char_length_spec RPAREN
    ;

// F77 character length syntax (legacy support)
// ISO 1539:1980 Section 4.8: CHARACTER*len
character_length_f77
    : MULTIPLY INTEGER_LITERAL                       // CHARACTER*10
    | MULTIPLY LPAREN MULTIPLY RPAREN                // CHARACTER*(*) assumed length
    | MULTIPLY LPAREN expr_f90 RPAREN                // CHARACTER*(len-expr)
    ;

char_length_spec
    : c_interop_type                         // C interop: (c_char)
    | expr_f2003                             // Simple length: (50)
    | LEN EQUALS expr_f2003                  // Explicit length: (len=50)
    ;

// Derived type specification (ISO/IEC 1539-1:2004 R455)
// R455: derived-type-spec -> type-name [(type-param-spec-list)]
derived_type_spec
    : IDENTIFIER                                          // Basic type name
    | IDENTIFIER LPAREN type_param_spec_list RPAREN       // Parameterized type
    | c_interop_type                                      // C interop types
    ;

// Type parameter specification list (ISO/IEC 1539-1:2004 R456)
type_param_spec_list
    : type_param_spec (COMMA type_param_spec)*
    ;

// Type parameter specification (ISO/IEC 1539-1:2004 R456)
// R456: type-param-spec -> [keyword =] type-param-value
type_param_spec
    : identifier_or_keyword EQUALS type_param_value    // kind=real64, n=:, m=*
    | type_param_value                                  // positional parameter
    ;

// Type parameter value (ISO/IEC 1539-1:2004 R457)
// R457: type-param-value -> scalar-int-expr | : | *
type_param_value
    : expr_f90        // Expression like 8, real64, or 100
    | COLON           // Deferred parameter (:)
    | '*'             // Assumed parameter (*)
    ;

// Attribute specification list (ISO/IEC 1539-1:2004 R503)
attr_spec_list
    : attr_spec (COMMA attr_spec)*
    ;

// Attribute specification (ISO/IEC 1539-1:2004 R503)
// R503: attr-spec -> ALLOCATABLE | ASYNCHRONOUS | BIND(C) | ...
// F2003 adds VALUE (Section 15.3.5), VOLATILE (Section 5.1.2.16),
// PROTECTED (Section 5.1.2.10)
attr_spec
    : PUBLIC
    | PRIVATE
    | ALLOCATABLE
    | POINTER
    | INTENT LPAREN intent_spec RPAREN
    | OPTIONAL
    | TARGET
    | VOLATILE          // F2003 (Section 5.1.2.16)
    | PROTECTED         // F2003 (Section 5.1.2.10)
    | PARAMETER
    | VALUE             // F2003 C interoperability (Section 15.3.5)
    ;

// Intent specification (ISO/IEC 1539-1:2004 R517)
// R517: intent-spec -> IN | OUT | INOUT
intent_spec
    : IN
    | OUT
    | INOUT
    ;

// Entity declaration list (ISO/IEC 1539-1:2004 R504)
entity_decl_list
    : entity_decl (COMMA entity_decl)*
    ;

// Entity declaration (ISO/IEC 1539-1:2004 R504)
// R504: entity-decl -> object-name [(array-spec)] [*char-length] [initialization]
entity_decl
    : identifier_or_keyword (LPAREN array_spec RPAREN)? (EQUALS expr_f2003)?
    ;

// Override F90 entity declaration
entity_decl_f90
    : identifier_or_keyword (LPAREN array_spec_f90 RPAREN)? (MULTIPLY char_length)?
      (ASSIGN expr_f90)?
    ;

// Module subprogram part (ISO/IEC 1539-1:2004 R1107)
module_subprogram_part
    : contains_stmt NEWLINE* (module_subprogram NEWLINE*)*
    ;

// Array specification (ISO/IEC 1539-1:2004 R510)
// R510: array-spec -> explicit-shape-spec-list | assumed-shape-spec-list
//                   | deferred-shape-spec-list | assumed-size-spec
array_spec
    : array_spec_element (COMMA array_spec_element)*
    ;

array_spec_element
    : expr_f90 (COLON expr_f90)?    // lower:upper or just upper
    | expr_f90 COLON                // lower: (assumed shape)
    | COLON                         // : (deferred shape)
    ;

// Execution part (inherited)
execution_part
    : executable_construct*
    ;

// Executable construct (inherited with F2003 additions)
executable_construct
    : assignment_stmt
    | call_stmt
    | print_stmt
    | stop_stmt
    | associate_construct
    | block_construct
    | allocate_stmt_f2003
    | deallocate_stmt
    | move_alloc_stmt
    | wait_stmt
    | flush_stmt
    | if_construct
    | do_construct
    | select_case_construct
    ;

// ====================================================================
// ASSIGNMENT AND EXPRESSIONS (ISO/IEC 1539-1:2004 Section 7.4)
// ====================================================================
//
// F2003 expressions support OOP features (component access, type-bound
// procedure calls) and procedure pointer assignments.
// - R734 (assignment-stmt) -> variable = expr
// - R735 (pointer-assignment-stmt) -> data-pointer-object => data-target
//                                   | proc-pointer-object => proc-target
//
// F2003 adds procedure pointer assignment (Section 7.4.2)

// Assignment statement (ISO/IEC 1539-1:2004 R734, R735)
assignment_stmt
    : lhs_expression EQUALS expr_f2003
    | lhs_expression POINTER_ASSIGN primary
        // Procedure pointer assignment (Section 7.4.2)
    ;

// ====================================================================
// F2003 EXPRESSIONS (ISO/IEC 1539-1:2004 Section 7)
// ====================================================================
//
// Section 7 defines expressions and their evaluation.
// - R722 (expr) -> [expr defined-binary-op] level-5-expr
// - R723 (defined-binary-op) -> .letter[letter]...
//
// Operator precedence (highest to lowest):
// 1. ** (power)
// 2. *, / (multiply, divide)
// 3. +, - (unary, add, subtract)
// 4. // (concatenation)
// 5. .EQ., .NE., .LT., .LE., .GT., .GE., ==, /=, <, <=, >, >=
// 6. .NOT. (unary)
// 7. .AND.
// 8. .OR.
// 9. .EQV., .NEQV.
// 10. Defined-binary-op

// F2003 expression (ISO/IEC 1539-1:2004 R722)
expr_f2003
    : expr_f2003 DOT_OR expr_f2003            // Logical OR (.or.)
    | expr_f2003 DOT_AND expr_f2003           // Logical AND (.and.)
    | DOT_NOT expr_f2003                      // Logical NOT (.not.)
    | expr_f2003 (GT | GT_OP) expr_f2003      // Greater than
    | expr_f2003 (LT | LT_OP) expr_f2003      // Less than
    | expr_f2003 (GE | GE_OP) expr_f2003      // Greater than or equal
    | expr_f2003 (LE | LE_OP) expr_f2003      // Less than or equal
    | expr_f2003 (EQ | EQ_OP) expr_f2003      // Equal
    | expr_f2003 (NE | NE_OP) expr_f2003      // Not equal
    | expr_f2003 CONCAT expr_f2003            // String concatenation (//)
    | expr_f2003 POWER expr_f2003             // Exponentiation
    | expr_f2003 (MULTIPLY | SLASH) expr_f2003  // Multiply/divide
    | expr_f2003 (PLUS | MINUS) expr_f2003    // Add/subtract
    | MINUS expr_f2003                        // Unary minus
    | PLUS expr_f2003                         // Unary plus
    | expr_f90                                // Inherit F90 expressions
    | primary                                 // F2003 primary
    ;

// Left-hand side expression (variable reference)
// ISO/IEC 1539-1:2004 R601: variable -> designator
// F2003 adds component access for OOP (Section 6.1.2)
lhs_expression
    : identifier_or_keyword                                          // Simple variable
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN           // Array element
    | identifier_or_keyword PERCENT identifier_or_keyword            // Component
    | identifier_or_keyword PERCENT identifier_or_keyword
      LPAREN actual_arg_list? RPAREN  // Component array/method
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN PERCENT
      identifier_or_keyword  // Array element component: shapes(i)%draw
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN PERCENT
      identifier_or_keyword LPAREN actual_arg_list? RPAREN
        // Array element component method: shapes(i)%draw()
    ;

// CALL statement (ISO/IEC 1539-1:2004 R1218)
// R1218: call-stmt -> CALL procedure-designator [( [actual-arg-spec-list] )]
// F2003 supports type-bound procedure calls (Section 12.4)
call_stmt
    : CALL lhs_expression
        // Type-bound procedure calls
    | CALL identifier_or_keyword (LPAREN actual_arg_list? RPAREN)?
        // Traditional procedure calls
    ;

// Actual argument list (ISO/IEC 1539-1:2004 R1220)
actual_arg_list
    : actual_arg (COMMA actual_arg)*
    ;

// Actual argument (ISO/IEC 1539-1:2004 R1221)
// R1221: actual-arg -> expr | variable | procedure-name | ...
// F2003 keyword arguments: keyword = actual-arg
actual_arg
    : identifier_or_keyword EQUALS expr_f2003    // Named argument
    | expr_f2003                                 // Positional argument
    ;

// IF construct (ISO/IEC 1539-1:2004 R802)
// R802: if-construct -> if-then-stmt block [else-if-stmt block]...
//                       [else-stmt block] end-if-stmt
if_construct
    : IF LPAREN logical_expr RPAREN THEN NEWLINE
      execution_part_f2003?
      (ELSE NEWLINE execution_part_f2003?)?
      END IF NEWLINE?
    ;

logical_expr
    : expr_f2003
    ;

// DO construct (ISO/IEC 1539-1:2004 R817)
// R817: do-construct -> do-stmt block end-do
do_construct
    : DO NEWLINE
      execution_part_f2003?
      END DO NEWLINE?
    | DO identifier_or_keyword EQUALS expr_f2003 COMMA expr_f2003
      (COMMA expr_f2003)? NEWLINE
      do_body_f2003
      END DO NEWLINE?
    ;

do_body_f2003
    : (NEWLINE
    | assignment_stmt
    | call_stmt
    | print_stmt
    | allocate_stmt_f2003
    | deallocate_stmt
    | move_alloc_stmt
    | if_construct
    | do_construct
    | block_construct)*
    ;

// CASE construct (ISO/IEC 1539-1:2004 R808)
// R808: case-construct -> select-case-stmt [case-stmt block]... end-select-stmt
select_case_construct
    : SELECT CASE LPAREN primary RPAREN NEWLINE
      case_construct+
      END SELECT NEWLINE?
    ;

// Case statement (ISO/IEC 1539-1:2004 R812)
case_construct
    : CASE LPAREN case_value_list RPAREN NEWLINE
      execution_part_f2003?
    | CASE DEFAULT NEWLINE
      execution_part_f2003?
    ;

case_value_list
    : primary (COMMA primary)*
    ;

// ====================================================================
// PRIMARY EXPRESSIONS (ISO/IEC 1539-1:2004 Section 7.1)
// ====================================================================
//
// - R701 (primary) -> constant | designator | array-constructor
//                   | structure-constructor | function-reference
//                   | type-param-inquiry | type-param-name | (expr)
// F2003 adds PDT structure constructors and IEEE constants

// Primary (ISO/IEC 1539-1:2004 R701)
primary
    : pdt_structure_constructor
    | identifier_or_keyword (PERCENT identifier_or_keyword)*
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN
    | identifier_or_keyword DOUBLE_QUOTE_STRING      // Prefixed string
    | identifier_or_keyword SINGLE_QUOTE_STRING      // Prefixed string
    | intrinsic_function_call
    | ieee_constant
    | INTEGER_LITERAL
    | LABEL              // Accept LABEL as integer literal
    | REAL_LITERAL
    | SINGLE_QUOTE_STRING
    | DOUBLE_QUOTE_STRING
    | '*'                // List-directed I/O format
    | array_constructor
    | LPAREN primary RPAREN
    ;

// PDT structure constructor (ISO/IEC 1539-1:2004 R454)
// R454: structure-constructor -> derived-type-spec ( [component-spec-list] )
pdt_structure_constructor
    : derived_type_spec LPAREN component_spec_list_f2003? RPAREN
    ;

// Component specification list (ISO/IEC 1539-1:2004 R456)
component_spec_list_f2003
    : component_spec_f2003 (COMMA component_spec_f2003)*
    ;

// Component specification (ISO/IEC 1539-1:2004 R456)
// R456: component-spec -> [keyword =] component-data-source
component_spec_f2003
    : identifier_or_keyword EQUALS expr_f2003    // Named component
    | expr_f2003                                 // Positional component
    ;

// Array constructor (ISO/IEC 1539-1:2004 R465-R469)
// R465: array-constructor -> (/ ac-spec /) | [ ac-spec ]
// R466: ac-spec -> type-spec :: [ac-value-list] | ac-value-list
// Square brackets are F2003+ syntax
// Note: LBRACKET/RBRACKET tokens are used because F2008+ lexers shadow
// LSQUARE/RSQUARE with these tokens for coarray image selectors
array_constructor
    : LBRACKET ac_spec RBRACKET
    | LSQUARE ac_spec RSQUARE
    ;

// ISO/IEC 1539-1:2004 R466: ac-spec
// ac-spec -> type-spec :: [ac-value-list] | ac-value-list
ac_spec
    : ac_type_spec DOUBLE_COLON array_constructor_elements?
    | array_constructor_elements?
    ;

// Type spec for array constructor (intrinsic types only)
ac_type_spec
    : INTEGER kind_selector?
    | REAL kind_selector?
    | LOGICAL kind_selector?
    | CHARACTER char_selector_extended?
    | COMPLEX kind_selector?
    ;

array_constructor_elements
    : array_constructor_element (COMMA array_constructor_element)*
    ;

array_constructor_element
    : expr_f2003
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN
    ;

// IEEE constants (ISO/IEC 1539-1:2004 Section 14)
ieee_constant
    : ieee_special_value
    | ieee_exception_type
    | ieee_rounding_mode
    | ieee_feature_name
    ;

// ====================================================================
// INTRINSIC FUNCTIONS (ISO/IEC 1539-1:2004 Section 13)
// ====================================================================
//
// Section 13 defines intrinsic procedures. F2003 adds IEEE functions
// from Section 14 and C interoperability functions from Section 15.

// Intrinsic function call
intrinsic_function_call
    : SELECTED_REAL_KIND LPAREN actual_arg_list RPAREN
    | SELECTED_INT_KIND LPAREN actual_arg_list RPAREN
    | KIND LPAREN actual_arg_list RPAREN
    | REAL LPAREN actual_arg_list RPAREN        // Type conversion
    | INTEGER LPAREN actual_arg_list RPAREN     // Type conversion
    | LOGICAL LPAREN actual_arg_list RPAREN     // Type conversion
    | CHARACTER LPAREN actual_arg_list RPAREN   // Type conversion
    | COMPLEX LPAREN actual_arg_list RPAREN     // Type conversion
    | SUM_INTRINSIC LPAREN actual_arg_list RPAREN
    | ieee_function_call
    ;

// IEEE arithmetic function calls (ISO/IEC 1539-1:2004 Section 14.3)
ieee_function_call
    : ieee_inquiry_function LPAREN actual_arg_list RPAREN
    | ieee_value_function LPAREN actual_arg_list RPAREN
    | IDENTIFIER LPAREN actual_arg_list RPAREN
    ;

// IEEE inquiry functions (Section 14.3)
ieee_inquiry_function
    : IDENTIFIER  // ieee_is_nan, ieee_is_finite, etc.
    ;

// IEEE value functions (Section 14.3)
ieee_value_function
    : IDENTIFIER  // ieee_value, ieee_copy_sign, etc.
    ;

// Override F90 function reference
function_reference_f90
    : IDENTIFIER LPAREN actual_arg_spec_list? RPAREN
    | SELECTED_REAL_KIND LPAREN actual_arg_spec_list? RPAREN
    | SELECTED_INT_KIND LPAREN actual_arg_spec_list? RPAREN
    | KIND LPAREN actual_arg_spec_list? RPAREN
    ;

// Override F90 literal
literal_f90
    : INTEGER_LITERAL_KIND
    | INTEGER_LITERAL
    | LABEL
    | REAL_LITERAL_KIND
    | REAL_LITERAL
    | DOUBLE_QUOTE_STRING
    | SINGLE_QUOTE_STRING
    | logical_literal_f90
    | boz_literal_constant
    ;

// ====================================================================
// C INTEROPERABILITY (ISO/IEC 1539-1:2004 Section 15)
// ====================================================================
//
// Section 15 defines interoperability with the C programming language.
// - BIND(C) attribute for procedures and derived types (Section 15.3)
// - VALUE attribute for C-style pass-by-value (Section 15.3.5)
// - ISO_C_BINDING intrinsic module (Section 15.2)
//
// Key rules:
// - R1225 (proc-language-binding-spec) -> language-binding-spec
// - R509 (language-binding-spec) -> BIND (C [, NAME = scalar-char-initialization-expr])

// BIND(C) specification (ISO/IEC 1539-1:2004 R509)
// R509: language-binding-spec -> BIND (C [, NAME = scalar-char-initialization-expr])
binding_spec
    : BIND LPAREN C RPAREN                                               // BIND(C)
    | BIND LPAREN C COMMA NAME EQUALS string_literal RPAREN
        // BIND(C, NAME="func")
    ;

// String literal
string_literal
    : DOUBLE_QUOTE_STRING
    | SINGLE_QUOTE_STRING
    ;

// C interoperability types (ISO/IEC 1539-1:2004 Section 15.2.2)
// Named constants from ISO_C_BINDING module representing C types
// Table 15.2 in ISO/IEC 1539-1:2004 lists the interoperable types
c_interop_type
    : C_INT              // int
    | C_SHORT            // short int
    | C_LONG             // long int
    | C_LONG_LONG        // long long int
    | C_SIGNED_CHAR      // signed char
    | C_SIZE_T           // size_t
    | C_INT8_T           // int8_t
    | C_INT16_T          // int16_t
    | C_INT32_T          // int32_t
    | C_INT64_T          // int64_t
    | C_INT_LEAST8_T     // int_least8_t
    | C_INT_LEAST16_T    // int_least16_t
    | C_INT_LEAST32_T    // int_least32_t
    | C_INT_LEAST64_T    // int_least64_t
    | C_INT_FAST8_T      // int_fast8_t
    | C_INT_FAST16_T     // int_fast16_t
    | C_INT_FAST32_T     // int_fast32_t
    | C_INT_FAST64_T     // int_fast64_t
    | C_INTMAX_T         // intmax_t
    | C_INTPTR_T         // intptr_t
    | C_FLOAT            // float
    | C_DOUBLE           // double
    | C_LONG_DOUBLE      // long double
    | C_FLOAT_COMPLEX    // float _Complex
    | C_DOUBLE_COMPLEX   // double _Complex
    | C_LONG_DOUBLE_COMPLEX  // long double _Complex
    | C_BOOL             // _Bool
    | C_CHAR             // char
    | C_PTR              // void * (C_PTR derived type)
    | C_FUNPTR           // Function pointer (C_FUNPTR derived type)
    | C_NULL_PTR         // NULL pointer constant
    | C_NULL_FUNPTR      // NULL function pointer constant
    ;

// Type name
type_name
    : identifier_or_keyword
    ;
