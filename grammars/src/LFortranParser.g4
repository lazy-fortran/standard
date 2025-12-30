// ============================================================================
// LFortran Parser - LFortran Standard Extensions to Fortran 2023
// ============================================================================
//
// This parser extends the Fortran 2023 parser with LFortran-specific features:
//
// 1. J3 Generics (Fortran 202Y preview):
//    - TEMPLATE construct with deferred type parameters
//    - REQUIREMENT construct for type constraints
//    - REQUIRE statement for constraint specifications
//    - INSTANTIATE statement for explicit instantiation
//
// 2. Global scope support (infer mode):
//    - Bare statements at top level without program/end program
//    - Bare expressions at top level
//    - Bare declarations at top level
//
// Reference: LFortran compiler (https://lfortran.org)
// ============================================================================

parser grammar LFortranParser;

options { tokenVocab = LFortranLexer; }

import Fortran2023Parser;

// ============================================================================
// TOP-LEVEL PROGRAM STRUCTURE (Extended for Global Scope)
// ============================================================================
// LFortran allows "script mode" where bare statements, declarations,
// and expressions can appear at the top level without a program wrapper.

program_lfortran
    : script_unit_list EOF
    ;

script_unit_list
    : script_unit+
    ;

// A script unit can be a standard Fortran program unit OR bare code
script_unit
    : program_unit_f2023          // Standard: program, module, submodule, etc.
    | template_construct          // J3 Generics: template definition
    | requirement_construct       // J3 Generics: requirement definition
    | instantiate_stmt            // J3 Generics: instantiation
    | use_stmt                    // Bare use statement (global scope)
    | implicit_stmt               // Bare implicit statement (global scope)
    | declaration_construct       // Bare declaration (global scope)
    | executable_construct        // Bare statement (global scope)
    | expr end_of_stmt            // Bare expression (global scope / REPL)
    ;

// ============================================================================
// J3 GENERICS: TEMPLATE CONSTRUCT
// ============================================================================
// Defines parameterized procedures with deferred type parameters.
//
// Syntax:
//   TEMPLATE template-name ( type-parameter-list )
//     [ template-specification-part ]
//   CONTAINS
//     [ template-subprogram-part ]
//   END TEMPLATE [ template-name ]

template_construct
    : TEMPLATE_KW NAME LPAREN template_param_list RPAREN end_of_stmt
      template_specification_part?
      template_contains_part?
      END_TEMPLATE NAME? end_of_stmt
    ;

template_param_list
    : template_param (COMMA template_param)*
    ;

template_param
    : NAME                        // Type parameter name
    ;

template_specification_part
    : template_specification_stmt+
    ;

template_specification_stmt
    : type_deferred_stmt          // TYPE, DEFERRED :: T
    | require_stmt                // REQUIRE :: constraint
    | use_stmt
    | implicit_stmt
    | declaration_construct
    ;

// TYPE, DEFERRED :: type-parameter
type_deferred_stmt
    : TYPE COMMA DEFERRED_KW DOUBLE_COLON NAME end_of_stmt
    ;

template_contains_part
    : CONTAINS end_of_stmt
      template_subprogram+
    ;

template_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;

// ============================================================================
// J3 GENERICS: REQUIREMENT CONSTRUCT
// ============================================================================
// Defines interface requirements for template type parameters.
//
// Syntax:
//   REQUIREMENT requirement-name ( type-parameter-list )
//     [ requirement-specification-part ]
//     [ requirement-interface-part ]
//   END REQUIREMENT [ requirement-name ]

requirement_construct
    : REQUIREMENT_KW NAME LPAREN template_param_list RPAREN end_of_stmt
      requirement_specification_part?
      requirement_interface_part?
      END_REQUIREMENT NAME? end_of_stmt
    ;

requirement_specification_part
    : requirement_specification_stmt+
    ;

requirement_specification_stmt
    : type_deferred_stmt
    | implicit_stmt
    ;

requirement_interface_part
    : interface_block
    | abstract_interface_body+
    ;

abstract_interface_body
    : function_stmt
      specification_part?
      END_FUNCTION NAME? end_of_stmt
    | subroutine_stmt
      specification_part?
      END_SUBROUTINE NAME? end_of_stmt
    ;

// ============================================================================
// J3 GENERICS: REQUIRE STATEMENT
// ============================================================================
// Specifies that a template parameter must satisfy a requirement.
//
// Syntax:
//   REQUIRE :: requirement-reference-list

require_stmt
    : REQUIRE_KW DOUBLE_COLON requirement_reference_list end_of_stmt
    ;

requirement_reference_list
    : requirement_reference (COMMA requirement_reference)*
    ;

requirement_reference
    : NAME LPAREN actual_param_list RPAREN
    ;

actual_param_list
    : NAME (COMMA NAME)*
    ;

// ============================================================================
// J3 GENERICS: INSTANTIATE STATEMENT
// ============================================================================
// Explicit instantiation of a template with concrete types.
//
// Syntax:
//   INSTANTIATE template-name ( type-list ) [, ONLY : rename-list ]

instantiate_stmt
    : INSTANTIATE_KW NAME LPAREN instantiate_type_list RPAREN
      instantiate_only_clause? end_of_stmt
    ;

instantiate_type_list
    : instantiate_type (COMMA instantiate_type)*
    ;

instantiate_type
    : intrinsic_type_spec         // INTEGER, REAL(8), etc.
    | derived_type_spec           // TYPE(my_type)
    ;

instantiate_only_clause
    : COMMA ONLY COLON rename_list
    ;

rename_list
    : rename (COMMA rename)*
    ;

rename
    : NAME ARROW NAME             // local-name => template-name
    | NAME                        // use-name
    ;
