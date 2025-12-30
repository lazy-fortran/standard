// ============================================================================
// LFortran Parser - LFortran Standard (F2023 + J3 Generics)
// ============================================================================
//
// This parser extends the Fortran 2023 parser with J3 Generics:
//
// J3 Generics (Fortran 202Y preview):
//   - TEMPLATE construct with deferred type parameters
//   - REQUIREMENT construct for type constraints
//   - REQUIRE statement for constraint specifications
//   - INSTANTIATE statement for explicit instantiation
//
// LFortran Standard semantic defaults (enforced by compiler, not grammar):
//   - Bounds checking ON
//   - Implicit typing OFF
//   - Default intent(in)
//   - 8-byte real as default
//
// Reference: LFortran compiler (https://lfortran.org)
// J3 Generics: Based on J3/24-107r1 proposal
// ============================================================================

parser grammar LFortranParser;

options { tokenVocab = LFortranLexer; }

import Fortran2023Parser;

// ============================================================================
// TOP-LEVEL PROGRAM STRUCTURE (Standard Fortran + J3 Generics)
// ============================================================================

program_lfortran
    : lfortran_unit_list EOF
    ;

lfortran_unit_list
    : lfortran_unit+
    ;

// LFortran program unit: standard Fortran program units + J3 generics
lfortran_unit
    : NEWLINE* program_unit_f2023 NEWLINE*        // Standard: program, module, etc.
    | NEWLINE* template_construct NEWLINE*        // J3 Generics: template definition
    | NEWLINE* requirement_construct NEWLINE*     // J3 Generics: requirement definition
    | NEWLINE* simple_template_function NEWLINE*   // J3: template function
    | NEWLINE* simple_template_subroutine NEWLINE* // J3: template subroutine
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
    : TEMPLATE_KW NAME LBRACE template_param_list RBRACE NEWLINE*
      template_specification_part?
      template_contains_part?
      END_TEMPLATE NAME? NEWLINE*
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
    | requires_stmt               // REQUIRES :: constraint{args}
    | use_stmt
    | implicit_stmt
    | declaration_construct
    ;

// TYPE, DEFERRED :: type-parameter
type_deferred_stmt
    : TYPE COMMA DEFERRED_KW DOUBLE_COLON NAME NEWLINE*
    ;

template_contains_part
    : CONTAINS NEWLINE*
      template_subprogram+
    ;

template_subprogram
    : function_subprogram_f2018
    | subroutine_subprogram_f2018
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
    : REQUIREMENT_KW NAME LBRACE template_param_list RBRACE NEWLINE*
      requirement_specification_part?
      requirement_interface_part?
      END_REQUIREMENT NAME? NEWLINE*
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
    : function_stmt_f2018
      specification_part?
      END_FUNCTION NAME? NEWLINE*
    | subroutine_stmt_f2018
      specification_part?
      END_SUBROUTINE NAME? NEWLINE*
    ;

// ============================================================================
// J3 GENERICS: REQUIRES STATEMENT
// ============================================================================
// Specifies that a template parameter must satisfy a requirement.
//
// Syntax (J3/24-107r1):
//   REQUIRES [::] requirement-name { instantiation-arg-spec-list }

requires_stmt
    : REQUIRES_KW DOUBLE_COLON? requirement_reference_list NEWLINE*
    ;

requirement_reference_list
    : requirement_reference (COMMA requirement_reference)*
    ;

requirement_reference
    : NAME LBRACE instantiation_arg_spec_list? RBRACE
    ;

instantiation_arg_spec_list
    : instantiation_arg_spec (COMMA instantiation_arg_spec)*
    ;

instantiation_arg_spec
    : (NAME EQUALS)? instantiation_arg
    ;

instantiation_arg
    : NAME                        // Type parameter or procedure name
    | intrinsic_type_spec_f95     // INTEGER, REAL(8), etc.
    | derived_type_spec           // TYPE(my_type)
    ;

// ============================================================================
// J3 GENERICS: INSTANTIATE STATEMENT
// ============================================================================
// Explicit instantiation of a template with concrete types.
//
// Syntax (J3/24-107r1):
//   INSTANTIATE [::] template-name { arg-spec-list } [, ONLY : rename-list ]

instantiate_stmt
    : INSTANTIATE_KW DOUBLE_COLON? NAME LBRACE instantiation_arg_spec_list? RBRACE
      instantiate_only_clause? NEWLINE*
    ;

instantiate_only_clause
    : COMMA ONLY COLON rename_list
    ;

rename_list
    : rename (COMMA rename)*
    ;

rename
    : NAME POINTER_ASSIGN NAME    // local-name => template-name
    | NAME                        // use-name
    ;

// ============================================================================
// J3 GENERICS: SIMPLE TEMPLATE PROCEDURES
// ============================================================================
// Simple template procedures embed deferred type parameters in the name.
// This is shorthand for a full TEMPLATE construct.
//
// Syntax:
//   function name{T}(args) result(res)
//   subroutine name{T}(args)
//
// Reference: J3/24-107r1 - Simple template procedures

simple_template_function
    : prefix? FUNCTION NAME LBRACE deferred_arg_list RBRACE
      LPAREN dummy_arg_name_list? RPAREN suffix? NEWLINE*
      template_specification_part?
      specification_part?
      execution_part?
      internal_subprogram_part_f2003?
      END_FUNCTION NAME? NEWLINE*
    ;

simple_template_subroutine
    : prefix? SUBROUTINE NAME LBRACE deferred_arg_list RBRACE
      LPAREN dummy_arg_name_list? RPAREN NEWLINE*
      template_specification_part?
      specification_part?
      execution_part?
      internal_subprogram_part_f2003?
      END_SUBROUTINE NAME? NEWLINE*
    ;

deferred_arg_list
    : NAME (COMMA NAME)*
    ;

// ============================================================================
// J3 GENERICS: INLINE INSTANTIATION
// ============================================================================
// Inline instantiation allows calling templates without explicit INSTANTIATE.
// Two syntaxes are supported:
//   - Curly braces: name{type}(args) (J3/24-107r1)
//   - Caret: name^(type)(args) (J3 r4 revision)
//
// Reference: J3/24-107r1 and J3 r4 revision

// Inline instantiation argument list (types to instantiate with)
inline_instantiate_args
    : instantiation_arg (COMMA instantiation_arg)*
    ;

// Inline instantiated procedure reference (for function calls in expressions)
// Matches: name{type}(args) or name^(type)(args)
inline_instantiated_function_ref
    : NAME LBRACE inline_instantiate_args RBRACE LPAREN actual_arg_list? RPAREN
    | NAME CARET LPAREN inline_instantiate_args RPAREN LPAREN actual_arg_list? RPAREN
    ;

// Inline instantiated call statement
// Matches: CALL name{type}(args) or CALL name^(type)(args)
inline_instantiate_call_stmt
    : CALL NAME LBRACE inline_instantiate_args RBRACE
      LPAREN actual_arg_list? RPAREN NEWLINE*
    | CALL NAME CARET LPAREN inline_instantiate_args RPAREN
      LPAREN actual_arg_list? RPAREN NEWLINE*
    ;

// ============================================================================
// EXECUTABLE CONSTRUCT OVERRIDE (LFortran Extensions)
// ============================================================================
// Override F2023 executable_construct to include J3 Generics inline instantiation

executable_construct_lfortran
    : inline_instantiate_call_stmt       // J3 Generics: call name{type}(args)
    | instantiate_stmt                   // J3 Generics: explicit instantiate
    | executable_construct_f2018         // Inherit F2023 constructs
    ;

// ============================================================================
// EXPRESSION OVERRIDE (LFortran Extensions)
// ============================================================================
// Override to include inline instantiated function calls in expressions

// Primary expression extended with inline instantiation
primary_lfortran
    : inline_instantiated_function_ref   // J3 Generics: name{type}(args)
    | primary                            // Inherit from F2023
    ;
