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
    : TEMPLATE_KW identifier_or_keyword LBRACE template_param_list RBRACE NEWLINE*
      template_specification_part?
      template_contains_part?
      END_TEMPLATE identifier_or_keyword? NEWLINE*
    ;

template_param_list
    : template_param (COMMA template_param)*
    ;

template_param
    : identifier_or_keyword       // Type parameter name
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
    : TYPE COMMA DEFERRED_KW DOUBLE_COLON identifier_or_keyword NEWLINE*
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
    : REQUIREMENT_KW identifier_or_keyword LBRACE template_param_list RBRACE NEWLINE*
      requirement_specification_part?
      requirement_interface_part?
      END_REQUIREMENT identifier_or_keyword? NEWLINE*
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
      end_function_stmt
    | subroutine_stmt_f2018
      specification_part?
      end_subroutine_stmt
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
    : identifier_or_keyword LBRACE instantiation_arg_spec_list? RBRACE
    ;

instantiation_arg_spec_list
    : instantiation_arg_spec (COMMA instantiation_arg_spec)*
    ;

instantiation_arg_spec
    : (identifier_or_keyword EQUALS)? instantiation_arg
    ;

instantiation_arg
    : identifier_or_keyword       // Type parameter or procedure name
    | intrinsic_type_spec_f95     // INTEGER, REAL(8), etc.
    | derived_type_spec           // TYPE(my_type)
    | TYPE LPAREN identifier_or_keyword RPAREN
    ;

// ============================================================================
// J3 GENERICS: INSTANTIATE STATEMENT
// ============================================================================
// Explicit instantiation of a template with concrete types.
//
// Syntax (J3/24-107r1):
//   INSTANTIATE [::] template-name { arg-spec-list } [, ONLY : rename-list ]

instantiate_stmt
    : INSTANTIATE_KW DOUBLE_COLON? identifier_or_keyword LBRACE
      instantiation_arg_spec_list? RBRACE
      instantiate_only_clause? NEWLINE*
    ;

instantiate_only_clause
    : COMMA ONLY COLON rename_list
    ;

rename_list
    : rename (COMMA rename)*
    ;

rename
    : identifier_or_keyword POINTER_ASSIGN identifier_or_keyword
      // local-name => template-name
    | identifier_or_keyword                                        // use-name
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
    : prefix? FUNCTION identifier_or_keyword LBRACE deferred_arg_list RBRACE
      LPAREN dummy_arg_name_list? RPAREN suffix? NEWLINE*
      template_specification_part?
      specification_part?
      execution_part?
      internal_subprogram_part_f2003?
      end_function_stmt
    ;

simple_template_subroutine
    : prefix? SUBROUTINE identifier_or_keyword LBRACE deferred_arg_list RBRACE
      LPAREN dummy_arg_name_list? RPAREN NEWLINE*
      template_specification_part?
      specification_part?
      execution_part?
      internal_subprogram_part_f2003?
      end_subroutine_stmt
    ;

deferred_arg_list
    : identifier_or_keyword (COMMA identifier_or_keyword)*
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
    : identifier_or_keyword LBRACE inline_instantiate_args RBRACE
      LPAREN actual_arg_list? RPAREN
    | identifier_or_keyword CARET LPAREN inline_instantiate_args RPAREN
      LPAREN actual_arg_list? RPAREN
    ;

// Inline instantiated call statement
// Matches: CALL name{type}(args) or CALL name^(type)(args)
inline_instantiate_call_stmt
    : CALL identifier_or_keyword LBRACE inline_instantiate_args RBRACE
      LPAREN actual_arg_list? RPAREN NEWLINE*
    | CALL identifier_or_keyword CARET LPAREN inline_instantiate_args RPAREN
      LPAREN actual_arg_list? RPAREN NEWLINE*
    ;

// Names accepted by LFortran extensions in contexts where the base grammars
// still require strict IDENTIFIER matching.
lfortran_name
    : identifier_or_keyword
    | C
    | DOUBLE
    ;

// ============================================================================
// PROCEDURE/PREFIX OVERRIDES (LFortran Extensions)
// ============================================================================
// Use modern prefix specs for all contexts that still reference legacy `prefix`.
// Allow keyword-like names where LFortran accepts them.

prefix
    : prefix_spec_f2008+
    ;

function_stmt_f2018
    : prefix? FUNCTION lfortran_name LPAREN dummy_arg_name_list? RPAREN
      suffix? binding_spec? NEWLINE
    ;

subroutine_stmt_f2018
    : prefix? SUBROUTINE lfortran_name (LPAREN dummy_arg_name_list? RPAREN)?
      binding_spec? NEWLINE
    ;

function_stmt_interface
    : (prefix)? FUNCTION lfortran_name LPAREN dummy_arg_name_list? RPAREN
      (suffix)? binding_spec? NEWLINE
    ;

subroutine_stmt_interface
    : (prefix)? SUBROUTINE lfortran_name
      (LPAREN dummy_arg_name_list? RPAREN)? binding_spec? NEWLINE
    ;

end_subroutine_stmt_interface
    : END (SUBROUTINE (lfortran_name)?)? NEWLINE
    ;

end_function_stmt_interface
    : END (FUNCTION (lfortran_name)?)? NEWLINE
    ;

end_subroutine_stmt
    : END (SUBROUTINE (lfortran_name)?)? NEWLINE?
    ;

end_function_stmt
    : END (FUNCTION (lfortran_name)?)? NEWLINE?
    ;

// ============================================================================
// NAME-ACCEPTANCE OVERRIDES (LFortran Extensions)
// ============================================================================
// Keep declaration and designator parsing robust when lexer tokenizes short
// names like `c` and words like `double` as keyword tokens.

designator_component
    : lfortran_name designator_parentheses?
    ;

derived_component
    : PERCENT lfortran_name designator_parentheses?
    ;

entity_decl
    : lfortran_name (LPAREN array_spec RPAREN)? (EQUALS expr_f2003)?
    ;

entity_decl_f90
    : lfortran_name (LPAREN array_spec_f90 RPAREN)? (MULTIPLY char_length)?
      (ASSIGN expr_f90)?
    ;

// ============================================================================
// DECLARATION CONSTRUCT OVERRIDE (LFortran Extensions)
// ============================================================================
// Allow J3 generics constructs in F2018 specification parts (e.g. modules).

declaration_construct_f2018
    : template_construct
    | requirement_construct
    | instantiate_stmt
    | derived_type_def_f2003
    | class_declaration_stmt
    | procedure_declaration_stmt
    | interface_block
    | volatile_stmt
    | protected_stmt
    | contiguous_stmt
    | event_declaration_stmt
    | team_declaration_stmt
    | type_declaration_stmt_f2018
    | format_stmt
    | declaration_construct_f2008
    ;

// ============================================================================
// EXECUTABLE CONSTRUCT OVERRIDE (LFortran Extensions)
// ============================================================================
// Override F2023 executable_construct to include J3 Generics inline
// instantiation and explicit instantiate statements.

executable_construct_f2018
    : inline_instantiate_call_stmt       // J3 Generics: call name{type}(args)
    | instantiate_stmt                   // J3 Generics: explicit instantiate
    | type_declaration_stmt_f2018
      // Keep declarations reachable if execution-part is chosen
    | assignment_stmt                 // Inherit from F2003
    | call_stmt                       // Inherit from F2003
    | print_stmt                      // Inherit from F2003
    | stop_stmt_f2018                 // Enhanced in F2018 (R1160)
    | error_stop_stmt_f2018           // Enhanced in F2018 (R1161)
    | select_type_construct           // Inherit from F2003
    | select_rank_construct           // NEW in F2018 (R1148-R1151)
    | associate_construct             // Inherit from F2003
    | block_construct_f2008           // Inherit from F2008
    | allocate_stmt_f2008             // Inherit from F2008
    | collective_subroutine_call      // NEW in F2018 (Section 16.9.46-50)
    | team_construct                  // NEW in F2018 (teams: R1111-R1115, R1175-R1178)
    | event_construct                 // NEW in F2018 (events: R1170-R1173)
    | notify_wait_stmt_f2023          // NEW in F2023 (Section 11.6, R1179)
    | sync_construct                  // Inherit from F2008
    | wait_stmt                       // Inherit from F2003
    | flush_stmt                      // Inherit from F2003
    | open_stmt                       // I/O: OPEN (Section 12.5.6)
    | close_stmt                      // I/O: CLOSE (Section 12.5.7)
    | write_stmt                      // I/O: WRITE (Section 12.6.2)
    | read_stmt                       // I/O: READ (Section 12.6.1)
    | inquire_stmt                    // I/O: INQUIRE (Section 12.10.3)
    | if_construct                    // Inherit from F95
    | do_construct_f2018              // Enhanced in F2018 (R1119-R1132, DO CONCURRENT)
    | select_case_construct           // Inherit from F95
    | executable_construct_f2008      // Inherit F2008 constructs
    ;

// ============================================================================
// ASSIGNMENT OVERRIDE (LFortran Extensions)
// ============================================================================
// Extend assignment syntax with walrus declaration operator used by infer mode.
// This mirrors LFortran parser behavior where walrus support is syntax-level
// and semantic restrictions are enforced later in compilation.

assignment_stmt
    : lhs_expression EQUALS expr_f2003
    | lhs_expression POINTER_ASSIGN primary
    | lhs_expression COLON_EQUAL expr_f2003
    ;

// ============================================================================
// EXPRESSION OVERRIDE (LFortran Extensions)
// ============================================================================
// Override to include inline instantiated function calls in expressions

primary
    : inline_instantiated_function_ref   // J3 Generics: name{type}(args)
    | complex_literal_constant           // Legacy and modern complex literal syntax
    | designator
    | complex_part_designator
    | type_param_inquiry
    | identifier_or_keyword (PERCENT identifier_or_keyword)*
    | identifier_or_keyword LPAREN actual_arg_list? RPAREN
    | identifier_or_keyword DOUBLE_QUOTE_STRING
    | identifier_or_keyword SINGLE_QUOTE_STRING
    | intrinsic_function_call_f2018
    | ieee_constant
    | INTEGER_LITERAL
    | LABEL
    | REAL_LITERAL
    | SINGLE_QUOTE_STRING
    | DOUBLE_QUOTE_STRING
    | '*'
    | array_constructor
    | LPAREN primary RPAREN
    ;

// Complex literal constant:
//   (real-part, imag-part)
complex_literal_constant
    : LPAREN expr_f2003 COMMA expr_f2003 RPAREN
    ;
