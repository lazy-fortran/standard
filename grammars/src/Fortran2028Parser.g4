/*
 * Fortran2028Parser.g4
 *
 * Fortran 2028 Working Draft parser overlay.
 * Base: Fortran 2023 parser
 * Source reference: J3 standing document 26-007 (Fortran 2028 Working Draft)
 *
 * This grammar adds the F2028 template facility syntax family while
 * preserving all F2023 syntax through inheritance.
 */

parser grammar Fortran2028Parser;

import Fortran2023Parser;

options {
    tokenVocab = Fortran2028Lexer;
}

// ============================================================================
// FORTRAN 2028 PROGRAM STRUCTURE
// ============================================================================

program_unit_f2028
    : NEWLINE* (main_program_f2028
      | module_f2028
      | submodule_f2008
      | external_subprogram_f2028
      | template_construct_f2028
      | requirement_construct_f2028
      | templated_function_subprogram_f2028
      | templated_subroutine_subprogram_f2028
      | instantiate_stmt_f2028) NEWLINE*
    ;

main_program_f2028
    : main_program_f2023
    ;

module_f2028
    : module_f2023
    ;

external_subprogram_f2028
    : external_subprogram_f2023
    ;

// ============================================================================
// TEMPLATE CONSTRUCT (F2028 WD Clause 16)
// ============================================================================

template_construct_f2028
    : TEMPLATE_KW identifier_or_keyword LPAREN deferred_arg_name_list_f2028? RPAREN NEWLINE*
      template_specification_part_f2028?
      template_contains_part_f2028?
      END_TEMPLATE_STMT identifier_or_keyword? NEWLINE*
    ;

template_specification_part_f2028
    : template_specification_stmt_f2028+
    ;

template_specification_stmt_f2028
    : deferred_arg_decl_stmt_f2028
    | use_stmt
    | implicit_stmt
    | declaration_construct
    ;

template_contains_part_f2028
    : CONTAINS NEWLINE*
      template_subprogram_f2028+
    ;

template_subprogram_f2028
    : function_subprogram_f2018
    | subroutine_subprogram_f2018
    | templated_function_subprogram_f2028
    | templated_subroutine_subprogram_f2028
    ;

// ============================================================================
// TEMPLATED PROCEDURES (F2028 WD)
// ============================================================================

templated_function_subprogram_f2028
    : prefix? TEMPLATE_KW FUNCTION identifier_or_keyword LPAREN deferred_arg_name_list_f2028? RPAREN
      LPAREN dummy_arg_name_list? RPAREN suffix? NEWLINE*
      templated_subp_specification_part_f2028?
      execution_part?
      internal_subprogram_part_f2003?
      end_function_stmt
    ;

templated_subroutine_subprogram_f2028
    : prefix? TEMPLATE_KW SUBROUTINE identifier_or_keyword LPAREN deferred_arg_name_list_f2028? RPAREN
      LPAREN dummy_arg_name_list? RPAREN NEWLINE*
      templated_subp_specification_part_f2028?
      execution_part?
      internal_subprogram_part_f2003?
      end_subroutine_stmt
    ;

templated_subp_specification_part_f2028
    : templated_subprogram_declaration_f2028*
      specification_part?
    ;

templated_subprogram_declaration_f2028
    : deferred_arg_decl_stmt_f2028
    | require_stmt_f2028
    ;

// ============================================================================
// DEFERRED ARGUMENT DECLARATIONS (F2028 WD)
// ============================================================================

deferred_arg_decl_stmt_f2028
    : deferred_type_declaration_stmt_f2028
    | deferred_const_declaration_stmt_f2028
    | deferred_proc_declaration_stmt_f2028
    | require_stmt_f2028
    ;

deferred_type_declaration_stmt_f2028
    : TYPE COMMA DEFERRED_KW DOUBLE_COLON deferred_arg_name_list_f2028 NEWLINE*
    ;

deferred_const_declaration_stmt_f2028
    : type_spec_f2023 COMMA CONSTANT_KW DOUBLE_COLON deferred_arg_name_list_f2028 NEWLINE*
    ;

deferred_proc_declaration_stmt_f2028
    : PROCEDURE (LPAREN identifier_or_keyword RPAREN)? DOUBLE_COLON deferred_arg_name_list_f2028 NEWLINE*
    ;

deferred_arg_name_list_f2028
    : identifier_or_keyword (COMMA identifier_or_keyword)*
    ;

// ============================================================================
// REQUIREMENT CONSTRUCT + REQUIRE STATEMENT (F2028 WD)
// ============================================================================

requirement_construct_f2028
    : REQUIREMENT_KW identifier_or_keyword LBRACE deferred_arg_name_list_f2028? RBRACE NEWLINE*
      requirement_specification_part_f2028?
      END_REQUIREMENT_STMT identifier_or_keyword? NEWLINE*
    ;

requirement_specification_part_f2028
    : requirement_specification_stmt_f2028+
    ;

requirement_specification_stmt_f2028
    : deferred_arg_decl_stmt_f2028
    | implicit_stmt
    | declaration_construct
    | interface_block
    ;

require_stmt_f2028
    : REQUIRE_KW DOUBLE_COLON? requirement_reference_list_f2028 NEWLINE*
    ;

requirement_reference_list_f2028
    : requirement_reference_f2028 (COMMA requirement_reference_f2028)*
    ;

requirement_reference_f2028
    : identifier_or_keyword LBRACE instantiation_arg_spec_list_f2028? RBRACE
    ;

// ============================================================================
// INSTANTIATE STATEMENT (F2028 WD)
// ============================================================================

instantiate_stmt_f2028
    : INSTANTIATE_KW DOUBLE_COLON? identifier_or_keyword LBRACE instantiation_arg_spec_list_f2028? RBRACE
      instantiate_only_clause_f2028? NEWLINE*
    | INSTANTIATE_KW DOUBLE_COLON? identifier_or_keyword POINTER_ASSIGN templated_subp_name_f2028 NEWLINE*
    ;

templated_subp_name_f2028
    : identifier_or_keyword
    ;

instantiate_only_clause_f2028
    : COMMA ONLY COLON rename_list_f2028
    ;

rename_list_f2028
    : rename_f2028 (COMMA rename_f2028)*
    ;

rename_f2028
    : identifier_or_keyword POINTER_ASSIGN identifier_or_keyword
    | identifier_or_keyword
    ;

instantiation_arg_spec_list_f2028
    : instantiation_arg_spec_f2028 (COMMA instantiation_arg_spec_f2028)*
    ;

instantiation_arg_spec_f2028
    : (identifier_or_keyword EQUALS)? instantiation_arg_f2028
    ;

instantiation_arg_f2028
    : identifier_or_keyword
    | intrinsic_type_spec_f95
    | derived_type_spec
    | expr_f2003
    ;
