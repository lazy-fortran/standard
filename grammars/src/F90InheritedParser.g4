// Fortran 90 Inherited/Compatibility Constructs
// Delegate grammar for F77 compatibility rules
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90InheritedParser;

// ====================================================================
// INHERITED CONSTRUCTS (from FORTRAN 77 Parser)
// ====================================================================
// These constructs are inherited from FORTRAN 77 and earlier standards, but
// Fortran 90 widens several specification statements (notably by permitting
// full F90 expressions/array specs and optional `::` in free form). Because
// descendant grammars do not override `expr`, `variable`, or `dimension_stmt`,
// we must re-export Fortran 90–correct forms here to maintain feature
// completeness and historical accuracy.

// ====================================================================
// PARAMETER AND DATA STATEMENTS (F90-widened)
// ====================================================================

parameter_stmt
    : PARAMETER LPAREN parameter_list RPAREN
    ;

parameter_list
    : parameter_assignment (COMMA parameter_assignment)*
    ;

parameter_assignment
    : IDENTIFIER EQUALS expr_f90
    ;

data_stmt
    : DATA data_stmt_set (COMMA data_stmt_set)*
    ;

data_stmt_set
    : data_stmt_object_list SLASH data_stmt_value_list SLASH
    ;

data_stmt_object_list
    : data_stmt_object (COMMA data_stmt_object)*
    ;

data_stmt_object
    : variable_f90          // Simple variable or array element
    | data_implied_do_f90   // Implied-DO list for DATA
    ;

data_stmt_value_list
    : data_stmt_value (COMMA data_stmt_value)*
    ;

data_stmt_value
    : expr_f90
    ;

// Data implied-DO list for DATA statements - ISO/IEC 1539:1991 R534
// Syntax: (data_stmt_object_list, integer_var = expr, expr [, expr])
// Used to initialize ranges of array elements, supports nested implied-DO
data_implied_do_f90
    : LPAREN data_stmt_object_list COMMA IDENTIFIER EQUALS expr_f90 COMMA
      expr_f90 (COMMA expr_f90)? RPAREN
    ;

// ====================================================================
// COMMON AND EQUIVALENCE STATEMENTS (F90-compatible forms)
// ====================================================================

common_stmt
    : COMMON (common_block_name)? common_block_object_list
      (COMMA common_block_name common_block_object_list)*
    ;

common_block_name
    : SLASH IDENTIFIER SLASH
    | SLASH SLASH
    ;

common_block_object_list
    : common_block_object (COMMA common_block_object)*
    ;

common_block_object
    : variable_name (LPAREN array_spec_f90 RPAREN)?
    ;

variable_name
    : IDENTIFIER
    ;

equivalence_stmt
    : EQUIVALENCE equivalence_set_list
    ;

equivalence_set_list
    : equivalence_set (COMMA equivalence_set)*
    ;

equivalence_set
    : LPAREN equivalence_object_list RPAREN
    ;

equivalence_object_list
    : equivalence_object (COMMA equivalence_object)*
    ;

equivalence_object
    : variable_f90
    ;

// ====================================================================
// DIMENSION AND ATTRIBUTE STATEMENTS (F90-widened)
// ====================================================================

dimension_stmt
    : DIMENSION (DOUBLE_COLON)? array_declarator_list
    ;

array_declarator_list
    : array_declarator (COMMA array_declarator)*
    ;

array_declarator
    : IDENTIFIER LPAREN array_spec_f90 RPAREN
    ;

save_stmt
    : SAVE
    | SAVE saved_entity_list
    | SAVE DOUBLE_COLON saved_entity_list
    ;

saved_entity_list
    : saved_entity (COMMA saved_entity)*
    ;

saved_entity
    : IDENTIFIER
    | SLASH IDENTIFIER SLASH
    ;

external_stmt
    : EXTERNAL external_name_list
    | EXTERNAL DOUBLE_COLON external_name_list
    ;

external_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

intrinsic_stmt
    : INTRINSIC intrinsic_name_list
    | INTRINSIC DOUBLE_COLON intrinsic_name_list
    ;

intrinsic_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ====================================================================
// F90 ATTRIBUTE STATEMENTS
// ====================================================================

allocatable_stmt
    : ALLOCATABLE (DOUBLE_COLON)? allocatable_decl_list
    ;

pointer_stmt
    : POINTER (DOUBLE_COLON)? pointer_decl_list
    ;

target_stmt
    : TARGET (DOUBLE_COLON)? target_decl_list
    ;

optional_stmt
    : OPTIONAL (DOUBLE_COLON)? IDENTIFIER (COMMA IDENTIFIER)*
    ;

intent_stmt
    : INTENT LPAREN intent_spec RPAREN (DOUBLE_COLON)? IDENTIFIER (COMMA IDENTIFIER)*
    ;

public_stmt
    : PUBLIC (DOUBLE_COLON access_id_list)? NEWLINE?
    ;

private_stmt
    : PRIVATE (DOUBLE_COLON access_id_list)? NEWLINE?
    ;

access_id_list
    : access_id (COMMA access_id)*
    ;

access_id
    : IDENTIFIER
    | generic_spec
    ;

allocatable_decl_list
    : allocatable_decl (COMMA allocatable_decl)*
    ;

allocatable_decl
    : IDENTIFIER (LPAREN deferred_shape_spec_list RPAREN)?
    ;

pointer_decl_list
    : pointer_decl (COMMA pointer_decl)*
    ;

pointer_decl
    : IDENTIFIER (LPAREN deferred_shape_spec_list RPAREN)?
    ;

target_decl_list
    : target_decl (COMMA target_decl)*
    ;

target_decl
    : IDENTIFIER (LPAREN array_spec_f90 RPAREN)?
    ;

// ====================================================================
// IMPLICIT STATEMENT (F90)
// ====================================================================
// ISO/IEC 1539:1991 Section 5.2 defines two forms:
//   IMPLICIT NONE
//   IMPLICIT implicit-spec-list
// where implicit-spec is: type-spec (letter-spec-list)
// and letter-spec is: letter or letter-letter (e.g., A-H).
//
// Fortran 90 extends the F77 IMPLICIT with:
// - IMPLICIT NONE (disables implicit typing for a scoping unit)
// - KIND selectors in type specifications
//
// Examples:
//   IMPLICIT NONE
//   IMPLICIT INTEGER (I-N), REAL (A-H, O-Z)
//   IMPLICIT INTEGER(KIND=4) (I-N)
//   IMPLICIT COMPLEX(8) (Z)

implicit_stmt_f90
    : IMPLICIT NONE NEWLINE?
    | IMPLICIT implicit_spec_list_f90 NEWLINE?
    ;

implicit_spec_list_f90
    : implicit_spec_f90 (COMMA implicit_spec_f90)*
    ;

implicit_spec_f90
    : intrinsic_type_spec_f90 LPAREN letter_spec_list_f90 RPAREN
    ;

letter_spec_list_f90
    : letter_spec_f90 (COMMA letter_spec_f90)*
    ;

// Letter specification: single letter or letter range (A-Z)
// ISO/IEC 1539:1991 Section 5.2
letter_spec_f90
    : IDENTIFIER                         // Single letter (e.g., A)
    | IDENTIFIER MINUS IDENTIFIER        // Letter range (e.g., A-H)
    ;
