// Fortran 90 Inherited/Compatibility Constructs
// Delegate grammar for F77 compatibility rules
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90InheritedParser;

// ====================================================================
// INHERITED CONSTRUCTS (from FORTRAN 77 Parser)
// ====================================================================
// FORTRAN 77 specification statements (PARAMETER, DATA, COMMON, EQUIVALENCE,
// DIMENSION, SAVE, EXTERNAL, INTRINSIC) are imported by Fortran90Parser.g4
// via FORTRAN77Parser and should not be redefined here. This delegate file
// only defines constructs that are NEW or extended in Fortran 90.

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
