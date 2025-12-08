// Fortran 90 Procedures and Subprograms
// Reference: ISO/IEC 1539:1991 Section 12 (Procedures)
// Delegate grammar for functions, subroutines, and procedure calls
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90ProcsParser;

// ====================================================================
// PROCEDURES - ISO/IEC 1539:1991 Section 12
// ====================================================================
//
// ISO/IEC 1539:1991 Section 12 defines procedures:
// - Section 12.5.2: Function subprograms (R1213-R1218)
// - Section 12.5.3: Subroutine subprograms (R1219-R1222)
// - Section 12.4: Procedure references (R1208-R1212)
// - R1214 (prefix) -> prefix-spec [prefix-spec]...
// - R1215 (prefix-spec) -> type-spec | RECURSIVE
// - R1216 (suffix) -> RESULT (result-name)
//
// Procedures with RECURSIVE prefix and RESULT clause.

// Function statement (F90 enhancements)
// ISO/IEC 1539:1991 Section 12.5.2
function_stmt
    : (prefix)? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN (suffix)?
        NEWLINE?
    ;

// Subroutine statement (F90 enhancements)
// ISO/IEC 1539:1991 Section 12.5.3
subroutine_stmt
    : (prefix)? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? NEWLINE?
    ;

// Procedure prefix (F90 enhancements)
// ISO/IEC 1539:1991 Section 12.5.2
prefix
    : prefix_spec+
    ;

prefix_spec
    : RECURSIVE                     // F90 recursive procedures
    | type_spec_f90                 // Function return type
    // NOTE: PURE and ELEMENTAL are Fortran 95 features, not F90.
    // They are defined in Fortran95Parser.g4 prefix_spec_f95.
    ;

// Function suffix (F90 RESULT clause)
// ISO/IEC 1539:1991 Section 12.5.2.2
suffix
    : RESULT LPAREN identifier_or_keyword RPAREN
    ;

dummy_arg_name_list
    : identifier_or_keyword (COMMA identifier_or_keyword)*
    ;

// Enhanced procedure call (F90 keyword arguments)
// ISO/IEC 1539:1991 Section 12.4
call_stmt_f90
    : CALL procedure_designator (LPAREN actual_arg_spec_list? RPAREN)?
    ;

procedure_designator
    : IDENTIFIER
    | variable_f90                  // F90 procedure pointers
    ;

// Procedure statement (in interface blocks)
procedure_stmt
    : PROCEDURE
    ;

// ====================================================================
// SUBPROGRAM STRUCTURES (F90)
// ====================================================================
// ISO/IEC 1539:1991 Section 12.5

// External subprogram (enhanced for F90)
external_subprogram
    : function_subprogram
    | subroutine_subprogram
    | module                        // Modules can be external
    ;

function_subprogram
    : function_stmt specification_part? execution_part?
      internal_subprogram_part? end_function_stmt
    ;

subroutine_subprogram
    : subroutine_stmt specification_part? execution_part?
      internal_subprogram_part? end_subroutine_stmt
    ;

end_function_stmt
    : END (FUNCTION (IDENTIFIER)?)? NEWLINE?
    ;

end_subroutine_stmt
    : END (SUBROUTINE (IDENTIFIER)?)? NEWLINE?
    ;

// Internal subprogram part (F90 feature)
// ISO/IEC 1539:1991 Section 12.5.1
internal_subprogram_part
    : contains_stmt NEWLINE* (internal_subprogram NEWLINE*)*
    ;

internal_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;
