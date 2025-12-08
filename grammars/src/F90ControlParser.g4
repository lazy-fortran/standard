// Fortran 90 Control Structures
// Delegate grammar for SELECT CASE, WHERE, DO, and IF constructs
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90ControlParser;

// ====================================================================
// SELECT CASE CONSTRUCT (F90 MAJOR INNOVATION)
// ====================================================================
// ISO/IEC 1539:1991 Section 8.1.3
// Multi-way branching based on expression value.

// SELECT CASE construct (F90 major innovation)
select_case_construct
    : select_case_stmt NEWLINE (NEWLINE* case_construct)* NEWLINE* end_select_stmt
    ;

select_case_stmt
    : (IDENTIFIER COLON)? SELECT CASE LPAREN expr_f90 RPAREN
    ;

case_construct
    : case_stmt NEWLINE execution_part?
    ;

case_stmt
    : CASE case_selector (IDENTIFIER)?
    ;

case_selector
    : LPAREN case_value_range_list RPAREN
    | DEFAULT
    ;

case_value_range_list
    : case_value_range (COMMA case_value_range)*
    ;

case_value_range
    : expr_f90                      // Single value
    | expr_f90 COLON                // Lower bound only
    | COLON expr_f90                // Upper bound only
    | expr_f90 COLON expr_f90       // Range
    ;

end_select_stmt
    : END_SELECT (IDENTIFIER)? NEWLINE?
    ;

// ====================================================================
// WHERE CONSTRUCT (F90 ARRAY-ORIENTED CONDITIONAL)
// ====================================================================
// ISO/IEC 1539:1991 Section 7.5.3
// Array assignment with masking conditions.

// WHERE construct (F90 array-oriented conditional)
where_construct
    : where_construct_stmt execution_part?
      (elsewhere_stmt execution_part?)* end_where_stmt
    ;

where_construct_stmt
    : (IDENTIFIER COLON)? WHERE LPAREN logical_expr_f90 RPAREN
    ;

elsewhere_stmt
    : ELSEWHERE (LPAREN logical_expr_f90 RPAREN)? (IDENTIFIER)?
    ;

end_where_stmt
    : END_WHERE (IDENTIFIER)?
    ;

logical_expr_f90
    : expr_f90                      // Must be logical array expression
    ;

// Simple WHERE statement (F90 array conditional)
where_stmt
    : WHERE LPAREN logical_expr_f90 RPAREN assignment_stmt_f90
    ;

// ====================================================================
// ENHANCED DO CONSTRUCT (F90 IMPROVEMENTS)
// ====================================================================
// ISO/IEC 1539:1991 Section 8.1.4
// Loop construct with optional WHILE clause.

// Enhanced DO construct (F90 improvements)
do_construct_f90
    : do_stmt_f90 execution_part? end_do_stmt
    ;

do_stmt_f90
    : (IDENTIFIER COLON)? DO (loop_control)?
    ;

loop_control
    : (COMMA)? variable_f90 EQUALS expr_f90 COMMA expr_f90 (COMMA expr_f90)?
        // Counted loop
    | (COMMA)? WHILE LPAREN logical_expr_f90 RPAREN
        // WHILE loop
    ;

end_do_stmt
    : END DO (IDENTIFIER)?
    ;

// Enhanced loop control statements (F90)
cycle_stmt
    : CYCLE (IDENTIFIER)?
    ;

exit_stmt
    : EXIT (IDENTIFIER)?
    ;

// ====================================================================
// IF CONSTRUCT (F90 ENHANCED)
// ====================================================================
// ISO/IEC 1539:1991 Section 8.1.1
// Conditional execution with optional named constructs.

if_construct
    : if_then_stmt ENDIF (IDENTIFIER)? // Empty IF with ENDIF
    | if_then_stmt END IF (IDENTIFIER)? // Empty IF with END IF
    | if_then_stmt execution_part (else_if_stmt execution_part?)*
      (else_stmt execution_part?)? end_if_stmt // IF with body
    | if_then_stmt (else_if_stmt execution_part?)*
      (else_stmt execution_part?)? end_if_stmt // IF with else/elseif
    ;

if_then_stmt
    : (IDENTIFIER COLON)? IF LPAREN expr_f90 RPAREN THEN (IDENTIFIER)?
    ;

else_if_stmt
    : ELSE IF LPAREN expr_f90 RPAREN THEN (IDENTIFIER)?
    ;

else_stmt
    : ELSE (IDENTIFIER)?
    ;

end_if_stmt
    : END IF (IDENTIFIER)?
    | ENDIF (IDENTIFIER)?
    ;

// ====================================================================
// INHERITED CONTROL STATEMENTS
// ====================================================================
// From FORTRAN 77 and earlier - maintained for compatibility.

arithmetic_if_stmt
    : IF LPAREN expr_f90 RPAREN label COMMA label COMMA label
    ;

continue_stmt
    : CONTINUE
    ;

goto_stmt
    : GOTO label
    ;

stop_stmt
    : STOP (expr_f90)?
    ;

return_stmt
    : RETURN (expr_f90)?
    ;
