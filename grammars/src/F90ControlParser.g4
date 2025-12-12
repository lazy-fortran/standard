// Fortran 90 Control Structures
// Reference: ISO/IEC 1539:1991 Section 7.5.3 (WHERE) and Section 8 (Execution Control)
// Delegate grammar for SELECT CASE, WHERE, DO, and IF constructs
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90ControlParser;

// ====================================================================
// SELECT CASE CONSTRUCT - ISO/IEC 1539:1991 Section 8.1.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 8.1.3 defines the CASE construct:
// - R808 (case-construct) -> select-case-stmt [case-stmt block]...
//                            end-select-stmt
// - R809 (select-case-stmt) -> [case-construct-name :] SELECT CASE (case-expr)
// - R811 (case-stmt) -> CASE case-selector [case-construct-name]
// - R812 (case-selector) -> (case-value-range-list) | DEFAULT
// - R813 (end-select-stmt) -> END SELECT [case-construct-name]

// SELECT CASE construct - ISO/IEC 1539:1991 Section 8.1.3, R808
select_case_construct
    : select_case_stmt NEWLINE (NEWLINE* case_construct)* NEWLINE* end_select_stmt
    ;

// SELECT CASE statement - ISO/IEC 1539:1991 Section 8.1.3, R809
select_case_stmt
    : (IDENTIFIER COLON)? SELECT CASE LPAREN expr_f90 RPAREN
    ;

// Case body - ISO/IEC 1539:1991 Section 8.1.3
case_construct
    : case_stmt NEWLINE execution_part?
    ;

// CASE statement - ISO/IEC 1539:1991 Section 8.1.3.2, R811
case_stmt
    : CASE case_selector (IDENTIFIER)?
    ;

// Case selector - ISO/IEC 1539:1991 Section 8.1.3.2, R812
case_selector
    : LPAREN case_value_range_list RPAREN
    | DEFAULT
    ;

// Case value range list - ISO/IEC 1539:1991 Section 8.1.3.2
case_value_range_list
    : case_value_range (COMMA case_value_range)*
    ;

// Case value range - ISO/IEC 1539:1991 Section 8.1.3.2, R814
case_value_range
    : expr_f90                      // Single value
    | expr_f90 COLON                // Lower bound only
    | COLON expr_f90                // Upper bound only
    | expr_f90 COLON expr_f90       // Range
    ;

// END SELECT statement - ISO/IEC 1539:1991 Section 8.1.3, R813
end_select_stmt
    : END_SELECT (IDENTIFIER)? NEWLINE?
    ;

// ====================================================================
// WHERE CONSTRUCT - ISO/IEC 1539:1991 Section 7.5.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 7.5.3 defines the WHERE construct:
// - R738 (where-construct) -> where-construct-stmt [where-body-construct]...
//                             [elsewhere-stmt [where-body-construct]...]...
//                             end-where-stmt
// - R739 (where-construct-stmt) -> [where-construct-name :]
//                                  WHERE (mask-expr)
// - R741 (elsewhere-stmt) -> ELSEWHERE [(mask-expr)] [where-construct-name]
// - R743 (end-where-stmt) -> END WHERE [where-construct-name]
//
// WHERE provides array assignment with masking conditions.

// WHERE construct - ISO/IEC 1539:1991 Section 7.5.3, R738
where_construct
    : where_construct_stmt execution_part?
      (elsewhere_stmt execution_part?)* end_where_stmt
    ;

// WHERE construct statement - ISO/IEC 1539:1991 Section 7.5.3, R739
where_construct_stmt
    : (IDENTIFIER COLON)? WHERE LPAREN logical_expr_f90 RPAREN
    ;

// ELSEWHERE statement - ISO/IEC 1539:1991 Section 7.5.3, R741
elsewhere_stmt
    : ELSEWHERE (LPAREN logical_expr_f90 RPAREN)? (IDENTIFIER)?
    ;

// END WHERE statement - ISO/IEC 1539:1991 Section 7.5.3, R743
end_where_stmt
    : END_WHERE (IDENTIFIER)?
    ;

// Logical expression (mask-expr) - ISO/IEC 1539:1991 Section 7.5.3
logical_expr_f90
    : expr_f90                      // Must be logical array expression
    ;

// WHERE statement - ISO/IEC 1539:1991 Section 7.5.3, R737
where_stmt
    : WHERE LPAREN logical_expr_f90 RPAREN assignment_stmt_f90
    ;

// ====================================================================
// DO CONSTRUCT - ISO/IEC 1539:1991 Section 8.1.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 8.1.4 defines the DO construct:
// - R816 (do-construct) -> do-stmt do-block end-do
// - R817 (do-stmt) -> [do-construct-name :] DO [label] [loop-control]
// - R820 (loop-control) -> [,] do-variable = scalar-int-expr,
//                          scalar-int-expr [, scalar-int-expr]
//                        | [,] WHILE (scalar-logical-expr)
// - R823 (end-do) -> end-do-stmt | do-terminal-stmt

// DO construct - ISO/IEC 1539:1991 Section 8.1.4, R816
do_construct_f90
    : do_stmt_f90 execution_part? end_do
    ;

// DO statement - ISO/IEC 1539:1991 Section 8.1.4.1, R817
do_stmt_f90
    : (IDENTIFIER COLON)? DO (label_f90)? (loop_control)?
    ;

// End-do rule - ISO/IEC 1539:1991 Section 8.1.4.2, R823
// Supports both F90-style END DO and F77-style terminal statements
end_do
    : end_do_stmt
    | do_terminal_stmt
    ;

// DO terminal statement - ISO/IEC 1539:1991 Section 8.1.4.3, R831-R832
// For F77 compatibility: label-DO uses a labeled statement as the construct end
// The terminal statement is an action statement with a label that terminates a label-DO
do_terminal_stmt
    : label_f90 CONTINUE
    ;

// Loop control - ISO/IEC 1539:1991 Section 8.1.4.1.1, R820
loop_control
    : (COMMA)? variable_f90 EQUALS expr_f90 COMMA expr_f90 (COMMA expr_f90)?
        // Counted loop (Section 8.1.4.1.1)
    | (COMMA)? WHILE LPAREN logical_expr_f90 RPAREN
        // WHILE loop (Section 8.1.4.1.1)
    ;

// END DO statement - ISO/IEC 1539:1991 Section 8.1.4.2, R824
end_do_stmt
    : END DO (IDENTIFIER)?
    ;

// ====================================================================
// LOOP CONTROL STATEMENTS - ISO/IEC 1539:1991 Section 8.1.4.4
// ====================================================================

// CYCLE statement - ISO/IEC 1539:1991 Section 8.1.4.4.1, R834
cycle_stmt
    : CYCLE (IDENTIFIER)?
    ;

// EXIT statement - ISO/IEC 1539:1991 Section 8.1.4.4.2, R835
exit_stmt
    : EXIT (IDENTIFIER)?
    ;

// ====================================================================
// IF STATEMENT AND CONSTRUCT - ISO/IEC 1539:1991 Section 8.1.1
// ====================================================================
//
// ISO/IEC 1539:1991 Section 8.1.1 defines two forms of IF:
// 1. IF statement (R807) - Inline form with single action statement
//    - R807 (if-stmt) -> IF (scalar-logical-expr) action-stmt
// 2. IF construct (R802-R806) - Block form with IF-THEN-END IF
//    - R802 (if-construct) -> if-then-stmt block [else-if-stmt block]...
//                             [else-stmt block] end-if-stmt
//    - R803 (if-then-stmt) -> [if-construct-name :]
//                             IF (scalar-logical-expr) THEN
//    - R804 (else-if-stmt) -> ELSE IF (scalar-logical-expr) THEN [if-construct-name]
//    - R805 (else-stmt) -> ELSE [if-construct-name]
//    - R806 (end-if-stmt) -> END IF [if-construct-name]

// IF statement - ISO/IEC 1539:1991 Section 8.1.1, R807
// This is the inline form: IF (condition) action-stmt
// NOTE: We use action_stmt_f90_non_control to avoid left-recursion.
// The action statement after IF cannot be another IF statement or construct.
// This prevents ambiguity and performance issues from backtracking.
if_stmt_f90
    : IF LPAREN expr_f90 RPAREN action_stmt_f90_non_control
    ;

// Action statements that can appear after inline IF (excluding control structures)
// ISO/IEC 1539:1991 Section 2.3.3, R215 (restricted form)
// Excludes if_stmt_f90, constructs, and do-terminal-stmt to prevent left-recursion
action_stmt_f90_non_control
    : cycle_stmt                     // Section 8.1.4.4.1 - F90 loop control (common)
    | exit_stmt                      // Section 8.1.4.4.2 - F90 loop control (common)
    | assignment_stmt_f90            // Section 7.5.1 - Variable assignments (common)
    | call_stmt_f90                  // Section 12.4 - Enhanced procedure calls
    | return_stmt                    // Section 12.4.3
    | stop_stmt                      // Section 8.3
    | continue_stmt                  // Section 8.2
    | goto_stmt                      // Section 8.2
    | arithmetic_if_stmt             // Section 8.1.1.2 (obsolescent)
    | read_stmt_f90                  // Section 9.4 - Enhanced I/O
    | write_stmt_f90                 // Section 9.4 - Enhanced I/O
    | print_stmt_f90                 // Section 9.4 - F77-inherited print
    | allocate_stmt                  // Section 6.3.1 - F90 memory management
    | deallocate_stmt                // Section 6.3.3 - F90 memory management
    | nullify_stmt                   // Section 6.3.2 - F90 pointer nullification
    | where_stmt                     // Section 7.5.3 - F90 array conditional
    | pointer_assignment_stmt        // Section 7.5.2 - F90 pointer assignment
    | open_stmt_f90                  // Section 9.7.1 - File connection (R904)
    | close_stmt_f90                 // Section 9.7.2 - File disconnection (R908)
    | inquire_stmt_f90               // Section 9.7.3 - File inquiry (R929)
    | backspace_stmt_f90             // Section 9.6.1 - File positioning (R923)
    | endfile_stmt_f90               // Section 9.6.2 - File positioning (R924)
    | rewind_stmt_f90                // Section 9.6.3 - File positioning (R925)
    | entry_stmt_f90                 // Section 12.5.2.4 - F90 ENTRY statement
    ;

// IF construct - ISO/IEC 1539:1991 Section 8.1.1, R802
// NOTE: Uses _f90 suffix to avoid collision with F77 if_then_stmt which
// uses logical_expr (no F90 symbolic operators like <= >= == /=).
// F90 version uses expr_f90 which supports both .LE. and <= forms.
if_construct
    : if_then_stmt_f90 ENDIF (IDENTIFIER)? // Empty IF with ENDIF
    | if_then_stmt_f90 END IF (IDENTIFIER)? // Empty IF with END IF
    | if_then_stmt_f90 execution_part (else_if_stmt_f90 execution_part?)*
      (else_stmt_f90 execution_part?)? end_if_stmt_f90 // IF with body
    | if_then_stmt_f90 (else_if_stmt_f90 execution_part?)*
      (else_stmt_f90 execution_part?)? end_if_stmt_f90 // IF with else/elseif
    ;

// IF-THEN statement - ISO/IEC 1539:1991 Section 8.1.1.1, R803
// Renamed to _f90 to use expr_f90 (supports <= >= == /=) instead of F77 logical_expr
if_then_stmt_f90
    : (IDENTIFIER COLON)? IF LPAREN expr_f90 RPAREN THEN (IDENTIFIER)?
    ;

// ELSE IF statement - ISO/IEC 1539:1991 Section 8.1.1.2, R804
// Renamed to _f90 to use expr_f90 (supports <= >= == /=) instead of F77 logical_expr
else_if_stmt_f90
    : ELSE IF LPAREN expr_f90 RPAREN THEN (IDENTIFIER)?
    ;

// ELSE statement - ISO/IEC 1539:1991 Section 8.1.1.2, R805
// Renamed to _f90 for consistency with other F90 IF construct rules
else_stmt_f90
    : ELSE (IDENTIFIER)?
    ;

// END IF statement - ISO/IEC 1539:1991 Section 8.1.1.2, R806
// Renamed to _f90 for consistency with other F90 IF construct rules
end_if_stmt_f90
    : END IF (IDENTIFIER)?
    | ENDIF (IDENTIFIER)?
    ;

// ====================================================================
// INHERITED CONTROL STATEMENTS - ISO/IEC 1539:1991 Section 8
// ====================================================================
//
// From FORTRAN 77 and earlier - maintained for F90 compatibility.
// Some are marked obsolescent in ISO/IEC 1539:1991 Annex B.1.

// Arithmetic IF statement - ISO/IEC 1539:1991 Section 8.1.1.2 (obsolescent)
arithmetic_if_stmt
    : IF LPAREN expr_f90 RPAREN label COMMA label COMMA label
    ;

// CONTINUE statement - ISO/IEC 1539:1991 Section 8.2
continue_stmt
    : CONTINUE
    ;

// GOTO statement - ISO/IEC 1539:1991 Section 8.2
goto_stmt
    : go_to_keyword label
    ;

// GO TO keyword accepts both the historic two-word form and the modern alias
go_to_keyword
    : GOTO
    | GO TO
    ;

// STOP statement - ISO/IEC 1539:1991 Section 8.3, R841
stop_stmt
    : STOP (expr_f90)?
    ;

// RETURN statement - ISO/IEC 1539:1991 Section 12.4.3
return_stmt
    : RETURN (expr_f90)?
    ;
