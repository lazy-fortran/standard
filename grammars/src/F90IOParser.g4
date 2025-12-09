// Fortran 90 Input/Output Statements
// Reference: ISO/IEC 1539:1991 Section 5.4 (NAMELIST) and Section 9 (I/O)
// Delegate grammar for READ, WRITE, PRINT, and NAMELIST
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90IOParser;

// ====================================================================
// NAMELIST STATEMENT - ISO/IEC 1539:1991 Section 5.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 5.4 defines the NAMELIST statement:
// - R527 (namelist-stmt) -> NAMELIST / namelist-group-name / namelist-group-object-list
//                           [[,] / namelist-group-name / namelist-group-object-list]...
// Named groups of variables for structured I/O.

// NAMELIST declaration (F90 structured I/O)
namelist_stmt
    : NAMELIST SLASH IDENTIFIER SLASH namelist_item_list
    ;

namelist_item_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ====================================================================
// ENHANCED READ/WRITE STATEMENTS (F90 IMPROVEMENTS)
// ====================================================================
// ISO/IEC 1539:1991 Section 9.4

// Enhanced READ statement (F90 improvements)
read_stmt_f90
    : READ LPAREN io_control_spec_list RPAREN (input_item_list)?
    | READ namelist_name             // NAMELIST read
    | READ format (COMMA input_item_list)?    // Traditional format
    ;

// Enhanced WRITE statement (F90 improvements)
write_stmt_f90
    : WRITE LPAREN io_control_spec_list RPAREN (output_item_list_f90)?
    | WRITE namelist_name            // NAMELIST write
    ;

// PRINT statement (F77 inherited)
print_stmt_f90
    : PRINT MULTIPLY (COMMA output_item_list_f90)? NEWLINE?   // PRINT *, list
    | PRINT format_spec (COMMA output_item_list_f90)? NEWLINE?  // PRINT format, list
    ;

// ====================================================================
// I/O CONTROL SPECIFICATIONS (F90 ENHANCEMENTS)
// ====================================================================
// ISO/IEC 1539:1991 Section 9.4.1

// I/O control specification (F90 enhancements)
io_control_spec_list
    : io_control_spec (COMMA io_control_spec)*
    ;

io_control_spec
    : UNIT EQUALS io_unit_f90       // Unit specification (R910)
    | FMT EQUALS format_spec        // Format specification (R910)
    | IOSTAT EQUALS variable_f90    // I/O status
    | ERR EQUALS label_f90          // Error handling
    | END EQUALS label_f90          // End-of-file handling
    | EOR EQUALS label_f90          // End-of-record handling (F90)
    | ADVANCE EQUALS expr_f90       // Non-advancing I/O (F90)
    | SIZE EQUALS variable_f90      // Characters transferred (F90)
    | REC EQUALS expr_f90           // Record number (direct access)
    | MULTIPLY                      // Positional * for io-unit or format (R901/R912)
    | expr_f90                      // Positional unit or format expression
    ;

// IO unit - ISO/IEC 1539:1991 Section 9.4, R901
// io-unit -> external-file-unit | * | internal-file-unit
io_unit_f90
    : expr_f90                      // external-file-unit or internal-file-unit
    | MULTIPLY                      // preconnected unit (stdout/stdin)
    ;

format_spec
    : expr_f90                      // Format expression
    | MULTIPLY                      // List-directed format
    | label                         // Format label
    | namelist_name                 // NAMELIST format (F90)
    ;

namelist_name
    : IDENTIFIER
    ;

format
    : label
    | MULTIPLY
    ;

// ====================================================================
// I/O ITEM LISTS
// ====================================================================
// ISO/IEC 1539:1991 Section 9.4.2

// Input/output item lists
input_item_list
    : input_item (COMMA input_item)*
    ;

input_item
    : variable_f90
    | io_implied_do_input
    ;

// Implied DO for input items - ISO/IEC 1539:1991 Section 9.4.2
io_implied_do_input
    : LPAREN input_item_list COMMA do_variable EQUALS expr_f90 COMMA expr_f90
      (COMMA expr_f90)? RPAREN
    ;

output_item_list_f90
    : output_item_f90 (COMMA output_item_f90)*
    ;

output_item_f90
    : expr_f90
    | io_implied_do_f90
    ;

io_implied_do_f90
    : LPAREN output_item_list_f90 COMMA do_variable EQUALS expr_f90 COMMA expr_f90
      (COMMA expr_f90)? RPAREN
    ;

do_variable
    : IDENTIFIER
    ;

label_f90
    : LABEL
    | INTEGER_LITERAL
    ;

// ====================================================================
// FILE CONNECTION/POSITIONING STATEMENTS - ISO/IEC 1539:1991 Section 9.7
// ====================================================================
//
// These F90 statements override the F77 versions to use F90 lexer tokens.
// ISO/IEC 1539:1991 Section 9.7.1: OPEN statement (R904)
// ISO/IEC 1539:1991 Section 9.7.2: CLOSE statement (R908)
// ISO/IEC 1539:1991 Section 9.7.3: INQUIRE statement (R929)
// ISO/IEC 1539:1991 Section 9.6.1: BACKSPACE statement (R923)
// ISO/IEC 1539:1991 Section 9.6.2: ENDFILE statement (R924)
// ISO/IEC 1539:1991 Section 9.6.3: REWIND statement (R925)

// OPEN statement - ISO/IEC 1539:1991 Section 9.7.1, R904
open_stmt_f90
    : OPEN LPAREN connect_spec_list_f90 RPAREN
    ;

// CLOSE statement - ISO/IEC 1539:1991 Section 9.7.2, R908
close_stmt_f90
    : CLOSE LPAREN close_spec_list_f90 RPAREN
    ;

// INQUIRE statement - ISO/IEC 1539:1991 Section 9.7.3, R929
inquire_stmt_f90
    : INQUIRE LPAREN inquire_spec_list_f90 RPAREN
    ;

// BACKSPACE statement - ISO/IEC 1539:1991 Section 9.6.1, R923
backspace_stmt_f90
    : BACKSPACE expr_f90
    | BACKSPACE LPAREN position_spec_list_f90 RPAREN
    ;

// ENDFILE statement - ISO/IEC 1539:1991 Section 9.6.2, R924
endfile_stmt_f90
    : ENDFILE expr_f90
    | ENDFILE LPAREN position_spec_list_f90 RPAREN
    ;

// REWIND statement - ISO/IEC 1539:1991 Section 9.6.3, R925
rewind_stmt_f90
    : REWIND expr_f90
    | REWIND LPAREN position_spec_list_f90 RPAREN
    ;

// ====================================================================
// F90 I/O SPECIFIER LISTS
// ====================================================================

// Connect specifier list - ISO/IEC 1539:1991 Section 9.7.1
connect_spec_list_f90
    : connect_spec_f90 (COMMA connect_spec_f90)*
    ;

// Connect specifier - ISO/IEC 1539:1991 Section 9.7.1, R905
connect_spec_f90
    : UNIT EQUALS expr_f90              // UNIT=u
    | FILE EQUALS expr_f90              // FILE=fn
    | STATUS EQUALS expr_f90            // STATUS=sta
    | ACCESS EQUALS expr_f90            // ACCESS=acc
    | FORM EQUALS expr_f90              // FORM=fm
    | RECL EQUALS expr_f90              // RECL=rl
    | BLANK EQUALS expr_f90             // BLANK=blnk
    | IOSTAT EQUALS variable_f90        // IOSTAT=ios
    | ERR EQUALS label_f90              // ERR=s
    | POSITION EQUALS expr_f90          // POSITION=pos (F90)
    | ACTION EQUALS expr_f90            // ACTION=act (F90)
    | DELIM EQUALS expr_f90             // DELIM=del (F90)
    | PAD EQUALS expr_f90               // PAD=pad (F90)
    | expr_f90                          // Positional unit number
    ;

// Close specifier list - ISO/IEC 1539:1991 Section 9.7.2
close_spec_list_f90
    : close_spec_f90 (COMMA close_spec_f90)*
    ;

// Close specifier - ISO/IEC 1539:1991 Section 9.7.2, R909
close_spec_f90
    : UNIT EQUALS expr_f90              // UNIT=u
    | STATUS EQUALS expr_f90            // STATUS=sta
    | IOSTAT EQUALS variable_f90        // IOSTAT=ios
    | ERR EQUALS label_f90              // ERR=s
    | expr_f90                          // Positional unit number
    ;

// Inquire specifier list - ISO/IEC 1539:1991 Section 9.7.3
inquire_spec_list_f90
    : inquire_spec_f90 (COMMA inquire_spec_f90)*
    ;

// Inquire specifier - ISO/IEC 1539:1991 Section 9.7.3, R930
// NOTE: Some specifiers (NAME, NAMED, NUMBER, FORMATTED, UNFORMATTED) use
// io_specifier_name which matches IDENTIFIER to avoid conflicts with other uses.
inquire_spec_f90
    : UNIT EQUALS expr_f90              // UNIT=u
    | FILE EQUALS expr_f90              // FILE=fn
    | IOSTAT EQUALS variable_f90        // IOSTAT=ios
    | ERR EQUALS label_f90              // ERR=s
    | EXIST EQUALS variable_f90         // EXIST=ex
    | OPENED EQUALS variable_f90        // OPENED=od
    | ACCESS EQUALS variable_f90        // ACCESS=acc
    | SEQUENTIAL EQUALS variable_f90    // SEQUENTIAL=seq
    | DIRECT EQUALS variable_f90        // DIRECT=dir
    | FORM EQUALS variable_f90          // FORM=fm
    | RECL EQUALS variable_f90          // RECL=rl
    | NEXTREC EQUALS variable_f90       // NEXTREC=nr
    | BLANK EQUALS variable_f90         // BLANK=blnk
    | POSITION EQUALS variable_f90      // POSITION=pos (F90)
    | ACTION EQUALS variable_f90        // ACTION=act (F90)
    | READ EQUALS variable_f90          // READ=rd (F90)
    | WRITE EQUALS variable_f90         // WRITE=wr (F90)
    | READWRITE EQUALS variable_f90     // READWRITE=rw (F90)
    | DELIM EQUALS variable_f90         // DELIM=del (F90)
    | PAD EQUALS variable_f90           // PAD=pad (F90)
    | io_specifier_name EQUALS variable_f90  // NAME, NAMED, etc.
    | expr_f90                          // Positional unit number
    ;

// I/O specifier names that match via IDENTIFIER to avoid conflicts
// These include: NAME, NAMED, NUMBER, FORMATTED, UNFORMATTED
io_specifier_name
    : IDENTIFIER
    ;

// File positioning specifier list - ISO/IEC 1539:1991 Section 9.6
position_spec_list_f90
    : position_spec_f90 (COMMA position_spec_f90)*
    ;

// File positioning specifier - ISO/IEC 1539:1991 Section 9.6
position_spec_f90
    : UNIT EQUALS expr_f90              // UNIT=u
    | IOSTAT EQUALS variable_f90        // IOSTAT=ios
    | ERR EQUALS label_f90              // ERR=s
    | expr_f90                          // Positional unit number
    ;
