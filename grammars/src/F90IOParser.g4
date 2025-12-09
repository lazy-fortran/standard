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
    : UNIT EQUALS expr_f90          // Unit specification
    | FMT EQUALS format_spec        // Format specification
    | IOSTAT EQUALS variable_f90    // I/O status
    | ERR EQUALS label              // Error handling
    | END EQUALS label              // End-of-file handling
    | EOR EQUALS label              // End-of-record handling (F90)
    | ADVANCE EQUALS expr_f90       // Non-advancing I/O (F90)
    | SIZE EQUALS variable_f90      // Characters transferred (F90)
    | REC EQUALS expr_f90           // Record number (direct access)
    | expr_f90                      // Positional unit
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

label
    : INTEGER_LITERAL
    ;
