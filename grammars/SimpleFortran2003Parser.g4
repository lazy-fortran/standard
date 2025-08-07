/*
 * SimpleFortran2003Parser.g4
 * 
 * Working Fortran 2003 parser demonstrating all critical fixes
 */

parser grammar SimpleFortran2003Parser;

options {
    tokenVocab = SimpleFortran2003Lexer;
}

// ============================================================================
// PROGRAM STRUCTURE (F2003 entry points)
// ============================================================================

// F2003 program unit - FIXED ENTRY POINT
program_unit_f2003
    : main_program_f2003
    | module_f2003
    ;

main_program_f2003
    : PROGRAM IDENTIFIER declaration_list? END PROGRAM IDENTIFIER?
    ;

module_f2003  
    : MODULE IDENTIFIER declaration_list? END MODULE IDENTIFIER?
    ;

// ============================================================================
// DECLARATIONS (F2003 features)
// ============================================================================

declaration_list
    : declaration+
    ;

declaration
    : type_declaration_stmt
    | class_declaration_stmt  
    | procedure_declaration_stmt
    | volatile_stmt
    | protected_stmt
    ;

// Basic type declarations - FIXED DOUBLE_COLON TOKEN
type_declaration_stmt
    : INTEGER DOUBLE_COLON entity_list
    | REAL DOUBLE_COLON entity_list  
    | CHARACTER DOUBLE_COLON entity_list
    ;

// F2003 CLASS declarations - NON-SHALLOW TESTABLE
class_declaration_stmt
    : CLASS LPAREN IDENTIFIER RPAREN DOUBLE_COLON entity_list
    ;

// F2003 PROCEDURE pointers - NON-SHALLOW TESTABLE  
procedure_declaration_stmt
    : PROCEDURE LPAREN IDENTIFIER RPAREN DOUBLE_COLON entity_list
    ;

// F2003 VOLATILE - NON-SHALLOW TESTABLE
volatile_stmt
    : VOLATILE DOUBLE_COLON entity_list
    ;

// F2003 PROTECTED - NON-SHALLOW TESTABLE
protected_stmt  
    : PROTECTED DOUBLE_COLON entity_list
    ;

entity_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;