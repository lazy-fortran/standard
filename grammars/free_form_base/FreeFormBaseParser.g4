// Free Form Base Parser - PURE FORMAT RULES ONLY
// This parser contains ONLY the formatting rules for free-form Fortran
// NO language features or constructs - just format handling
parser grammar FreeFormBaseParser;

import SharedCoreParser;  // Import universal constructs

options {
    tokenVocab = FreeFormBaseLexer;
}

// ====================================================================
// FREE FORM FORMAT RULES (1990+)
// ====================================================================
//
// This parser handles ONLY the structural aspects of free-form format:
// - Statement separation and continuation
// - Multiple statements per line
// - Comment handling
//
// All actual language constructs are defined in the importing grammars
// ====================================================================

// ====================================================================
// FREE FORM PROGRAM STRUCTURE
// ====================================================================

// Free-form program (sequence of lines)
// The actual parsing of statements is done by importing grammars
free_form_program
    : free_form_line* EOF
    ;

// A free-form source line
free_form_line
    : statement_sequence? NEWLINE
    | NEWLINE  // Blank line
    ;

// Multiple statements separated by semicolons
statement_sequence
    : statement_content (SEMICOLON statement_content)*
    ;

// Statement content (to be overridden by language grammars)
// This is where actual language parsing happens
statement_content
    : FREE_FORM_IDENTIFIER  // Placeholder - language grammars override this
    | literal_content
    | operator_content
    ;

// ====================================================================
// FREE FORM LITERALS (FORMAT RULES ONLY)
// ====================================================================

// Literal content handled at format level
literal_content
    : INTEGER_WITH_KIND
    | REAL_WITH_KIND
    | DOUBLE_QUOTE_STRING
    | SINGLE_QUOTE_STRING
    | INTEGER_LITERAL
    | REAL_LITERAL
    ;

// Basic operators at format level
operator_content
    : PERCENT
    | LBRACKET
    | RBRACKET
    | LPAREN
    | RPAREN
    ;

// ====================================================================
// FREE FORM BASE PARSER STATUS
// ====================================================================
//
// This parser provides ONLY the structural rules for free-form source.
// It handles:
// - Program structure as sequence of lines
// - Statement separation (semicolons)
// - Basic format recognition
//
// It does NOT handle:
// - Statement parsing (IF, DO, etc.) - in language grammars
// - Expression evaluation - in SharedCore or language grammars
// - Type declarations - in language grammars
// - Any language semantics - purely structural
//
// Language grammars that import this base will:
// 1. Override statement_content to parse actual Fortran statements
// 2. Add their specific language constructs
// 3. Inherit the free-form structure handling
//
// This separation ensures clean modularity between format and language.
// ====================================================================