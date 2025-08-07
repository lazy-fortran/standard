// Fixed Form Base Parser - PURE FORMAT RULES ONLY
// This parser contains ONLY the formatting rules for fixed-form FORTRAN/Fortran
// NO language features or constructs - just format handling
parser grammar FixedFormBaseParser;

import SharedCoreParser;  // Import universal constructs

options {
    tokenVocab = FixedFormBaseLexer;
}

// ====================================================================
// FIXED FORM FORMAT RULES (1957-2018)
// ====================================================================
//
// This parser handles ONLY the structural aspects of fixed-form format:
// - Line structure with labels and continuations
// - Statement field extraction
// - Comment and blank line handling
//
// All actual language constructs are defined in the importing grammars
// ====================================================================

// ====================================================================
// FIXED FORM LINE STRUCTURE
// ====================================================================

// A fixed-form source line
fixed_form_line
    : comment_line
    | continuation_line
    | statement_line
    | blank_line
    ;

// Comment line (C, c, *, or ! in column 1)
comment_line
    : FIXED_COMMENT EOL?
    ;

// Debug line (D in column 1 - conditional compilation)
debug_line
    : DEBUG_LINE EOL?
    ;

// Continuation line (non-blank in column 6)
continuation_line
    : BLANK* CONTINUATION_MARKER statement_text? EOL?
    ;

// Regular statement line
statement_line
    : label_field? statement_field? EOL?
    ;

// Blank line
blank_line
    : BLANK* EOL
    ;

// ====================================================================
// FIXED FORM FIELDS
// ====================================================================

// Label field (columns 1-5)
label_field
    : STATEMENT_LABEL
    ;

// Statement field (columns 7-72)
// This is where actual FORTRAN statements go
// The content is parsed by the importing language grammar
statement_field
    : statement_text
    ;

// Statement text (the actual content to be parsed by language grammars)
statement_text
    : (STATEMENT_FIELD | BLANK)+
    ;

// ====================================================================
// FIXED FORM PROGRAM STRUCTURE
// ====================================================================

// Fixed-form program (sequence of lines)
// The actual parsing of statements is done by importing grammars
fixed_form_program
    : fixed_form_line* EOF
    ;

// ====================================================================
// HOLLERITH HANDLING
// ====================================================================

// Hollerith constant (nHtext)
// This is a format feature specific to fixed form
hollerith_constant
    : HOLLERITH
    ;

// ====================================================================
// FIXED FORM BASE PARSER STATUS
// ====================================================================
//
// This parser provides ONLY the structural rules for fixed-form source.
// It handles:
// - Line classification (comment, continuation, statement, blank)
// - Field extraction (label, statement)
// - Basic program structure as sequence of lines
//
// It does NOT handle:
// - Statement parsing (IF, DO, etc.) - in language grammars
// - Expression evaluation - in SharedCore or language grammars
// - Type declarations - in language grammars
// - Any language semantics - purely structural
//
// Language grammars that import this base will:
// 1. Override statement_field to parse actual FORTRAN statements
// 2. Add their specific language constructs
// 3. Inherit the fixed-form structure handling
//
// This separation ensures clean modularity between format and language.
// ====================================================================