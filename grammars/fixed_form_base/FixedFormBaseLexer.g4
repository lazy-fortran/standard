// Fixed Form Base Lexer - PURE FORMAT RULES ONLY
// This lexer contains ONLY the formatting rules for fixed-form FORTRAN/Fortran
// NO language features, keywords, or operators - just format handling
lexer grammar FixedFormBaseLexer;

import SharedCoreLexer;  // Import universal tokens

// ====================================================================
// FIXED FORM FORMAT RULES (1957-2018)
// ====================================================================
//
// Fixed-form source format specifications:
// - Column 1: Comment indicator (C, c, *, !)
// - Columns 1-5: Statement labels (1-99999)
// - Column 6: Continuation indicator (any non-blank character except 0)
// - Columns 7-72: FORTRAN statements
// - Columns 73-80: Sequence numbers (ignored)
//
// This base grammar handles ONLY the format, not the language features
// ====================================================================

// ====================================================================
// FIXED FORM COMMENT HANDLING
// ====================================================================

// Fixed-form comments (column 1 indicators)
FIXED_COMMENT
    : {getCharPositionInLine() == 0}? [Cc*!] ~[\r\n]* -> channel(HIDDEN)
    ;

// Debug lines (D in column 1 - special conditional compilation)
DEBUG_LINE
    : {getCharPositionInLine() == 0}? [Dd] ~[\r\n]* -> channel(HIDDEN)
    ;

// ====================================================================
// FIXED FORM CONTINUATION
// ====================================================================

// Continuation marker in column 6
// In fixed form, any non-blank, non-zero character in column 6 continues previous line
CONTINUATION_MARKER
    : {getCharPositionInLine() == 5}? ~[ 0\r\n\t]
    ;

// ====================================================================
// STATEMENT LABELS
// ====================================================================

// Statement label in columns 1-5
// Must be numeric, right-justified with leading blanks
STATEMENT_LABEL
    : {getCharPositionInLine() < 5}? [ ]* [0-9]+
    ;

// ====================================================================
// COLUMN TRACKING
// ====================================================================

// Column 6 marker (for continuation detection)
COLUMN_6
    : {getCharPositionInLine() == 5}? .
    ;

// Columns 7-72 (statement field)
STATEMENT_FIELD
    : {getCharPositionInLine() >= 6 && getCharPositionInLine() < 72}? .
    ;

// Columns 73-80 (sequence field - ignored but recognized)
SEQUENCE_FIELD
    : {getCharPositionInLine() >= 72}? ~[\r\n]+ -> channel(HIDDEN)
    ;

// ====================================================================
// HOLLERITH CONSTANTS (FORMAT-SPECIFIC)
// ====================================================================

// Hollerith constant (nHtext where n is the character count)
// This is a format feature, not a language feature
HOLLERITH
    : [0-9]+ [Hh] .+?  // Simple pattern - actual length check in parser
    ;

// ====================================================================
// WHITESPACE AND LINE HANDLING
// ====================================================================

// Tab character (non-standard but sometimes used)
// Tabs in fixed form are problematic and implementation-dependent
TAB
    : '\t' -> channel(HIDDEN)
    ;

// Blank padding (significant in fixed form!)
// Blanks are NOT ignored in fixed form - they're significant
BLANK
    : ' '
    ;

// End of line
EOL
    : '\r'? '\n'
    | '\r'
    ;

// ====================================================================
// FIXED FORM LEXER MODES
// ====================================================================

// Note: Fixed form doesn't use lexer modes like free form
// Everything is position-dependent based on column

// ====================================================================
// FIXED FORM BASE LEXER STATUS
// ====================================================================
//
// This lexer provides ONLY the format rules for fixed-form source.
// It handles:
// - Column-based layout (1-5: labels, 6: continuation, 7-72: statements)
// - Comment detection (C, c, *, ! in column 1)
// - Continuation lines (non-blank in column 6)
// - Hollerith constants (format feature)
//
// It does NOT handle:
// - Language keywords (IF, DO, GOTO, etc.) - in language grammars
// - Operators (+, -, *, /, etc.) - in SharedCore or language grammars
// - Data types (INTEGER, REAL, etc.) - in language grammars
// - Any language constructs - purely format
//
// This clean separation allows language grammars to focus on features
// while this base handles the peculiar fixed-form format requirements.
// ====================================================================