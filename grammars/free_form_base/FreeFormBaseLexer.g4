// Free Form Base Lexer - PURE FORMAT RULES ONLY
// This lexer contains ONLY the formatting rules for free-form Fortran
// NO language features, keywords, or operators - just format handling
lexer grammar FreeFormBaseLexer;

import SharedCoreLexer;  // Import universal tokens

// ====================================================================
// FREE FORM FORMAT RULES (1990+)
// ====================================================================
//
// Free-form source format specifications:
// - No column restrictions (statements can start anywhere)
// - ! indicates comments (anywhere on line)
// - & indicates continuation (at end of line to continue, at start for continued)
// - Maximum line length: 132 characters (expandable)
// - Case insensitive (except in character strings)
// - Multiple statements per line separated by ;
//
// This base grammar handles ONLY the format, not the language features
// ====================================================================

// ====================================================================
// FREE FORM COMMENT HANDLING
// ====================================================================

// Free-form comment (! to end of line)
FREE_COMMENT
    : '!' ~[\r\n]* -> channel(HIDDEN)
    ;

// ====================================================================
// FREE FORM CONTINUATION
// ====================================================================

// Continuation character at end of line
CONTINUATION
    : '&' [ \t]* FREE_COMMENT? NEWLINE -> channel(HIDDEN)
    ;

// Continuation character at start of continued line
CONTINUED_LINE_START
    : NEWLINE [ \t]* '&' -> channel(HIDDEN)
    ;

// ====================================================================
// STATEMENT SEPARATORS
// ====================================================================

// Semicolon for multiple statements per line
SEMICOLON : ';' ;

// ====================================================================
// WHITESPACE HANDLING
// ====================================================================

// Whitespace (spaces and tabs) - ignored except in strings
WS
    : [ \t]+ -> channel(HIDDEN)
    ;

// Newline - significant for statement termination
NEWLINE
    : '\r'? '\n'
    | '\r'
    ;

// ====================================================================
// IDENTIFIER FORMAT RULES
// ====================================================================

// Free-form identifiers (up to 63 characters in F90, 31 in standard)
// This is a format rule about identifier structure, not specific identifiers
// Actual keywords are defined in language-specific grammars
FREE_FORM_IDENTIFIER
    : [a-zA-Z] [a-zA-Z0-9_]*
    ;

// ====================================================================
// STRING LITERAL FORMAT
// ====================================================================

// Double-quoted strings (F90+ addition to single quotes)
DOUBLE_QUOTE_STRING
    : '"' (~["\r\n] | '""')* '"'
    ;

// Single-quoted strings (traditional)
SINGLE_QUOTE_STRING
    : '\'' (~['\r\n] | '\'\'')* '\''
    ;

// ====================================================================
// NUMERIC LITERAL FORMAT
// ====================================================================

// Integer literals with optional kind
INTEGER_WITH_KIND
    : [0-9]+ '_' FREE_FORM_IDENTIFIER
    ;

// Real literals with optional kind
REAL_WITH_KIND
    : ([0-9]+ '.' [0-9]* | '.' [0-9]+) ([eEdD] [+-]? [0-9]+)? '_' FREE_FORM_IDENTIFIER
    | [0-9]+ [eEdD] [+-]? [0-9]+ '_' FREE_FORM_IDENTIFIER
    ;

// ====================================================================
// SPECIAL SEPARATORS
// ====================================================================

// Percent for derived type component access
PERCENT : '%' ;

// Square brackets for array constructors (F90+)
LBRACKET : '[' ;
RBRACKET : ']' ;

// ====================================================================
// FREE FORM BASE LEXER STATUS
// ====================================================================
//
// This lexer provides ONLY the format rules for free-form source.
// It handles:
// - Comment syntax (! comments)
// - Continuation syntax (& continuations)
// - Statement separators (; for multiple per line)
// - Whitespace handling (ignored except in strings)
// - Identifier format (not specific keywords)
// - String literal formats (single and double quotes)
// - Numeric literal formats (with kind suffixes)
//
// It does NOT handle:
// - Language keywords (IF, DO, MODULE, etc.) - in language grammars
// - Operators (except format-specific %, [, ]) - in SharedCore or language grammars
// - Data type keywords - in language grammars
// - Any language constructs - purely format
//
// This clean separation allows language grammars to focus on features
// while this base handles the free-form format requirements.
// ====================================================================