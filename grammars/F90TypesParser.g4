// Fortran 90 Type System and Derived Types
// Delegate grammar for type declarations, derived types, and kind selectors
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90TypesParser;

// ====================================================================
// DERIVED TYPES (F90 MAJOR INNOVATION)
// ====================================================================
// ISO/IEC 1539:1991 Section 4.4
// User-defined data structures (similar to C structs).

// Derived type definition (user-defined structures)
derived_type_def
    : derived_type_stmt NEWLINE* (component_def_stmt NEWLINE*)* end_type_stmt
    ;

derived_type_stmt
    : TYPE type_name NEWLINE?
    | TYPE DOUBLE_COLON type_name NEWLINE?
    ;

type_name
    : IDENTIFIER
    ;

component_def_stmt
    : type_declaration_stmt_f90     // Component declarations
    | private_sequence_stmt         // PRIVATE or SEQUENCE
    ;

private_sequence_stmt
    : PRIVATE NEWLINE?
    | SEQUENCE NEWLINE?
    ;

end_type_stmt
    : END_TYPE (type_name)? NEWLINE?
    ;

// Structure constructor (F90 derived type initialization)
// ISO/IEC 1539:1991 Section 4.4.4
structure_constructor
    : type_name LPAREN component_spec_list? RPAREN
    ;

component_spec_list
    : component_spec (COMMA component_spec)*
    ;

component_spec
    : IDENTIFIER EQUALS expr_f90    // Named component
    | expr_f90                      // Positional component
    ;

// ====================================================================
// ENHANCED TYPE DECLARATIONS (F90 EXTENSIONS)
// ====================================================================
// ISO/IEC 1539:1991 Section 5.1

// F90 type declaration statement (major enhancements)
type_declaration_stmt_f90
    : type_spec_f90 (COMMA attr_spec_f90)* DOUBLE_COLON? entity_decl_list_f90 NEWLINE?
    ;

// Type specification (enhanced for F90)
type_spec_f90
    : intrinsic_type_spec_f90
    | derived_type_spec_f90
    ;

// Intrinsic type specification with F90 enhancements
// ISO/IEC 1539:1991 Section 4.3
intrinsic_type_spec_f90
    : INTEGER (kind_selector)?
    | REAL (kind_selector)?
    | DOUBLE PRECISION             // F77 compatibility
    | COMPLEX (kind_selector)?
    | LOGICAL (kind_selector)?
    | CHARACTER (char_selector)?
    ;

// Derived type specification (F90 feature)
derived_type_spec_f90
    : TYPE LPAREN type_name RPAREN
    ;

// Kind selector for type parameters (F90 innovation)
// ISO/IEC 1539:1991 Section 4.3.1
kind_selector
    : LPAREN (KIND EQUALS)? expr_f90 RPAREN
    ;

// Character length and kind selector (F90 enhancement)
// ISO/IEC 1539:1991 Section 4.3.2
char_selector
    : LPAREN (LEN EQUALS)? expr_f90 (COMMA (KIND EQUALS)? expr_f90)? RPAREN
    | LPAREN expr_f90 RPAREN        // Legacy F77 style
    ;

// Attribute specifications (F90 major extensions)
// ISO/IEC 1539:1991 Section 5.1.2
attr_spec_f90
    : PARAMETER                     // Inherited from shared core
    | DIMENSION LPAREN array_spec_f90 RPAREN
    | ALLOCATABLE                   // F90 dynamic arrays
    | POINTER                       // F90 pointers
    | TARGET                        // F90 pointer targets
    | PUBLIC                        // F90 module visibility
    | PRIVATE                       // F90 module visibility
    | INTENT LPAREN intent_spec RPAREN  // F90 procedure arguments
    | OPTIONAL                      // F90 optional arguments
    | EXTERNAL                      // External procedure
    | INTRINSIC                     // Intrinsic procedure
    | SAVE                          // Variable persistence
    ;

// Intent specification (F90 procedure interface)
// ISO/IEC 1539:1991 Section 5.1.2.3
intent_spec
    : IN | OUT | INOUT
    ;

// ====================================================================
// ARRAY SPECIFICATIONS (F90 ENHANCEMENTS)
// ====================================================================
// ISO/IEC 1539:1991 Section 5.1.2.4

// Array specification (F90 enhancements)
array_spec_f90
    : explicit_shape_spec_list
    | assumed_shape_spec_list       // F90: dummy arguments (:,:)
    | deferred_shape_spec_list      // F90: ALLOCATABLE/POINTER (:,:)
    | assumed_size_spec             // F77 compatibility: (*)
    ;

// Explicit shape specification
explicit_shape_spec_list
    : explicit_shape_spec (COMMA explicit_shape_spec)*
    ;

explicit_shape_spec
    : expr_f90 (COLON expr_f90)?    // lower:upper or just upper
    ;

// Assumed shape specification (F90 dummy arguments)
assumed_shape_spec_list
    : assumed_shape_spec (COMMA assumed_shape_spec)*
    ;

assumed_shape_spec
    : COLON                         // Just :
    | expr_f90 COLON                // lower:
    ;

// Deferred shape specification (F90 ALLOCATABLE/POINTER)
deferred_shape_spec_list
    : deferred_shape_spec (COMMA deferred_shape_spec)*
    ;

deferred_shape_spec
    : COLON                         // Just :
    ;

// Assumed size specification (F77 compatibility)
assumed_size_spec
    : (explicit_shape_spec COMMA)* MULTIPLY    // ..., *
    ;

// Entity declaration list (F90 enhancements)
entity_decl_list_f90
    : entity_decl_f90 (COMMA entity_decl_f90)*
    ;

entity_decl_f90
    : identifier_or_keyword (LPAREN array_spec_f90 RPAREN)? (MULTIPLY char_length)?
      (EQUALS expr_f90)?
    ;

char_length
    : expr_f90
    | MULTIPLY                      // Assumed length character
    ;

// Keywords that can be used as identifiers in certain contexts
identifier_or_keyword
    : IDENTIFIER
    | DATA         // DATA can be used as a variable name
    | SIZE         // SIZE can be used as a variable name
    | KIND         // KIND can be used as a parameter name
    | LEN          // LEN can be used as a parameter name
    | RESULT       // RESULT can be used as a variable name
    | STAT         // STAT can be used as a variable name
    | SUM_INTRINSIC  // SUM can be used as a variable/result name
    | PRODUCT_INTRINSIC  // PRODUCT can be used as a result name
    ;
