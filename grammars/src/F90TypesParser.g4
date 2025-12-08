// Fortran 90 Type System and Derived Types
// Reference: ISO/IEC 1539:1991 Section 4 (Types) and Section 5 (Declarations)
// Delegate grammar for type declarations, derived types, and kind selectors
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90TypesParser;

// ====================================================================
// DERIVED TYPES - ISO/IEC 1539:1991 Section 4.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4.4 defines derived types:
// - R422 (derived-type-def) -> derived-type-stmt [private-sequence-stmt]...
//                              component-def-stmt... end-type-stmt
// - R423 (derived-type-stmt) -> TYPE [[, access-spec] ::] type-name
// - R424 (private-sequence-stmt) -> PRIVATE | SEQUENCE
// - R425 (component-def-stmt) -> type-spec [[, component-attr-spec-list] ::]
//                                component-decl-list
// - R429 (end-type-stmt) -> END TYPE [type-name]
//
// Derived types provide user-defined data structures.

// Derived type definition - ISO/IEC 1539:1991 Section 4.4, R422
derived_type_def
    : derived_type_stmt NEWLINE* (component_def_stmt NEWLINE*)* end_type_stmt
    ;

// Derived type statement - ISO/IEC 1539:1991 Section 4.4, R423
derived_type_stmt
    : TYPE type_name NEWLINE?
    | TYPE DOUBLE_COLON type_name NEWLINE?
    ;

// Type name - ISO/IEC 1539:1991 Section 4.4
type_name
    : IDENTIFIER
    ;

// Component definition statement - ISO/IEC 1539:1991 Section 4.4, R425
component_def_stmt
    : type_declaration_stmt_f90     // Component declarations
    | private_sequence_stmt         // PRIVATE or SEQUENCE
    ;

// Private/sequence statement - ISO/IEC 1539:1991 Section 4.4.1, R424
private_sequence_stmt
    : PRIVATE NEWLINE?
    | SEQUENCE NEWLINE?
    ;

// END TYPE statement - ISO/IEC 1539:1991 Section 4.4, R429
end_type_stmt
    : END_TYPE (type_name)? NEWLINE?
    ;

// ====================================================================
// STRUCTURE CONSTRUCTOR - ISO/IEC 1539:1991 Section 4.4.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4.4.4 defines structure constructors:
// - R430 (structure-constructor) -> type-name (component-spec-list)
// - R431 (component-spec) -> [keyword =] component-data-source

// Structure constructor - ISO/IEC 1539:1991 Section 4.4.4, R430
structure_constructor
    : type_name LPAREN component_spec_list? RPAREN
    ;

// Component spec list - ISO/IEC 1539:1991 Section 4.4.4
component_spec_list
    : component_spec (COMMA component_spec)*
    ;

// Component spec - ISO/IEC 1539:1991 Section 4.4.4, R431
component_spec
    : IDENTIFIER EQUALS expr_f90    // Named component
    | expr_f90                      // Positional component
    ;

// ====================================================================
// TYPE DECLARATIONS - ISO/IEC 1539:1991 Section 5.1
// ====================================================================
//
// ISO/IEC 1539:1991 Section 5.1 defines type declaration statements:
// - R501 (type-declaration-stmt) -> type-spec [[, attr-spec]... ::]
//                                   entity-decl-list
// - R502 (type-spec) -> intrinsic-type-spec | derived-type-spec
// - R503 (attr-spec) -> PARAMETER | access-spec | ALLOCATABLE | ...
// - R504 (entity-decl) -> object-name [(array-spec)] [*char-length]
//                         [= initialization-expr]

// Type declaration statement - ISO/IEC 1539:1991 Section 5.1, R501
type_declaration_stmt_f90
    : type_spec_f90 (COMMA attr_spec_f90)* DOUBLE_COLON? entity_decl_list_f90 NEWLINE?
    ;

// Type specification - ISO/IEC 1539:1991 Section 5.1, R502
type_spec_f90
    : intrinsic_type_spec_f90
    | derived_type_spec_f90
    ;

// ====================================================================
// INTRINSIC TYPE SPECIFICATION - ISO/IEC 1539:1991 Section 4.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4.3 defines intrinsic types:
// - R502 (intrinsic-type-spec) -> INTEGER [kind-selector] |
//                                  REAL [kind-selector] |
//                                  DOUBLE PRECISION | COMPLEX [kind-selector] |
//                                  LOGICAL [kind-selector] |
//                                  CHARACTER [char-selector]

// Intrinsic type specification - ISO/IEC 1539:1991 Section 4.3
intrinsic_type_spec_f90
    : INTEGER (kind_selector)?      // Section 4.3.1.1
    | REAL (kind_selector)?         // Section 4.3.1.2
    | DOUBLE PRECISION              // F77 compatibility (Section 4.3.1.3)
    | COMPLEX (kind_selector)?      // Section 4.3.1.4
    | LOGICAL (kind_selector)?      // Section 4.3.4
    | CHARACTER (char_selector)?    // Section 4.3.2
    ;

// Derived type specification - ISO/IEC 1539:1991 Section 5.1, R502
derived_type_spec_f90
    : TYPE LPAREN type_name RPAREN
    ;

// Kind selector - ISO/IEC 1539:1991 Section 4.3.1, R404
kind_selector
    : LPAREN (KIND EQUALS)? expr_f90 RPAREN
    ;

// Character selector - ISO/IEC 1539:1991 Section 4.3.2, R406
char_selector
    : LPAREN (LEN EQUALS)? expr_f90 (COMMA (KIND EQUALS)? expr_f90)? RPAREN
    | LPAREN expr_f90 RPAREN        // Legacy F77 style
    ;

// ====================================================================
// ATTRIBUTE SPECIFICATIONS - ISO/IEC 1539:1991 Section 5.1.2
// ====================================================================
//
// ISO/IEC 1539:1991 Section 5.1.2 defines declaration attributes:
// - R503 (attr-spec) -> PARAMETER | access-spec | ALLOCATABLE |
//                       DIMENSION (array-spec) | EXTERNAL |
//                       INTENT (intent-spec) | INTRINSIC |
//                       OPTIONAL | POINTER | SAVE | TARGET

// Attribute specification - ISO/IEC 1539:1991 Section 5.1.2, R503
attr_spec_f90
    : PARAMETER                     // Section 5.1.2.11
    | DIMENSION LPAREN array_spec_f90 RPAREN  // Section 5.1.2.4.1
    | ALLOCATABLE                   // Section 5.1.2.4.3
    | POINTER                       // Section 5.1.2.4.4
    | TARGET                        // Section 5.1.2.4.5
    | PUBLIC                        // Section 5.1.2.1
    | PRIVATE                       // Section 5.1.2.1
    | INTENT LPAREN intent_spec RPAREN  // Section 5.1.2.3
    | OPTIONAL                      // Section 5.1.2.6
    | EXTERNAL                      // Section 5.1.2.7
    | INTRINSIC                     // Section 5.1.2.8
    | SAVE                          // Section 5.1.2.5
    ;

// Intent specification - ISO/IEC 1539:1991 Section 5.1.2.3, R519
intent_spec
    : IN | OUT | INOUT
    ;

// ====================================================================
// ARRAY SPECIFICATIONS - ISO/IEC 1539:1991 Section 5.1.2.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 5.1.2.4 defines array specifications:
// - R509 (array-spec) -> explicit-shape-spec-list | assumed-shape-spec-list |
//                        deferred-shape-spec-list | assumed-size-spec
// - R510 (explicit-shape-spec) -> [lower-bound :] upper-bound
// - R512 (assumed-shape-spec) -> [lower-bound] :
// - R514 (deferred-shape-spec) -> :
// - R516 (assumed-size-spec) -> [explicit-shape-spec ,]... [lower-bound :] *

// Array specification - ISO/IEC 1539:1991 Section 5.1.2.4, R509
array_spec_f90
    : explicit_shape_spec_list       // Section 5.1.2.4.1
    | assumed_shape_spec_list        // Section 5.1.2.4.2 - F90 dummy arguments
    | deferred_shape_spec_list       // Section 5.1.2.4.3 - F90 ALLOCATABLE/POINTER
    | assumed_size_spec              // Section 5.1.2.4.4 - F77 compatibility
    ;

// Explicit shape specification list - ISO/IEC 1539:1991 Section 5.1.2.4.1
explicit_shape_spec_list
    : explicit_shape_spec (COMMA explicit_shape_spec)*
    ;

// Explicit shape spec - ISO/IEC 1539:1991 Section 5.1.2.4.1, R510
explicit_shape_spec
    : expr_f90 (COLON expr_f90)?    // lower:upper or just upper
    ;

// Assumed shape specification list - ISO/IEC 1539:1991 Section 5.1.2.4.2
assumed_shape_spec_list
    : assumed_shape_spec (COMMA assumed_shape_spec)*
    ;

// Assumed shape spec - ISO/IEC 1539:1991 Section 5.1.2.4.2, R512
assumed_shape_spec
    : COLON                         // Just :
    | expr_f90 COLON                // lower:
    ;

// Deferred shape specification list - ISO/IEC 1539:1991 Section 5.1.2.4.3
deferred_shape_spec_list
    : deferred_shape_spec (COMMA deferred_shape_spec)*
    ;

// Deferred shape spec - ISO/IEC 1539:1991 Section 5.1.2.4.3, R514
deferred_shape_spec
    : COLON                         // Just :
    ;

// Assumed size specification - ISO/IEC 1539:1991 Section 5.1.2.4.4, R516
assumed_size_spec
    : (explicit_shape_spec COMMA)* MULTIPLY    // ..., *
    ;

// ====================================================================
// ENTITY DECLARATIONS - ISO/IEC 1539:1991 Section 5.1
// ====================================================================

// Entity declaration list - ISO/IEC 1539:1991 Section 5.1, R504
entity_decl_list_f90
    : entity_decl_f90 (COMMA entity_decl_f90)*
    ;

// Entity declaration - ISO/IEC 1539:1991 Section 5.1, R504
entity_decl_f90
    : identifier_or_keyword (LPAREN array_spec_f90 RPAREN)? (MULTIPLY char_length)?
      (EQUALS expr_f90)?
    ;

// Character length - ISO/IEC 1539:1991 Section 5.1.1.5
char_length
    : expr_f90
    | MULTIPLY                      // Assumed length character
    ;

// Keywords that can be used as identifiers in certain contexts
// ISO/IEC 1539:1991 allows most keywords as user names except in ambiguous contexts
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
