// Fortran 95 (1995) Parser - Enhanced Modern Foundation
// Building on F90 with FORALL, WHERE enhancements, and Pure Procedures
// Reference: ISO/IEC 1539-1:1997 (Fortran 95)
//            J3/98-114 (Fortran 95 Request for Interpretation)
parser grammar Fortran95Parser;

import Fortran90Parser;  // F90 unified format support

options {
    tokenVocab = Fortran95Lexer;
}

// ====================================================================
// FORTRAN 95 PARSER OVERVIEW
// ====================================================================
//
// This parser implements syntax rules for Fortran 95 as defined in:
//   ISO/IEC 1539-1:1997 (Fortran 95 International Standard)
//   J3/98-114 (Fortran 95 Request for Interpretation)
//
// Fortran 95 (ISO/IEC 1539-1:1997) builds incrementally on Fortran 90,
// adding several important enhancements while maintaining complete
// backward compatibility.
//
// This parser inherits unified format support (fixed/free) from F90
// and adds F95-specific language constructs.
//
// MAJOR F95 ENHANCEMENTS IMPLEMENTED (with ISO section refs):
// - FORALL construct and statements (Section 7.5.4)
// - Enhanced WHERE constructs with multiple ELSEWHERE (Section 7.5.3)
// - PURE and ELEMENTAL procedure prefixes (Section 12.6)
// - Derived type default initialization (Section 4.4.1)
// - Pointer association enhancements (Section 6.3.1)
// - Extended intrinsic functions (Section 13)
//
// INHERITANCE ARCHITECTURE (IN THIS REPO):
// FORTRAN / FORTRANII / FORTRAN66 / FORTRAN77
//   → Fortran90Parser
//   → Fortran95Parser
//   → F2003+ standards
//
// ====================================================================

// ====================================================================
// FORTRAN 95 PROGRAM ENTRY POINT (ISO/IEC 1539-1:1997 Section 11)
// ====================================================================
//
// Section 11 of ISO/IEC 1539-1:1997 defines the structure of Fortran
// program units: main programs, external subprograms, modules, and
// block data program units.
//
// This entry point integrates F95 constructs (FORALL, enhanced WHERE,
// F95 type rules and I/O) into the program structure. It reuses the F90
// program unit structure but augments execution_part and construct
// rules to reach F95-specific constructs.
//
// ISO/IEC 1539-1:1997 Section 11.1: Program units and scoping
// - R201 (program) -> main-program | external-subprogram | module
// - R1101 (main-program) -> [program-stmt] [specification-part]
//                           [execution-part] [internal-subprogram-part]
//
// Issue #179: integrate F95 constructs into program and execution structure.

program_unit_f95
    : NEWLINE* (main_program_f95 | module_f95 | external_subprogram_f95) NEWLINE*
    ;

// Main program (ISO/IEC 1539-1:1997 Section 11.1, R1101)
main_program_f95
    : program_stmt specification_part_f95? execution_part_f95?
      internal_subprogram_part_f95? end_program_stmt
    ;

// Module (ISO/IEC 1539-1:1997 Section 11.3, R1104)
module_f95
    : module_stmt specification_part_f95? module_subprogram_part_f95? end_module_stmt
    ;

// Module subprogram part (ISO/IEC 1539-1:1997 Section 11.3.1, R1107)
module_subprogram_part_f95
    : contains_stmt NEWLINE* (module_subprogram_f95 NEWLINE*)*
    ;

module_subprogram_f95
    : function_subprogram_f95
    | subroutine_subprogram_f95
    ;

// External subprogram (ISO/IEC 1539-1:1997 Section 11.2, R1102)
external_subprogram_f95
    : function_subprogram_f95
    | subroutine_subprogram_f95
    | module_f95
    ;

// Function subprogram (ISO/IEC 1539-1:1997 Section 12.5.2, R1216)
function_subprogram_f95
    : function_stmt_f95 specification_part_f95? execution_part_f95?
      internal_subprogram_part_f95? end_function_stmt
    ;

// Subroutine subprogram (ISO/IEC 1539-1:1997 Section 12.5.3, R1220)
subroutine_subprogram_f95
    : subroutine_stmt_f95 specification_part_f95? execution_part_f95?
      internal_subprogram_part_f95? end_subroutine_stmt
    ;

// Internal subprogram part (ISO/IEC 1539-1:1997 Section 12.5.1, R1213)
internal_subprogram_part_f95
    : contains_stmt NEWLINE* (internal_subprogram_f95 NEWLINE*)*
    ;

internal_subprogram_f95
    : function_subprogram_f95
    | subroutine_subprogram_f95
    ;

// ====================================================================
// F95 SPECIFICATION PART (ISO/IEC 1539-1:1997 Section 11.1)
// ====================================================================
//
// The specification part (R204) precedes the execution part and contains
// declarations that specify the properties of data objects, procedure
// interfaces, and derived type definitions.
//
// ISO/IEC 1539-1:1997 Section 11.1:
// - R204 (specification-part) -> [use-stmt]... [implicit-part]
//                                [declaration-construct]...
// - R207 (declaration-construct) -> derived-type-def | interface-block
//                                   | type-declaration-stmt | ...
//
// The specification part may include F95-specific type declarations and
// derived type definitions with default initialization (Section 4.4.1).

specification_part_f95
    : (NEWLINE* (use_stmt | import_stmt))*
      (NEWLINE* implicit_stmt_f90)?
      (NEWLINE* declaration_construct_f95)* NEWLINE*
    ;

// Declaration construct (ISO/IEC 1539-1:1997 Section 11.1, R207)
declaration_construct_f95
    : type_declaration_stmt_f95
    | derived_type_def_f95
    | declaration_construct
    ;

// ====================================================================
// F95 EXECUTION PART (ISO/IEC 1539-1:1997 Section 11.1)
// ====================================================================
//
// The execution part (R208) contains executable statements and constructs
// that control program execution and perform computations.
//
// ISO/IEC 1539-1:1997 Section 11.1:
// - R208 (execution-part) -> executable-construct [execution-part-construct]...
// - R214 (executable-construct) -> action-stmt | construct
//
// The execution part includes F95 executable constructs such as FORALL
// (Section 7.5.4) and enhanced WHERE (Section 7.5.3) alongside the F90 constructs.

execution_part_f95
    : (NEWLINE* execution_item_f95)* NEWLINE*
    ;

// Execution item: allows semicolons between executable constructs
// Fortran 95 retains the F90 semicolon-separated statement feature
// (ISO/IEC 1539:1991 3.4.2)
execution_item_f95
    : executable_construct_f95 (SEMICOLON executable_construct_f95)*
    ;

// ====================================================================
// F95 IDENTIFIER-OR-KEYWORD (INTRINSIC PROCEDURE NAMES)
// ====================================================================
//
// Fortran 95 intrinsic procedure names are recognized as keyword tokens
// by the lexer. To allow these names to appear in expressions and calls,
// we define a rule that accepts both IDENTIFIER and all F95 intrinsic
// tokens.
//
// ISO/IEC 1539-1:1997 Section 13 defines the intrinsic procedures.
// These tokens may appear wherever a function reference or variable is
// allowed, enabling code like x = CEILING(y) or CALL CPU_TIME(t).
//
// Intrinsic procedures by category (Section 13):
// - Numeric functions: CEILING, FLOOR, MODULO (Section 13.10)
// - Bit manipulation: BIT_SIZE, BTEST, IAND, IBCLR, IBITS, IBSET,
//                     IEOR, IOR, ISHFT, ISHFTC, NOT (Section 13.10)
// - Data transfer: TRANSFER (Section 13.10.112)
// - Timing subroutines: CPU_TIME, SYSTEM_CLOCK (Section 13.11)
//
identifier_or_keyword_f95
    : IDENTIFIER
    // F90/F95 numeric intrinsics (Section 13.10)
    | CEILING_INTRINSIC
    | FLOOR_INTRINSIC
    | MODULO_INTRINSIC
    // F95 pointer intrinsic (Section 13.10.79)
    | NULL_INTRINSIC
    // F90/F95 bit manipulation intrinsics (Section 13.10)
    | BIT_SIZE_INTRINSIC
    | BTEST_INTRINSIC
    | IAND_INTRINSIC
    | IBCLR_INTRINSIC
    | IBITS_INTRINSIC
    | IBSET_INTRINSIC
    | IEOR_INTRINSIC
    | IOR_INTRINSIC
    | ISHFT_INTRINSIC
    | ISHFTC_INTRINSIC
    | NOT_INTRINSIC
    // F90/F95 data transfer intrinsic (Section 13.10.112)
    | TRANSFER_INTRINSIC
    // F95 timing intrinsics (Section 13.11)
    | CPU_TIME_INTRINSIC
    | SYSTEM_CLOCK_INTRINSIC
    // Common keywords that can be used as identifiers in argument contexts
    | KIND                         // KIND= keyword argument
    | LEN                          // LEN= keyword argument
    | SIZE                         // SIZE intrinsic / keyword argument
    ;

// ====================================================================
// FORALL CONSTRUCTS (ISO/IEC 1539-1:1997 Section 7.5.4)
// ====================================================================
//
// FORALL is a major F95 innovation for parallel array assignment.
//
// ISO/IEC 1539-1:1997 Section 7.5.4 defines:
// - R736 (forall-construct) -> forall-construct-stmt [forall-body-construct]...
//                              end-forall-stmt
// - R738 (forall-construct-stmt) -> [forall-construct-name :] FORALL forall-header
// - R740 (forall-stmt) -> FORALL forall-header forall-assignment-stmt
// - R741 (forall-header) -> (forall-triplet-spec-list [, scalar-mask-expr])
// - R742 (forall-triplet-spec) -> index-name = subscript : subscript [: stride]
//
// SEMANTIC CONSTRAINTS (Section 7.5.4, NOT enforced by grammar):
// ==============================================================
// These constraints require semantic analysis (type checking, scope analysis)
// and are NOT enforced by the grammar syntax rules.
//
// SCALAR MASK EXPRESSION TYPE CONSTRAINT (Section 7.5.4, R741):
// - scalar-mask-expr (if present) must have LOGICAL type (not INTEGER, REAL, etc.)
// - scalar-mask-expr must be SCALAR (rank 0), not an array
// Violation example (incorrectly accepted by grammar):
//   integer :: i, j, m(10,10)
//   real :: a(10,10), b(10,10)
//   integer :: mask(10,10)     ! Integer, not logical!
//   real :: scalar_mask         ! Scalar mask (correct for scalar requirement)
//
//   FORALL (i=1:10, j=1:10, mask(i,j))  ! INVALID - mask is INTEGER, not LOGICAL
//     a(i,j) = b(i,j)
//   END FORALL
//
//   FORALL (i=1:10, scalar_mask)  ! Valid syntax but mask type not enforced
//     a(i,i) = b(i,i)
//   END FORALL
//
// INDEX VARIABLE RESTRICTIONS (Section 7.5.4):
// - Index variables (from forall-triplet-spec) have NO storage association
// - Index variables are LOCAL to the FORALL construct
// - Index variables CANNOT be modified within the FORALL body
// - Index variables CANNOT be passed as ACTUAL ARGUMENTS to procedures
// Violation example (incorrectly accepted by grammar):
//   FORALL (i=1:10)
//     i = i + 1              ! INVALID - modifying index variable
//     CALL foo(i)            ! INVALID - index variable as argument
//   END FORALL
//
// FORALL BODY RESTRICTIONS (Section 7.5.4, R737):
// - FORALL body constructs: assignment-stmt | pointer-assignment-stmt |
//                           WHERE construct | nested FORALL
// - NO I/O statements (READ, WRITE, PRINT, OPEN, CLOSE, etc.)
// - NO STOP, RETURN, EXIT, CYCLE statements
// - NO procedure calls with side effects
// Violation example (incorrectly accepted by grammar):
//   FORALL (i=1:10)
//     PRINT *, i             ! INVALID - I/O statement in FORALL body
//     a(i) = b(i)
//   END FORALL
//
// DEPENDENCY CONSTRAINTS (Section 7.5.4):
// - All RHS evaluations occur before any assignments (array semantics)
// - LHS and RHS of assignments must not interfere
// - Index variables on LHS and RHS determine evaluation order
// Violation example (undefined order):
//   FORALL (i=1:n)
//     a(i) = a(i+1)  ! Order dependency - different element accessed
//   END FORALL
//
// FUTURE WORK:
// A semantic analyzer phase must be implemented to enforce these constraints.
// Related issue tracking: #402 (comprehensive documentation)

// FORALL construct (ISO/IEC 1539-1:1997 Section 7.5.4.1, R736)
forall_construct
    : forall_construct_stmt NEWLINE
      (NEWLINE* forall_body_construct)* NEWLINE* end_forall_stmt
    ;

// FORALL construct statement (Section 7.5.4.1, R738)
forall_construct_stmt
    : (IDENTIFIER COLON)? FORALL forall_header
    ;

// FORALL statement - single statement form (Section 7.5.4.2, R740)
forall_stmt
    : FORALL forall_header forall_assignment_stmt NEWLINE?
    ;

// FORALL header with triplet specifications (Section 7.5.4, R741)
// NOTE: scalar_mask_expr is accepted as any expr_f95, but semantic constraint
// requires it to be a LOGICAL SCALAR expression. Grammar does not enforce this.
// See constraint documentation above.
forall_header
    : LPAREN forall_triplet_spec_list (COMMA scalar_mask_expr)? RPAREN
    ;

// Forall triplet spec list (Section 7.5.4, part of R741)
forall_triplet_spec_list
    : forall_triplet_spec (COMMA forall_triplet_spec)*
    ;

// Forall triplet specification (Section 7.5.4, R742)
// index-name = subscript : subscript [ : stride ]
forall_triplet_spec
    : IDENTIFIER EQUALS expr_f95 COLON expr_f95 (COLON expr_f95)?
    ;

// Scalar mask expression (Section 7.5.4, part of R741)
// SEMANTIC CONSTRAINTS (not enforced by grammar):
// - Must be a SCALAR logical expression (not an array, not non-logical)
// - Grammar accepts any expr_f95 without type or shape checking
// - A semantic analyzer must validate:
//   1. Type: mask expression has LOGICAL type
//   2. Shape: mask expression is scalar (rank 0)
// Invalid examples (incorrectly accepted by grammar):
//   FORALL (i=1:10, integer_array(i,j))  ! Type error - INTEGER not LOGICAL
//   FORALL (i=1:10, logical_array > 0.0)  ! Shape error - LOGICAL array not scalar
// See FORALL semantic constraints section above for comprehensive documentation.
scalar_mask_expr
    : expr_f95
    ;

// FORALL body construct (Section 7.5.4.1, R737)
// forall-body-construct -> forall-assignment-stmt | where-construct | forall-construct
// SEMANTIC CONSTRAINTS (not enforced by grammar):
// - forall-assignment-stmt: assignment-stmt | pointer-assignment-stmt
// - Nested WHERE constructs allowed (Section 7.5.3)
// - Nested FORALL constructs allowed (Section 7.5.4)
// RESTRICTIONS (not enforced by grammar):
// - NO I/O statements (READ, WRITE, PRINT, OPEN, CLOSE, INQUIRE, BACKSPACE,
//   REWIND, ENDFILE)
// - NO control flow statements (STOP, RETURN, EXIT, CYCLE, GO TO)
// - NO procedure calls with side effects
// - All operations must be pure (no global state modification)
// Grammar currently allows I/O and control flow via executable_stmt_f95 in
// forall_assignment_stmt. A semantic analyzer must restrict this.
// See FORALL semantic constraints section above for comprehensive documentation.
forall_body_construct
    : forall_assignment_stmt NEWLINE?
    | where_construct_f95
    | forall_construct
    ;

// FORALL assignment statement (Section 7.5.4.1, R739)
// forall-assignment-stmt -> assignment-stmt | pointer-assignment-stmt
forall_assignment_stmt
    : assignment_stmt_f95
    | pointer_assignment_stmt
    | where_stmt
    | forall_stmt                 // Nested FORALL statement
    ;

// END FORALL statement (Section 7.5.4.1, R743)
end_forall_stmt
    : END_FORALL (IDENTIFIER)? NEWLINE?
    ;

// ====================================================================
// ENHANCED WHERE CONSTRUCTS (ISO/IEC 1539-1:1997 Section 7.5.3)
// ====================================================================
//
// WHERE provides masked array assignment. F95 enhances F90 WHERE with:
// - Multiple ELSEWHERE clauses with optional masks
// - Clearer nesting rules
//
// ISO/IEC 1539-1:1997 Section 7.5.3 defines:
// - R727 (where-construct) -> where-construct-stmt [where-body-construct]...
//                             [masked-elsewhere-stmt [where-body-construct]...]...
//                             [elsewhere-stmt [where-body-construct]...]
//                             end-where-stmt
// - R728 (where-construct-stmt) -> [where-construct-name :] WHERE (mask-expr)
// - R732 (masked-elsewhere-stmt) -> ELSEWHERE (mask-expr) [where-construct-name]
// - R733 (elsewhere-stmt) -> ELSEWHERE [where-construct-name]
// - R734 (end-where-stmt) -> END WHERE [where-construct-name]
// - R735 (where-stmt) -> WHERE (mask-expr) where-assignment-stmt
//
// SEMANTIC CONSTRAINTS (Section 7.5.3, NOT enforced by grammar):
// ================================================================
// These constraints require semantic analysis (type checking, shape analysis)
// and are NOT enforced by the grammar syntax rules.
//
// MASK EXPRESSION TYPE CONSTRAINT (Section 7.5.3):
// - mask-expr in WHERE construct-stmt must have LOGICAL type (not INTEGER, REAL, etc.)
// - mask-expr in masked ELSEWHERE must have LOGICAL type
// Violation example (incorrectly accepted by grammar):
//   integer :: mask(10)
//   real :: a(10), b(10)
//   where (mask > 0)        ! INVALID - mask is INTEGER, not LOGICAL
//     a = b
//   end where
//
// MASK EXPRESSION SHAPE CONSTRAINT (Section 7.5.3):
// - mask-expr must be an ARRAY expression (rank > 0), not a scalar
// Violation example (incorrectly accepted by grammar):
//   real :: a(10), b(10)
//   real :: scalar_mask
//   where (scalar_mask > 0.0)  ! INVALID - scalar_mask is scalar, not array
//     a = b
//   end where
//
// CONFORMABILITY CONSTRAINT (Section 7.5.3):
// - All arrays in mask-expr, where-body, and elsewhere-body must be CONFORMABLE
//   with each other (same rank and shape)
// - Arrays with different shapes or ranks are INVALID
// Violation example (incorrectly accepted by grammar):
//   real :: a(10), b(10)
//   where (a > 0.0)
//     a = b
//   elsewhere
//     a(1:5) = 0.0             ! INVALID - different shape than WHERE mask
//   end where
//
// FUTURE WORK:
// A semantic analyzer phase must be implemented to enforce these constraints.
// Related issue tracking: #419 (document), future issue for semantic implementation

// WHERE construct (ISO/IEC 1539-1:1997 Section 7.5.3, R727)
where_construct_f95
    : where_construct_stmt_f95 NEWLINE (NEWLINE* where_body_construct_f95)*
      NEWLINE* end_where_stmt
    ;

// WHERE construct statement (Section 7.5.3, R728)
// NOTE: Grammar accepts any expression, but semantic constraint requires
// logical_expr_f95 to be a LOGICAL ARRAY expression (not scalar, not non-logical).
// See constraints documented above - enforced by semantic analyzer, not grammar.
where_construct_stmt_f95
    : (IDENTIFIER COLON)? WHERE LPAREN logical_expr_f95 RPAREN
    ;

// WHERE body construct (Section 7.5.3, R729)
// where-body-construct -> where-assignment-stmt | where-construct
where_body_construct_f95
    : where_assignment_stmt_f95 NEWLINE?
    | where_construct_f95         // Nested WHERE
    | elsewhere_part_f95
    ;

// ELSEWHERE part (Section 7.5.3, R731-R733)
elsewhere_part_f95
    : elsewhere_stmt_f95 NEWLINE (NEWLINE* elsewhere_assignment_stmt_f95)*
    ;

// ELSEWHERE statement - with optional mask (Section 7.5.3, R732/R733)
// NOTE: When mask is present (masked ELSEWHERE, R732), semantic constraint requires
// logical_expr_f95 to be a LOGICAL ARRAY expression.
// See constraints documented above - enforced by semantic analyzer, not grammar.
elsewhere_stmt_f95
    : ELSEWHERE (LPAREN logical_expr_f95 RPAREN)? (IDENTIFIER)?
    ;

// WHERE assignment statement (Section 7.5.3, R730)
// where-assignment-stmt -> assignment-stmt
where_assignment_stmt_f95
    : assignment_stmt_f95
    | pointer_assignment_stmt
    | where_stmt
    ;

// ELSEWHERE body assignment (follows R730)
elsewhere_assignment_stmt_f95
    : assignment_stmt_f95 NEWLINE?
    | pointer_assignment_stmt NEWLINE?
    | where_stmt NEWLINE?
    ;

// WHERE statement - single statement form (Section 7.5.3.2, R735)
// NOTE: Grammar accepts any expression, but semantic constraint requires
// logical_expr_f95 to be a LOGICAL ARRAY expression.
// See constraints documented above - enforced by semantic analyzer, not grammar.
where_stmt_f95
    : WHERE LPAREN logical_expr_f95 RPAREN assignment_stmt_f95
    ;

// ====================================================================
// ENHANCED TYPE DECLARATIONS (ISO/IEC 1539-1:1997 Section 4.4.1)
// ====================================================================
//
// Fortran 95 introduces default initialization for derived type components.
//
// ISO/IEC 1539-1:1997 Section 4.4.1 defines:
// - R424 (derived-type-def) -> derived-type-stmt [private-sequence-stmt]...
//                              [component-def-stmt]... end-type-stmt
// - R429 (component-decl) -> component-name [(component-array-spec)]
//                            [*char-length] [= initialization-expr]
//
// Section 5.1 (Type declaration statements):
// - R501 (type-declaration-stmt) -> type-spec [[, attr-spec]... ::]
//                                   entity-decl-list
// - R504 (entity-decl) -> object-name [(array-spec)] [*char-length]
//                         [= initialization-expr]
//
// Default initialization allows derived type components to have default
// values that are used when no explicit value is provided.

// F95 type declaration (Section 5.1, R501)
type_declaration_stmt_f95
    : type_spec_f95 (COMMA attr_spec_f95)* DOUBLE_COLON? entity_decl_list_f95
    ;

// Entity declaration with default initialization (Section 5.1, R504)
entity_decl_f95
    : IDENTIFIER (LPAREN array_spec_f95 RPAREN)? (MULTIPLY char_length)?
      (ASSIGN initialization_expr)?
    ;

entity_decl_list_f95
    : entity_decl_f95 (COMMA entity_decl_f95)*
    ;

// ====================================================================
// INITIALIZATION EXPRESSIONS (ISO/IEC 1539-1:1997 Section 7.1.6)
// ====================================================================
//
// Initialization expressions are a restricted subset of expressions used
// in type declarations and data initialization contexts. Unlike general
// expressions, they must be evaluable at compile-time or with compile-time
// known values.
//
// SEMANTIC CONSTRAINTS (NOT enforced by grammar):
// =================================================
// Per ISO/IEC 1539-1:1997 Section 7.1.6, an initialization expression must be:
//
// 1. A **constant expression** (Section 7.1.6.1), which is an expression
//    containing only:
//    - Literal constants (integer, real, complex, logical, character)
//    - Named constants (parameters)
//    - Intrinsic operators applied to constant expressions
//    - Certain intrinsic functions with constant arguments:
//      * BIT_SIZE, KIND, LEN, SELECTED_INT_KIND, SELECTED_REAL_KIND
//      * TRIM, REPEAT, RESHAPE, TRANSFER (with constant arguments)
//
// 2. A **structure constructor** where each component is an initialization
//    expression (recursively)
//
// 3. An **array constructor** where each ac-value is an initialization
//    expression
//
// RESTRICTIONS on initialization expressions (Section 7.1.6):
// ============================================================
// An initialization expression CANNOT contain:
// - References to variables (local or host-associated)
// - References to non-constant functions
// - References to array elements with non-constant subscripts
// - References to character substrings with non-constant bounds
// - Implicit type conversions via function references
// - DO loop variables or implied-DO index variables with non-constant bounds
//
// SCOPE OF APPLICATION:
// =======================
// Initialization expressions appear in these contexts:
//
// 1. **Entity declarations with initialization** (Section 5.1, R504):
//    - `INTEGER :: x = 42`           (constant - VALID)
//    - `INTEGER :: y = x + 1`        (references variable x - INVALID)
//    - `REAL :: r = compute()`       (non-constant function - INVALID)
//
// 2. **Derived type component initialization** (Section 4.4.1, R429):
//    - `TYPE :: point_t`
//    -   `REAL :: x = 0.0`           (constant - VALID)
//    -   `REAL :: y = get_y()`       (function call - INVALID)
//    - `END TYPE`
//
// 3. **Array constructor with initialization** (Section 4.5, R464):
//    - `REAL :: arr(:) = [1.0, 2.0, 3.0]`  (constants - VALID)
//    - `REAL :: arr(:) = [1.0, x, 3.0]`    (variable x - INVALID)
//
// STANDARD COMPLIANCE STATUS:
// =============================
// **NON-COMPLIANT** with ISO/IEC 1539-1:1997 Section 7.1.6
//
// The grammar rule below accepts ANY expr_f95, which violates the standard's
// requirement that only constant expressions and structure constructors with
// constant components are allowed. This is a **semantic gap** that cannot be
// fully enforced by ANTLR grammar alone, as it requires:
//
// - Constant expression evaluation
// - Symbol table analysis to detect variable references
// - Type and rank analysis for structure constructors
// - Intrinsic function classification
//
// FUTURE WORK:
// =============
// A future semantic analyzer must implement:
//
// 1. **Constant expression validator** (Section 7.1.6.1):
//    - Detect and reject variable references in initialization context
//    - Detect and reject non-constant function calls
//    - Evaluate constant expressions where possible
//
// 2. **Initialization expression classifier**:
//    - Distinguish between constant expressions, structure constructors,
//      and array constructors
//    - Validate that nested components are also initialization expressions
//
// 3. **Compile-time diagnostics**:
//    - Clear error messages for invalid initializers
//    - Suggest valid alternatives (e.g., "use parameter instead")
//    - Track initialization context for better error reporting
//
// VIOLATION EXAMPLE (accepted by grammar, violates standard):
// =============================================================
// This code is syntactically valid but semantically non-compliant:
//
//   program bad_initializers
//     implicit none
//     integer :: n = 10
//     integer :: arr(10)
//     integer, parameter :: const = 5
//
//     ! VALID - constant expression
//     integer :: x = const + 2
//
//     ! INVALID - references variable n
//     integer :: y = n + 1
//
//     ! INVALID - function call
//     integer :: z = compute()
//
//     ! INVALID - runtime variable in structure
//     type :: point_t
//       real :: x = n * 2.0   ! References variable n!
//     end type
//
//   contains
//     integer function compute()
//       compute = 42
//     end function
//   end program bad_initializers
//
// All of the above are parsed successfully by the grammar but violate
// Section 7.1.6 and would be rejected by a standards-conforming compiler.

// Initialization expression (Section 7.1.6)
// NOTE: Grammar accepts any expr_f95, but semantic constraint requires
// initialization expressions to be constant (Section 7.1.6.1).
// A semantic analyzer must validate this constraint.
// See comprehensive documentation above for details.
initialization_expr
    : expr_f95
    ;

// Derived type definition (Section 4.4.1, R424)
derived_type_def_f95
    : derived_type_stmt component_def_stmt_f95* end_type_stmt
    ;

// Component definition statement (Section 4.4.1, R425/R426)
component_def_stmt_f95
    : type_declaration_stmt_f95   // Can have default initialization
    | private_sequence_stmt
    ;

// ====================================================================
// ENHANCED PROCEDURE SPECIFICATIONS (ISO/IEC 1539-1:1997 Section 12.6)
// ====================================================================
//
// Fortran 95 introduces PURE and ELEMENTAL procedure prefixes.
// These are NOT available in Fortran 90.
//
// ISO/IEC 1539-1:1997 Section 12.6 defines:
// - PURE procedures (Section 12.6): Have no side effects, can be used in
//   specification expressions and FORALL constructs.
// - ELEMENTAL procedures (Section 12.6): Scalar procedures that can be
//   applied element-wise to array arguments. ELEMENTAL implies PURE.
//
// Syntax (Section 12.5.2, R1216-R1217):
// - R1216 (function-subprogram) -> function-stmt [specification-part]
//                                  [execution-part] [internal-subprogram-part]
//                                  end-function-stmt
// - R1217 (function-stmt) -> [prefix] FUNCTION function-name (...)
// - R1224 (prefix) -> prefix-spec [prefix-spec]...
// - R1225 (prefix-spec) -> RECURSIVE | PURE | ELEMENTAL | type-spec

// ====================================================================
// FORTRAN 95 PROCEDURE PREFIXES - PURE/ELEMENTAL SEMANTICS
// ====================================================================
//
// ISO/IEC 1539-1:1997 Section 12.6 defines PURE and ELEMENTAL procedures.
// These are major F95 enhancements with extensive semantic restrictions.
//
// NOTE: The grammar below ONLY accepts the syntactic forms. Semantic
// restrictions (no side effects, no I/O, no global state modification, etc.)
// are NOT enforced by the grammar and require a semantic analyzer.
// See issue #425 for comprehensive requirements documentation.
//
// PURE PROCEDURES (ISO/IEC 1539-1:1997 Section 12.6.1)
// =====================================================
//
// A PURE procedure must NOT:
//
// 1. Modify global state (module variables, host-associated variables,
//    common block variables)
//
// 2. Violate argument usage restrictions:
//    - All dummy arguments without INTENT(IN) must have explicit INTENT(OUT/INOUT)
//    - Cannot modify INTENT(IN) arguments
//    - Pointer dummy arguments must have INTENT(IN)
//
// 3. Perform I/O or external interactions:
//    - No I/O statements (READ, WRITE, PRINT, OPEN, CLOSE, etc.)
//    - No STOP statement
//    - Can only call other PURE procedures
//
// 4. Violate local variable restrictions:
//    - Local variables with SAVE attribute cannot be modified
//    - Local pointers cannot be associated with non-local targets
//
// ELEMENTAL PROCEDURES (ISO/IEC 1539-1:1997 Section 12.6.2)
// ===========================================================
//
// An ELEMENTAL procedure must be PURE and additionally:
//
// 1. All dummy arguments must be SCALAR (not arrays)
// 2. Function result must be SCALAR
// 3. Cannot have alternate returns
// 4. ELEMENTAL implies PURE (all PURE restrictions apply)
//
// USES OF PURE FUNCTIONS (ISO/IEC 1539-1:1997 Section 12.6.3)
// ============================================================
//
// PURE functions can appear in:
// - Specification expressions (array bounds, character lengths)
// - FORALL constructs and statements
// - WHERE constructs
//
// These contexts require compile-time evaluation or parallelization,
// which pure functions enable.
//
// GRAMMAR STATUS
// ===============
//
// This grammar accepts PURE and ELEMENTAL procedure prefixes but does
// NOT enforce any of the above semantic constraints. Compliance with
// Section 12.6 restrictions requires a separate semantic analysis pass.
//
// Non-Compliant Example (accepted by grammar but violates standard):
//
//   pure function bad_pure() result(y)
//     integer, save :: counter = 0    ! VIOLATION: modifies SAVE variable
//     print *, 'Computing'             ! VIOLATION: I/O statement
//     counter = counter + 1             ! VIOLATION: modifies SAVE
//     y = counter
//   end function
//
// Issue Tracking
// ===============
// See GitHub issue #425 for comprehensive semantic requirements that
// must be implemented in a future semantic analyzer.
//
// F95 procedure prefix (Section 12.5.2, R1224)
prefix_f95
    : prefix_spec_f95+
    ;

// Prefix specification (Section 12.5.2, R1225)
prefix_spec_f95
    : RECURSIVE                     // F90 recursive procedures (12.5.2.1)
    | PURE                          // F95 pure (12.6) - see sec 12.6 constraints
    | ELEMENTAL                     // F95 elemental (12.6) - see section 12.6
    | type_spec_f95                 // Function return type (5.1)
    ;

// Function statement (Section 12.5.2, R1217)
//
// SEMANTIC CONSTRAINTS (not enforced by grammar):
// If prefixed with PURE or ELEMENTAL, see restrictions in Section 12.6
// and comments above.
function_stmt_f95
    : (prefix_f95)? FUNCTION IDENTIFIER LPAREN dummy_arg_name_list? RPAREN (suffix)?
        NEWLINE?
    ;

// Subroutine statement (Section 12.5.3, R1221)
//
// SEMANTIC CONSTRAINTS (not enforced by grammar):
// If prefixed with PURE or ELEMENTAL, see restrictions in Section 12.6
// and comments above.
subroutine_stmt_f95
    : (prefix_f95)? SUBROUTINE IDENTIFIER (LPAREN dummy_arg_name_list? RPAREN)? NEWLINE?
    ;

// ====================================================================
// F95 EXPRESSIONS (ISO/IEC 1539-1:1997 Section 7)
// ====================================================================
//
// ISO/IEC 1539-1:1997 Section 7 defines the expression syntax.
// - R702 (expr) -> [expr defined-binary-op] level-5-expr
// - Section 7.1 covers expression formation rules
// - Section 7.2 covers primaries (literals, variables, function refs, etc.)
//
// The expression rules follow F90 with F95 enhancements for FORALL and WHERE.

// F95 expressions (Section 7.1, R702)
expr_f95
    : expr_f95 DOT_EQV expr_f95                          # EquivalenceExprF95
    | expr_f95 DOT_NEQV expr_f95                         # NotEquivalenceExprF95
    | expr_f95 DOT_OR expr_f95                           # LogicalOrExprF95
    | expr_f95 DOT_AND expr_f95                          # LogicalAndExprF95
    | DOT_NOT expr_f95                                   # LogicalNotExprF95
    | expr_f95 (DOT_EQ | EQ_OP) expr_f95                 # EqualExprF95
    | expr_f95 (DOT_NE | NE_OP) expr_f95                 # NotEqualExprF95  
    | expr_f95 (DOT_LT | LT_OP) expr_f95                 # LessExprF95
    | expr_f95 (DOT_LE | LE_OP) expr_f95                 # LessEqualExprF95
    | expr_f95 (DOT_GT | GT_OP) expr_f95                 # GreaterExprF95
    | expr_f95 (DOT_GE | GE_OP) expr_f95                 # GreaterEqualExprF95
    | expr_f95 CONCAT expr_f95                           # ConcatExprF95
    | expr_f95 POWER expr_f95                            # PowerExprF95
    | expr_f95 (MULTIPLY | SLASH) expr_f95              # MultDivExprF95
    | expr_f95 (PLUS | MINUS) expr_f95                   # AddSubExprF95
    | (PLUS | MINUS) expr_f95                            # UnaryExprF95
    | primary_f95                                        # PrimaryExprF95
    ;

// F95 primary expressions (Section 7.2, R701)
// primary -> constant | constant-subobject | variable | array-constructor |
//            structure-constructor | function-reference | (expr)
primary_f95
    : literal_f95
    | variable_f95
    | function_reference_f95
    | array_constructor_f95
    | structure_constructor_f95
    | LPAREN expr_f95 RPAREN
    ;

// F95 variables (ISO/IEC 1539-1:1997 Section 6.1, R601)
// variable -> designator (scalar-variable | array-element | array-section | substring)
// Uses identifier_or_keyword_f95 to allow intrinsic procedure names
variable_f95
    : identifier_or_keyword_f95 (substring_range_f95)?
    | identifier_or_keyword_f95 LPAREN section_subscript_list_f95 RPAREN
      (substring_range_f95)?
    | variable_f95 PERCENT identifier_or_keyword_f95 (substring_range_f95)?
    | variable_f95 LPAREN section_subscript_list_f95 RPAREN (substring_range_f95)?
    ;

section_subscript_list_f95
    : section_subscript_f95 (COMMA section_subscript_f95)*
    ;

section_subscript_f95
    : expr_f95
    | subscript_triplet_f95
    ;

subscript_triplet_f95
    : expr_f95? COLON expr_f95? (COLON expr_f95)?
    ;

substring_range_f95
    : LPAREN expr_f95? COLON expr_f95? RPAREN
    ;

// F95 logical expressions (enhanced)
logical_expr_f95
    : expr_f95                      // Must be logical expression
    ;

// ====================================================================
// F95 LITERALS (ISO/IEC 1539-1:1997 Section 4)
// ====================================================================
//
// Literals (constants) are defined in Section 4 of ISO/IEC 1539-1:1997.
// - R402 (literal-constant) -> int-literal | real-literal | complex-literal |
//                              logical-literal | char-literal | boz-literal
// - Section 4.3 covers integer and real literals
// - Section 4.4.4 covers boz-literal-constants (binary/octal/hex)

// F95 literals (Section 4.3, R402)
literal_f95
    : INTEGER_LITERAL_KIND          // Integer with kind (123_int32)
    | INTEGER_LITERAL               // Traditional integer literal
    | LABEL                         // Accept LABEL as integer (token precedence issue)
    | REAL_LITERAL_KIND             // Real with kind (3.14_real64)
    | REAL_LITERAL                  // Traditional real literal
    | DOUBLE_QUOTE_STRING           // Double-quoted string
    | SINGLE_QUOTE_STRING           // Single-quoted string
    | logical_literal_f95           // Enhanced logical literals
    | boz_literal_constant          // Binary/octal/hex literals
    ;

logical_literal_f95
    : DOT_TRUE
    | DOT_FALSE
    ;

// ====================================================================
// F95 ARRAY OPERATIONS (ISO/IEC 1539-1:1997 Section 4.5)
// ====================================================================
//
// Array constructors are defined in Section 4.5 of ISO/IEC 1539-1:1997.
//
// ISO/IEC 1539-1:1997 Section 4.5 defines:
// - R431 (array-constructor) -> (/ ac-spec /)
// - R432 (ac-spec) -> type-spec :: | [type-spec ::] ac-value-list
// - R434 (ac-value) -> expr | ac-implied-do
// - R435 (ac-implied-do) -> (ac-value-list, ac-implied-do-control)
//
// NOTE: Only (/ ... /) form is standard Fortran 95.
// Square bracket syntax [ ... ] is a Fortran 2003 feature (ISO/IEC 1539-1:2004).

// Array constructor (Section 4.5, R431)
array_constructor_f95
    : LPAREN SLASH ac_spec_f95 SLASH RPAREN
    ;

// Array constructor specification (Section 4.5, R432)
ac_spec_f95
    : ac_value_list_f95
    ;

// Array constructor value list (Section 4.5, R433)
ac_value_list_f95
    : ac_value_f95 (COMMA ac_value_f95)*
    ;

// Array constructor value (Section 4.5, R434)
ac_value_f95
    : expr_f95
    | ac_implied_do_f95
    ;

// Array constructor implied-DO (Section 4.5, R435)
ac_implied_do_f95
    : LPAREN ac_value_list_f95 COMMA do_variable EQUALS expr_f95 COMMA expr_f95
      (COMMA expr_f95)? RPAREN
    ;

// Structure constructor (Section 4.4.6, R430)
// Fortran 95 allows default initialization for missing components
structure_constructor_f95
    : type_name LPAREN component_spec_list_f95? RPAREN
    ;

// Component specification list (Section 4.4.6)
component_spec_list_f95
    : component_spec_f95 (COMMA component_spec_f95)*
    ;

// Component specification - allows keyword arguments and positional
component_spec_f95
    : identifier_or_keyword_f95 EQUALS expr_f95
    | expr_f95
    ;

// ====================================================================
// ENHANCED TYPE AND ARRAY SPECIFICATIONS (ISO/IEC 1539-1:1997 Section 5)
// ====================================================================
//
// Type specifications are defined in Section 5 of ISO/IEC 1539-1:1997.
//
// ISO/IEC 1539-1:1997 Section 5 defines:
// - R502 (type-spec) -> type-spec | derived-type-spec
// - R503 (attr-spec) -> access-spec | ALLOCATABLE | DIMENSION | ...
// - R509 (array-spec) -> explicit-shape-spec-list | assumed-shape-spec-list |
//                        deferred-shape-spec-list | assumed-size-spec

// Type specification (Section 5.1, R502)
type_spec_f95
    : intrinsic_type_spec_f95
    | derived_type_spec_f95
    ;

// Intrinsic type specification (Section 5.1, R502)
intrinsic_type_spec_f95
    : INTEGER (kind_selector_f95)?
    | REAL (kind_selector_f95)?
    | DOUBLE PRECISION
    | COMPLEX (kind_selector_f95)?
    | LOGICAL (kind_selector_f95)?
    | CHARACTER (char_selector_f95)?
    ;

// Derived type specification (Section 5.1, R502)
derived_type_spec_f95
    : TYPE LPAREN type_name RPAREN
    ;

// Kind selector (Section 5.1, R506)
kind_selector_f95
    : LPAREN (KIND ASSIGN)? expr_f95 RPAREN
    ;

// Character selector (Section 5.1, R507)
char_selector_f95
    : LPAREN (LEN ASSIGN)? expr_f95 (COMMA (KIND ASSIGN)? expr_f95)? RPAREN
    | LPAREN expr_f95 RPAREN
    ;

// Array specification (Section 5.1.2, R509)
array_spec_f95
    : explicit_shape_spec_list_f95
    | assumed_shape_spec_list_f95
    | deferred_shape_spec_list_f95
    | assumed_size_spec_f95
    ;

// Explicit shape specification list (Section 5.1.2.1, R510)
explicit_shape_spec_list_f95
    : explicit_shape_spec_f95 (COMMA explicit_shape_spec_f95)*
    ;

// Explicit shape specification (Section 5.1.2.1, R511)
explicit_shape_spec_f95
    : expr_f95 (COLON expr_f95)?
    ;

// Assumed shape specification list (Section 5.1.2.2, R512)
assumed_shape_spec_list_f95
    : assumed_shape_spec_f95 (COMMA assumed_shape_spec_f95)*
    ;

// Assumed shape specification (Section 5.1.2.2, R513)
assumed_shape_spec_f95
    : COLON
    | expr_f95 COLON
    ;

// Deferred shape specification list (Section 5.1.2.3, R515)
deferred_shape_spec_list_f95
    : deferred_shape_spec_f95 (COMMA deferred_shape_spec_f95)*
    ;

// Deferred shape specification (Section 5.1.2.3, R516)
deferred_shape_spec_f95
    : COLON
    ;

// Assumed size specification (Section 5.1.2.4, R517)
assumed_size_spec_f95
    : (explicit_shape_spec_f95 COMMA)* MULTIPLY
    ;

// Attribute specification (Section 5.1, R503)
attr_spec_f95
    : PARAMETER
    | DIMENSION LPAREN array_spec_f95 RPAREN
    | ALLOCATABLE
    | POINTER
    | TARGET
    | PUBLIC
    | PRIVATE
    | INTENT LPAREN intent_spec RPAREN
    | OPTIONAL
    | EXTERNAL
    | INTRINSIC
    | SAVE
    ;

// ====================================================================
// ENHANCED PROGRAM CONSTRUCTS (ISO/IEC 1539-1:1997 Section 8)
// ====================================================================
//
// Executable constructs are defined in Section 8 of ISO/IEC 1539-1:1997.
//
// ISO/IEC 1539-1:1997 Section 8 defines:
// - R214 (executable-construct) -> action-stmt | construct
// - R215 (action-stmt) -> assignment-stmt | call-stmt | goto-stmt | ...
// - R216 (construct) -> if-construct | case-construct | do-construct |
//                       where-construct | forall-construct

// Executable construct (Section 8.1, R214)
executable_construct_f95
    : executable_stmt_f95
    | construct_f95
    ;

// Executable statement (Section 8.1, R215 action-stmt)
executable_stmt_f95
    : assignment_stmt_f95
    | pointer_assignment_stmt
    | call_stmt_f95
    | return_stmt
    | stop_stmt
    | cycle_stmt
    | exit_stmt
    | goto_stmt
    | arithmetic_if_stmt
    | continue_stmt
    | read_stmt_f95
    | write_stmt_f95
    | print_stmt_f90              // F90 print (inherited)
    | open_stmt_f90                  // Section 9.3.1 - File connection (R904)
    | close_stmt_f90                 // Section 9.3.2 - File disconnection (R908)
    | inquire_stmt_f90               // Section 9.5.3 - File inquiry (R929)
    | backspace_stmt_f90             // Section 9.4.1 - File positioning (R923)
    | endfile_stmt_f90               // Section 9.4.2 - File positioning (R924)
    | rewind_stmt_f90                // Section 9.4.3 - File positioning (R925)
    | allocate_stmt
    | deallocate_stmt
    | nullify_stmt
    | where_stmt_f95
    | forall_stmt                 // F95 addition (Section 7.5.4)
    | entry_stmt_f90              // F95 ENTRY (Section 12.5.4, obsolescent)
    ;

// Construct (Section 8.1, R216)
construct_f95
    : if_construct
    | select_case_construct
    | do_construct_f95
    | where_construct_f95         // F95 enhanced (Section 7.5.3)
    | forall_construct            // F95 addition (Section 7.5.4)
    ;

// DO construct (Section 8.1.4, inherits from F90)
do_construct_f95
    : do_construct_f90
    ;

// Assignment statement (Section 7.5.1, R735)
assignment_stmt_f95
    : variable_f95 EQUALS expr_f95
    ;

// CALL statement (Section 12.4.2, R1210)
call_stmt_f95
    : CALL procedure_designator_f95 (LPAREN actual_arg_spec_list_f95? RPAREN)?
    ;

// Procedure designator (Section 12.3, R1206)
// Uses identifier_or_keyword_f95 to allow intrinsic procedure names
procedure_designator_f95
    : identifier_or_keyword_f95
    | variable_f95
    ;

// Actual argument specification list (Section 12.4.1)
actual_arg_spec_list_f95
    : actual_arg_spec_f95 (COMMA actual_arg_spec_f95)*
    ;

// Actual argument specification (Section 12.4.1, R1207)
// actual-arg-spec -> [keyword =] actual-arg
actual_arg_spec_f95
    : identifier_or_keyword_f95 EQUALS expr_f95
    | expr_f95
    | MULTIPLY identifier_or_keyword_f95
    ;

// ====================================================================
// ENHANCED I/O STATEMENTS (ISO/IEC 1539-1:1997 Section 9)
// ====================================================================
//
// I/O statements are defined in Section 9 of ISO/IEC 1539-1:1997.
//
// ISO/IEC 1539-1:1997 Section 9 defines:
// - R908 (read-stmt) -> READ (io-control-spec-list) [input-item-list] |
//                       READ format [, input-item-list]
// - R909 (write-stmt) -> WRITE (io-control-spec-list) [output-item-list]
// - R912 (io-control-spec) -> [UNIT=]io-unit | [FMT=]format | ...

// READ statement (Section 9.4, R908)
read_stmt_f95
    : READ LPAREN io_control_spec_list_f95 RPAREN (input_item_list_f95)?
    | READ namelist_name
    | READ format (COMMA input_item_list_f95)?
    ;

// WRITE statement (Section 9.4, R909)
write_stmt_f95
    : WRITE LPAREN io_control_spec_list_f95 RPAREN (output_item_list_f95)?
    | WRITE namelist_name
    ;

// I/O control specification list (Section 9.4)
io_control_spec_list_f95
    : io_control_spec_f95 (COMMA io_control_spec_f95)*
    ;

// I/O control specification (Section 9.4, R912)
io_control_spec_f95
    : UNIT EQUALS expr_f95
    | FMT EQUALS format_spec_f95
    | IOSTAT EQUALS variable_f95
    | ERR EQUALS label
    | END EQUALS label
    | EOR EQUALS label
    | ADVANCE EQUALS expr_f95
    | SIZE EQUALS variable_f95
    | REC EQUALS expr_f95
    | expr_f95
    ;

// Format specification (Section 9.4)
format_spec_f95
    : expr_f95
    | MULTIPLY
    | label
    | namelist_name
    ;

// Input item list (Section 9.4.1, R914)
input_item_list_f95
    : input_item_f95 (COMMA input_item_f95)*
    ;

// Input item (Section 9.4.1, R915)
input_item_f95
    : variable_f95
    | io_implied_do_f95
    ;

// Output item list (Section 9.4.2, R916)
output_item_list_f95
    : output_item_f95 (COMMA output_item_f95)*
    ;

// Output item (Section 9.4.2, R917)
output_item_f95
    : expr_f95
    | io_implied_do_f95
    ;

// I/O implied-DO (Section 9.4, R918)
io_implied_do_f95
    : LPAREN output_item_list_f95 COMMA do_variable EQUALS expr_f95 COMMA expr_f95
      (COMMA expr_f95)? RPAREN
    ;

// Function reference (Section 12.4, R1209)
// function-reference -> function-name ( [actual-arg-spec-list] )
// Uses identifier_or_keyword_f95 to allow intrinsic procedure names
function_reference_f95
    : identifier_or_keyword_f95 LPAREN actual_arg_spec_list_f95? RPAREN
    ;

// ====================================================================
// FORTRAN 95 PARSER NOTES
// ====================================================================
//
// This parser extends the Fortran 90 parser with additional rules for
// the F95 features modelled here (FORALL, WHERE enhancements and
// related intrinsic procedures).
//
// The list below outlines the intended coverage of this grammar, based
// on the implementation rather than a formal conformance audit.
//
// MAJOR F95 FEATURES TARGETED (with ISO section references):
// ✅ FORALL constructs and statements (Section 7.5.4)
// ✅ Enhanced WHERE constructs with multiple ELSEWHERE (Section 7.5.3)
// ✅ PURE and ELEMENTAL procedure prefixes (Section 12.6)
// ✅ Derived type default initialization (Section 4.4.1)
// ✅ Enhanced pointer association (Section 6.3.1)
// ✅ Extended intrinsic function support (Section 13)
// ✅ All F90 features through inheritance
//
// BACKWARD COMPATIBILITY:
// ✅ Complete F90 compatibility through inheritance
// ✅ F77 compatibility through inheritance chain
// ✅ Legacy constructs fully supported
//
// FORWARD COMPATIBILITY:
// ✅ Foundation for F2003 object-oriented features
// ✅ Ready for parameterized derived types
// ✅ Prepared for C interoperability
// ✅ Extension points for IEEE arithmetic
//
// ISO/IEC 1539-1:1997 SPEC-GRAMMAR MAPPING (PARSER):
// Section 4 (Types)        -> type_spec_f95, literal_f95, derived_type_def_f95
// Section 5 (Declarations) -> type_declaration_stmt_f95, array_spec_f95, attr_spec_f95
// Section 6 (Variables)    -> variable_f95, section_subscript_f95
// Section 7 (Expressions)  -> expr_f95, primary_f95, forall_*, where_*_f95
// Section 8 (Constructs)   -> executable_construct_f95, construct_f95
// Section 9 (I/O)          -> read_stmt_f95, write_stmt_f95, io_control_spec_f95
// Section 11 (Programs)    -> program_unit_f95, main_program_f95, module_f95
// Section 12 (Procedures)  -> function_stmt_f95, subroutine_stmt_f95, prefix_f95
// Section 13 (Intrinsics)  -> identifier_or_keyword_f95 (token aliasing)
//
// J3/98-114 (Request for Interpretation):
// - Dummy argument and function result characteristics are semantic
//   constraints, not enforced by this grammar (see docs/fortran_95_audit.md)
//
// VALIDATION NOTES:
// ✅ Ready for targeted testing with representative F95 code
// ✅ FORALL and enhanced WHERE constructs are implemented
// ✅ Default initialization and PURE/ELEMENTAL prefixes are supported
//
// This parser extends the F90 grammar with the F95 constructs listed
// above and serves as the bridge between F90 and F2003 in the
// inheritance chain. It is not a formal claim of complete
// ISO/IEC 1539-1:1997 coverage.
//
// ====================================================================
