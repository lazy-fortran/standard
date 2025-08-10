/**
 * Fortran 95 (1995) - Enhanced Array Processing and FORALL
 * Inheriting from Fortran90 with focused enhancements
 * 
 * KEY F95 ENHANCEMENTS per ISO/IEC 1539-1:1997:
 * - FORALL construct for array operations (enhanced from F90)
 * - WHERE construct improvements with ELSEWHERE
 * - Enhanced intrinsic procedures
 * - PURE and ELEMENTAL procedure capabilities  
 * - Improved array handling
 * - Default initialization for derived types
 * - NULL intrinsic for pointer initialization
 * - Alternative array constructor syntax [...]
 * - Minor corrections and clarifications to F90
 */

const fortran90 = require('../Fortran90/grammar.js');

function grammar(base, config) {
  if (!config) {
    config = base;
    base = undefined;
  }
  return config;
}

module.exports = grammar(fortran90, {
  name: 'Fortran95',

  // Copy base extras
  extras: $ => [
    /\s+/,
    $.comment,
    $.free_form_comment
  ],

  conflicts: $ => [
    [$.simple_variable, $.function_name],
    [$.simple_variable, $.subroutine_name],
    [$.variable, $.logical_variable],
    [$.constant, $.logical_primary],
    [$.expression, $.if_then_statement],
    [$.subscripted_variable, $.character_variable],
    [$.variable, $.logical_variable, $.character_variable],
    [$.constant, $.character_expression],
    [$.module_name, $.simple_variable],
    [$.type_name, $.simple_variable],
    [$.component_name, $.simple_variable]
  ],

  // Copy base precedences
  precedences: $ => [
    ['power', 'mult', 'add', 'concat', 'relop', 'not', 'and', 'or']
  ],

  word: $ => $.simple_variable,

  inline: $ => [
    $.label,
    $.subscript,
    $.letter_range,
    $.component_name
  ],

  rules: {
    // ========================================================================
    // F95 ENHANCED FORALL - More powerful than F90
    // ========================================================================

    // Enhanced FORALL with F95 improvements
    forall_construct: $ => choice(
      $.forall_stmt,
      seq(
        $.forall_construct_stmt,
        repeat($.forall_body_construct),
        $.end_forall_stmt
      )
    ),

    forall_stmt: $ => seq(
      'FORALL',
      '(',
      $.forall_header,
      ')',
      $.forall_assignment_stmt
    ),

    forall_construct_stmt: $ => seq(
      optional(seq($.construct_name, ':')),
      'FORALL',
      '(',
      $.forall_header,
      ')'
    ),

    forall_header: $ => seq(
      $.index_spec_list,
      optional(seq(',', $.mask_expr))
    ),

    index_spec_list: $ => sep1($.index_spec, ','),
    
    // Enhanced index specification for F95
    index_spec: $ => seq(
      $.index_name,
      '=',
      $.subscript,
      ':',
      $.subscript,
      optional(seq(':', $.stride))
    ),

    stride: $ => $.subscript,

    forall_assignment_stmt: $ => choice(
      $.assignment_stmt,
      $.pointer_assignment_stmt
    ),

    forall_body_construct: $ => choice(
      $.forall_assignment_stmt,
      $.forall_construct,
      $.where_construct,
      $.forall_stmt,
      $.where_stmt
    ),

    end_forall_stmt: $ => seq('END', 'FORALL', optional($.construct_name)),

    // ========================================================================
    // F95 ENHANCED WHERE CONSTRUCT
    // ========================================================================

    // Enhanced WHERE with better mask handling
    where_construct: $ => choice(
      $.where_stmt,
      seq(
        $.where_construct_stmt,
        repeat($.where_body_construct),
        repeat($.masked_elsewhere_part),
        optional($.elsewhere_part),
        $.end_where_stmt
      )
    ),

    where_construct_stmt: $ => seq(
      optional(seq($.construct_name, ':')),
      'WHERE',
      '(',
      $.mask_expr,
      ')'
    ),

    where_body_construct: $ => choice(
      $.assignment_stmt,
      $.where_stmt,
      $.where_construct
    ),

    // F95 enhancement: masked ELSEWHERE
    masked_elsewhere_part: $ => seq(
      $.masked_elsewhere_stmt,
      repeat($.where_body_construct)
    ),

    masked_elsewhere_stmt: $ => seq(
      'ELSEWHERE',
      '(',
      $.mask_expr,
      ')',
      optional($.construct_name)
    ),

    elsewhere_part: $ => seq(
      $.elsewhere_stmt,
      repeat($.where_body_construct)
    ),

    elsewhere_stmt: $ => seq(
      'ELSEWHERE',
      optional($.construct_name)
    ),

    end_where_stmt: $ => seq('END', 'WHERE', optional($.construct_name)),

    // ========================================================================
    // F95 ENHANCED DERIVED TYPES - Default initialization
    // ========================================================================

    // Enhanced component definition with default initialization
    component_def_stmt: $ => seq(
      $.declaration_type_spec,
      optional(seq(',', $.component_attr_spec_list)),
      '::',
      $.component_decl_list
    ),

    component_decl: $ => seq(
      $.component_name,
      optional($.array_spec),
      optional($.component_initialization)  // F95: Default initialization
    ),

    // F95: Component default initialization
    component_initialization: $ => choice(
      seq('=', $.initialization_expr),
      seq('=>', $.null_init)
    ),

    null_init: $ => 'NULL',

    // ========================================================================
    // F95 ENHANCED PURE AND ELEMENTAL PROCEDURES
    // ========================================================================

    // Enhanced prefix with F95 procedure attributes
    prefix: $ => choice(
      'RECURSIVE',
      'PURE',
      'ELEMENTAL',                 // F95: Enhanced elemental procedures
      seq('PURE', 'RECURSIVE'),    // F95: Combined attributes
      seq('ELEMENTAL', 'PURE')     // F95: Elemental procedures are implicitly pure
    ),

    // ========================================================================
    // F95 ENHANCED INTRINSIC PROCEDURES
    // ========================================================================

    // Enhanced intrinsic function references with F95 additions
    intrinsic_function_name: $ => choice(
      // Array intrinsics enhanced in F95
      'MAXLOC',
      'MINLOC',
      'ALL',
      'ANY',
      'COUNT',
      'PRODUCT',
      'SUM',
      'MERGE',
      'PACK',
      'UNPACK',
      'SPREAD',
      'RESHAPE',
      'CSHIFT',
      'EOSHIFT',
      'TRANSPOSE',
      'MATMUL',                    // F95: Matrix multiplication
      // Character intrinsics
      'ADJUSTL',                   // F95: Left adjust
      'ADJUSTR',                   // F95: Right adjust
      // Bit manipulation (enhanced in F95)
      'ISHFT',
      'ISHFTC',
      'IAND',
      'IOR',
      'IEOR',
      'NOT',
      // Other F95 additions
      'NULL',                      // F95: Null pointer
      'ASSOCIATED',                // F95: Pointer association
      'PRESENT',                   // F95: Optional argument presence
      'HUGE',
      'TINY',
      'PRECISION',
      'RANGE',
      'DIGITS',
      'EPSILON',
      'MAXEXPONENT',
      'MINEXPONENT',
      'RADIX',
      'SCALE',
      'SET_EXPONENT',
      'SPACING',
      'FRACTION',
      'EXPONENT',
      'NEAREST',
      'RRSPACING'
    ),

    // ========================================================================
    // F95 ENHANCED ARRAY CONSTRUCTOR
    // ========================================================================

    // Enhanced array constructor with F95 syntax improvements
    array_constructor: $ => choice(
      seq('(/', $.ac_spec, '/)'),         // F90 syntax
      seq('[', $.ac_spec, ']')            // F95 alternative syntax
    ),

    ac_spec: $ => choice(
      $.type_spec,                        // F95: Type specification
      $.ac_value_list
    ),

    ac_value: $ => choice(
      $.expression,
      $.ac_implied_do
    ),

    ac_implied_do: $ => seq(
      '(',
      $.ac_value_list,
      ',',
      $.implied_do_variable,
      '=',
      $.scalar_int_expr,
      ',',
      $.scalar_int_expr,
      optional(seq(',', $.scalar_int_expr)),
      ')'
    ),

    implied_do_variable: $ => $.simple_variable,

    // ========================================================================
    // F95 ENHANCED SPECIFICATION STATEMENTS
    // ========================================================================

    // Enhanced declaration statements with F95 features
    attr_spec: $ => choice(
      'PARAMETER',
      'ALLOCATABLE',
      seq('DIMENSION', '(', $.array_spec, ')'),
      'EXTERNAL',
      seq('INTENT', '(', $.intent_spec, ')'),
      'INTRINSIC',
      'OPTIONAL',
      'POINTER',
      'PRIVATE',
      'PUBLIC',
      'SAVE',
      'TARGET'
    ),

    // Enhanced initialization for F95
    initialization: $ => choice(
      seq('=', $.initialization_expr),
      seq('=>', $.null_init)              // F95: Pointer initialization to NULL
    ),

    // ========================================================================
    // F95 ENHANCED NULL INTRINSIC
    // ========================================================================

    // F95 NULL intrinsic for pointer initialization
    null_expr: $ => seq('NULL', '(', optional($.mold), ')'),
    mold: $ => $.expression,

    // Extend primary base to include NULL
    primary_base: $ => choice(
      $.constant,
      $.designator,
      $.array_constructor,
      $.structure_constructor,
      $.function_reference,
      $.null_expr,                        // F95: NULL intrinsic
      seq('(', $.expression, ')')
    ),

    // ========================================================================
    // F95 ENHANCED POINTER ASSIGNMENT
    // ========================================================================

    // Enhanced pointer assignment with NULL support
    pointer_assignment_stmt: $ => seq(
      $.data_pointer_object,
      '=>',
      choice(
        $.data_target,
        $.null_expr                       // F95: Assignment to NULL
      )
    ),

    // ========================================================================
    // F95 ENHANCED ALLOCATION
    // ========================================================================

    // Enhanced ALLOCATE with F95 features
    allocate_stmt: $ => seq(
      'ALLOCATE',
      '(',
      $.allocation_list,
      optional(seq(',', 'STAT', '=', $.stat_variable)),
      ')'
    ),

    allocation: $ => choice(
      seq($.allocate_object, '(', $.allocate_shape_spec_list, ')'),
      $.allocate_object                   // F95: Simple allocation without bounds
    ),

    // ========================================================================
    // F95 MODULE ENHANCEMENTS
    // ========================================================================

    // Enhanced USE statement with F95 improvements
    use_stmt: $ => seq(
      'USE',
      $.module_name,
      optional(seq(',', $.only_option))
    ),

    only_option: $ => choice(
      seq('ONLY', ':', $.only_list),
      $.rename_list
    ),

    only_list: $ => sep1($.generic_spec, ','),

    generic_spec: $ => choice(
      $.generic_name,
      seq('OPERATOR', '(', $.defined_operator, ')'),
      seq('ASSIGNMENT', '(', '=', ')')
    ),

    generic_name: $ => $.simple_variable,
    defined_operator: $ => choice(
      $.defined_unary_op,
      $.defined_binary_op
    ),

    defined_unary_op: $ => seq('.', $.letter_sequence, '.'),
    defined_binary_op: $ => seq('.', $.letter_sequence, '.'),
    letter_sequence: $ => /[A-Za-z]+/,

    rename_list: $ => sep1($.rename, ','),
    rename: $ => choice(
      seq($.local_name, '=>', $.use_name),
      seq('OPERATOR', '(', $.local_defined_operator, ')', '=>', 
          'OPERATOR', '(', $.use_defined_operator, ')')
    ),

    local_defined_operator: $ => $.defined_operator,
    use_defined_operator: $ => $.defined_operator
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}