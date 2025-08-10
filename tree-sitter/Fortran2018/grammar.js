/**
 * Fortran 2018 (2018) - Enhanced Parallel Programming and Error Handling
 * Inheriting from Fortran2008 with teams, events, and error handling
 * 
 * KEY F2018 ENHANCEMENTS:
 * - Teams for hierarchical parallelism
 * - Events and event handling
 * - Enhanced error handling with STOP/ERROR STOP improvements
 * - Collective subroutines for parallel programming
 * - Enhanced C interoperability with assumed-type and assumed-rank
 * - Atomic subroutines and variables
 */

const fortran2008 = require('../Fortran2008/grammar.js');

module.exports = grammar(fortran2008, {
  name: 'Fortran2018',

  // Copy base configuration
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
    [$.component_name, $.simple_variable],
    [$.class_name, $.simple_variable],
    [$.binding_name, $.simple_variable],
    [$.procedure_name, $.simple_variable],
    [$.submodule_name, $.simple_variable],
    [$.coarray_name, $.simple_variable],
    [$.team_name, $.simple_variable],
    [$.event_name, $.simple_variable]
  ],

  precedences: $ => [
    ['power', 'mult', 'add', 'concat', 'relop', 'not', 'and', 'or', 'defined_op']
  ],

  word: $ => $.simple_variable,

  inline: $ => [
    $.label,
    $.subscript,
    $.letter_range,
    $.component_name,
    $.binding_name
  ],

  rules: {
    // ========================================================================
    // F2018 TEAMS - Hierarchical parallelism
    // ========================================================================

    // Enhanced synchronization with team support
    action_stmt: $ => choice(
      $.allocate_stmt,
      $.assignment_stmt,
      $.call_stmt,
      $.continue_stmt,
      $.deallocate_stmt,
      $.exit_stmt,
      $.goto_stmt,
      $.if_stmt,
      $.nullify_stmt,
      $.pointer_assignment_stmt,
      $.return_stmt,
      $.stop_stmt,
      $.error_stop_stmt,                  // F2018: Enhanced ERROR STOP
      $.cycle_stmt,
      $.wait_stmt,
      $.sync_all_stmt,
      $.sync_images_stmt,
      $.sync_memory_stmt,
      $.sync_team_stmt,                   // F2018: SYNC TEAM
      $.lock_stmt,
      $.unlock_stmt,
      $.event_post_stmt,                  // F2018: EVENT POST
      $.event_wait_stmt,                  // F2018: EVENT WAIT
      $.fail_image_stmt,                  // F2018: FAIL IMAGE
      $.form_team_stmt,                   // F2018: FORM TEAM
      $.change_team_stmt,                 // F2018: CHANGE TEAM
      $.end_team_stmt,                    // F2018: END TEAM
      $.atomic_define_stmt,               // F2018: ATOMIC operations
      $.atomic_ref_stmt
    ),

    // F2018: Enhanced ERROR STOP
    error_stop_stmt: $ => seq(
      'ERROR', 'STOP',
      optional($.stop_code),
      optional(seq(',', 'QUIET', '=', $.logical_expr))
    ),

    logical_expr: $ => $.logical_expression,

    // F2018: Team synchronization
    sync_team_stmt: $ => seq(
      'SYNC', 'TEAM',
      '(',
      $.team_value,
      optional(seq(',', $.sync_stat_list)),
      ')'
    ),

    team_value: $ => $.scalar_expr,
    scalar_expr: $ => $.expression,

    // F2018: Event statements
    event_post_stmt: $ => seq(
      'EVENT', 'POST',
      '(',
      $.event_variable,
      optional(seq(',', $.sync_stat_list)),
      ')'
    ),

    event_wait_stmt: $ => seq(
      'EVENT', 'WAIT',
      '(',
      $.event_variable,
      optional(seq(',', $.wait_spec_list)),
      ')'
    ),

    event_variable: $ => $.variable,

    // Enhanced wait spec list for events
    wait_spec: $ => choice(
      seq('UNTIL_COUNT', '=', $.scalar_int_expr),
      seq('STAT', '=', $.stat_variable),
      seq('ERRMSG', '=', $.errmsg_variable)
    ),

    // F2018: FAIL IMAGE statement
    fail_image_stmt: $ => 'FAIL IMAGE',

    // F2018: Team management
    form_team_stmt: $ => seq(
      'FORM', 'TEAM',
      '(',
      $.team_number,
      ',',
      $.team_variable,
      optional(seq(',', $.form_team_spec_list)),
      ')'
    ),

    team_number: $ => $.scalar_int_expr,
    team_variable: $ => $.variable,

    form_team_spec_list: $ => sep1($.form_team_spec, ','),
    form_team_spec: $ => choice(
      seq('NEW_INDEX', '=', $.scalar_int_expr),
      seq('STAT', '=', $.stat_variable),
      seq('ERRMSG', '=', $.errmsg_variable)
    ),

    change_team_stmt: $ => seq(
      'CHANGE', 'TEAM',
      '(',
      $.team_value,
      optional(seq(',', $.coarray_association_list)),
      ')'
    ),

    end_team_stmt: $ => seq(
      'END', 'TEAM',
      optional(seq('(', optional($.sync_stat_list), ')'))
    ),

    coarray_association_list: $ => sep1($.coarray_association, ','),
    coarray_association: $ => seq(
      $.codimension_decl,
      '=>',
      $.selector
    ),

    codimension_decl: $ => seq(
      $.coarray_name,
      '[', $.coarray_spec, ']'
    ),

    coarray_name: $ => $.simple_variable,

    // ========================================================================
    // F2018 ATOMIC OPERATIONS
    // ========================================================================

    // F2018: Atomic operations
    atomic_define_stmt: $ => seq(
      'ATOMIC_DEFINE',
      '(',
      $.atom,
      ',',
      $.value,
      ')'
    ),

    atomic_ref_stmt: $ => seq(
      'ATOMIC_REF',
      '(',
      $.value,
      ',',
      $.atom,
      ')'
    ),

    atom: $ => $.variable,
    value: $ => $.expression,

    // ========================================================================
    // F2018 ENHANCED ATTRIBUTES
    // ========================================================================

    // Enhanced attribute specifications
    attr_spec: $ => choice(
      'PARAMETER',
      'ALLOCATABLE',
      seq('DIMENSION', '(', $.array_spec, ')'),
      seq('CODIMENSION', '[', $.coarray_spec, ']'),
      'EXTERNAL',
      seq('INTENT', '(', $.intent_spec, ')'),
      'INTRINSIC',
      'OPTIONAL',
      'POINTER',
      'PRIVATE',
      'PUBLIC',
      'SAVE',
      'TARGET',
      'VALUE',
      'VOLATILE',
      seq('BIND', '(', 'C', optional(seq(',', 'NAME', '=', $.scalar_char_constant)), ')'),
      'ABSTRACT',
      'ASYNCHRONOUS',
      'CONTIGUOUS'
    ),

    // ========================================================================
    // F2018 ENHANCED INTRINSIC PROCEDURES
    // ========================================================================

    // F2018 intrinsic procedures
    f2018_intrinsic_name: $ => choice(
      // Team intrinsics
      'TEAM_NUMBER',
      'GET_TEAM',
      'TEAM_SIZE',
      // Image failure intrinsics
      'FAILED_IMAGES',
      'IMAGE_STATUS',
      'STOPPED_IMAGES',
      // Collective subroutines
      'CO_BROADCAST',
      'CO_MAX',
      'CO_MIN',
      'CO_REDUCE',
      'CO_SUM',
      // Atomic intrinsics
      'ATOMIC_ADD',
      'ATOMIC_AND',
      'ATOMIC_CAS',
      'ATOMIC_DEFINE',
      'ATOMIC_FETCH_ADD',
      'ATOMIC_FETCH_AND',
      'ATOMIC_FETCH_OR',
      'ATOMIC_FETCH_XOR',
      'ATOMIC_OR',
      'ATOMIC_REF',
      'ATOMIC_XOR',
      // Event intrinsics
      'EVENT_QUERY',
      // Other F2018 intrinsics
      'COSHAPE',
      'OUT_OF_RANGE',
      'RANDOM_INIT',
      'REDUCE'
    ),

    // ========================================================================
    // F2018 ENHANCED DECLARATION TYPES - Assumed-type and assumed-rank
    // ========================================================================

    // Enhanced declaration type specification
    declaration_type_spec: $ => choice(
      'INTEGER',
      'REAL',
      'COMPLEX',
      'CHARACTER',
      'LOGICAL',
      seq('DOUBLE', 'PRECISION'),
      seq('TYPE', '(', $.derived_type_spec, ')'),
      seq('CLASS', '(', $.derived_type_spec, ')'),
      seq('CLASS', '(', '*', ')'),
      seq('TYPE', '(', '*', ')'),           // F2018: Assumed-type
      seq('DIMENSION', '(', '..', ')')      // F2018: Assumed-rank
    ),

    // ========================================================================
    // F2018 ENHANCED ERROR HANDLING
    // ========================================================================

    // Enhanced STOP statement
    stop_stmt: $ => seq(
      'STOP',
      optional($.stop_code),
      optional(seq(',', 'QUIET', '=', $.logical_expr))
    ),

    // ========================================================================
    // F2018 COLLECTIVE PROCEDURES
    // ========================================================================

    // Enhanced call statement for collective procedures
    call_stmt: $ => seq(
      'CALL',
      $.procedure_designator,
      optional(seq('(', optional($.actual_arg_spec_list), ')'))
    ),

    // ========================================================================
    // F2018 ENHANCED C INTEROPERABILITY
    // ========================================================================

    // Enhanced C types for F2018
    c_type_spec: $ => choice(
      'C_INT',
      'C_SHORT',
      'C_LONG',
      'C_LONG_LONG',
      'C_SIGNED_CHAR',
      'C_SIZE_T',
      'C_INT8_T',
      'C_INT16_T',
      'C_INT32_T',
      'C_INT64_T',
      'C_INT_LEAST8_T',
      'C_INT_LEAST16_T',
      'C_INT_LEAST32_T',
      'C_INT_LEAST64_T',
      'C_INT_FAST8_T',
      'C_INT_FAST16_T',
      'C_INT_FAST32_T',
      'C_INT_FAST64_T',
      'C_INTMAX_T',
      'C_INTPTR_T',
      'C_FLOAT',
      'C_DOUBLE',
      'C_LONG_DOUBLE',
      'C_FLOAT_COMPLEX',
      'C_DOUBLE_COMPLEX',
      'C_LONG_DOUBLE_COMPLEX',
      'C_BOOL',
      'C_CHAR',
      'C_NULL_PTR',
      'C_NULL_FUNPTR'
    ),

    // ========================================================================
    // F2018 ENHANCED COARRAY FEATURES
    // ========================================================================

    // Enhanced image selector with team support
    image_selector: $ => seq(
      '[',
      $.cosubscript_list,
      optional(seq(',', $.image_selector_spec_list)),
      ']'
    ),

    image_selector_spec_list: $ => sep1($.image_selector_spec, ','),
    image_selector_spec: $ => choice(
      seq('STAT', '=', $.stat_variable),
      seq('TEAM', '=', $.team_value),      // F2018: Team specification
      seq('TEAM_NUMBER', '=', $.scalar_int_expr)
    ),

    // ========================================================================
    // F2018 ENHANCED MODULE FEATURES
    // ========================================================================

    // Enhanced use statement with F2018 features
    use_stmt: $ => seq(
      'USE',
      optional(seq(',', $.module_nature, ':')),  // F2018: Module nature
      $.module_name,
      optional(seq(',', $.only_option))
    ),

    module_nature: $ => choice('INTRINSIC', 'NON_INTRINSIC')
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}