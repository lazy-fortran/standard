/**
 * Fortran 2023 (2023) - Minor Revision with Corrections and Small Additions
 * Inheriting from Fortran2018 with corrections and enumerated types
 * 
 * ACTUAL F2023 ENHANCEMENTS (historically accurate):
 * - Enumerated types (ENUM construct)
 * - Corrections and clarifications to F2018 standard
 * - Enhanced error handling improvements
 * - Minor syntax improvements and clarifications
 * - Improved interoperability features
 * 
 * NOTE: F2023 is a minor revision focused on corrections,
 * not major new features like generics or parameterized modules
 */

const fortran2018 = require('../Fortran2018/grammar.js');

function grammar(base, config) {
  if (!config) {
    config = base;
    base = undefined;
  }
  return config;
}

module.exports = grammar(fortran2018, {
  name: 'Fortran2023',

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
    [$.event_name, $.simple_variable],
    [$.generic_name, $.simple_variable]
  ],

  precedences: $ => [
    ['power', 'mult', 'add', 'concat', 'relop', 'not', 'and', 'or', 'defined_op', 'conditional']
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
    // F2023 ENUMERATED TYPES - Actual new feature!
    // ========================================================================

    // Enhanced specification construct with enumerated types
    specification_construct: $ => choice(
      $.use_stmt,
      $.implicit_stmt,
      $.parameter_stmt,
      $.format_stmt,
      $.declaration_construct,
      $.interface_block,
      $.abstract_interface_block,
      $.enum_def                          // F2023: Enumerated types
    ),

    // F2023: Enumerated type definition
    enum_def: $ => seq(
      $.enum_def_stmt,
      repeat($.enumerator_def_stmt),
      $.end_enum_stmt
    ),

    enum_def_stmt: $ => choice(
      seq('ENUM', ',', 'BIND', '(', 'C', ')'),      // C-compatible enum
      seq('ENUM', '::', optional($.enum_name))      // F2023 native enum
    ),

    enum_name: $ => $.simple_variable,

    enumerator_def_stmt: $ => seq(
      'ENUMERATOR',
      optional('::'),
      $.enumerator_list
    ),

    enumerator_list: $ => sep1($.enumerator, ','),
    enumerator: $ => seq(
      $.enumerator_name,
      optional(seq('=', $.integer_expr))
    ),

    enumerator_name: $ => $.simple_variable,
    integer_expr: $ => $.expression,

    end_enum_stmt: $ => seq('END', 'ENUM', optional($.enum_name)),

    // ========================================================================
    // F2023 CONDITIONAL EXPRESSIONS - New ternary operator
    // ========================================================================

    // F2023: Conditional expression (ternary operator)
    // Syntax: (condition ? true_expr : false_expr)
    conditional_expression: $ => seq(
      '(',
      $.logical_expression,
      '?',
      $.expression,
      ':',
      $.expression,
      ')'
    ),

    logical_expression: $ => $.expression,

    // Enhanced primary base to include conditional expressions
    primary_base: $ => choice(
      $.constant,
      $.designator,
      $.array_constructor,
      $.structure_constructor,
      $.function_reference,
      $.null_expr,
      $.conditional_expression,           // F2023: Conditional expressions
      seq('(', $.expression, ')')
    ),

    // ========================================================================
    // F2023 ENHANCED INTRINSIC PROCEDURES
    // ========================================================================

    // F2023 intrinsic procedures (primarily corrections and small additions)
    f2023_intrinsic_name: $ => choice(
      // Enhanced IEEE functions
      'IEEE_MAX',
      'IEEE_MIN', 
      'IEEE_MAX_MAG',
      'IEEE_MIN_MAG',
      // Enhanced system functions
      'SYSTEM_CLOCK',
      // Other minor additions
      'RANDOM_INIT'
    )
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}