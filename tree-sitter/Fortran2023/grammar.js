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
    // F2023 MINOR ENHANCEMENTS - Corrections and clarifications only
    // ========================================================================

    // F2023 is primarily corrections - expression handling unchanged from F2018
    // No conditional expressions or major syntax changes were added

    // F2023 maintains F2018 type system - no new intrinsic types added

    // F2023 adds minimal intrinsic enhancements - primarily corrections
    // Most functionality remains from F2018
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}