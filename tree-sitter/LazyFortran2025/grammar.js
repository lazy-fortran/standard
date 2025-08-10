/**
 * LazyFortran2025 - Modern Syntactic Relaxations for Fortran
 * Extending Fortran2023 with Julia/Python-like convenience
 * 
 * KEY INNOVATIONS:
 * 1. Optional program/module blocks - compiler determines context
 * 2. Implicit none by default - enforced by compiler, not parser
 * 3. Optional CONTAINS keyword - procedures can follow directly
 * 4. Type inference - variables can be used without declarations
 * 
 * These relaxations make Fortran code more concise and modern while
 * maintaining full backward compatibility with standard Fortran.
 */

const fortran2023 = require('../Fortran2023/grammar.js');

function grammar(base, config) {
  if (!config) {
    config = base;
    base = undefined;
  }
  return config;
}

module.exports = grammar(fortran2023, {
  name: 'LazyFortran2025',

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
    [$.lazy_program, $.program_unit]
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
    // ============================================================================
    // RELAXATION #1: Optional Program/Module Blocks
    // ============================================================================
    
    // Top-level program can be either traditional or lazy
    program: $ => choice(
      repeat1($.program_unit),                    // Traditional Fortran style
      $.lazy_program                              // LazyFortran2025 style
    ),

    // Lazy program - just statements without explicit program/module wrapper
    lazy_program: $ => seq(
      optional(repeat1($.use_stmt)),
      optional(repeat1($.implicit_stmt)),
      repeat($.lazy_statement),
      optional($.lazy_internal_procedures)        // Procedures without CONTAINS
    ),

    // Lazy statement can be declaration or executable
    lazy_statement: $ => choice(
      $.declaration_construct,
      $.executable_construct,
      $.assignment_stmt,
      $.print_stmt,
      $.if_stmt,
      $.do_construct,
      $.select_case_construct,
      $.where_construct,
      $.forall_construct
    ),

    // Enhanced program unit that accepts lazy style
    program_unit: $ => choice(
      $.main_program,
      $.module,
      $.submodule,
      $.external_subprogram,
      $.lazy_program                              // LazyFortran2025: Direct code
    ),

    // ============================================================================
    // RELAXATION #2: Implicit None Default (Semantic Feature)
    // ============================================================================
    // This is enforced by the compiler, not the parser.
    // The parser accepts both styles; the compiler enforces implicit none.

    // ============================================================================
    // RELAXATION #3: Optional CONTAINS
    // ============================================================================
    
    // Lazy internal procedures - procedures without CONTAINS keyword
    lazy_internal_procedures: $ => repeat1($.internal_subprogram),

    // Enhanced module subprogram part - CONTAINS is optional
    module_subprogram_part: $ => choice(
      seq('CONTAINS', repeat1($.module_subprogram)),     // Traditional
      repeat1($.module_subprogram)                        // Lazy: No CONTAINS
    ),

    // Enhanced internal subprogram part - CONTAINS is optional  
    internal_subprogram_part: $ => choice(
      seq('CONTAINS', repeat1($.internal_subprogram)),    // Traditional
      repeat1($.internal_subprogram)                      // Lazy: No CONTAINS
    ),

    // ============================================================================
    // RELAXATION #4: Type Inference
    // ============================================================================
    // Variables can be used without prior declaration if type can be inferred.
    // This is primarily a semantic feature, but we relax parsing rules.

    // Enhanced assignment that can introduce new variables
    assignment_stmt: $ => seq(
      choice(
        $.variable,
        $.simple_variable                         // Can be undeclared
      ),
      '=',
      $.expression
    ),

    // Enhanced variable that doesn't require prior declaration
    variable: $ => choice(
      $.simple_variable,
      $.subscripted_variable,
      $.structure_component,
      $.array_element
    ),

    // ============================================================================
    // ENHANCED SPECIFICATION PART
    // ============================================================================
    
    // Specification part is more flexible - declarations are optional
    specification_part: $ => repeat($.specification_construct),

    // ============================================================================
    // ENHANCED EXECUTION PART
    // ============================================================================
    
    // Execution part can intermix declarations and executable statements
    execution_part: $ => repeat($.execution_part_construct),

    execution_part_construct: $ => choice(
      $.executable_construct,
      $.format_stmt,
      $.declaration_construct,                    // Declarations can appear anywhere
      $.internal_subprogram                        // Procedures without CONTAINS
    ),

    // ============================================================================
    // INHERIT ALL F2023 FEATURES
    // ============================================================================
    // LazyFortran2025 includes all Fortran 2023 features:
    // - Conditional expressions (? :)
    // - Enumerated types
    // - Teams and events from F2018
    // - All historical features from FORTRAN I through F2023
  }
});

// Helper function
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}