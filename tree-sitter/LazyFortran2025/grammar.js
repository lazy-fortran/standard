/**
 * LazyFortran2025 - Modern relaxed Fortran syntax
 * Tree-sitter implementation with TRUE RELAXED PARSING
 * 
 * This is where Tree-sitter SHINES compared to ANTLR4!
 * We can actually make rules optional and accept bare code!
 * 
 * Key features:
 * 1. Optional program/module blocks - YES, it works!
 * 2. Optional contains keyword - YES, it works!
 * 3. Type inference - YES, undeclared variables accepted!
 * 4. Implicit none default - compiler feature
 */

const { extendGrammar, prepend, makeOptional, replace } = require('../lib/fortran-base');
const fortran2023 = require('../Fortran2023/grammar');

module.exports = extendGrammar(fortran2023, {
  name: 'LazyFortran2025',

  rules: {
    // ============================================================================
    // GAME CHANGER: Accept bare code at top level!
    // This is what ANTLR4 couldn't do properly!
    // ============================================================================
    
    // Program can be just bare statements - no wrapper needed!
    program: prepend($ => choice(
      // Bare executable code - no program/module wrapper!
      repeat1(choice(
        $.assignment_statement,
        $.call_statement,
        $.print_statement,
        $.if_construct,
        $.do_construct,
        $.declaration,
        $.internal_procedure  // procedures without contains!
      )),
      // Or traditional program units (backward compatible)
      repeat1($.program_unit)
    )),

    // ============================================================================
    // OPTIONAL PROGRAM/MODULE BLOCKS
    // ============================================================================

    // Make PROGRAM statement optional
    program_statement: makeOptional(),
    end_program_statement: makeOptional(),

    // Make MODULE statement optional  
    module_statement: makeOptional(),
    end_module_statement: makeOptional(),

    // Main program can be just statements
    main_program: replace(($, base) => choice(
      base($),  // Traditional with PROGRAM
      // Or just bare code!
      seq(
        repeat($.statement),
        optional($.internal_subprogram_part)
      )
    )),

    // ============================================================================
    // OPTIONAL CONTAINS - Procedures can follow directly!
    // ============================================================================

    // Make CONTAINS optional for internal procedures
    internal_subprogram_part: replace(($, base) => choice(
      base($),  // Traditional with CONTAINS
      // Or just procedures without CONTAINS!
      repeat1($.internal_subprogram)
    )),

    // Make CONTAINS optional for module procedures
    module_subprogram_part: replace(($, base) => choice(
      base($),  // Traditional with CONTAINS
      // Or just procedures without CONTAINS!
      repeat1($.module_subprogram)
    )),

    // Allow procedures anywhere
    internal_procedure: $ => choice(
      $.subroutine_subprogram,
      $.function_subprogram
    ),

    // ============================================================================
    // TYPE INFERENCE - Accept undeclared variables!
    // ============================================================================

    // Assignment can use undeclared variables (type inferred)
    assignment_statement: prepend($ => seq(
      $.undeclared_variable,  // NEW: No declaration needed!
      '=',
      $.expression
    )),

    // Any identifier can be a variable (type inference!)
    undeclared_variable: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Primary expressions can be undeclared names
    primary: prepend($ => $.undeclared_variable),

    // ============================================================================
    // SIMPLIFIED SYNTAX for common operations
    // ============================================================================

    // Simplified print without format
    print_statement: prepend($ => seq(
      'print',
      '*',
      ',',
      $.output_list
    )),

    // Array literals with brackets (modern syntax)
    array_literal: $ => seq(
      '[',
      sep1($.expression, ','),
      ']'
    ),

    // String concatenation with //
    string_concat: $ => prec.left(seq(
      $.expression,
      '//',
      $.expression
    )),

    // ============================================================================
    // IMPLICIT NONE DEFAULT
    // ============================================================================
    
    // No need for 'implicit none' - it's the default!
    // This is handled at semantic level, parser accepts both styles
  }
});

// Helper function
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}