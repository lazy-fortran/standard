/**
 * FORTRAN II (1958) - Revolutionary Advances  
 * Inheriting from FORTRAN (1957) with Tree-sitter composition
 * 
 * REVOLUTIONARY additions in FORTRAN II:
 * - Independent compilation (modular programming)
 * - SUBROUTINE and FUNCTION subprograms
 * - COMMON blocks for data sharing
 * - Enhanced I/O with READ and WRITE statements
 * - EXTERNAL declaration for user-defined functions
 */

const fortran = require('../FORTRAN/grammar.js');

module.exports = grammar(fortran, {
  name: 'FORTRANII',

  // Copy base extras and maintain them
  extras: $ => [
    /\s+/,
    $.comment
  ],

  conflicts: $ => [
    [$.simple_variable, $.function_name],
    [$.simple_variable, $.subroutine_name],
    [$.variable, $.constant]
  ],

  // Copy base precedences
  precedences: $ => [
    ['power', 'mult', 'add', 'relop']
  ],

  rules: {
    // ============================================================================
    // OVERRIDE: Program structure to support multiple program units
    // ============================================================================
    
    // Override program to support multiple compilation units (revolutionary!)
    program: $ => repeat1($.program_unit),

    // NEW: Program units enable independent compilation
    program_unit: $ => choice(
      $.main_program,           // From FORTRAN (1957)  
      $.subroutine_subprogram,  // NEW in FORTRAN II!
      $.function_subprogram     // NEW in FORTRAN II!
    ),

    // Main program is just the statements that aren't in subroutines/functions
    // We need to be careful here - a main program should be a sequence of statements
    // that ends with END and isn't part of a subroutine or function
    main_program: $ => seq(
      repeat($.statement_without_end),
      $.end_statement
    ),

    // Statement without END (to avoid ambiguity in main programs)
    statement_without_end: $ => seq(
      optional($.label),
      choice(
        $.arithmetic_statement,
        $.control_statement,
        $.io_statement,
        $.specification_statement,
        $.format_statement
      )
    ),

    // ============================================================================
    // NEW: SUBROUTINE SUBPROGRAM 
    // ============================================================================
    
    subroutine_subprogram: $ => prec(1, seq(
      $.subroutine_statement,
      repeat($.statement_without_end),
      $.end_statement
    )),

    subroutine_statement: $ => seq(
      'SUBROUTINE',
      $.subroutine_name,
      optional(seq('(', $.dummy_argument_list, ')'))
    ),

    subroutine_name: $ => /[A-Z][A-Z0-9]*/,

    // ============================================================================
    // NEW: FUNCTION SUBPROGRAM
    // ============================================================================
    
    function_subprogram: $ => prec(1, seq(
      $.function_statement,
      repeat($.statement_without_end),
      $.end_statement
    )),

    function_statement: $ => seq(
      'FUNCTION',
      $.function_name,
      '(',
      $.dummy_argument_list,
      ')'
    ),

    // Override function name base to handle user-defined functions
    function_name_base: $ => choice(
      $.intrinsic_function,
      $.user_function  
    ),

    // Intrinsic functions (built-in) - still end with F
    intrinsic_function: $ => /[A-Z]+F/,

    // User-defined functions (declared with FUNCTION or EXTERNAL)
    user_function: $ => $.simple_variable,

    dummy_argument_list: $ => sep1($.simple_variable, ','),

    // ============================================================================
    // EXTEND: Control statements with new FORTRAN II features
    // ============================================================================

    // Extend control statement base to include FORTRAN II additions
    control_statement_base: $ => choice(
      // All FORTRAN (1957) control statements
      $.goto_statement,
      $.if_statement,
      $.do_statement,
      $.continue_statement,
      $.stop_statement,
      $.pause_statement,
      // NEW in FORTRAN II
      $.call_statement,
      $.return_statement
    ),

    // NEW: CALL statement for invoking subroutines
    call_statement: $ => seq(
      'CALL',
      $.subroutine_name,
      optional(seq('(', $.actual_argument_list, ')'))
    ),

    actual_argument_list: $ => sep1($.expression, ','),

    // NEW: RETURN statement for subroutines/functions
    return_statement: $ => 'RETURN',

    // ============================================================================
    // EXTEND: Specification statements with COMMON blocks
    // ============================================================================

    // Extend specification statement base to include FORTRAN II additions  
    specification_statement_base: $ => choice(
      // All FORTRAN (1957) specification statements
      $.dimension_statement,
      $.equivalence_statement,
      $.frequency_statement,
      // NEW in FORTRAN II
      $.common_statement,
      $.external_statement
    ),

    // NEW: COMMON blocks for sharing data between program units
    common_statement: $ => seq(
      'COMMON',
      prec.left(repeat1($.common_block))
    ),

    common_block: $ => seq(
      optional(seq('/', $.common_block_name, '/')),
      $.common_item_list
    ),

    common_block_name: $ => /[A-Z][A-Z0-9]*/,

    common_item_list: $ => sep1($.common_item, ','),

    common_item: $ => choice(
      $.simple_variable,
      $.array_declarator
    ),

    // NEW: EXTERNAL statement for external functions
    external_statement: $ => seq(
      'EXTERNAL',
      sep1($.external_name, ',')
    ),

    external_name: $ => /[A-Z][A-Z0-9]*/,

    // ============================================================================
    // OVERRIDE: Function calls now support user-defined functions
    // ============================================================================

    // Override function_call to handle user-defined functions (not just intrinsic)
    function_call: $ => seq(
      $.function_name,
      '(',
      $.argument_list,
      ')'
    ),

    // ============================================================================ 
    // OVERRIDE: Fix format specification to not be greedy
    // ============================================================================

    format_specification: $ => /[^)]*/  // Match everything except closing paren

    // All other rules inherited from FORTRAN (1957)!
  }
});

// Helper function
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}