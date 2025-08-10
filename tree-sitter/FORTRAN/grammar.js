/**
 * FORTRAN (1957) - The Original IBM 704 Grammar
 * Tree-sitter implementation with better composability than ANTLR4
 * 
 * Historical significance: First high-level programming language
 * Released: October 15, 1957
 * Target: IBM 704 mainframe
 */

module.exports = grammar({
  name: 'FORTRAN',

  extras: $ => [
    /\s/,  // Whitespace
  ],

  rules: {
    // ============================================================================
    // PROGRAM STRUCTURE: IBM 704 FORTRAN
    // ============================================================================
    
    program: $ => repeat($.statement),

    statement: $ => seq(
      optional($.label),
      choice(
        $.arithmetic_statement,
        $.control_statement,
        $.io_statement,
        $.specification_statement,
        $.format_statement,
        $.end_statement
      )
    ),

    label: $ => /[1-9][0-9]{0,4}/,

    // ============================================================================
    // ARITHMETIC STATEMENTS (1957)
    // ============================================================================

    arithmetic_statement: $ => $.assignment_statement,

    assignment_statement: $ => seq(
      $.variable,
      '=',
      $.expression
    ),

    variable: $ => choice(
      $.simple_variable,
      $.subscripted_variable
    ),

    simple_variable: $ => /[A-Z][A-Z0-9]*/,

    subscripted_variable: $ => seq(
      $.simple_variable,
      '(',
      $.subscript_list,
      ')'
    ),

    subscript_list: $ => sep1($.subscript, ','),

    subscript: $ => $.expression,

    // ============================================================================
    // EXPRESSIONS (1957)
    // ============================================================================

    expression: $ => choice(
      $.arithmetic_expression,
      $.logical_expression
    ),

    arithmetic_expression: $ => choice(
      $.term,
      prec.left(1, seq(
        $.arithmetic_expression,
        choice('+', '-'),
        $.term
      ))
    ),

    term: $ => choice(
      $.factor,
      prec.left(2, seq(
        $.term,
        choice('*', '/'),
        $.factor
      ))
    ),

    factor: $ => choice(
      $.primary,
      prec.right(3, seq(
        $.primary,
        '**',
        $.factor
      ))
    ),

    primary: $ => choice(
      $.constant,
      $.variable,
      $.function_call,
      seq('(', $.arithmetic_expression, ')')
    ),

    constant: $ => choice(
      $.integer_constant,
      $.real_constant
    ),

    integer_constant: $ => /[0-9]+/,
    
    real_constant: $ => /[0-9]+\.[0-9]+([E][+-]?[0-9]+)?/,

    function_call: $ => seq(
      /[A-Z]+F/,  // Functions end with F in FORTRAN I
      '(',
      $.argument_list,
      ')'
    ),

    argument_list: $ => sep1($.expression, ','),

    // ============================================================================
    // CONTROL STATEMENTS (1957)
    // ============================================================================

    control_statement: $ => choice(
      $.goto_statement,
      $.if_statement,
      $.do_statement,
      $.continue_statement,
      $.stop_statement,
      $.pause_statement
    ),

    goto_statement: $ => seq(
      'GO', 'TO',
      $.label
    ),

    // Arithmetic IF - three-way branch (unique to early FORTRAN)
    if_statement: $ => seq(
      'IF',
      '(',
      $.expression,
      ')',
      $.label,  // negative
      ',',
      $.label,  // zero
      ',',
      $.label   // positive
    ),

    do_statement: $ => seq(
      'DO',
      $.label,
      $.simple_variable,
      '=',
      $.expression,
      ',',
      $.expression,
      optional(seq(',', $.expression))
    ),

    continue_statement: $ => 'CONTINUE',
    
    stop_statement: $ => seq('STOP', optional($.integer_constant)),
    
    pause_statement: $ => seq('PAUSE', optional($.integer_constant)),

    // ============================================================================
    // I/O STATEMENTS (1957)
    // ============================================================================

    io_statement: $ => choice(
      $.read_statement,
      $.print_statement,
      $.punch_statement,
      $.read_tape_statement,
      $.write_tape_statement
    ),

    read_statement: $ => seq(
      'READ',
      $.integer_constant,
      ',',
      $.io_list
    ),

    print_statement: $ => seq(
      'PRINT',
      $.integer_constant,
      ',',
      $.io_list
    ),

    punch_statement: $ => seq(
      'PUNCH',
      $.integer_constant,
      ',',
      $.io_list
    ),

    read_tape_statement: $ => seq(
      'READ', 'TAPE',
      $.integer_constant,
      ',',
      $.io_list
    ),

    write_tape_statement: $ => seq(
      'WRITE', 'TAPE',
      $.integer_constant,
      ',',
      $.io_list
    ),

    io_list: $ => sep1($.variable, ','),

    // ============================================================================
    // SPECIFICATION STATEMENTS (1957)
    // ============================================================================

    specification_statement: $ => choice(
      $.dimension_statement,
      $.equivalence_statement,
      $.frequency_statement
    ),

    dimension_statement: $ => seq(
      'DIMENSION',
      sep1($.array_declarator, ',')
    ),

    array_declarator: $ => seq(
      $.simple_variable,
      '(',
      sep1($.integer_constant, ','),
      ')'
    ),

    equivalence_statement: $ => seq(
      'EQUIVALENCE',
      '(',
      sep1($.variable, ','),
      ')'
    ),

    frequency_statement: $ => seq(
      'FREQUENCY',
      sep1(seq($.label, '(', $.integer_constant, ')'), ',')
    ),

    // ============================================================================
    // FORMAT STATEMENT (1957)
    // ============================================================================

    format_statement: $ => seq(
      'FORMAT',
      '(',
      $.format_specification,
      ')'
    ),

    format_specification: $ => /.*/,  // Simplified for now

    // ============================================================================
    // END STATEMENT (1957)
    // ============================================================================

    end_statement: $ => 'END',

    // ============================================================================
    // LOGICAL EXPRESSIONS (limited in 1957)
    // ============================================================================

    logical_expression: $ => $.relational_expression,

    relational_expression: $ => seq(
      $.arithmetic_expression,
      choice('.GT.', '.GE.', '.LT.', '.LE.', '.EQ.', '.NE.'),
      $.arithmetic_expression
    )
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}