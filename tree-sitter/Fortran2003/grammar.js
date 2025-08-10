/**
 * Fortran 2003 (2003) - Object-Oriented Programming Revolution
 * Inheriting from Fortran95 with major OOP and C interoperability features
 * 
 * REVOLUTIONARY F2003 FEATURES:
 * - Object-oriented programming (classes, inheritance, polymorphism)
 * - Parameterized derived types (PDTs)
 * - C interoperability (ISO_C_BINDING)
 * - Enhanced array features and allocatable components
 * - IEEE arithmetic support
 * - Enhanced I/O features (STREAM, ASYNCHRONOUS)
 * - Abstract interfaces and procedure pointers
 * - User-defined derived type I/O
 */

const fortran95 = require('../Fortran95/grammar.js');

module.exports = grammar(fortran95, {
  name: 'Fortran2003',

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
    [$.component_name, $.simple_variable],
    [$.class_name, $.simple_variable],
    [$.binding_name, $.simple_variable],
    [$.procedure_name, $.simple_variable]
  ],

  // Enhanced precedences for F2003
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
    // F2003 OBJECT-ORIENTED PROGRAMMING - Revolutionary!
    // ========================================================================

    // Enhanced derived type with OOP features
    derived_type_def: $ => seq(
      $.derived_type_stmt,
      repeat($.type_param_def_stmt),      // F2003: Parameterized derived types
      repeat($.component_part),
      optional($.type_bound_procedure_part),  // F2003: OOP methods
      $.end_type_stmt
    ),

    derived_type_stmt: $ => seq(
      'TYPE',
      optional($.access_spec),
      optional($.type_attr_spec_list),
      '::',
      $.type_name,
      optional(seq('(', $.type_param_name_list, ')'))  // F2003: Type parameters
    ),

    // F2003: Type attributes for OOP
    type_attr_spec_list: $ => sep1($.type_attr_spec, ','),
    type_attr_spec: $ => choice(
      'ABSTRACT',                         // F2003: Abstract types
      'BIND', '(', 'C', ')',             // F2003: C interoperability
      seq('EXTENDS', '(', $.parent_type_name, ')')  // F2003: Inheritance
    ),

    parent_type_name: $ => $.type_name,

    type_param_name_list: $ => sep1($.type_param_name, ','),
    type_param_name: $ => $.simple_variable,

    // F2003: Type parameter definitions
    type_param_def_stmt: $ => seq(
      $.declaration_type_spec,
      ',',
      $.type_param_attr_spec,
      '::',
      $.type_param_decl_list
    ),

    type_param_attr_spec: $ => choice('KIND', 'LEN'),
    type_param_decl_list: $ => sep1($.type_param_decl, ','),
    type_param_decl: $ => seq(
      $.type_param_name,
      optional(seq('=', $.initialization_expr))
    ),

    component_part: $ => choice(
      $.component_def_stmt,
      $.proc_component_def_stmt          // F2003: Procedure components
    ),

    // F2003: Procedure component definition
    proc_component_def_stmt: $ => seq(
      'PROCEDURE',
      '(',
      optional($.proc_interface),
      ')',
      ',',
      $.proc_component_attr_spec_list,
      '::',
      $.proc_decl_list
    ),

    proc_interface: $ => choice(
      $.interface_name,
      'REAL',
      'INTEGER',
      'LOGICAL',
      'CHARACTER',
      'COMPLEX'
    ),

    interface_name: $ => $.simple_variable,

    proc_component_attr_spec_list: $ => sep1($.proc_component_attr_spec, ','),
    proc_component_attr_spec: $ => choice(
      'POINTER',
      'NOPASS',
      seq('PASS', optional(seq('(', $.arg_name, ')')))
    ),

    arg_name: $ => $.simple_variable,

    proc_decl_list: $ => sep1($.proc_decl, ','),
    proc_decl: $ => seq(
      $.procedure_entity_name,
      optional(seq('=>', $.proc_target))
    ),

    procedure_entity_name: $ => $.simple_variable,
    proc_target: $ => $.procedure_name,

    // F2003: Type-bound procedure part (OOP methods)
    type_bound_procedure_part: $ => seq(
      'CONTAINS',
      repeat($.binding_private_stmt),
      repeat1($.proc_binding_stmt)
    ),

    binding_private_stmt: $ => seq('PRIVATE'),

    proc_binding_stmt: $ => choice(
      $.specific_binding,
      $.generic_binding,
      $.final_binding
    ),

    // F2003: Specific procedure binding
    specific_binding: $ => seq(
      'PROCEDURE',
      optional(seq(',', $.binding_attr_list)),
      '::',
      $.binding_name,
      optional(seq('=>', $.procedure_name))
    ),

    binding_name: $ => $.simple_variable,

    binding_attr_list: $ => sep1($.binding_attr, ','),
    binding_attr: $ => choice(
      optional($.access_spec),
      'DEFERRED',                         // F2003: Deferred bindings
      'NON_OVERRIDABLE',                  // F2003: Non-overridable methods
      'NOPASS',
      seq('PASS', optional(seq('(', $.arg_name, ')')))
    ),

    // F2003: Generic procedure binding
    generic_binding: $ => seq(
      'GENERIC',
      optional(seq(',', $.access_spec)),
      '::',
      $.generic_spec,
      '=>',
      $.binding_name_list
    ),

    binding_name_list: $ => sep1($.binding_name, ','),

    // F2003: Final procedure binding
    final_binding: $ => seq(
      'FINAL',
      '::',
      $.final_procedure_name_list
    ),

    final_procedure_name_list: $ => sep1($.procedure_name, ','),

    // ========================================================================
    // F2003 PARAMETERIZED DERIVED TYPES (PDTs)
    // ========================================================================

    // Enhanced declaration type spec with PDTs
    declaration_type_spec: $ => choice(
      'INTEGER',
      'REAL', 
      'COMPLEX',
      'CHARACTER',
      'LOGICAL',
      seq('DOUBLE', 'PRECISION'),
      seq('TYPE', '(', $.derived_type_spec, ')'),  // F2003: Enhanced
      seq('CLASS', '(', $.derived_type_spec, ')'), // F2003: Polymorphic
      seq('CLASS', '(', '*', ')')                  // F2003: Unlimited polymorphic
    ),

    derived_type_spec: $ => seq(
      $.type_name,
      optional(seq('(', $.type_param_spec_list, ')'))  // F2003: Type parameters
    ),

    type_param_spec_list: $ => sep1($.type_param_spec, ','),
    type_param_spec: $ => choice(
      $.type_param_value,
      seq($.keyword, '=', $.type_param_value)
    ),

    type_param_value: $ => choice(
      $.scalar_int_expr,
      '*',
      ':'
    ),

    // ========================================================================
    // F2003 ABSTRACT INTERFACES
    // ========================================================================

    // Enhanced specification construct with abstract interfaces
    specification_construct: $ => choice(
      $.use_stmt,
      $.implicit_stmt,
      $.parameter_stmt,
      $.format_stmt,
      $.declaration_construct,
      $.interface_block,                  // F2003: Enhanced interfaces
      $.abstract_interface_block          // F2003: Abstract interfaces
    ),

    abstract_interface_block: $ => seq(
      $.abstract_interface_stmt,
      repeat($.interface_specification),
      $.end_interface_stmt
    ),

    abstract_interface_stmt: $ => seq('ABSTRACT', 'INTERFACE'),

    interface_block: $ => seq(
      $.interface_stmt,
      repeat($.interface_specification),
      $.end_interface_stmt
    ),

    interface_stmt: $ => choice(
      'INTERFACE',
      seq('INTERFACE', $.generic_spec)
    ),

    interface_specification: $ => choice(
      $.interface_body,
      $.procedure_stmt
    ),

    interface_body: $ => choice(
      $.function_stmt,
      $.subroutine_stmt
    ),

    procedure_stmt: $ => seq(
      optional('MODULE'),
      'PROCEDURE',
      '::',
      $.procedure_name_list
    ),

    procedure_name_list: $ => sep1($.procedure_name, ','),

    end_interface_stmt: $ => seq('END', 'INTERFACE', optional($.generic_spec)),

    // ========================================================================
    // F2003 PROCEDURE POINTERS
    // ========================================================================

    // Enhanced attribute specification with procedure pointers
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
      'TARGET',
      'VALUE',                            // F2003: VALUE attribute
      'VOLATILE',                         // F2003: VOLATILE attribute
      seq('BIND', '(', 'C', optional(seq(',', 'NAME', '=', $.scalar_char_constant)), ')'), // F2003: C binding
      'ABSTRACT',                         // F2003: Abstract procedures
      'ASYNCHRONOUS'                      // F2003: Asynchronous I/O
    ),

    scalar_char_constant: $ => $.character_constant,

    // F2003: Procedure declaration
    proc_decl_stmt: $ => seq(
      'PROCEDURE',
      '(',
      optional($.proc_interface),
      ')',
      optional(seq(',', $.proc_attr_spec_list)),
      '::',
      $.proc_entity_decl_list
    ),

    proc_attr_spec_list: $ => sep1($.proc_attr_spec, ','),
    proc_attr_spec: $ => choice(
      $.access_spec,
      seq('INTENT', '(', $.intent_spec, ')'),
      'OPTIONAL',
      'POINTER',
      'SAVE'
    ),

    proc_entity_decl_list: $ => sep1($.proc_entity_decl, ','),
    proc_entity_decl: $ => seq(
      $.procedure_entity_name,
      optional(seq('=>', $.proc_target))
    ),

    // ========================================================================
    // F2003 ENHANCED ALLOCATABLE COMPONENTS
    // ========================================================================

    // Enhanced component attributes
    component_attr_spec: $ => choice(
      'POINTER',
      'ALLOCATABLE',                      // F2003: Allocatable components
      $.dimension_spec,
      $.access_spec
    ),

    // Enhanced component declaration
    component_def_stmt: $ => seq(
      $.declaration_type_spec,
      optional(seq(',', $.component_attr_spec_list)),
      '::',
      $.component_decl_list
    ),

    component_attr_spec_list: $ => sep1($.component_attr_spec, ','),

    // ========================================================================
    // F2003 C INTEROPERABILITY
    // ========================================================================

    // C interoperable types
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
    // F2003 ENHANCED I/O
    // ========================================================================

    // Enhanced I/O statements with F2003 features
    io_control_spec: $ => choice(
      seq('UNIT', '=', $.io_unit),
      seq('FMT', '=', $.format),
      seq('NML', '=', $.namelist_group_name),
      seq('ADVANCE', '=', $.scalar_char_expr),
      seq('ASYNCHRONOUS', '=', $.scalar_char_expr),  // F2003: Asynchronous I/O
      seq('BLANK', '=', $.scalar_char_expr),
      seq('DECIMAL', '=', $.scalar_char_expr),       // F2003: Decimal I/O
      seq('DELIM', '=', $.scalar_char_expr),
      seq('END', '=', $.label),
      seq('EOR', '=', $.label),
      seq('ERR', '=', $.label),
      seq('ID', '=', $.int_variable),                // F2003: Asynchronous ID
      seq('IOMSG', '=', $.iomsg_variable),           // F2003: I/O message
      seq('IOSTAT', '=', $.scalar_int_variable),
      seq('PAD', '=', $.scalar_char_expr),
      seq('POS', '=', $.scalar_int_expr),            // F2003: Stream positioning
      seq('REC', '=', $.scalar_int_expr),
      seq('ROUND', '=', $.scalar_char_expr),         // F2003: Rounding mode
      seq('SIGN', '=', $.scalar_char_expr),          // F2003: Sign mode
      seq('SIZE', '=', $.int_variable),
      seq('STREAM', '=', $.scalar_char_expr)         // F2003: Stream access
    ),

    io_unit: $ => choice(
      $.scalar_int_expr,
      '*'
    ),

    format: $ => choice(
      $.label,
      $.scalar_char_expr,
      '*'
    ),

    namelist_group_name: $ => $.simple_variable,
    scalar_char_expr: $ => $.expression,
    int_variable: $ => $.variable,
    iomsg_variable: $ => $.variable,
    scalar_int_variable: $ => $.variable,

    // F2003: WAIT statement for asynchronous I/O
    wait_stmt: $ => seq(
      'WAIT',
      '(',
      $.wait_spec_list,
      ')'
    ),

    wait_spec_list: $ => sep1($.wait_spec, ','),
    wait_spec: $ => choice(
      seq('UNIT', '=', $.io_unit),
      seq('END', '=', $.label),
      seq('EOR', '=', $.label),
      seq('ERR', '=', $.label),
      seq('ID', '=', $.scalar_int_expr),
      seq('IOMSG', '=', $.iomsg_variable),
      seq('IOSTAT', '=', $.scalar_int_variable)
    ),

    // ========================================================================
    // F2003 ENHANCED EXPRESSIONS - Object references
    // ========================================================================

    // Enhanced designator with F2003 object references
    designator: $ => choice(
      $.data_ref,
      $.substring
    ),

    data_ref: $ => seq(
      $.part_ref,
      repeat(seq(choice('.', '%'), $.part_ref))  // F2003: Component references
    ),

    part_ref: $ => seq(
      $.part_name,
      optional($.section_subscript_list),
      optional(seq('%', $.image_selector))      // F2003: Coarray support (basic)
    ),

    image_selector: $ => seq(
      '[',
      $.cosubscript_list,
      ']'
    ),

    cosubscript_list: $ => sep1($.cosubscript, ','),
    cosubscript: $ => $.scalar_int_expr,

    // F2003: Polymorphic allocation
    allocate_stmt: $ => seq(
      'ALLOCATE',
      '(',
      $.type_spec,                        // F2003: Type specification
      $.allocation_list,
      optional(seq(',', $.alloc_opt_list)),
      ')'
    ),

    type_spec: $ => choice(
      $.declaration_type_spec,
      seq('SOURCE', '=', $.source_expr)   // F2003: Source allocation
    ),

    source_expr: $ => $.expression,

    alloc_opt_list: $ => sep1($.alloc_opt, ','),
    alloc_opt: $ => choice(
      seq('STAT', '=', $.stat_variable),
      seq('ERRMSG', '=', $.errmsg_variable),      // F2003: Error message
      seq('SOURCE', '=', $.source_expr)
    ),

    errmsg_variable: $ => $.variable,

    // F2003: SELECT TYPE construct for polymorphism
    select_type_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.select_type_stmt,
      repeat($.type_guard_block),
      $.end_select_stmt
    ),

    select_type_stmt: $ => seq(
      'SELECT', 'TYPE',
      '(',
      optional(seq($.associate_name, '=>')),
      $.selector,
      ')'
    ),

    type_guard_block: $ => seq(
      $.type_guard_stmt,
      repeat($.execution_part_construct)
    ),

    type_guard_stmt: $ => seq(
      choice(
        seq('TYPE', 'IS', '(', $.type_spec, ')'),
        seq('CLASS', 'IS', '(', $.derived_type_spec, ')'),
        seq('CLASS', 'DEFAULT')
      ),
      optional($.construct_name)
    ),

    // ========================================================================
    // F2003 USER-DEFINED DERIVED TYPE I/O
    // ========================================================================

    // Enhanced interface block with user-defined I/O
    interface_stmt: $ => choice(
      'INTERFACE',
      seq('INTERFACE', $.generic_spec),
      seq('INTERFACE', 'READ', '(', 'FORMATTED', ')'),    // F2003: User-defined I/O
      seq('INTERFACE', 'READ', '(', 'UNFORMATTED', ')'),
      seq('INTERFACE', 'WRITE', '(', 'FORMATTED', ')'),
      seq('INTERFACE', 'WRITE', '(', 'UNFORMATTED', ')')
    ),

    // ========================================================================
    // F2003 ENHANCED INTRINSIC PROCEDURES
    // ========================================================================

    // F2003 intrinsic procedures
    f2003_intrinsic_name: $ => choice(
      // Type inquiry
      'EXTENDS_TYPE_OF',
      'SAME_TYPE_AS',
      // C interoperability
      'C_ASSOCIATED',
      'C_F_POINTER',
      'C_F_PROCPOINTER',
      'C_FUNLOC',
      'C_LOC',
      'C_SIZEOF',
      // IEEE arithmetic
      'IEEE_IS_FINITE',
      'IEEE_IS_NAN',
      'IEEE_IS_NEGATIVE',
      'IEEE_IS_NORMAL',
      // Enhanced intrinsics
      'COMMAND_ARGUMENT_COUNT',
      'GET_COMMAND',
      'GET_COMMAND_ARGUMENT',
      'GET_ENVIRONMENT_VARIABLE',
      'MOVE_ALLOC',
      'NEW_LINE'
    ),

    // Add F2003 intrinsics to primary base
    primary_base: $ => choice(
      $.constant,
      $.designator,
      $.array_constructor,
      $.structure_constructor,
      $.function_reference,
      $.null_expr,
      seq('(', $.expression, ')')
    ),

    // ========================================================================
    // F2003 ENHANCED ACTION STATEMENTS  
    // ========================================================================

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
      $.cycle_stmt,
      $.wait_stmt                         // F2003: WAIT statement
    ),

    // Enhanced executable construct
    executable_construct: $ => choice(
      $.action_stmt,
      $.associate_construct,
      $.case_construct,
      $.do_construct,
      $.forall_construct,
      $.if_construct,
      $.where_construct,
      $.select_type_construct             // F2003: SELECT TYPE
    )
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}