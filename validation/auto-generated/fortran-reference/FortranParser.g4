
// $antlr-format alignColons hanging, alignSemicolons hanging, alignTrailingComments true, allowShortBlocksOnASingleLine true
// $antlr-format allowShortRulesOnASingleLine false, columnLimit 150, maxEmptyLinesToKeep 1, minEmptyLines 1, reflowComments false, useTab false

parser grammar FortranParser;

options {
    tokenVocab = FortranLexer;
}

name
    : NAME
    | PROGRAM
    | END
    | FUNCTION
    | SUBROUTINE
    | MODULE
    | SUBMODULE
    | BLOCK
    | DATA
    | INTRINSIC
    | NONINTRINSIC
    | OPERATOR
    | READ
    | FORMATTED
    | UNFORMATTED
    | WRITE
    | ASSIGNMENT
    | USE
    | ONLY
    | IMPORT
    | NONE
    | ALL
    | KIND
    | INTEGER
    | LEN
    | REAL
    | DOUBLE
    | PRECISION
    | COMPLEX
    | CHARACTER
    | LOGICAL
    | TYPE
    | CLASS
    | EXTERNAL
    | IMPLICIT
    | PARAMETER
    | FORMAT
    | BIND
    | NAME
    | RESULT
    | ENTRY
    | STAT
    | TEAM
    | TEAMNUMBER
    | RE
    | IM
    | SEQUENCE
    | PRIVATE
    | PROCEDURE
    | NOPASS
    | PASS
    | POINTER
    | ALLOCATABLE
    | CODIMENSION
    | CONTIGUOUS
    | DIMENSION
    | PUBLIC
    | CONTAINS
    | FINAL
    | GENERIC
    | DEFERRED
    | NONOVERRIDABLE
    | INTENT
    | OPTIONAL
    | PROTECTED
    | SAVE
    | IN
    | OUT
    | INOUT
    | INTERFACE
    | ABSTRACT
    | ENUM
    | ENUMERATOR
    | ASYNCHRONOUS
    | TARGET
    | VALUE
    | VOLATILE
    | EQUIVALENCE
    | COMMON
    | NAMELIST
    | EVENT
    | WAIT
    | UNTILCOUNT
    | POST
    | ERRMSG
    | ERROR
    | STOP
    | QUIET
    | ENDFILE
    | DEALLOCATE
    | CYCLE
    | CONTINUE
    | CLOSE
    | UNIT
    | IOSTAT
    | IOMSG
    | ERR
    | STATUS
    | CALL
    | BACKSPACE
    | ALLOCATE
    | MOLD
    | SOURCE
    | OPEN
    | ACCESS
    | ACTION
    | BLANK
    | DECIMAL
    | DELIM
    | ENCODING
    | FILE
    | FORM
    | NEWUNIT
    | PAD
    | POSITION
    | RECL
    | ROUND
    | SIGN
    | NULLIFY
    | LOCK
    | ACQUIREDLOCK
    | INQUIRE
    | IOLENGTH
    | EXIST
    | ID
    | NAMED
    | NEXTREC
    | NUMBER
    | OPENED
    | PENDING
    | POS
    | READWRITE
    | SEQUENTIAL
    | SIZE
    | STREAM
    | IF
    | GO
    | TO
    | NEWINDEX
    | FLUSH
    | FAIL
    | IMAGE
    | EXIT
    | FORALL
    | WHERE
    | EOR
    | UNLOCK
    | SYNC
    | MEMORY
    | IMAGES
    | REWIND
    | RETURN
    | FMT
    | NML
    | ADVANCE
    | REC
    | PRINT
    | CRITICAL
    | CHANGE
    | SELECT
    | CASE
    | DEFAULT
    | ASSOCIATE
    | ELSEWHERE
    | IS
    | RANK
    | ELSE
    | THEN
    | DO
    | CONCURRENT
    | WHILE
    | SHARED
    | LOCAL
    | LOCALINIT
    | RECURSIVE
    | PURE
    | NONRECURSIVE
    | IMPURE
    | ELEMENTAL
    | NOTIFY
    | TYPEOF
    | CLASSOF
    | ENUMERATION
    | DIRECT
    | LEADINGZERO
    | REDUCE
    | SIMPLE
    ;

// add in start rule.
file_
    : program EOF
    ;

program
    : program_unit program_unit*
    ;

program_unit
    : main_program
    | external_subprogram
    | module
    | submodule
    | block_data
    ;

main_program
    : program_stmt? specification_part? execution_part? internal_subprogram_part? end_program_stmt
    ;

external_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;

function_subprogram
    : function_stmt specification_part? execution_part? internal_subprogram_part? end_function_stmt
    ;

subroutine_subprogram
    : subroutine_stmt specification_part? execution_part? internal_subprogram_part? end_subroutine_stmt
    ;

module
    : module_stmt specification_part? module_subprogram_part? end_module_stmt
    ;

submodule
    : submodule_stmt specification_part? module_subprogram_part? end_submodule_stmt
    ;

block_data
    : block_data_stmt specification_part? end_block_data_stmt
    ;

specification_part
    : use_stmt* import_stmt* implicit_part? declaration_construct*
    ;

implicit_part
    : implicit_part_stmt* implicit_stmt
    ;

implicit_part_stmt
    : implicit_stmt
    | parameter_stmt
    | format_stmt
    | entry_stmt
    ;

declaration_construct
    : specification_construct
    | data_stmt
    | format_stmt
    | entry_stmt
    | stmt_function_stmt
    ;

specification_construct
    : derived_type_def
    | enum_def
    | enumeration_type_def
    | generic_stmt
    | interface_block
    | parameter_stmt
    | procedure_declaration_stmt
    | other_specification_stmt
    | type_declaration_stmt
    ;

execution_part
    : executable_construct execution_part_construct*
    ;

execution_part_construct
    : executable_construct
    | format_stmt
    | entry_stmt
    | data_stmt
    ;

internal_subprogram_part
    : contains_stmt internal_subprogram*
    ;

internal_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;

module_subprogram_part
    : contains_stmt module_subprogram*
    ;

module_subprogram
    : function_subprogram
    | subroutine_subprogram
    | separate_module_subprogram
    ;

separate_module_subprogram
    : mp_subprogram_stmt specification_part? execution_part? internal_subprogram_part? end_mp_subprogram_stmt
    ;

other_specification_stmt
    : access_stmt
    | allocatable_stmt
    | asynchronous_stmt
    | bind_stmt
    | codimension_stmt
    | contiguous_stmt
    | dimension_stmt
    | external_stmt
    | intent_stmt
    | intrinsic_stmt
    | namelist_stmt
    | optional_stmt
    | pointer_stmt
    | protected_stmt
    | save_stmt
    | target_stmt
    | volatile_stmt
    | value_stmt
    | common_stmt
    | equivalence_stmt
    ;

executable_construct
    : action_stmt
    | associate_construct
    | block_construct
    | case_construct
    | change_team_construct
    | critical_construct
    | do_construct
    | if_construct
    | select_rank_construct
    | select_type_construct
    | where_construct
    | forall_construct
    ;

action_stmt
    : allocate_stmt
    | assignment_stmt
    | backspace_stmt
    | call_stmt
    | close_stmt
    | continue_stmt
    | cycle_stmt
    | deallocate_stmt
    | endfile_stmt
    | error_stop_stmt
    | event_post_stmt
    | event_wait_stmt
    | exit_stmt
    | fail_image_stmt
    | flush_stmt
    | form_team_stmt
    | goto_stmt
    | if_stmt
    | inquire_stmt
    | lock_stmt
    | notify_wait_stmt
    | nullify_stmt
    | open_stmt
    | pointer_assignment_stmt
    | print_stmt
    | read_stmt
    | return_stmt
    | rewind_stmt
    | stop_stmt
    | sync_all_stmt
    | sync_images_stmt
    | sync_memory_stmt
    | sync_team_stmt
    | unlock_stmt
    | wait_stmt
    | where_stmt
    | write_stmt
    | computed_goto_stmt
    | forall_stmt
    ;

keyword
    : name
    ;

underscore
    : UNDERSCORE
    ;

constant
    : literal_constant
    | named_constant
    ;

literal_constant
    : int_literal_constant
    | real_literal_constant
    | complex_literal_constant
    | logical_literal_constant
    | char_literal_constant
    | boz_literal_constant
    ;

named_constant
    : name
    ;

int_constant
    : constant
    ;

intrinsic_operator
    : power_op
    | mult_op
    | add_op
    | concat_op
    | rel_op
    | not_op
    | and_op
    | or_op
    | equiv_op
    ;

power_op
    : '**'
    ;

mult_op
    : ASTERIK
    | SLASH
    ;

add_op
    : PLUS
    | MINUS
    ;

concat_op
    : '//'
    ;

rel_op
    : '.EQ.'
    | '.NE.'
    | '.LT.'
    | '.LE.'
    | '.GT.'
    | '.GE.'
    | '=='
    | '/='
    | '<'
    | '<='
    | '>'
    | '>='
    ;

not_op
    : '.NOT.'
    ;

and_op
    : '.AND.'
    ;

or_op
    : '.OR.'
    ;

equiv_op
    : '.EQV.'
    | '.NEQV.'
    ;

defined_operator
    : defined_unary_op
    | defined_binary_op
    | extended_intrinsic_op
    ;

defined_unary_op
    : DEFINEDUNARYBINARYOP
    ;

defined_binary_op
    : DEFINEDUNARYBINARYOP
    ;

extended_intrinsic_op
    : intrinsic_operator
    ;

label
    : DIGITSTRING
    ;

type_param_value
    : scalar_int_expr
    | ASTERIK
    | COLON
    ;

type_spec
    : intrinsic_type_spec
    | derived_type_spec
    | enum_type_spec
    | enumeration_type_spec
    ;

declaration_type_spec
    : intrinsic_type_spec
    | 'TYPE' LPAREN intrinsic_type_spec RPAREN
    | 'TYPE' LPAREN derived_type_spec RPAREN
    | 'TYPE' LPAREN enum_type_spec RPAREN
    | 'TYPE' LPAREN enumeration_type_spec RPAREN
    | 'CLASS' LPAREN derived_type_spec RPAREN
    | 'CLASS' LPAREN ASTERIK RPAREN
    | 'TYPE' LPAREN ASTERIK RPAREN
    | 'TYPEOF' LPAREN data_ref RPAREN
    | 'CLASSOF' LPAREN data_ref RPAREN
    ;

intrinsic_type_spec
    : integer_type_spec
    | 'REAL' kind_selector?
    | 'DOUBLE' 'PRECISION'
    | 'COMPLEX' kind_selector?
    | 'CHARACTER' char_selector?
    | 'LOGICAL' kind_selector?
    ;

integer_type_spec
    : 'INTEGER' kind_selector?
    ;

kind_selector
    : LPAREN ('KIND' '=')? scalar_int_constant_expr RPAREN
    ;

signed_int_literal_constant
    : sign? int_literal_constant
    ;

int_literal_constant
    : digit_string (UNDERSCORE kind_param)?
    ;

kind_param
    : digit_string
    | scalar_int_constant_name
    ;

signed_digit_string
    : sign? digit_string
    ;

digit_string
    : DIGITSTRING
    ;

sign
    : PLUS
    | MINUS
    ;

signed_real_literal_constant
    : sign? real_literal_constant
    ;

real_literal_constant
    : significand (exponent_letter exponent)? (UNDERSCORE kind_param)?
    | digit_string exponent_letter exponent ( UNDERSCORE kind_param)?
    ;

significand
    : digit_string DOT digit_string?
    | DOT digit_string
    ;

exponent_letter
    : E
    | D
    ;

exponent
    : signed_digit_string
    ;

complex_literal_constant
    : LPAREN real_part COMMA imag_part RPAREN
    ;

real_part
    : signed_int_literal_constant
    | signed_real_literal_constant
    | named_constant
    ;

imag_part
    : signed_int_literal_constant
    | signed_real_literal_constant
    | named_constant
    ;

char_selector
    : length_selector
    | LPAREN 'LEN' '=' type_param_value COMMA 'KIND' '=' scalar_int_constant_expr RPAREN
    | LPAREN type_param_value COMMA ( 'KIND' '=')? scalar_int_constant_expr RPAREN
    | LPAREN 'KIND' '=' scalar_int_constant_expr (COMMA 'LEN' '=' type_param_value)? RPAREN
    ;

length_selector
    : LPAREN ('LEN' '=')? type_param_value RPAREN
    | ASTERIK char_length COMMA?
    ;

char_length
    : LPAREN type_param_value RPAREN
    | int_literal_constant
    ;

char_literal_constant
    : (kind_param UNDERSCORE)? SQUOTE_REP_CHAR
    | ( kind_param UNDERSCORE)? DQUOTE_REP_CHAR
    ;

logical_literal_constant
    : '.TRUE.' (UNDERSCORE kind_param)?
    | '.FALSE.' ( UNDERSCORE kind_param)?
    ;

derived_type_def
    : derived_type_stmt type_param_def_stmt* private_or_sequence* component_part? type_bound_procedure_part? end_type_stmt
    ;

derived_type_stmt
    : 'TYPE' (( COMMA type_attr_spec_list)? '::')? type_name (LPAREN type_param_name_list RPAREN)?
    ;

type_attr_spec
    : 'ABSTRACT'
    | access_spec
    | 'BIND' LPAREN C RPAREN
    | 'EXTENDS' LPAREN parent_type_name RPAREN
    ;

private_or_sequence
    : private_components_stmt
    | sequence_stmt
    ;

end_type_stmt
    : 'END' 'TYPE' type_name?
    ;

sequence_stmt
    : 'SEQUENCE'
    ;

type_param_def_stmt
    : integer_type_spec COMMA type_param_attr_spec '::' type_param_decl_list
    ;

type_param_decl
    : type_param_name ('=' scalar_int_constant_expr)?
    ;

type_param_attr_spec
    : 'KIND'
    | 'LEN'
    ;

component_part
    : component_def_stmt*
    ;

component_def_stmt
    : data_component_def_stmt
    | proc_component_def_stmt
    ;

data_component_def_stmt
    : declaration_type_spec (( COMMA component_attr_spec_list)? '::')? component_decl_list
    ;

component_attr_spec
    : access_spec
    | 'ALLOCATABLE'
    | 'CODIMENSION' lbracket coarray_spec rbracket
    | 'CONTIGUOUS'
    | 'DIMENSION' LPAREN component_array_spec RPAREN
    | 'POINTER'
    ;

component_decl
    : component_name (LPAREN component_array_spec RPAREN)? (lbracket coarray_spec rbracket)? (
        ASTERIK char_length
    )? component_initialization?
    ;

component_array_spec
    : explicit_shape_spec_list
    | deferred_shape_spec_list
    ;

proc_component_def_stmt
    : 'PROCEDURE' LPAREN proc_interface? RPAREN COMMA proc_component_attr_spec_list '::' proc_decl_list
    ;

proc_component_attr_spec
    : access_spec
    | 'NOPASS'
    | 'PASS' ( LPAREN arg_name RPAREN)?
    | 'POINTER'
    ;

component_initialization
    : '=' constant_expr
    | '=>' null_init
    | '=>' initial_data_target
    ;

initial_data_target
    : designator
    ;

private_components_stmt
    : 'PRIVATE'
    ;

type_bound_procedure_part
    : contains_stmt binding_private_stmt? type_bound_proc_binding*
    ;

binding_private_stmt
    : 'PRIVATE'
    ;

type_bound_proc_binding
    : type_bound_procedure_stmt
    | type_bound_generic_stmt
    | final_procedure_stmt
    ;

type_bound_procedure_stmt
    : 'PROCEDURE' (( COMMA binding_attr_list)? '::')? type_bound_proc_decl_list
    | 'PROCEDURE' LPAREN interface_name RPAREN COMMA binding_attr_list '::' binding_name_list
    ;

type_bound_proc_decl
    : binding_name ('=>' procedure_name)?
    ;

type_bound_generic_stmt
    : 'GENERIC' (COMMA access_spec)? '::' generic_spec '=>' binding_name_list
    ;

binding_attr
    : access_spec
    | 'DEFERRED'
    | 'NON_OVERRIDABLE'
    | 'NOPASS'
    | 'PASS' ( LPAREN arg_name RPAREN)?
    ;

final_procedure_stmt
    : 'FINAL' '::'? final_subroutine_name_list
    ;

derived_type_spec
    : type_name (LPAREN type_param_spec_list RPAREN)?
    ;

type_param_spec
    : (keyword '=')? type_param_value
    ;

structure_constructor
    : derived_type_spec LPAREN component_spec_list? RPAREN
    ;

component_spec
    : (keyword '=')? component_data_source
    ;

component_data_source
    : expr
    | data_target
    | proc_target
    ;

enum_def
    : enum_def_stmt enumerator_def_stmt enumerator_def_stmt* end_enum_stmt
    ;

enum_def_stmt
    : ENUM COMMA BIND LPAREN C RPAREN ('::' enum_type_name)?
    ;

enumerator_def_stmt
    : 'ENUMERATOR' '::'? enumerator_list
    ;

enumerator
    : named_constant ('=' scalar_int_constant_expr)?
    ;

end_enum_stmt
    : 'END' 'ENUM'
    ;

enum_type_spec
    : enum_type_name
    ;

enum_constructor
    : enum_type_spec LPAREN scalar_expr RPAREN
    ;

enumeration_type_def
    : enumeration_type_stmt enumeration_enumerator_stmt enumeration_enumerator_stmt* end_enumeration_type_stmt
    ;

enumeration_type_stmt
    : 'ENUMERATION' 'TYPE' (( COMMA access_spec)? '::')? enumeration_type_name
    ;

enumeration_enumerator_stmt
    : 'ENUMERATOR' '::'? enumerator_name_list
    ;

end_enumeration_type_stmt
    : 'END' 'ENUMERATION' 'TYPE' enumeration_type_name?
    ;

enumeration_type_spec
    : enumeration_type_name
    ;

enumeration_constructor
    : enumeration_type_spec LPAREN scalar_int_expr RPAREN
    ;

boz_literal_constant
    : BINARY_CONSTANT
    | OCTAL_CONSTANT
    | HEX_CONSTANT
    ;

array_constructor
    : '(/' ac_spec '/)'
    | lbracket ac_spec rbracket
    ;

ac_spec
    : type_spec '::'
    | (type_spec '::')? ac_value_list
    ;

lbracket
    : '['
    ;

rbracket
    : ']'
    ;

ac_value
    : expr
    | ac_implied_do
    ;

ac_implied_do
    : LPAREN ac_value_list COMMA ac_implied_do_control RPAREN
    ;

ac_implied_do_control
    : (integer_type_spec '::')? ac_do_variable '=' scalar_int_expr COMMA scalar_int_expr (
        COMMA scalar_int_expr
    )?
    ;

ac_do_variable
    : do_variable
    ;

type_declaration_stmt
    : declaration_type_spec (( COMMA attr_spec)* '::')? entity_decl_list
    ;

attr_spec
    : access_spec
    | 'ALLOCATABLE'
    | 'ASYNCHRONOUS'
    | 'CODIMENSION' lbracket coarray_spec rbracket
    | 'CONTIGUOUS'
    | 'DIMENSION' LPAREN array_spec RPAREN
    | 'EXTERNAL'
    | 'INTENT' LPAREN intent_spec RPAREN
    | 'INTRINSIC'
    | language_binding_spec
    | 'OPTIONAL'
    | 'PARAMETER'
    | 'POINTER'
    | 'PROTECTED'
    | rank_clause
    | 'SAVE'
    | 'TARGET'
    | 'VALUE'
    | 'VOLATILE'
    ;

entity_decl
    : object_name (LPAREN array_spec RPAREN)? (lbracket coarray_spec rbracket)? (
        ASTERIK char_length
    )? initialization?
    | function_name ( ASTERIK char_length)?
    ;

object_name
    : name
    ;

initialization
    : '=' constant_expr
    | '=>' null_init
    | '=>' initial_data_target
    ;

null_init
    : function_reference
    ;

access_spec
    : 'PUBLIC'
    | 'PRIVATE'
    ;

language_binding_spec
    : 'BIND' LPAREN C (COMMA 'NAME' '=' scalar_default_char_constant_expr)? RPAREN
    ;

coarray_spec
    : deferred_coshape_spec_list
    | explicit_coshape_spec
    ;

deferred_coshape_spec
    : COLON
    ;

explicit_coshape_spec
    : (( lower_cobound COLON)? upper_cobound COMMA)* (lower_cobound COLON)? ASTERIK
    ;

lower_cobound
    : specification_expr
    ;

upper_cobound
    : specification_expr
    ;

array_spec
    : explicit_shape_spec_list
    | explicit_shape_bounds_spec
    | assumed_shape_spec_list
    | assumed_shape_bounds_spec
    | deferred_shape_spec_list
    | assumed_size_spec
    | implied_shape_spec
    | implied_shape_or_assumed_size_spec
    | assumed_rank_spec
    ;

explicit_shape_spec
    : (lower_bound COLON)? upper_bound
    ;

lower_bound
    : specification_expr
    ;

upper_bound
    : specification_expr
    ;

explicit_shape_bounds_spec
    : (explicit_bounds_expr COLON)? explicit_bounds_expr
    | lower_bound COLON explicit_bounds_expr
    | explicit_bounds_expr COLON upper_bound
    ;

explicit_bounds_expr
    : int_expr
    ;

assumed_shape_spec
    : lower_bound? COLON
    ;

assumed_shape_bounds_spec
    : explicit_bounds_expr COLON
    ;

deferred_shape_spec
    : COLON
    ;

assumed_implied_spec
    : (lower_bound COLON)? ASTERIK
    ;

assumed_size_spec
    : explicit_shape_spec_list COMMA assumed_implied_spec
    ;

implied_shape_or_assumed_size_spec
    : assumed_implied_spec
    ;

implied_shape_spec
    : assumed_implied_spec COMMA assumed_implied_spec_list
    ;

assumed_rank_spec
    : '..'
    ;

intent_spec
    : 'IN'
    | 'OUT'
    | 'INOUT'
    ;

rank_clause
    : 'RANK' LPAREN scalar_int_constant_expr RPAREN
    ;

access_stmt
    : access_spec ('::'? access_id_list)?
    ;

access_id
    : access_name
    | generic_spec
    ;

allocatable_stmt
    : 'ALLOCATABLE' '::'? allocatable_decl_list
    ;

allocatable_decl
    : object_name (LPAREN array_spec RPAREN)? (lbracket coarray_spec rbracket)?
    ;

asynchronous_stmt
    : 'ASYNCHRONOUS' '::'? object_name_list
    ;

bind_stmt
    : language_binding_spec '::'? bind_entity_list
    ;

bind_entity
    : entity_name
    | SLASH common_block_name SLASH
    ;

codimension_stmt
    : 'CODIMENSION' '::'? codimension_decl_list
    ;

codimension_decl
    : coarray_name lbracket coarray_spec rbracket
    ;

contiguous_stmt
    : 'CONTIGUOUS' '::'? object_name_list
    ;

data_stmt
    : 'DATA' data_stmt_set (COMMA? data_stmt_set)*
    ;

data_stmt_set
    : data_stmt_object_list SLASH data_stmt_value_list SLASH
    ;

data_stmt_object
    : variable
    | data_implied_do
    ;

data_implied_do
    : LPAREN data_i_do_object_list COMMA (integer_type_spec '::')? data_i_do_variable '=' scalar_int_constant_expr COMMA scalar_int_constant_expr (
        COMMA scalar_int_constant_expr
    )? RPAREN
    ;

data_i_do_object
    : array_element
    | scalar_structure_component
    | data_implied_do
    ;

data_i_do_variable
    : do_variable
    ;

data_stmt_value
    : (data_stmt_repeat ASTERIK)? data_stmt_constant
    ;

data_stmt_repeat
    : scalar_int_constant
    | scalar_int_constant_subobject
    ;

data_stmt_constant
    : scalar_constant
    | scalar_constant_subobject
    | signed_int_literal_constant
    | signed_real_literal_constant
    | null_init
    | initial_data_target
    | structure_constructor
    | enum_constructor
    | enumeration_constructor
    ;

int_constant_subobject
    : constant_subobject
    ;

constant_subobject
    : designator
    ;

dimension_stmt
    : 'DIMENSION' '::'? array_name LPAREN array_spec RPAREN (
        COMMA array_name LPAREN array_spec RPAREN
    )*
    ;

intent_stmt
    : 'INTENT' LPAREN intent_spec RPAREN '::'? dummy_arg_name_list
    ;

optional_stmt
    : 'OPTIONAL' '::'? dummy_arg_name_list
    ;

parameter_stmt
    : 'PARAMETER' LPAREN named_constant_def_list RPAREN
    ;

named_constant_def
    : named_constant '=' constant_expr
    ;

pointer_stmt
    : 'POINTER' '::'? pointer_decl_list
    ;

pointer_decl
    : object_name (LPAREN deferred_shape_spec_list RPAREN)?
    | procptr_entity_name
    ;

protected_stmt
    : 'PROTECTED' '::'? entity_name_list
    ;

save_stmt
    : 'SAVE' ('::'? saved_entity_list)?
    ;

saved_entity
    : object_name
    | proc_pointer_name
    | SLASH common_block_name SLASH
    ;

proc_pointer_name
    : name
    ;

target_stmt
    : 'TARGET' '::'? target_decl_list
    ;

target_decl
    : object_name (LPAREN array_spec RPAREN)? (lbracket coarray_spec rbracket)?
    ;

value_stmt
    : 'VALUE' '::'? dummy_arg_name_list
    ;

volatile_stmt
    : 'VOLATILE' '::'? object_name_list
    ;

implicit_stmt
    : 'IMPLICIT' implicit_spec_list
    | 'IMPLICIT' 'NONE' ( LPAREN implicit_none_spec_list? RPAREN)?
    ;

implicit_spec
    : declaration_type_spec LPAREN letter_spec_list RPAREN
    ;

letter_spec
    : LETTER_SPEC
    ;

implicit_none_spec
    : 'EXTERNAL'
    | 'TYPE'
    ;

import_stmt
    : 'IMPORT' ('::'? import_name_list)?
    | IMPORT COMMA 'ONLY' COLON import_name_list
    | IMPORT COMMA 'NONE'
    | IMPORT COMMA 'ALL'
    ;

namelist_stmt
    : 'NAMELIST' SLASH namelist_group_name SLASH namelist_group_object_list (
        COMMA? SLASH namelist_group_name SLASH namelist_group_object_list
    )*
    ;

namelist_group_object
    : variable_name
    ;

equivalence_stmt
    : 'EQUIVALENCE' equivalence_set_list
    ;

equivalence_set
    : LPAREN equivalence_object COMMA equivalence_object_list RPAREN
    ;

equivalence_object
    : variable_name
    | array_element
    | substring
    ;

common_stmt
    : 'COMMON' (SLASH common_block_name? SLASH)? common_block_object_list (
        COMMA? SLASH common_block_name? SLASH common_block_object_list
    )*
    ;

common_block_object
    : variable_name (LPAREN array_spec RPAREN)?
    ;

designator
    : object_name
    | array_element
    | data_ref ( LPAREN substring_range RPAREN)?
    | designator PERCENT RE
    | designator PERCENT IM
    | coindexed_named_object
    | structure_component
    | substring
    ;

variable
    : designator
    | procedure_name LPAREN actual_arg_spec_list? RPAREN
    | variable PERCENT procedure_component_name LPAREN actual_arg_spec_list? RPAREN
    | data_ref PERCENT binding_name LPAREN actual_arg_spec_list? RPAREN
    ;

variable_name
    : name
    ;

logical_variable
    : variable
    ;

char_variable
    : variable
    ;

default_char_variable
    : variable
    ;

int_variable
    : variable
    ;

substring
    : parent_string LPAREN substring_range RPAREN
    ;

parent_string
    : scalar_variable_name
    | array_element
    | coindexed_named_object
    | scalar_structure_component
    | scalar_constant
    ;

substring_range
    : scalar_int_expr? COLON scalar_int_expr?
    ;

data_ref
    : part_ref ('%' part_ref)*
    ;

part_ref
    : part_name (LPAREN section_subscript_list RPAREN)? image_selector?
    ;

structure_component
    : data_ref
    ;

coindexed_named_object
    : data_ref
    ;

complex_part_designator
    : designator '%' 'RE'
    | designator '%' 'IM'
    ;

type_param_inquiry
    : designator '%' type_param_name
    ;

array_element
    : data_ref
    ;

array_section
    : data_ref (LPAREN substring_range RPAREN)?
    | complex_part_designator
    ;

subscript
    : scalar_int_expr
    ;

multiple_subscript
    : '@' int_expr
    ;

section_subscript
    : subscript
    | multiple_subscript
    | subscript_triplet
    | multiple_subscript_triplet
    | vector_subscript
    ;

subscript_triplet
    : subscript? COLON subscript? (COLON stride)?
    ;

multiple_subscript_triplet
    : '@' int_expr? COLON int_expr? (COLON int_expr)?
    ;

stride
    : scalar_int_expr
    ;

vector_subscript
    : int_expr
    ;

image_selector
    : lbracket cosubscript_list (COMMA image_selector_spec_list)? rbracket
    ;

cosubscript
    : scalar_int_expr
    ;

image_selector_spec
    : 'NOTIFY' '=' notify_variable
    | 'STAT' '=' stat_variable
    | 'TEAM' '=' team_value
    | 'TEAM_NUMBER' '=' scalar_int_expr
    ;

allocate_stmt
    : 'ALLOCATE' LPAREN (type_spec '::')? allocation_list (COMMA alloc_opt_list)? RPAREN
    ;

alloc_opt
    : 'ERRMSG' '=' errmsg_variable
    | 'MOLD' '=' source_expr
    | 'SOURCE' '=' source_expr
    | 'STAT' '=' stat_variable
    ;

errmsg_variable
    : scalar_default_char_variable
    ;

source_expr
    : expr
    ;

allocation
    : allocate_object (LPAREN allocate_shape_spec_list RPAREN)? (
        lbracket allocate_coarray_spec rbracket
    )?
    | LPAREN (lower_bounds_expr COLON)? upper_bounds_expr RPAREN (
        lbracket allocate_coarray_spec rbracket
    )?
    ;

allocate_object
    : variable_name
    | structure_component
    ;

allocate_shape_spec
    : (lower_bound_expr COLON)? upper_bound_expr
    ;

lower_bound_expr
    : scalar_int_expr
    ;

lower_bounds_expr
    : int_expr
    ;

upper_bound_expr
    : scalar_int_expr
    ;

upper_bounds_expr
    : int_expr
    ;

allocate_coarray_spec
    : (allocate_coshape_spec_list COMMA)? (lower_bound_expr COLON)? ASTERIK
    ;

allocate_coshape_spec
    : (lower_bound_expr COLON)? upper_bound_expr
    ;

nullify_stmt
    : 'NULLIFY' LPAREN pointer_object_list RPAREN
    ;

pointer_object
    : variable_name
    | structure_component
    | proc_pointer_name
    ;

deallocate_stmt
    : 'DEALLOCATE' LPAREN allocate_object_list (COMMA dealloc_opt_list)? RPAREN
    ;

dealloc_opt
    : 'STAT' '=' stat_variable
    | 'ERRMSG' '=' errmsg_variable
    ;

stat_variable
    : scalar_int_variable
    ;

primary
    : literal_constant
    | designator
    | array_constructor
    | structure_constructor
    | enum_constructor
    | enumeration_constructor
    | function_reference
    | type_param_inquiry
    | type_param_name
    | LPAREN expr RPAREN
    | conditional_expr
    ;

conditional_expr
    : LPAREN scalar_logical_expr '?' expr (COLON scalar_logical_expr '?' expr)* COLON expr RPAREN
    ;

level_1_expr
    : defined_unary_op? primary
    ;

mult_operand
    : level_1_expr (power_op mult_operand)?
    ;

add_operand
    : mult_operand (mult_op mult_operand)*
    ;

level_2_expr
    : (add_operand | add_op add_operand) (add_op add_operand)*
    ;

level_3_expr
    : level_2_expr (concat_op level_2_expr)*
    ;

level_4_expr
    : (level_3_expr rel_op)? level_3_expr
    ;

and_operand
    : not_op? level_4_expr
    ;

or_operand
    : and_operand (and_op and_operand)*
    ;

equiv_operand
    : or_operand (or_op or_operand)*
    ;

level_5_expr
    : equiv_operand (equiv_op equiv_operand)*
    ;

expr
    : level_5_expr (defined_binary_op level_5_expr)*
    ;

logical_expr
    : expr
    ;

default_char_expr
    : expr
    ;

int_expr
    : expr
    ;

numeric_expr
    : expr
    ;

specification_expr
    : scalar_int_expr
    ;

constant_expr
    : expr
    ;

default_char_constant_expr
    : default_char_expr
    ;

int_constant_expr
    : int_expr
    ;

assignment_stmt
    : variable '=' expr
    ;

pointer_assignment_stmt
    : data_pointer_object (LPAREN bounds_spec_list RPAREN)? '=>' data_target
    | data_pointer_object LPAREN lower_bounds_expr COLON RPAREN '=>' data_target
    | data_pointer_object LPAREN bounds_remapping_list RPAREN '=>' data_target
    | data_pointer_object LPAREN lower_bounds_expr COLON upper_bounds_expr RPAREN '=>' data_target
    | proc_pointer_object '=>' proc_target
    ;

data_pointer_object
    : variable_name
    | scalar_variable '%' data_pointer_component_name
    ;

bounds_spec
    : lower_bound_expr COLON
    ;

bounds_remapping
    : lower_bound_expr COLON upper_bound_expr
    ;

data_target
    : expr
    ;

proc_pointer_object
    : proc_pointer_name
    | proc_component_ref
    ;

proc_component_ref
    : scalar_variable '%' procedure_component_name
    ;

proc_target
    : expr
    | procedure_name
    | proc_component_ref
    ;

where_stmt
    : 'WHERE' LPAREN mask_expr RPAREN where_assignment_stmt
    ;

where_construct
    : where_construct_stmt where_body_construct* (masked_elsewhere_stmt where_body_construct*)* (
        elsewhere_stmt where_body_construct*
    )? end_where_stmt
    ;

where_construct_stmt
    : (where_construct_name COLON)? 'WHERE' LPAREN mask_expr RPAREN
    ;

where_body_construct
    : where_assignment_stmt
    | where_stmt
    | where_construct
    ;

where_assignment_stmt
    : assignment_stmt
    ;

mask_expr
    : logical_expr
    ;

masked_elsewhere_stmt
    : 'ELSEWHERE' LPAREN mask_expr RPAREN where_construct_name?
    ;

elsewhere_stmt
    : 'ELSEWHERE' where_construct_name?
    ;

end_where_stmt
    : 'END' 'WHERE' where_construct_name?
    ;

forall_construct
    : forall_construct_stmt forall_body_construct* end_forall_stmt
    ;

forall_construct_stmt
    : (forall_construct_name COLON)? 'FORALL' concurrent_header
    ;

forall_body_construct
    : forall_assignment_stmt
    | where_stmt
    | where_construct
    | forall_construct
    | forall_stmt
    ;

forall_assignment_stmt
    : assignment_stmt
    | pointer_assignment_stmt
    ;

end_forall_stmt
    : 'END' 'FORALL' forall_construct_name?
    ;

forall_stmt
    : 'FORALL' concurrent_header forall_assignment_stmt
    ;

block
    : execution_part_construct*
    ;

associate_construct
    : associate_stmt block end_associate_stmt
    ;

associate_stmt
    : (associate_construct_name COLON)? 'ASSOCIATE' LPAREN association_list RPAREN
    ;

association
    : associate_name '=>' selector
    ;

selector
    : expr
    | variable
    ;

end_associate_stmt
    : 'END' 'ASSOCIATE' associate_construct_name?
    ;

block_construct
    : block_stmt block_specification_part? block end_block_stmt
    ;

block_stmt
    : (block_construct_name COLON)? 'BLOCK'
    ;

block_specification_part
    : use_stmt* import_stmt* declaration_construct*
    ;

end_block_stmt
    : 'END' 'BLOCK' block_construct_name?
    ;

change_team_construct
    : change_team_stmt block end_change_team_stmt
    ;

change_team_stmt
    : (team_construct_name COLON)? 'CHANGE' 'TEAM' LPAREN team_value (
        COMMA coarray_association_list
    )? (COMMA sync_stat_list)? RPAREN
    ;

coarray_association
    : codimension_decl '=>' selector
    ;

end_change_team_stmt
    : 'END' 'TEAM' (LPAREN sync_stat_list? RPAREN)? team_construct_name?
    ;

team_value
    : scalar_expr
    ;

critical_construct
    : critical_stmt block end_critical_stmt
    ;

critical_stmt
    : (critical_construct_name COLON)? 'CRITICAL' (LPAREN sync_stat_list? RPAREN)?
    ;

end_critical_stmt
    : 'END' 'CRITICAL' critical_construct_name?
    ;

do_construct
    : do_stmt block end_do
    ;

do_stmt
    : nonlabel_do_stmt
    | label_do_stmt
    ;

label_do_stmt
    : (do_construct_name COLON)? 'DO' label loop_control?
    ;

nonlabel_do_stmt
    : (do_construct_name COLON)? 'DO' loop_control?
    ;

loop_control
    : COMMA? do_variable '=' scalar_int_expr COMMA scalar_int_expr (COMMA scalar_int_expr)?
    | COMMA? 'WHILE' LPAREN scalar_logical_expr RPAREN
    | COMMA? 'CONCURRENT' concurrent_header concurrent_locality
    ;

do_variable
    : scalar_int_variable_name
    ;

concurrent_header
    : LPAREN (integer_type_spec '::')? concurrent_control_list (COMMA scalar_mask_expr)? RPAREN
    ;

concurrent_control
    : index_name '=' concurrent_limit COLON concurrent_limit (COLON concurrent_step)?
    ;

concurrent_limit
    : scalar_int_expr
    ;

concurrent_step
    : scalar_int_expr
    ;

concurrent_locality
    : locality_spec*
    ;

locality_spec
    : 'LOCAL' LPAREN variable_name_list RPAREN
    | 'LOCAL_INIT' LPAREN variable_name_list RPAREN
    | 'REDUCE' LPAREN reduce_operation COLON variable_name_list RPAREN
    | 'SHARED' LPAREN variable_name_list RPAREN
    | 'DEFAULT' LPAREN 'NONE' RPAREN
    ;

reduce_operation
    : binary_reduce_op
    | function_reduction_name
    ;

binary_reduce_op
    : PLUS
    | ASTERIK
    | '.AND.'
    | '.OR.'
    | '.EQV.'
    | '.NEQV.'
    ;

end_do
    : end_do_stmt
    | continue_stmt
    ;

end_do_stmt
    : 'END' 'DO' do_construct_name?
    ;

cycle_stmt
    : 'CYCLE' do_construct_name?
    ;

if_construct
    : if_then_stmt block (else_if_stmt block)* (else_stmt block)? end_if_stmt
    ;

if_then_stmt
    : (if_construct_name COLON)? 'IF' LPAREN scalar_logical_expr RPAREN 'THEN'
    ;

else_if_stmt
    : 'ELSE' 'IF' LPAREN scalar_logical_expr RPAREN 'THEN' if_construct_name?
    ;

else_stmt
    : 'ELSE' if_construct_name?
    ;

end_if_stmt
    : 'END' 'IF' if_construct_name?
    ;

if_stmt
    : 'IF' LPAREN scalar_logical_expr RPAREN action_stmt
    ;

case_construct
    : select_case_stmt (case_stmt block)* end_select_stmt
    ;

select_case_stmt
    : (case_construct_name COLON)? 'SELECT' 'CASE' LPAREN case_expr RPAREN
    ;

case_stmt
    : 'CASE' case_selector case_construct_name?
    ;

end_select_stmt
    : 'END' 'SELECT' case_construct_name?
    ;

case_expr
    : scalar_expr
    ;

case_selector
    : LPAREN case_value_range_list RPAREN
    | 'DEFAULT'
    ;

case_value_range
    : case_value
    | case_value COLON
    | COLON case_value
    | case_value COLON case_value
    ;

case_value
    : scalar_constant_expr
    ;

select_rank_construct
    : select_rank_stmt (select_rank_case_stmt block)* end_select_rank_stmt
    ;

select_rank_stmt
    : (select_construct_name COLON)? 'SELECT' 'RANK' LPAREN (associate_name '=>')? selector RPAREN
    ;

select_rank_case_stmt
    : 'RANK' LPAREN scalar_int_constant_expr RPAREN select_construct_name?
    | 'RANK' LPAREN ASTERIK RPAREN select_construct_name?
    | 'RANK' 'DEFAULT' select_construct_name?
    ;

end_select_rank_stmt
    : 'END' 'SELECT' select_construct_name?
    ;

select_type_construct
    : select_type_stmt (type_guard_stmt block)* end_select_type_stmt
    ;

select_type_stmt
    : (select_construct_name COLON)? 'SELECT' 'TYPE' LPAREN (associate_name '=>')? selector RPAREN
    ;

type_guard_stmt
    : 'TYPE' 'IS' LPAREN type_spec RPAREN select_construct_name?
    | 'CLASS' 'IS' LPAREN derived_type_spec RPAREN select_construct_name?
    | 'CLASS' 'DEFAULT' select_construct_name?
    ;

end_select_type_stmt
    : 'END' 'SELECT' select_construct_name?
    ;

exit_stmt
    : 'EXIT' construct_name?
    ;

goto_stmt
    : GO TO label
    ;

computed_goto_stmt
    : GO TO LPAREN label_list RPAREN COMMA? scalar_int_expr
    ;

continue_stmt
    : 'CONTINUE'
    ;

stop_stmt
    : 'STOP' stop_code? (COMMA 'QUIET' '=' scalar_logical_expr)?
    ;

error_stop_stmt
    : 'ERROR' 'STOP' stop_code? (COMMA 'QUIET' '=' scalar_logical_expr)?
    ;

stop_code
    : scalar_default_char_expr
    | scalar_int_expr
    ;

fail_image_stmt
    : 'FAIL' 'IMAGE'
    ;

notify_wait_stmt
    : 'NOTIFY' 'WAIT' LPAREN notify_variable (COMMA event_wait_spec_list)? RPAREN
    ;

notify_variable
    : scalar_variable
    ;

sync_all_stmt
    : 'SYNC' 'ALL' (LPAREN sync_stat_list? RPAREN)?
    ;

sync_stat
    : 'STAT' '=' stat_variable
    | 'ERRMSG' '=' errmsg_variable
    ;

sync_images_stmt
    : 'SYNC' 'IMAGES' LPAREN image_set (COMMA sync_stat_list)? RPAREN
    ;

image_set
    : int_expr
    | ASTERIK
    ;

sync_memory_stmt
    : 'SYNC' 'MEMORY' (LPAREN sync_stat_list? RPAREN)?
    ;

sync_team_stmt
    : 'SYNC' 'TEAM' LPAREN team_value (COMMA sync_stat_list)? RPAREN
    ;

event_post_stmt
    : 'EVENT' 'POST' LPAREN event_variable (COMMA sync_stat_list)? RPAREN
    ;

event_variable
    : scalar_variable
    ;

event_wait_stmt
    : 'EVENT' 'WAIT' LPAREN event_variable (COMMA event_wait_spec_list)? RPAREN
    ;

event_wait_spec
    : until_spec
    | sync_stat
    ;

until_spec
    : 'UNTIL_COUNT' '=' scalar_int_expr
    ;

form_team_stmt
    : 'FORM' 'TEAM' LPAREN team_number COMMA team_variable (COMMA form_team_spec_list)? RPAREN
    ;

team_number
    : scalar_int_expr
    ;

team_variable
    : scalar_variable
    ;

form_team_spec
    : 'NEW_INDEX' '=' scalar_int_expr
    | sync_stat
    ;

lock_stmt
    : 'LOCK' LPAREN lock_variable (COMMA lock_stat_list)? RPAREN
    ;

lock_stat
    : 'ACQUIRED_LOCK' '=' scalar_logical_variable
    | sync_stat
    ;

unlock_stmt
    : 'UNLOCK' LPAREN lock_variable (COMMA sync_stat_list)? RPAREN
    ;

lock_variable
    : scalar_variable
    ;

io_unit
    : file_unit_number
    | ASTERIK
    | internal_file_variable
    ;

file_unit_number
    : scalar_int_expr
    ;

internal_file_variable
    : char_variable
    ;

open_stmt
    : 'OPEN' LPAREN connect_spec_list RPAREN
    ;

connect_spec
    : ('UNIT' '=')? file_unit_number
    | 'ACCESS' '=' scalar_default_char_expr
    | 'ACTION' '=' scalar_default_char_expr
    | 'ASYNCHRONOUS' '=' scalar_default_char_expr
    | 'BLANK' '=' scalar_default_char_expr
    | 'DECIMAL' '=' scalar_default_char_expr
    | 'DELIM' '=' scalar_default_char_expr
    | 'ENCODING' '=' scalar_default_char_expr
    | 'ERR' '=' label
    | 'FILE' '=' file_name_expr
    | 'FORM' '=' scalar_default_char_expr
    | 'IOMSG' '=' iomsg_variable
    | 'IOSTAT' '=' stat_variable
    | 'LEADING_ZERO' '=' scalar_default_char_expr
    | 'NEWUNIT' '=' scalar_int_variable
    | 'PAD' '=' scalar_default_char_expr
    | 'POSITION' '=' scalar_default_char_expr
    | 'RECL' '=' scalar_int_expr
    | 'ROUND' '=' scalar_default_char_expr
    | 'SIGN' '=' scalar_default_char_expr
    | 'STATUS' '=' scalar_default_char_expr
    ;

file_name_expr
    : scalar_default_char_expr
    ;

iomsg_variable
    : scalar_default_char_variable
    ;

close_stmt
    : 'CLOSE' LPAREN close_spec_list RPAREN
    ;

close_spec
    : ('UNIT' '=')? file_unit_number
    | 'IOSTAT' '=' stat_variable
    | 'IOMSG' '=' iomsg_variable
    | 'ERR' '=' label
    | 'STATUS' '=' scalar_default_char_expr
    ;

read_stmt
    : 'READ' LPAREN io_control_spec_list RPAREN input_item_list?
    | 'READ' format ( COMMA input_item_list)?
    ;

write_stmt
    : 'WRITE' LPAREN io_control_spec_list RPAREN output_item_list?
    ;

print_stmt
    : 'PRINT' format (COMMA output_item_list)?
    ;

io_control_spec
    : ('UNIT' '=')? io_unit
    | ( 'FMT' '=')? format
    | ( 'NML' '=')? namelist_group_name
    | 'ADVANCE' '=' scalar_default_char_expr
    | 'ASYNCHRONOUS' '=' scalar_default_char_constant_expr
    | 'BLANK' '=' scalar_default_char_expr
    | 'DECIMAL' '=' scalar_default_char_expr
    | 'DELIM' '=' scalar_default_char_expr
    | 'END' '=' label
    | 'EOR' '=' label
    | 'ERR' '=' label
    | 'ID' '=' id_variable
    | 'IOMSG' '=' iomsg_variable
    | 'IOSTAT' '=' stat_variable
    | 'LEADING_ZERO' '=' scalar_default_char_expr
    | 'PAD' '=' scalar_default_char_expr
    | 'POS' '=' scalar_int_expr
    | 'REC' '=' scalar_int_expr
    | 'ROUND' '=' scalar_default_char_expr
    | 'SIGN' '=' scalar_default_char_expr
    | 'SIZE' '=' scalar_int_variable
    ;

id_variable
    : scalar_int_variable
    ;

format
    : default_char_expr
    | label
    | ASTERIK
    ;

input_item
    : variable
    | io_implied_do
    ;

output_item
    : expr
    | io_implied_do
    ;

io_implied_do
    : LPAREN io_implied_do_object_list COMMA io_implied_do_control RPAREN
    ;

io_implied_do_object
    : input_item
    | output_item
    ;

io_implied_do_control
    : do_variable '=' scalar_int_expr COMMA scalar_int_expr (COMMA scalar_int_expr)?
    ;

dtv_type_spec
    : TYPE LPAREN derived_type_spec RPAREN
    | CLASS LPAREN derived_type_spec RPAREN
    ;

wait_stmt
    : 'WAIT' LPAREN wait_spec_list RPAREN
    ;

wait_spec
    : ('UNIT' '=')? file_unit_number
    | 'END' '=' label
    | 'EOR' '=' label
    | 'ERR' '=' label
    | 'ID' '=' scalar_int_expr
    | 'IOMSG' '=' iomsg_variable
    | 'IOSTAT' '=' stat_variable
    ;

backspace_stmt
    : 'BACKSPACE' file_unit_number
    | 'BACKSPACE' LPAREN position_spec_list RPAREN
    ;

endfile_stmt
    : 'ENDFILE' file_unit_number
    | 'ENDFILE' LPAREN position_spec_list RPAREN
    ;

rewind_stmt
    : 'REWIND' file_unit_number
    | 'REWIND' LPAREN position_spec_list RPAREN
    ;

position_spec
    : ('UNIT' '=')? file_unit_number
    | 'IOMSG' '=' iomsg_variable
    | 'IOSTAT' '=' stat_variable
    | 'ERR' '=' label
    ;

flush_stmt
    : 'FLUSH' file_unit_number
    | 'FLUSH' LPAREN flush_spec_list RPAREN
    ;

flush_spec
    : ('UNIT' '=')? file_unit_number
    | 'IOSTAT' '=' stat_variable
    | 'IOMSG' '=' iomsg_variable
    | 'ERR' '=' label
    ;

inquire_stmt
    : 'INQUIRE' LPAREN inquire_spec_list RPAREN
    | 'INQUIRE' LPAREN 'IOLENGTH' '=' scalar_int_variable RPAREN output_item_list
    ;

inquire_spec
    : ('UNIT' '=')? file_unit_number
    | 'FILE' '=' file_name_expr
    | 'ACCESS' '=' scalar_default_char_variable
    | 'ACTION' '=' scalar_default_char_variable
    | 'ASYNCHRONOUS' '=' scalar_default_char_variable
    | 'BLANK' '=' scalar_default_char_variable
    | 'DECIMAL' '=' scalar_default_char_variable
    | 'DELIM' '=' scalar_default_char_variable
    | 'DIRECT' '=' scalar_default_char_variable
    | 'ENCODING' '=' scalar_default_char_variable
    | 'ERR' '=' label
    | 'EXIST' '=' scalar_logical_variable
    | 'FORM' '=' scalar_default_char_variable
    | 'FORMATTED' '=' scalar_default_char_variable
    | 'ID' '=' scalar_int_expr
    | 'IOMSG' '=' iomsg_variable
    | 'IOSTAT' '=' stat_variable
    | 'LEADING_ZERO' '=' scalar_default_char_variable
    | 'NAME' '=' scalar_default_char_variable
    | 'NAMED' '=' scalar_logical_variable
    | 'NEXTREC' '=' scalar_int_variable
    | 'NUMBER' '=' scalar_int_variable
    | 'OPENED' '=' scalar_logical_variable
    | 'PAD' '=' scalar_default_char_variable
    | 'PENDING' '=' scalar_logical_variable
    | 'POS' '=' scalar_int_variable
    | 'POSITION' '=' scalar_default_char_variable
    | 'READ' '=' scalar_default_char_variable
    | 'READWRITE' '=' scalar_default_char_variable
    | 'RECL' '=' scalar_int_variable
    | 'ROUND' '=' scalar_default_char_variable
    | 'SEQUENTIAL' '=' scalar_default_char_variable
    | 'SIGN' '=' scalar_default_char_variable
    | 'SIZE' '=' scalar_int_variable
    | 'STREAM' '=' scalar_default_char_variable
    | 'UNFORMATTED' '=' scalar_default_char_variable
    | 'WRITE' '=' scalar_default_char_variable
    ;

format_stmt
    : 'FORMAT' format_specification
    ;

format_specification
    : LPAREN format_items? RPAREN
    | LPAREN ( format_items COMMA)? unlimited_format_item RPAREN
    ;

format_items
    : format_item (COMMA? format_item)*
    ;

format_item
    : r? data_edit_desc
    | control_edit_desc
    | char_string_edit_desc
    | r? LPAREN format_items RPAREN
    ;

unlimited_format_item
    : ASTERIK LPAREN format_items RPAREN
    ;

r
    : int_literal_constant
    ;

data_edit_desc
    : 'I' w (DOT m)?
    | B w ( DOT m)?
    | O w ( DOT m)?
    | Z w ( DOT m)?
    | F w DOT d
    | E w DOT d ( E e)?
    | 'EN' w DOT d ( E e)?
    | 'ES' w DOT d ( E e)?
    | 'EX' w DOT d ( E e)?
    | 'G' w ( DOT d ( E e)?)?
    | 'L' w
    | A w?
    | 'AT'
    | D w DOT d
    | 'DT' char_literal_constant? ( LPAREN v_list RPAREN)?
    ;

w
    : int_literal_constant
    ;

m
    : int_literal_constant
    ;

d
    : int_literal_constant
    ;

e
    : int_literal_constant
    ;

v
    : signed_int_literal_constant
    ;

control_edit_desc
    : blank_interp_edit_desc
    | decimal_edit_desc
    | leading_zero_edit_desc
    | position_edit_desc
    | round_edit_desc
    | sign_edit_desc
    | k 'P'
    | COLON
    | r? SLASH
    ;

k
    : signed_int_literal_constant
    ;

position_edit_desc
    : 'T' n
    | 'TL' n
    | 'TR' n
    | n 'X'
    ;

n
    : int_literal_constant
    ;

blank_interp_edit_desc
    : 'BN'
    | 'BZ'
    ;

decimal_edit_desc
    : 'DC'
    | 'DP'
    ;

leading_zero_edit_desc
    : 'LZS'
    | 'LZP'
    | 'LZ'
    ;

round_edit_desc
    : 'RU'
    | 'RD'
    | 'RZ'
    | 'RN'
    | 'RC'
    | 'RP'
    ;

sign_edit_desc
    : 'SS'
    | 'SP'
    | 'S'
    ;

char_string_edit_desc
    : char_literal_constant
    ;

program_stmt
    : 'PROGRAM' program_name
    ;

end_program_stmt
    : 'END' ('PROGRAM' program_name?)?
    ;

module_stmt
    : 'MODULE' module_name
    ;

end_module_stmt
    : 'END' ('MODULE' module_name?)?
    ;

use_stmt
    : 'USE' (( COMMA module_nature)? '::')? module_name (COMMA rename_list)?
    | 'USE' (( COMMA module_nature)? '::')? module_name COMMA 'ONLY' COLON only_list?
    ;

module_nature
    : 'INTRINSIC'
    | 'NON_INTRINSIC'
    ;

rename
    : local_name '=>' use_name
    | 'OPERATOR' LPAREN local_defined_operator RPAREN '=>' 'OPERATOR' LPAREN use_defined_operator RPAREN
    ;

only
    : generic_spec
    | only_use_name
    | rename
    ;

only_use_name
    : use_name
    ;

local_defined_operator
    : defined_unary_op
    | defined_binary_op
    ;

use_defined_operator
    : defined_unary_op
    | defined_binary_op
    ;

submodule_stmt
    : 'SUBMODULE' LPAREN parent_identifier RPAREN submodule_name
    ;

parent_identifier
    : ancestor_module_name (COLON parent_submodule_name)?
    ;

end_submodule_stmt
    : 'END' ('SUBMODULE' submodule_name?)?
    ;

block_data_stmt
    : 'BLOCK' 'DATA' block_data_name?
    ;

end_block_data_stmt
    : 'END' ('BLOCK' 'DATA' block_data_name?)?
    ;

interface_block
    : interface_stmt interface_specification* end_interface_stmt
    ;

interface_specification
    : interface_body
    | procedure_stmt
    ;

interface_stmt
    : 'INTERFACE' generic_spec?
    | 'ABSTRACT' 'INTERFACE'
    ;

end_interface_stmt
    : 'END' 'INTERFACE' generic_spec?
    ;

interface_body
    : function_stmt specification_part? end_function_stmt
    | subroutine_stmt specification_part? end_subroutine_stmt
    ;

procedure_stmt
    : 'MODULE'? 'PROCEDURE' '::'? specific_procedure_list
    ;

specific_procedure
    : procedure_name
    ;

generic_spec
    : generic_name
    | 'OPERATOR' LPAREN defined_operator RPAREN
    | 'ASSIGNMENT' LPAREN '=' RPAREN
    | defined_io_generic_spec
    ;

defined_io_generic_spec
    : 'READ' LPAREN FORMATTED RPAREN
    | 'READ' LPAREN UNFORMATTED RPAREN
    | 'WRITE' LPAREN FORMATTED RPAREN
    | 'WRITE' LPAREN UNFORMATTED RPAREN
    ;

generic_stmt
    : 'GENERIC' (COMMA access_spec)? '::' generic_spec '=>' specific_procedure_list
    ;

external_stmt
    : 'EXTERNAL' '::'? external_name_list
    ;

procedure_declaration_stmt
    : 'PROCEDURE' LPAREN proc_interface? RPAREN (( COMMA proc_attr_spec)* '::')? proc_decl_list
    ;

proc_interface
    : interface_name
    | declaration_type_spec
    ;

proc_attr_spec
    : access_spec
    | proc_language_binding_spec
    | 'INTENT' LPAREN intent_spec RPAREN
    | 'OPTIONAL'
    | 'POINTER'
    | 'PROTECTED'
    | 'SAVE'
    ;

proc_decl
    : procedure_entity_name ('=>' proc_pointer_init)?
    ;

interface_name
    : name
    ;

proc_pointer_init
    : null_init
    | initial_proc_target
    ;

initial_proc_target
    : procedure_name
    ;

intrinsic_stmt
    : 'INTRINSIC' '::'? intrinsic_procedure_name_list
    ;

function_reference
    : procedure_designator LPAREN actual_arg_spec_list? RPAREN
    ;

call_stmt
    : 'CALL' procedure_designator (LPAREN actual_arg_spec_list? RPAREN)?
    ;

procedure_designator
    : procedure_name
    | proc_component_ref
    | data_ref '%' binding_name
    ;

actual_arg_spec
    : (keyword '=')? actual_arg
    ;

actual_arg
    : expr
    | variable
    | procedure_name
    | proc_component_ref
    | conditional_arg
    | alt_return_spec
    ;

alt_return_spec
    : ASTERIK label
    ;

conditional_arg
    : LPAREN scalar_logical_expr '?' consequent (COLON scalar_logical_expr '?' consequent)* COLON consequent RPAREN
    ;

consequent
    : consequent_arg
    | NIL
    ;

consequent_arg
    : expr
    | variable
    ;

prefix
    : prefix_spec prefix_spec*
    ;

prefix_spec
    : declaration_type_spec
    | 'ELEMENTAL'
    | 'IMPURE'
    | 'MODULE'
    | 'NON_RECURSIVE'
    | 'PURE'
    | 'RECURSIVE'
    | 'SIMPLE'
    ;

proc_language_binding_spec
    : language_binding_spec
    ;

function_stmt
    : prefix? 'FUNCTION' function_name LPAREN dummy_arg_name_list? RPAREN suffix?
    ;

dummy_arg_name
    : name
    ;

suffix
    : proc_language_binding_spec ('RESULT' LPAREN result_name RPAREN)?
    | 'RESULT' LPAREN result_name RPAREN proc_language_binding_spec?
    ;

end_function_stmt
    : 'END' ('FUNCTION' function_name?)?
    ;

subroutine_stmt
    : prefix? 'SUBROUTINE' subroutine_name (
        LPAREN dummy_arg_list? RPAREN proc_language_binding_spec?
    )?
    ;

dummy_arg
    : dummy_arg_name
    | ASTERIK
    ;

end_subroutine_stmt
    : 'END' ('SUBROUTINE' subroutine_name?)?
    ;

mp_subprogram_stmt
    : 'MODULE' 'PROCEDURE' procedure_name
    ;

end_mp_subprogram_stmt
    : 'END' ('PROCEDURE' procedure_name?)?
    ;

entry_stmt
    : 'ENTRY' entry_name (LPAREN dummy_arg_list? RPAREN suffix?)?
    ;

return_stmt
    : 'RETURN' scalar_int_expr?
    ;

contains_stmt
    : 'CONTAINS'
    ;

stmt_function_stmt
    : function_name LPAREN dummy_arg_name_list? RPAREN '=' scalar_expr
    ;

ac_value_list
    : ac_value (COMMA ac_value)*
    ;

access_id_list
    : access_id (COMMA access_id)*
    ;

actual_arg_spec_list
    : actual_arg_spec (COMMA actual_arg_spec)*
    ;

alloc_opt_list
    : alloc_opt (COMMA alloc_opt)*
    ;

allocatable_decl_list
    : allocatable_decl (COMMA allocatable_decl)*
    ;

allocate_coshape_spec_list
    : allocate_coshape_spec (COMMA allocate_coshape_spec)*
    ;

allocate_object_list
    : allocate_object (COMMA allocate_object)*
    ;

allocate_shape_spec_list
    : allocate_shape_spec (COMMA allocate_shape_spec)*
    ;

allocation_list
    : allocation (COMMA allocation)*
    ;

association_list
    : association (COMMA association)*
    ;

assumed_implied_spec_list
    : assumed_implied_spec (COMMA assumed_implied_spec)*
    ;

assumed_shape_spec_list
    : assumed_shape_spec (COMMA assumed_shape_spec)*
    ;

bind_entity_list
    : bind_entity (COMMA bind_entity)*
    ;

binding_attr_list
    : binding_attr (COMMA binding_attr)*
    ;

binding_name_list
    : binding_name (COMMA binding_name)*
    ;

bounds_remapping_list
    : bounds_remapping (COMMA bounds_remapping)*
    ;

bounds_spec_list
    : bounds_spec (COMMA bounds_spec)*
    ;

case_value_range_list
    : case_value_range (COMMA case_value_range)*
    ;

close_spec_list
    : close_spec (COMMA close_spec)*
    ;

coarray_association_list
    : coarray_association (COMMA coarray_association)*
    ;

codimension_decl_list
    : codimension_decl (COMMA codimension_decl)*
    ;

common_block_object_list
    : common_block_object (COMMA common_block_object)*
    ;

component_attr_spec_list
    : component_attr_spec (COMMA component_attr_spec)*
    ;

component_decl_list
    : component_decl (COMMA component_decl)*
    ;

component_spec_list
    : component_spec (COMMA component_spec)*
    ;

concurrent_control_list
    : concurrent_control (COMMA concurrent_control)*
    ;

connect_spec_list
    : connect_spec (COMMA connect_spec)*
    ;

cosubscript_list
    : cosubscript (COMMA cosubscript)*
    ;

data_i_do_object_list
    : data_i_do_object (COMMA data_i_do_object)*
    ;

data_stmt_object_list
    : data_stmt_object (COMMA data_stmt_object)*
    ;

data_stmt_value_list
    : data_stmt_value (COMMA data_stmt_value)*
    ;

dealloc_opt_list
    : dealloc_opt (COMMA dealloc_opt)*
    ;

deferred_coshape_spec_list
    : deferred_coshape_spec (COMMA deferred_coshape_spec)*
    ;

deferred_shape_spec_list
    : deferred_shape_spec (COMMA deferred_shape_spec)*
    ;

dummy_arg_list
    : dummy_arg (COMMA dummy_arg)*
    ;

dummy_arg_name_list
    : dummy_arg_name (COMMA dummy_arg_name)*
    ;

entity_decl_list
    : entity_decl (COMMA entity_decl)*
    ;

entity_name_list
    : entity_name (COMMA entity_name)*
    ;

enumerator_list
    : enumerator (COMMA enumerator)*
    ;

enumerator_name_list
    : enumerator_name (COMMA enumerator_name)*
    ;

equivalence_object_list
    : equivalence_object (COMMA equivalence_object)*
    ;

equivalence_set_list
    : equivalence_set (COMMA equivalence_set)*
    ;

event_wait_spec_list
    : event_wait_spec (COMMA event_wait_spec)*
    ;

explicit_shape_spec_list
    : explicit_shape_spec (COMMA explicit_shape_spec)*
    ;

external_name_list
    : external_name (COMMA external_name)*
    ;

final_subroutine_name_list
    : final_subroutine_name (COMMA final_subroutine_name)*
    ;

flush_spec_list
    : flush_spec (COMMA flush_spec)*
    ;

form_team_spec_list
    : form_team_spec (COMMA form_team_spec)*
    ;

image_selector_spec_list
    : image_selector_spec (COMMA image_selector_spec)*
    ;

implicit_none_spec_list
    : implicit_none_spec (COMMA implicit_none_spec)*
    ;

implicit_spec_list
    : implicit_spec (COMMA implicit_spec)*
    ;

import_name_list
    : import_name (COMMA import_name)*
    ;

input_item_list
    : input_item (COMMA input_item)*
    ;

inquire_spec_list
    : inquire_spec (COMMA inquire_spec)*
    ;

intrinsic_procedure_name_list
    : intrinsic_procedure_name (COMMA intrinsic_procedure_name)*
    ;

io_control_spec_list
    : io_control_spec (COMMA io_control_spec)*
    ;

io_implied_do_object_list
    : io_implied_do_object (COMMA io_implied_do)*
    ;

label_list
    : label (COMMA label)*
    ;

letter_spec_list
    : LETTER_SPEC (COMMA LETTER_SPEC)*
    ;

lock_stat_list
    : lock_stat (COMMA lock_stat)*
    ;

named_constant_def_list
    : named_constant_def (COMMA named_constant_def)*
    ;

namelist_group_object_list
    : namelist_group_object (COMMA namelist_group_object)*
    ;

object_name_list
    : object_name (COMMA object_name)*
    ;

only_list
    : only (COMMA only)*
    ;

output_item_list
    : output_item (COMMA output_item)*
    ;

pointer_decl_list
    : pointer_decl (COMMA pointer_decl)*
    ;

pointer_object_list
    : pointer_object (COMMA pointer_object)*
    ;

position_spec_list
    : position_spec (COMMA position_spec)*
    ;

proc_component_attr_spec_list
    : proc_component_attr_spec (COMMA proc_component_attr_spec)*
    ;

proc_decl_list
    : proc_decl (COMMA proc_decl)*
    ;

rename_list
    : rename (COMMA rename)*
    ;

saved_entity_list
    : saved_entity (COMMA saved_entity)*
    ;

section_subscript_list
    : section_subscript (COMMA section_subscript)*
    ;

specific_procedure_list
    : specific_procedure (COMMA specific_procedure)*
    ;

sync_stat_list
    : sync_stat (COMMA sync_stat)*
    ;

target_decl_list
    : target_decl (COMMA target_decl)*
    ;

type_attr_spec_list
    : type_attr_spec (COMMA type_attr_spec)*
    ;

type_bound_proc_decl_list
    : type_bound_proc_decl (COMMA type_bound_proc_decl)*
    ;

type_param_decl_list
    : type_param_decl (COMMA type_param_decl)*
    ;

type_param_name_list
    : type_param_name (COMMA type_param_name)*
    ;

type_param_spec_list
    : type_param_spec (COMMA type_param_spec)*
    ;

v_list
    : v (COMMA v)*
    ;

variable_name_list
    : variable_name (COMMA variable_name)*
    ;

wait_spec_list
    : wait_spec (COMMA wait_spec)*
    ;

access_name
    : name
    ;

ancestor_module_name
    : name
    ;

arg_name
    : name
    ;

array_name
    : name
    ;

associate_construct_name
    : name
    ;

associate_name
    : name
    ;

binding_name
    : name
    ;

block_construct_name
    : name
    ;

block_data_name
    : name
    ;

case_construct_name
    : name
    ;

coarray_name
    : name
    ;

common_block_name
    : name
    ;

component_name
    : name
    ;

construct_name
    : name
    ;

critical_construct_name
    : name
    ;

data_pointer_component_name
    : name
    ;

do_construct_name
    : name
    ;

entity_name
    : name
    ;

entry_name
    : name
    ;

enum_type_name
    : name
    ;

enumeration_type_name
    : name
    ;

enumerator_name
    : name
    ;

external_name
    : name
    ;

final_subroutine_name
    : name
    ;

forall_construct_name
    : name
    ;

function_name
    : name
    ;

function_reduction_name
    : name
    ;

generic_name
    : name
    ;

if_construct_name
    : name
    ;

import_name
    : name
    ;

index_name
    : name
    ;

int_constant_name
    : name
    ;

int_variable_name
    : name
    ;

intrinsic_procedure_name
    : name
    ;

local_name
    : name
    ;

module_name
    : name
    ;

namelist_group_name
    : name
    ;

parent_submodule_name
    : name
    ;

parent_type_name
    : name
    ;

part_name
    : name
    ;

procedure_component_name
    : name
    ;

procedure_entity_name
    : name
    ;

procedure_name
    : name
    ;

procptr_entity_name
    : name
    ;

program_name
    : name
    ;

result_name
    : name
    ;

scalar_constant_expr
    : constant_expr
    ;

scalar_constant_subobject
    : constant_subobject
    ;

scalar_constant
    : constant
    ;

scalar_default_char_constant_expr
    : default_char_constant_expr
    ;

scalar_default_char_expr
    : default_char_expr
    ;

scalar_default_char_variable
    : default_char_variable
    ;

scalar_expr
    : expr
    ;

scalar_int_constant_expr
    : int_constant_expr
    ;

scalar_int_constant_name
    : int_constant_name
    ;

scalar_int_constant_subobject
    : int_constant_subobject
    ;

scalar_int_constant
    : int_constant
    ;

scalar_int_expr
    : int_expr
    ;

scalar_int_variable_name
    : int_variable_name
    ;

scalar_int_variable
    : int_variable
    ;

scalar_logical_expr
    : logical_expr
    ;

scalar_logical_variable
    : logical_variable
    ;

scalar_mask_expr
    : mask_expr
    ;

scalar_structure_component
    : structure_component
    ;

scalar_variable_name
    : variable_name
    ;

scalar_variable
    : variable
    ;

select_construct_name
    : name
    ;

submodule_name
    : name
    ;

subroutine_name
    : name
    ;

team_construct_name
    : name
    ;

type_name
    : name
    ;

type_param_name
    : name
    ;

use_name
    : name
    ;

where_construct_name
    : name
    ;