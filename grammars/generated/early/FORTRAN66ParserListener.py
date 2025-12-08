# Generated from FORTRAN66Parser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .FORTRAN66Parser import FORTRAN66Parser
else:
    from FORTRAN66Parser import FORTRAN66Parser

# This class defines a complete listener for a parse tree produced by FORTRAN66Parser.
class FORTRAN66ParserListener(ParseTreeListener):

    # Enter a parse tree produced by FORTRAN66Parser#type_spec.
    def enterType_spec(self, ctx:FORTRAN66Parser.Type_specContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#type_spec.
    def exitType_spec(self, ctx:FORTRAN66Parser.Type_specContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_expr.
    def enterLogical_expr(self, ctx:FORTRAN66Parser.Logical_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_expr.
    def exitLogical_expr(self, ctx:FORTRAN66Parser.Logical_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_term.
    def enterLogical_term(self, ctx:FORTRAN66Parser.Logical_termContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_term.
    def exitLogical_term(self, ctx:FORTRAN66Parser.Logical_termContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_factor.
    def enterLogical_factor(self, ctx:FORTRAN66Parser.Logical_factorContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_factor.
    def exitLogical_factor(self, ctx:FORTRAN66Parser.Logical_factorContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_primary.
    def enterLogical_primary(self, ctx:FORTRAN66Parser.Logical_primaryContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_primary.
    def exitLogical_primary(self, ctx:FORTRAN66Parser.Logical_primaryContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_literal.
    def enterLogical_literal(self, ctx:FORTRAN66Parser.Logical_literalContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_literal.
    def exitLogical_literal(self, ctx:FORTRAN66Parser.Logical_literalContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_variable.
    def enterLogical_variable(self, ctx:FORTRAN66Parser.Logical_variableContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_variable.
    def exitLogical_variable(self, ctx:FORTRAN66Parser.Logical_variableContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#relational_expr.
    def enterRelational_expr(self, ctx:FORTRAN66Parser.Relational_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#relational_expr.
    def exitRelational_expr(self, ctx:FORTRAN66Parser.Relational_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#relational_op.
    def enterRelational_op(self, ctx:FORTRAN66Parser.Relational_opContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#relational_op.
    def exitRelational_op(self, ctx:FORTRAN66Parser.Relational_opContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#logical_if_stmt.
    def enterLogical_if_stmt(self, ctx:FORTRAN66Parser.Logical_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#logical_if_stmt.
    def exitLogical_if_stmt(self, ctx:FORTRAN66Parser.Logical_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#fortran66_program.
    def enterFortran66_program(self, ctx:FORTRAN66Parser.Fortran66_programContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#fortran66_program.
    def exitFortran66_program(self, ctx:FORTRAN66Parser.Fortran66_programContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#program_unit.
    def enterProgram_unit(self, ctx:FORTRAN66Parser.Program_unitContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#program_unit.
    def exitProgram_unit(self, ctx:FORTRAN66Parser.Program_unitContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#main_program.
    def enterMain_program(self, ctx:FORTRAN66Parser.Main_programContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#main_program.
    def exitMain_program(self, ctx:FORTRAN66Parser.Main_programContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#subprogram.
    def enterSubprogram(self, ctx:FORTRAN66Parser.SubprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#subprogram.
    def exitSubprogram(self, ctx:FORTRAN66Parser.SubprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#subroutine_subprogram.
    def enterSubroutine_subprogram(self, ctx:FORTRAN66Parser.Subroutine_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#subroutine_subprogram.
    def exitSubroutine_subprogram(self, ctx:FORTRAN66Parser.Subroutine_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#function_subprogram.
    def enterFunction_subprogram(self, ctx:FORTRAN66Parser.Function_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#function_subprogram.
    def exitFunction_subprogram(self, ctx:FORTRAN66Parser.Function_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#block_data_subprogram.
    def enterBlock_data_subprogram(self, ctx:FORTRAN66Parser.Block_data_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#block_data_subprogram.
    def exitBlock_data_subprogram(self, ctx:FORTRAN66Parser.Block_data_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#block_data_name.
    def enterBlock_data_name(self, ctx:FORTRAN66Parser.Block_data_nameContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#block_data_name.
    def exitBlock_data_name(self, ctx:FORTRAN66Parser.Block_data_nameContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_initialization_part.
    def enterData_initialization_part(self, ctx:FORTRAN66Parser.Data_initialization_partContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_initialization_part.
    def exitData_initialization_part(self, ctx:FORTRAN66Parser.Data_initialization_partContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_initialization_statement.
    def enterData_initialization_statement(self, ctx:FORTRAN66Parser.Data_initialization_statementContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_initialization_statement.
    def exitData_initialization_statement(self, ctx:FORTRAN66Parser.Data_initialization_statementContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_initialization_body.
    def enterData_initialization_body(self, ctx:FORTRAN66Parser.Data_initialization_bodyContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_initialization_body.
    def exitData_initialization_body(self, ctx:FORTRAN66Parser.Data_initialization_bodyContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#type_declaration.
    def enterType_declaration(self, ctx:FORTRAN66Parser.Type_declarationContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#type_declaration.
    def exitType_declaration(self, ctx:FORTRAN66Parser.Type_declarationContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#statement_list.
    def enterStatement_list(self, ctx:FORTRAN66Parser.Statement_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#statement_list.
    def exitStatement_list(self, ctx:FORTRAN66Parser.Statement_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#statement.
    def enterStatement(self, ctx:FORTRAN66Parser.StatementContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#statement.
    def exitStatement(self, ctx:FORTRAN66Parser.StatementContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#label.
    def enterLabel(self, ctx:FORTRAN66Parser.LabelContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#label.
    def exitLabel(self, ctx:FORTRAN66Parser.LabelContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#statement_body.
    def enterStatement_body(self, ctx:FORTRAN66Parser.Statement_bodyContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#statement_body.
    def exitStatement_body(self, ctx:FORTRAN66Parser.Statement_bodyContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#rewind_stmt.
    def enterRewind_stmt(self, ctx:FORTRAN66Parser.Rewind_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#rewind_stmt.
    def exitRewind_stmt(self, ctx:FORTRAN66Parser.Rewind_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#backspace_stmt.
    def enterBackspace_stmt(self, ctx:FORTRAN66Parser.Backspace_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#backspace_stmt.
    def exitBackspace_stmt(self, ctx:FORTRAN66Parser.Backspace_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#endfile_stmt.
    def enterEndfile_stmt(self, ctx:FORTRAN66Parser.Endfile_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#endfile_stmt.
    def exitEndfile_stmt(self, ctx:FORTRAN66Parser.Endfile_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#external_stmt.
    def enterExternal_stmt(self, ctx:FORTRAN66Parser.External_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#external_stmt.
    def exitExternal_stmt(self, ctx:FORTRAN66Parser.External_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#intrinsic_stmt.
    def enterIntrinsic_stmt(self, ctx:FORTRAN66Parser.Intrinsic_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#intrinsic_stmt.
    def exitIntrinsic_stmt(self, ctx:FORTRAN66Parser.Intrinsic_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#identifier_list.
    def enterIdentifier_list(self, ctx:FORTRAN66Parser.Identifier_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#identifier_list.
    def exitIdentifier_list(self, ctx:FORTRAN66Parser.Identifier_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#variable_list.
    def enterVariable_list(self, ctx:FORTRAN66Parser.Variable_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#variable_list.
    def exitVariable_list(self, ctx:FORTRAN66Parser.Variable_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt.
    def enterData_stmt(self, ctx:FORTRAN66Parser.Data_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt.
    def exitData_stmt(self, ctx:FORTRAN66Parser.Data_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_set.
    def enterData_stmt_set(self, ctx:FORTRAN66Parser.Data_stmt_setContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_set.
    def exitData_stmt_set(self, ctx:FORTRAN66Parser.Data_stmt_setContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_object_list.
    def enterData_stmt_object_list(self, ctx:FORTRAN66Parser.Data_stmt_object_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_object_list.
    def exitData_stmt_object_list(self, ctx:FORTRAN66Parser.Data_stmt_object_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_object.
    def enterData_stmt_object(self, ctx:FORTRAN66Parser.Data_stmt_objectContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_object.
    def exitData_stmt_object(self, ctx:FORTRAN66Parser.Data_stmt_objectContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_implied_do.
    def enterData_implied_do(self, ctx:FORTRAN66Parser.Data_implied_doContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_implied_do.
    def exitData_implied_do(self, ctx:FORTRAN66Parser.Data_implied_doContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_value_list.
    def enterData_stmt_value_list(self, ctx:FORTRAN66Parser.Data_stmt_value_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_value_list.
    def exitData_stmt_value_list(self, ctx:FORTRAN66Parser.Data_stmt_value_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_value.
    def enterData_stmt_value(self, ctx:FORTRAN66Parser.Data_stmt_valueContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_value.
    def exitData_stmt_value(self, ctx:FORTRAN66Parser.Data_stmt_valueContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_repeat.
    def enterData_stmt_repeat(self, ctx:FORTRAN66Parser.Data_stmt_repeatContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_repeat.
    def exitData_stmt_repeat(self, ctx:FORTRAN66Parser.Data_stmt_repeatContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#unsigned_int.
    def enterUnsigned_int(self, ctx:FORTRAN66Parser.Unsigned_intContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#unsigned_int.
    def exitUnsigned_int(self, ctx:FORTRAN66Parser.Unsigned_intContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#data_stmt_constant.
    def enterData_stmt_constant(self, ctx:FORTRAN66Parser.Data_stmt_constantContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#data_stmt_constant.
    def exitData_stmt_constant(self, ctx:FORTRAN66Parser.Data_stmt_constantContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#assign_stmt.
    def enterAssign_stmt(self, ctx:FORTRAN66Parser.Assign_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#assign_stmt.
    def exitAssign_stmt(self, ctx:FORTRAN66Parser.Assign_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#assigned_goto_stmt.
    def enterAssigned_goto_stmt(self, ctx:FORTRAN66Parser.Assigned_goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#assigned_goto_stmt.
    def exitAssigned_goto_stmt(self, ctx:FORTRAN66Parser.Assigned_goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#do_stmt.
    def enterDo_stmt(self, ctx:FORTRAN66Parser.Do_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#do_stmt.
    def exitDo_stmt(self, ctx:FORTRAN66Parser.Do_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#literal.
    def enterLiteral(self, ctx:FORTRAN66Parser.LiteralContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#literal.
    def exitLiteral(self, ctx:FORTRAN66Parser.LiteralContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_item.
    def enterFormat_item(self, ctx:FORTRAN66Parser.Format_itemContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_item.
    def exitFormat_item(self, ctx:FORTRAN66Parser.Format_itemContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_repeat_count.
    def enterFormat_repeat_count(self, ctx:FORTRAN66Parser.Format_repeat_countContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_repeat_count.
    def exitFormat_repeat_count(self, ctx:FORTRAN66Parser.Format_repeat_countContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_descriptor_full.
    def enterFormat_descriptor_full(self, ctx:FORTRAN66Parser.Format_descriptor_fullContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_descriptor_full.
    def exitFormat_descriptor_full(self, ctx:FORTRAN66Parser.Format_descriptor_fullContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_decimal_part.
    def enterFormat_decimal_part(self, ctx:FORTRAN66Parser.Format_decimal_partContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_decimal_part.
    def exitFormat_decimal_part(self, ctx:FORTRAN66Parser.Format_decimal_partContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#fortran_program.
    def enterFortran_program(self, ctx:FORTRAN66Parser.Fortran_programContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#fortran_program.
    def exitFortran_program(self, ctx:FORTRAN66Parser.Fortran_programContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#parameter_list.
    def enterParameter_list(self, ctx:FORTRAN66Parser.Parameter_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#parameter_list.
    def exitParameter_list(self, ctx:FORTRAN66Parser.Parameter_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#call_stmt.
    def enterCall_stmt(self, ctx:FORTRAN66Parser.Call_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#call_stmt.
    def exitCall_stmt(self, ctx:FORTRAN66Parser.Call_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#statement_function_stmt.
    def enterStatement_function_stmt(self, ctx:FORTRAN66Parser.Statement_function_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#statement_function_stmt.
    def exitStatement_function_stmt(self, ctx:FORTRAN66Parser.Statement_function_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#statement_function_dummy_arg_list.
    def enterStatement_function_dummy_arg_list(self, ctx:FORTRAN66Parser.Statement_function_dummy_arg_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#statement_function_dummy_arg_list.
    def exitStatement_function_dummy_arg_list(self, ctx:FORTRAN66Parser.Statement_function_dummy_arg_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#assignment_stmt.
    def enterAssignment_stmt(self, ctx:FORTRAN66Parser.Assignment_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#assignment_stmt.
    def exitAssignment_stmt(self, ctx:FORTRAN66Parser.Assignment_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#goto_stmt.
    def enterGoto_stmt(self, ctx:FORTRAN66Parser.Goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#goto_stmt.
    def exitGoto_stmt(self, ctx:FORTRAN66Parser.Goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#computed_goto_stmt.
    def enterComputed_goto_stmt(self, ctx:FORTRAN66Parser.Computed_goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#computed_goto_stmt.
    def exitComputed_goto_stmt(self, ctx:FORTRAN66Parser.Computed_goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#arithmetic_if_stmt.
    def enterArithmetic_if_stmt(self, ctx:FORTRAN66Parser.Arithmetic_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#arithmetic_if_stmt.
    def exitArithmetic_if_stmt(self, ctx:FORTRAN66Parser.Arithmetic_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#continue_stmt.
    def enterContinue_stmt(self, ctx:FORTRAN66Parser.Continue_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#continue_stmt.
    def exitContinue_stmt(self, ctx:FORTRAN66Parser.Continue_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#stop_stmt.
    def enterStop_stmt(self, ctx:FORTRAN66Parser.Stop_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#stop_stmt.
    def exitStop_stmt(self, ctx:FORTRAN66Parser.Stop_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#pause_stmt.
    def enterPause_stmt(self, ctx:FORTRAN66Parser.Pause_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#pause_stmt.
    def exitPause_stmt(self, ctx:FORTRAN66Parser.Pause_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#return_stmt.
    def enterReturn_stmt(self, ctx:FORTRAN66Parser.Return_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#return_stmt.
    def exitReturn_stmt(self, ctx:FORTRAN66Parser.Return_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#end_stmt.
    def enterEnd_stmt(self, ctx:FORTRAN66Parser.End_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#end_stmt.
    def exitEnd_stmt(self, ctx:FORTRAN66Parser.End_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#read_stmt.
    def enterRead_stmt(self, ctx:FORTRAN66Parser.Read_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#read_stmt.
    def exitRead_stmt(self, ctx:FORTRAN66Parser.Read_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#print_stmt.
    def enterPrint_stmt(self, ctx:FORTRAN66Parser.Print_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#print_stmt.
    def exitPrint_stmt(self, ctx:FORTRAN66Parser.Print_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#punch_stmt.
    def enterPunch_stmt(self, ctx:FORTRAN66Parser.Punch_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#punch_stmt.
    def exitPunch_stmt(self, ctx:FORTRAN66Parser.Punch_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_stmt.
    def enterFormat_stmt(self, ctx:FORTRAN66Parser.Format_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_stmt.
    def exitFormat_stmt(self, ctx:FORTRAN66Parser.Format_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_specification.
    def enterFormat_specification(self, ctx:FORTRAN66Parser.Format_specificationContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_specification.
    def exitFormat_specification(self, ctx:FORTRAN66Parser.Format_specificationContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#format_descriptor.
    def enterFormat_descriptor(self, ctx:FORTRAN66Parser.Format_descriptorContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#format_descriptor.
    def exitFormat_descriptor(self, ctx:FORTRAN66Parser.Format_descriptorContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#dimension_stmt.
    def enterDimension_stmt(self, ctx:FORTRAN66Parser.Dimension_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#dimension_stmt.
    def exitDimension_stmt(self, ctx:FORTRAN66Parser.Dimension_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#array_declarator.
    def enterArray_declarator(self, ctx:FORTRAN66Parser.Array_declaratorContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#array_declarator.
    def exitArray_declarator(self, ctx:FORTRAN66Parser.Array_declaratorContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#dimension_list.
    def enterDimension_list(self, ctx:FORTRAN66Parser.Dimension_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#dimension_list.
    def exitDimension_list(self, ctx:FORTRAN66Parser.Dimension_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#equivalence_stmt.
    def enterEquivalence_stmt(self, ctx:FORTRAN66Parser.Equivalence_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#equivalence_stmt.
    def exitEquivalence_stmt(self, ctx:FORTRAN66Parser.Equivalence_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#equivalence_set.
    def enterEquivalence_set(self, ctx:FORTRAN66Parser.Equivalence_setContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#equivalence_set.
    def exitEquivalence_set(self, ctx:FORTRAN66Parser.Equivalence_setContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#frequency_stmt.
    def enterFrequency_stmt(self, ctx:FORTRAN66Parser.Frequency_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#frequency_stmt.
    def exitFrequency_stmt(self, ctx:FORTRAN66Parser.Frequency_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#common_stmt.
    def enterCommon_stmt(self, ctx:FORTRAN66Parser.Common_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#common_stmt.
    def exitCommon_stmt(self, ctx:FORTRAN66Parser.Common_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#expr.
    def enterExpr(self, ctx:FORTRAN66Parser.ExprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#expr.
    def exitExpr(self, ctx:FORTRAN66Parser.ExprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#additive_expr.
    def enterAdditive_expr(self, ctx:FORTRAN66Parser.Additive_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#additive_expr.
    def exitAdditive_expr(self, ctx:FORTRAN66Parser.Additive_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#additive_op.
    def enterAdditive_op(self, ctx:FORTRAN66Parser.Additive_opContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#additive_op.
    def exitAdditive_op(self, ctx:FORTRAN66Parser.Additive_opContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#multiplicative_expr.
    def enterMultiplicative_expr(self, ctx:FORTRAN66Parser.Multiplicative_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#multiplicative_expr.
    def exitMultiplicative_expr(self, ctx:FORTRAN66Parser.Multiplicative_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#multiplicative_op.
    def enterMultiplicative_op(self, ctx:FORTRAN66Parser.Multiplicative_opContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#multiplicative_op.
    def exitMultiplicative_op(self, ctx:FORTRAN66Parser.Multiplicative_opContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#unary_expr.
    def enterUnary_expr(self, ctx:FORTRAN66Parser.Unary_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#unary_expr.
    def exitUnary_expr(self, ctx:FORTRAN66Parser.Unary_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#unary_op.
    def enterUnary_op(self, ctx:FORTRAN66Parser.Unary_opContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#unary_op.
    def exitUnary_op(self, ctx:FORTRAN66Parser.Unary_opContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#power_expr.
    def enterPower_expr(self, ctx:FORTRAN66Parser.Power_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#power_expr.
    def exitPower_expr(self, ctx:FORTRAN66Parser.Power_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#primary.
    def enterPrimary(self, ctx:FORTRAN66Parser.PrimaryContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#primary.
    def exitPrimary(self, ctx:FORTRAN66Parser.PrimaryContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#variable.
    def enterVariable(self, ctx:FORTRAN66Parser.VariableContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#variable.
    def exitVariable(self, ctx:FORTRAN66Parser.VariableContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#label_list.
    def enterLabel_list(self, ctx:FORTRAN66Parser.Label_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#label_list.
    def exitLabel_list(self, ctx:FORTRAN66Parser.Label_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#input_list.
    def enterInput_list(self, ctx:FORTRAN66Parser.Input_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#input_list.
    def exitInput_list(self, ctx:FORTRAN66Parser.Input_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#output_list.
    def enterOutput_list(self, ctx:FORTRAN66Parser.Output_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#output_list.
    def exitOutput_list(self, ctx:FORTRAN66Parser.Output_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#expr_list.
    def enterExpr_list(self, ctx:FORTRAN66Parser.Expr_listContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#expr_list.
    def exitExpr_list(self, ctx:FORTRAN66Parser.Expr_listContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#integer_expr.
    def enterInteger_expr(self, ctx:FORTRAN66Parser.Integer_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#integer_expr.
    def exitInteger_expr(self, ctx:FORTRAN66Parser.Integer_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#program_unit_core.
    def enterProgram_unit_core(self, ctx:FORTRAN66Parser.Program_unit_coreContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#program_unit_core.
    def exitProgram_unit_core(self, ctx:FORTRAN66Parser.Program_unit_coreContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#if_stmt_arithmetic.
    def enterIf_stmt_arithmetic(self, ctx:FORTRAN66Parser.If_stmt_arithmeticContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#if_stmt_arithmetic.
    def exitIf_stmt_arithmetic(self, ctx:FORTRAN66Parser.If_stmt_arithmeticContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#if_stmt_sense_light.
    def enterIf_stmt_sense_light(self, ctx:FORTRAN66Parser.If_stmt_sense_lightContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#if_stmt_sense_light.
    def exitIf_stmt_sense_light(self, ctx:FORTRAN66Parser.If_stmt_sense_lightContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#if_stmt_sense_switch.
    def enterIf_stmt_sense_switch(self, ctx:FORTRAN66Parser.If_stmt_sense_switchContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#if_stmt_sense_switch.
    def exitIf_stmt_sense_switch(self, ctx:FORTRAN66Parser.If_stmt_sense_switchContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#if_stmt_accumulator_overflow.
    def enterIf_stmt_accumulator_overflow(self, ctx:FORTRAN66Parser.If_stmt_accumulator_overflowContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#if_stmt_accumulator_overflow.
    def exitIf_stmt_accumulator_overflow(self, ctx:FORTRAN66Parser.If_stmt_accumulator_overflowContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#if_stmt_quotient_overflow.
    def enterIf_stmt_quotient_overflow(self, ctx:FORTRAN66Parser.If_stmt_quotient_overflowContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#if_stmt_quotient_overflow.
    def exitIf_stmt_quotient_overflow(self, ctx:FORTRAN66Parser.If_stmt_quotient_overflowContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#if_stmt_divide_check.
    def enterIf_stmt_divide_check(self, ctx:FORTRAN66Parser.If_stmt_divide_checkContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#if_stmt_divide_check.
    def exitIf_stmt_divide_check(self, ctx:FORTRAN66Parser.If_stmt_divide_checkContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#sense_light_stmt.
    def enterSense_light_stmt(self, ctx:FORTRAN66Parser.Sense_light_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#sense_light_stmt.
    def exitSense_light_stmt(self, ctx:FORTRAN66Parser.Sense_light_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#do_stmt_basic.
    def enterDo_stmt_basic(self, ctx:FORTRAN66Parser.Do_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#do_stmt_basic.
    def exitDo_stmt_basic(self, ctx:FORTRAN66Parser.Do_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#read_stmt_basic.
    def enterRead_stmt_basic(self, ctx:FORTRAN66Parser.Read_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#read_stmt_basic.
    def exitRead_stmt_basic(self, ctx:FORTRAN66Parser.Read_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRAN66Parser#write_stmt_basic.
    def enterWrite_stmt_basic(self, ctx:FORTRAN66Parser.Write_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRAN66Parser#write_stmt_basic.
    def exitWrite_stmt_basic(self, ctx:FORTRAN66Parser.Write_stmt_basicContext):
        pass



del FORTRAN66Parser