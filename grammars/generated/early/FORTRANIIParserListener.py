# Generated from FORTRANIIParser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .FORTRANIIParser import FORTRANIIParser
else:
    from FORTRANIIParser import FORTRANIIParser

# This class defines a complete listener for a parse tree produced by FORTRANIIParser.
class FORTRANIIParserListener(ParseTreeListener):

    # Enter a parse tree produced by FORTRANIIParser#fortran_program.
    def enterFortran_program(self, ctx:FORTRANIIParser.Fortran_programContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#fortran_program.
    def exitFortran_program(self, ctx:FORTRANIIParser.Fortran_programContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#main_program.
    def enterMain_program(self, ctx:FORTRANIIParser.Main_programContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#main_program.
    def exitMain_program(self, ctx:FORTRANIIParser.Main_programContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#statement_list.
    def enterStatement_list(self, ctx:FORTRANIIParser.Statement_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#statement_list.
    def exitStatement_list(self, ctx:FORTRANIIParser.Statement_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#statement.
    def enterStatement(self, ctx:FORTRANIIParser.StatementContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#statement.
    def exitStatement(self, ctx:FORTRANIIParser.StatementContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#label.
    def enterLabel(self, ctx:FORTRANIIParser.LabelContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#label.
    def exitLabel(self, ctx:FORTRANIIParser.LabelContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#subroutine_subprogram.
    def enterSubroutine_subprogram(self, ctx:FORTRANIIParser.Subroutine_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#subroutine_subprogram.
    def exitSubroutine_subprogram(self, ctx:FORTRANIIParser.Subroutine_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#function_subprogram.
    def enterFunction_subprogram(self, ctx:FORTRANIIParser.Function_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#function_subprogram.
    def exitFunction_subprogram(self, ctx:FORTRANIIParser.Function_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#parameter_list.
    def enterParameter_list(self, ctx:FORTRANIIParser.Parameter_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#parameter_list.
    def exitParameter_list(self, ctx:FORTRANIIParser.Parameter_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#type_spec.
    def enterType_spec(self, ctx:FORTRANIIParser.Type_specContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#type_spec.
    def exitType_spec(self, ctx:FORTRANIIParser.Type_specContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#statement_body.
    def enterStatement_body(self, ctx:FORTRANIIParser.Statement_bodyContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#statement_body.
    def exitStatement_body(self, ctx:FORTRANIIParser.Statement_bodyContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#call_stmt.
    def enterCall_stmt(self, ctx:FORTRANIIParser.Call_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#call_stmt.
    def exitCall_stmt(self, ctx:FORTRANIIParser.Call_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#statement_function_stmt.
    def enterStatement_function_stmt(self, ctx:FORTRANIIParser.Statement_function_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#statement_function_stmt.
    def exitStatement_function_stmt(self, ctx:FORTRANIIParser.Statement_function_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#statement_function_dummy_arg_list.
    def enterStatement_function_dummy_arg_list(self, ctx:FORTRANIIParser.Statement_function_dummy_arg_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#statement_function_dummy_arg_list.
    def exitStatement_function_dummy_arg_list(self, ctx:FORTRANIIParser.Statement_function_dummy_arg_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#assignment_stmt.
    def enterAssignment_stmt(self, ctx:FORTRANIIParser.Assignment_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#assignment_stmt.
    def exitAssignment_stmt(self, ctx:FORTRANIIParser.Assignment_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#goto_stmt.
    def enterGoto_stmt(self, ctx:FORTRANIIParser.Goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#goto_stmt.
    def exitGoto_stmt(self, ctx:FORTRANIIParser.Goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#computed_goto_stmt.
    def enterComputed_goto_stmt(self, ctx:FORTRANIIParser.Computed_goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#computed_goto_stmt.
    def exitComputed_goto_stmt(self, ctx:FORTRANIIParser.Computed_goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#arithmetic_if_stmt.
    def enterArithmetic_if_stmt(self, ctx:FORTRANIIParser.Arithmetic_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#arithmetic_if_stmt.
    def exitArithmetic_if_stmt(self, ctx:FORTRANIIParser.Arithmetic_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#do_stmt.
    def enterDo_stmt(self, ctx:FORTRANIIParser.Do_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#do_stmt.
    def exitDo_stmt(self, ctx:FORTRANIIParser.Do_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#continue_stmt.
    def enterContinue_stmt(self, ctx:FORTRANIIParser.Continue_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#continue_stmt.
    def exitContinue_stmt(self, ctx:FORTRANIIParser.Continue_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#stop_stmt.
    def enterStop_stmt(self, ctx:FORTRANIIParser.Stop_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#stop_stmt.
    def exitStop_stmt(self, ctx:FORTRANIIParser.Stop_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#pause_stmt.
    def enterPause_stmt(self, ctx:FORTRANIIParser.Pause_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#pause_stmt.
    def exitPause_stmt(self, ctx:FORTRANIIParser.Pause_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#return_stmt.
    def enterReturn_stmt(self, ctx:FORTRANIIParser.Return_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#return_stmt.
    def exitReturn_stmt(self, ctx:FORTRANIIParser.Return_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#end_stmt.
    def enterEnd_stmt(self, ctx:FORTRANIIParser.End_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#end_stmt.
    def exitEnd_stmt(self, ctx:FORTRANIIParser.End_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#read_stmt.
    def enterRead_stmt(self, ctx:FORTRANIIParser.Read_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#read_stmt.
    def exitRead_stmt(self, ctx:FORTRANIIParser.Read_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#print_stmt.
    def enterPrint_stmt(self, ctx:FORTRANIIParser.Print_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#print_stmt.
    def exitPrint_stmt(self, ctx:FORTRANIIParser.Print_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#punch_stmt.
    def enterPunch_stmt(self, ctx:FORTRANIIParser.Punch_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#punch_stmt.
    def exitPunch_stmt(self, ctx:FORTRANIIParser.Punch_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#format_stmt.
    def enterFormat_stmt(self, ctx:FORTRANIIParser.Format_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#format_stmt.
    def exitFormat_stmt(self, ctx:FORTRANIIParser.Format_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#format_specification.
    def enterFormat_specification(self, ctx:FORTRANIIParser.Format_specificationContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#format_specification.
    def exitFormat_specification(self, ctx:FORTRANIIParser.Format_specificationContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#format_item.
    def enterFormat_item(self, ctx:FORTRANIIParser.Format_itemContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#format_item.
    def exitFormat_item(self, ctx:FORTRANIIParser.Format_itemContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#format_descriptor.
    def enterFormat_descriptor(self, ctx:FORTRANIIParser.Format_descriptorContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#format_descriptor.
    def exitFormat_descriptor(self, ctx:FORTRANIIParser.Format_descriptorContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#dimension_stmt.
    def enterDimension_stmt(self, ctx:FORTRANIIParser.Dimension_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#dimension_stmt.
    def exitDimension_stmt(self, ctx:FORTRANIIParser.Dimension_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#array_declarator.
    def enterArray_declarator(self, ctx:FORTRANIIParser.Array_declaratorContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#array_declarator.
    def exitArray_declarator(self, ctx:FORTRANIIParser.Array_declaratorContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#dimension_list.
    def enterDimension_list(self, ctx:FORTRANIIParser.Dimension_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#dimension_list.
    def exitDimension_list(self, ctx:FORTRANIIParser.Dimension_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#equivalence_stmt.
    def enterEquivalence_stmt(self, ctx:FORTRANIIParser.Equivalence_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#equivalence_stmt.
    def exitEquivalence_stmt(self, ctx:FORTRANIIParser.Equivalence_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#equivalence_set.
    def enterEquivalence_set(self, ctx:FORTRANIIParser.Equivalence_setContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#equivalence_set.
    def exitEquivalence_set(self, ctx:FORTRANIIParser.Equivalence_setContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#frequency_stmt.
    def enterFrequency_stmt(self, ctx:FORTRANIIParser.Frequency_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#frequency_stmt.
    def exitFrequency_stmt(self, ctx:FORTRANIIParser.Frequency_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#common_stmt.
    def enterCommon_stmt(self, ctx:FORTRANIIParser.Common_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#common_stmt.
    def exitCommon_stmt(self, ctx:FORTRANIIParser.Common_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#expr.
    def enterExpr(self, ctx:FORTRANIIParser.ExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#expr.
    def exitExpr(self, ctx:FORTRANIIParser.ExprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#additive_expr.
    def enterAdditive_expr(self, ctx:FORTRANIIParser.Additive_exprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#additive_expr.
    def exitAdditive_expr(self, ctx:FORTRANIIParser.Additive_exprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#additive_op.
    def enterAdditive_op(self, ctx:FORTRANIIParser.Additive_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#additive_op.
    def exitAdditive_op(self, ctx:FORTRANIIParser.Additive_opContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#multiplicative_expr.
    def enterMultiplicative_expr(self, ctx:FORTRANIIParser.Multiplicative_exprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#multiplicative_expr.
    def exitMultiplicative_expr(self, ctx:FORTRANIIParser.Multiplicative_exprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#multiplicative_op.
    def enterMultiplicative_op(self, ctx:FORTRANIIParser.Multiplicative_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#multiplicative_op.
    def exitMultiplicative_op(self, ctx:FORTRANIIParser.Multiplicative_opContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#unary_expr.
    def enterUnary_expr(self, ctx:FORTRANIIParser.Unary_exprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#unary_expr.
    def exitUnary_expr(self, ctx:FORTRANIIParser.Unary_exprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#unary_op.
    def enterUnary_op(self, ctx:FORTRANIIParser.Unary_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#unary_op.
    def exitUnary_op(self, ctx:FORTRANIIParser.Unary_opContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#power_expr.
    def enterPower_expr(self, ctx:FORTRANIIParser.Power_exprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#power_expr.
    def exitPower_expr(self, ctx:FORTRANIIParser.Power_exprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#primary.
    def enterPrimary(self, ctx:FORTRANIIParser.PrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#primary.
    def exitPrimary(self, ctx:FORTRANIIParser.PrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#literal.
    def enterLiteral(self, ctx:FORTRANIIParser.LiteralContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#literal.
    def exitLiteral(self, ctx:FORTRANIIParser.LiteralContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#variable.
    def enterVariable(self, ctx:FORTRANIIParser.VariableContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#variable.
    def exitVariable(self, ctx:FORTRANIIParser.VariableContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#label_list.
    def enterLabel_list(self, ctx:FORTRANIIParser.Label_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#label_list.
    def exitLabel_list(self, ctx:FORTRANIIParser.Label_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#variable_list.
    def enterVariable_list(self, ctx:FORTRANIIParser.Variable_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#variable_list.
    def exitVariable_list(self, ctx:FORTRANIIParser.Variable_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#input_list.
    def enterInput_list(self, ctx:FORTRANIIParser.Input_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#input_list.
    def exitInput_list(self, ctx:FORTRANIIParser.Input_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#output_list.
    def enterOutput_list(self, ctx:FORTRANIIParser.Output_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#output_list.
    def exitOutput_list(self, ctx:FORTRANIIParser.Output_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#expr_list.
    def enterExpr_list(self, ctx:FORTRANIIParser.Expr_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#expr_list.
    def exitExpr_list(self, ctx:FORTRANIIParser.Expr_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#integer_expr.
    def enterInteger_expr(self, ctx:FORTRANIIParser.Integer_exprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#integer_expr.
    def exitInteger_expr(self, ctx:FORTRANIIParser.Integer_exprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#program_unit_core.
    def enterProgram_unit_core(self, ctx:FORTRANIIParser.Program_unit_coreContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#program_unit_core.
    def exitProgram_unit_core(self, ctx:FORTRANIIParser.Program_unit_coreContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#assign_stmt.
    def enterAssign_stmt(self, ctx:FORTRANIIParser.Assign_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#assign_stmt.
    def exitAssign_stmt(self, ctx:FORTRANIIParser.Assign_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#assigned_goto_stmt.
    def enterAssigned_goto_stmt(self, ctx:FORTRANIIParser.Assigned_goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#assigned_goto_stmt.
    def exitAssigned_goto_stmt(self, ctx:FORTRANIIParser.Assigned_goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_arithmetic.
    def enterIf_stmt_arithmetic(self, ctx:FORTRANIIParser.If_stmt_arithmeticContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_arithmetic.
    def exitIf_stmt_arithmetic(self, ctx:FORTRANIIParser.If_stmt_arithmeticContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_sense_light.
    def enterIf_stmt_sense_light(self, ctx:FORTRANIIParser.If_stmt_sense_lightContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_sense_light.
    def exitIf_stmt_sense_light(self, ctx:FORTRANIIParser.If_stmt_sense_lightContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_sense_switch.
    def enterIf_stmt_sense_switch(self, ctx:FORTRANIIParser.If_stmt_sense_switchContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_sense_switch.
    def exitIf_stmt_sense_switch(self, ctx:FORTRANIIParser.If_stmt_sense_switchContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_accumulator_overflow.
    def enterIf_stmt_accumulator_overflow(self, ctx:FORTRANIIParser.If_stmt_accumulator_overflowContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_accumulator_overflow.
    def exitIf_stmt_accumulator_overflow(self, ctx:FORTRANIIParser.If_stmt_accumulator_overflowContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_quotient_overflow.
    def enterIf_stmt_quotient_overflow(self, ctx:FORTRANIIParser.If_stmt_quotient_overflowContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_quotient_overflow.
    def exitIf_stmt_quotient_overflow(self, ctx:FORTRANIIParser.If_stmt_quotient_overflowContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_divide_check.
    def enterIf_stmt_divide_check(self, ctx:FORTRANIIParser.If_stmt_divide_checkContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_divide_check.
    def exitIf_stmt_divide_check(self, ctx:FORTRANIIParser.If_stmt_divide_checkContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#sense_light_stmt.
    def enterSense_light_stmt(self, ctx:FORTRANIIParser.Sense_light_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#sense_light_stmt.
    def exitSense_light_stmt(self, ctx:FORTRANIIParser.Sense_light_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#do_stmt_basic.
    def enterDo_stmt_basic(self, ctx:FORTRANIIParser.Do_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#do_stmt_basic.
    def exitDo_stmt_basic(self, ctx:FORTRANIIParser.Do_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#read_stmt_basic.
    def enterRead_stmt_basic(self, ctx:FORTRANIIParser.Read_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#read_stmt_basic.
    def exitRead_stmt_basic(self, ctx:FORTRANIIParser.Read_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#write_stmt_basic.
    def enterWrite_stmt_basic(self, ctx:FORTRANIIParser.Write_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#write_stmt_basic.
    def exitWrite_stmt_basic(self, ctx:FORTRANIIParser.Write_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#RelationalExpression.
    def enterRelationalExpression(self, ctx:FORTRANIIParser.RelationalExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#RelationalExpression.
    def exitRelationalExpression(self, ctx:FORTRANIIParser.RelationalExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#RelationalPrimary.
    def enterRelationalPrimary(self, ctx:FORTRANIIParser.RelationalPrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#RelationalPrimary.
    def exitRelationalPrimary(self, ctx:FORTRANIIParser.RelationalPrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#relational_op.
    def enterRelational_op(self, ctx:FORTRANIIParser.Relational_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#relational_op.
    def exitRelational_op(self, ctx:FORTRANIIParser.Relational_opContext):
        pass



del FORTRANIIParser