# Generated from ../../grammars/FORTRANIIParser.g4 by ANTLR 4.13.2
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


    # Enter a parse tree produced by FORTRANIIParser#MultDivExpr.
    def enterMultDivExpr(self, ctx:FORTRANIIParser.MultDivExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#MultDivExpr.
    def exitMultDivExpr(self, ctx:FORTRANIIParser.MultDivExprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#PowerExpr.
    def enterPowerExpr(self, ctx:FORTRANIIParser.PowerExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#PowerExpr.
    def exitPowerExpr(self, ctx:FORTRANIIParser.PowerExprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#UnaryPlusExpr.
    def enterUnaryPlusExpr(self, ctx:FORTRANIIParser.UnaryPlusExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#UnaryPlusExpr.
    def exitUnaryPlusExpr(self, ctx:FORTRANIIParser.UnaryPlusExprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#PrimaryExpr.
    def enterPrimaryExpr(self, ctx:FORTRANIIParser.PrimaryExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#PrimaryExpr.
    def exitPrimaryExpr(self, ctx:FORTRANIIParser.PrimaryExprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#AddSubExpr.
    def enterAddSubExpr(self, ctx:FORTRANIIParser.AddSubExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#AddSubExpr.
    def exitAddSubExpr(self, ctx:FORTRANIIParser.AddSubExprContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#UnaryMinusExpr.
    def enterUnaryMinusExpr(self, ctx:FORTRANIIParser.UnaryMinusExprContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#UnaryMinusExpr.
    def exitUnaryMinusExpr(self, ctx:FORTRANIIParser.UnaryMinusExprContext):
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


    # Enter a parse tree produced by FORTRANIIParser#subscript_list.
    def enterSubscript_list(self, ctx:FORTRANIIParser.Subscript_listContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#subscript_list.
    def exitSubscript_list(self, ctx:FORTRANIIParser.Subscript_listContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#function_reference.
    def enterFunction_reference(self, ctx:FORTRANIIParser.Function_referenceContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#function_reference.
    def exitFunction_reference(self, ctx:FORTRANIIParser.Function_referenceContext):
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


    # Enter a parse tree produced by FORTRANIIParser#if_stmt_arithmetic.
    def enterIf_stmt_arithmetic(self, ctx:FORTRANIIParser.If_stmt_arithmeticContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#if_stmt_arithmetic.
    def exitIf_stmt_arithmetic(self, ctx:FORTRANIIParser.If_stmt_arithmeticContext):
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


    # Enter a parse tree produced by FORTRANIIParser#AdditiveExpression.
    def enterAdditiveExpression(self, ctx:FORTRANIIParser.AdditiveExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#AdditiveExpression.
    def exitAdditiveExpression(self, ctx:FORTRANIIParser.AdditiveExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#AdditivePrimary.
    def enterAdditivePrimary(self, ctx:FORTRANIIParser.AdditivePrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#AdditivePrimary.
    def exitAdditivePrimary(self, ctx:FORTRANIIParser.AdditivePrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#additive_op.
    def enterAdditive_op(self, ctx:FORTRANIIParser.Additive_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#additive_op.
    def exitAdditive_op(self, ctx:FORTRANIIParser.Additive_opContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#MultiplicativePrimary.
    def enterMultiplicativePrimary(self, ctx:FORTRANIIParser.MultiplicativePrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#MultiplicativePrimary.
    def exitMultiplicativePrimary(self, ctx:FORTRANIIParser.MultiplicativePrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#MultiplicativeExpression.
    def enterMultiplicativeExpression(self, ctx:FORTRANIIParser.MultiplicativeExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#MultiplicativeExpression.
    def exitMultiplicativeExpression(self, ctx:FORTRANIIParser.MultiplicativeExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#multiplicative_op.
    def enterMultiplicative_op(self, ctx:FORTRANIIParser.Multiplicative_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#multiplicative_op.
    def exitMultiplicative_op(self, ctx:FORTRANIIParser.Multiplicative_opContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#UnaryExpression.
    def enterUnaryExpression(self, ctx:FORTRANIIParser.UnaryExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#UnaryExpression.
    def exitUnaryExpression(self, ctx:FORTRANIIParser.UnaryExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#UnaryPrimary.
    def enterUnaryPrimary(self, ctx:FORTRANIIParser.UnaryPrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#UnaryPrimary.
    def exitUnaryPrimary(self, ctx:FORTRANIIParser.UnaryPrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#unary_op.
    def enterUnary_op(self, ctx:FORTRANIIParser.Unary_opContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#unary_op.
    def exitUnary_op(self, ctx:FORTRANIIParser.Unary_opContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#PowerExpression.
    def enterPowerExpression(self, ctx:FORTRANIIParser.PowerExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#PowerExpression.
    def exitPowerExpression(self, ctx:FORTRANIIParser.PowerExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANIIParser#PowerPrimary.
    def enterPowerPrimary(self, ctx:FORTRANIIParser.PowerPrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANIIParser#PowerPrimary.
    def exitPowerPrimary(self, ctx:FORTRANIIParser.PowerPrimaryContext):
        pass



del FORTRANIIParser