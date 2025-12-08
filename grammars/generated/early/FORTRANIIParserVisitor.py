# Generated from grammars/FORTRANIIParser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .FORTRANIIParser import FORTRANIIParser
else:
    from FORTRANIIParser import FORTRANIIParser

# This class defines a complete generic visitor for a parse tree produced by FORTRANIIParser.

class FORTRANIIParserVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by FORTRANIIParser#fortran_program.
    def visitFortran_program(self, ctx:FORTRANIIParser.Fortran_programContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#main_program.
    def visitMain_program(self, ctx:FORTRANIIParser.Main_programContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#statement_list.
    def visitStatement_list(self, ctx:FORTRANIIParser.Statement_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#statement.
    def visitStatement(self, ctx:FORTRANIIParser.StatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#label.
    def visitLabel(self, ctx:FORTRANIIParser.LabelContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#subroutine_subprogram.
    def visitSubroutine_subprogram(self, ctx:FORTRANIIParser.Subroutine_subprogramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#function_subprogram.
    def visitFunction_subprogram(self, ctx:FORTRANIIParser.Function_subprogramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#parameter_list.
    def visitParameter_list(self, ctx:FORTRANIIParser.Parameter_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#type_spec.
    def visitType_spec(self, ctx:FORTRANIIParser.Type_specContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#statement_body.
    def visitStatement_body(self, ctx:FORTRANIIParser.Statement_bodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#call_stmt.
    def visitCall_stmt(self, ctx:FORTRANIIParser.Call_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#statement_function_stmt.
    def visitStatement_function_stmt(self, ctx:FORTRANIIParser.Statement_function_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#statement_function_dummy_arg_list.
    def visitStatement_function_dummy_arg_list(self, ctx:FORTRANIIParser.Statement_function_dummy_arg_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#assignment_stmt.
    def visitAssignment_stmt(self, ctx:FORTRANIIParser.Assignment_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#goto_stmt.
    def visitGoto_stmt(self, ctx:FORTRANIIParser.Goto_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#computed_goto_stmt.
    def visitComputed_goto_stmt(self, ctx:FORTRANIIParser.Computed_goto_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#arithmetic_if_stmt.
    def visitArithmetic_if_stmt(self, ctx:FORTRANIIParser.Arithmetic_if_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#do_stmt.
    def visitDo_stmt(self, ctx:FORTRANIIParser.Do_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#continue_stmt.
    def visitContinue_stmt(self, ctx:FORTRANIIParser.Continue_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#stop_stmt.
    def visitStop_stmt(self, ctx:FORTRANIIParser.Stop_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#pause_stmt.
    def visitPause_stmt(self, ctx:FORTRANIIParser.Pause_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#return_stmt.
    def visitReturn_stmt(self, ctx:FORTRANIIParser.Return_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#end_stmt.
    def visitEnd_stmt(self, ctx:FORTRANIIParser.End_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#read_stmt.
    def visitRead_stmt(self, ctx:FORTRANIIParser.Read_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#print_stmt.
    def visitPrint_stmt(self, ctx:FORTRANIIParser.Print_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#punch_stmt.
    def visitPunch_stmt(self, ctx:FORTRANIIParser.Punch_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#format_stmt.
    def visitFormat_stmt(self, ctx:FORTRANIIParser.Format_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#format_specification.
    def visitFormat_specification(self, ctx:FORTRANIIParser.Format_specificationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#format_item.
    def visitFormat_item(self, ctx:FORTRANIIParser.Format_itemContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#format_descriptor.
    def visitFormat_descriptor(self, ctx:FORTRANIIParser.Format_descriptorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#dimension_stmt.
    def visitDimension_stmt(self, ctx:FORTRANIIParser.Dimension_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#array_declarator.
    def visitArray_declarator(self, ctx:FORTRANIIParser.Array_declaratorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#dimension_list.
    def visitDimension_list(self, ctx:FORTRANIIParser.Dimension_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#equivalence_stmt.
    def visitEquivalence_stmt(self, ctx:FORTRANIIParser.Equivalence_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#equivalence_set.
    def visitEquivalence_set(self, ctx:FORTRANIIParser.Equivalence_setContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#frequency_stmt.
    def visitFrequency_stmt(self, ctx:FORTRANIIParser.Frequency_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#common_stmt.
    def visitCommon_stmt(self, ctx:FORTRANIIParser.Common_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#expr.
    def visitExpr(self, ctx:FORTRANIIParser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#additive_expr.
    def visitAdditive_expr(self, ctx:FORTRANIIParser.Additive_exprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#additive_op.
    def visitAdditive_op(self, ctx:FORTRANIIParser.Additive_opContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#multiplicative_expr.
    def visitMultiplicative_expr(self, ctx:FORTRANIIParser.Multiplicative_exprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#multiplicative_op.
    def visitMultiplicative_op(self, ctx:FORTRANIIParser.Multiplicative_opContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#unary_expr.
    def visitUnary_expr(self, ctx:FORTRANIIParser.Unary_exprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#unary_op.
    def visitUnary_op(self, ctx:FORTRANIIParser.Unary_opContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#power_expr.
    def visitPower_expr(self, ctx:FORTRANIIParser.Power_exprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#primary.
    def visitPrimary(self, ctx:FORTRANIIParser.PrimaryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#literal.
    def visitLiteral(self, ctx:FORTRANIIParser.LiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#variable.
    def visitVariable(self, ctx:FORTRANIIParser.VariableContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#label_list.
    def visitLabel_list(self, ctx:FORTRANIIParser.Label_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#variable_list.
    def visitVariable_list(self, ctx:FORTRANIIParser.Variable_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#input_list.
    def visitInput_list(self, ctx:FORTRANIIParser.Input_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#output_list.
    def visitOutput_list(self, ctx:FORTRANIIParser.Output_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#expr_list.
    def visitExpr_list(self, ctx:FORTRANIIParser.Expr_listContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#integer_expr.
    def visitInteger_expr(self, ctx:FORTRANIIParser.Integer_exprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#program_unit_core.
    def visitProgram_unit_core(self, ctx:FORTRANIIParser.Program_unit_coreContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#assign_stmt.
    def visitAssign_stmt(self, ctx:FORTRANIIParser.Assign_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#assigned_goto_stmt.
    def visitAssigned_goto_stmt(self, ctx:FORTRANIIParser.Assigned_goto_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#if_stmt_arithmetic.
    def visitIf_stmt_arithmetic(self, ctx:FORTRANIIParser.If_stmt_arithmeticContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#if_stmt_sense_light.
    def visitIf_stmt_sense_light(self, ctx:FORTRANIIParser.If_stmt_sense_lightContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#if_stmt_sense_switch.
    def visitIf_stmt_sense_switch(self, ctx:FORTRANIIParser.If_stmt_sense_switchContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#if_stmt_accumulator_overflow.
    def visitIf_stmt_accumulator_overflow(self, ctx:FORTRANIIParser.If_stmt_accumulator_overflowContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#if_stmt_quotient_overflow.
    def visitIf_stmt_quotient_overflow(self, ctx:FORTRANIIParser.If_stmt_quotient_overflowContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#if_stmt_divide_check.
    def visitIf_stmt_divide_check(self, ctx:FORTRANIIParser.If_stmt_divide_checkContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#sense_light_stmt.
    def visitSense_light_stmt(self, ctx:FORTRANIIParser.Sense_light_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#do_stmt_basic.
    def visitDo_stmt_basic(self, ctx:FORTRANIIParser.Do_stmt_basicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#read_stmt_basic.
    def visitRead_stmt_basic(self, ctx:FORTRANIIParser.Read_stmt_basicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#write_stmt_basic.
    def visitWrite_stmt_basic(self, ctx:FORTRANIIParser.Write_stmt_basicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#RelationalExpression.
    def visitRelationalExpression(self, ctx:FORTRANIIParser.RelationalExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#RelationalPrimary.
    def visitRelationalPrimary(self, ctx:FORTRANIIParser.RelationalPrimaryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by FORTRANIIParser#relational_op.
    def visitRelational_op(self, ctx:FORTRANIIParser.Relational_opContext):
        return self.visitChildren(ctx)



del FORTRANIIParser