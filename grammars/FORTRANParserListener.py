# Generated from ../../grammars/FORTRANParser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .FORTRANParser import FORTRANParser
else:
    from FORTRANParser import FORTRANParser

# This class defines a complete listener for a parse tree produced by FORTRANParser.
class FORTRANParserListener(ParseTreeListener):

    # Enter a parse tree produced by FORTRANParser#program_unit_core.
    def enterProgram_unit_core(self, ctx:FORTRANParser.Program_unit_coreContext):
        pass

    # Exit a parse tree produced by FORTRANParser#program_unit_core.
    def exitProgram_unit_core(self, ctx:FORTRANParser.Program_unit_coreContext):
        pass


    # Enter a parse tree produced by FORTRANParser#statement_list.
    def enterStatement_list(self, ctx:FORTRANParser.Statement_listContext):
        pass

    # Exit a parse tree produced by FORTRANParser#statement_list.
    def exitStatement_list(self, ctx:FORTRANParser.Statement_listContext):
        pass


    # Enter a parse tree produced by FORTRANParser#statement.
    def enterStatement(self, ctx:FORTRANParser.StatementContext):
        pass

    # Exit a parse tree produced by FORTRANParser#statement.
    def exitStatement(self, ctx:FORTRANParser.StatementContext):
        pass


    # Enter a parse tree produced by FORTRANParser#statement_body.
    def enterStatement_body(self, ctx:FORTRANParser.Statement_bodyContext):
        pass

    # Exit a parse tree produced by FORTRANParser#statement_body.
    def exitStatement_body(self, ctx:FORTRANParser.Statement_bodyContext):
        pass


    # Enter a parse tree produced by FORTRANParser#assignment_stmt.
    def enterAssignment_stmt(self, ctx:FORTRANParser.Assignment_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANParser#assignment_stmt.
    def exitAssignment_stmt(self, ctx:FORTRANParser.Assignment_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANParser#goto_stmt.
    def enterGoto_stmt(self, ctx:FORTRANParser.Goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRANParser#goto_stmt.
    def exitGoto_stmt(self, ctx:FORTRANParser.Goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRANParser#if_stmt_arithmetic.
    def enterIf_stmt_arithmetic(self, ctx:FORTRANParser.If_stmt_arithmeticContext):
        pass

    # Exit a parse tree produced by FORTRANParser#if_stmt_arithmetic.
    def exitIf_stmt_arithmetic(self, ctx:FORTRANParser.If_stmt_arithmeticContext):
        pass


    # Enter a parse tree produced by FORTRANParser#do_stmt_basic.
    def enterDo_stmt_basic(self, ctx:FORTRANParser.Do_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRANParser#do_stmt_basic.
    def exitDo_stmt_basic(self, ctx:FORTRANParser.Do_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRANParser#read_stmt_basic.
    def enterRead_stmt_basic(self, ctx:FORTRANParser.Read_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRANParser#read_stmt_basic.
    def exitRead_stmt_basic(self, ctx:FORTRANParser.Read_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRANParser#write_stmt_basic.
    def enterWrite_stmt_basic(self, ctx:FORTRANParser.Write_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRANParser#write_stmt_basic.
    def exitWrite_stmt_basic(self, ctx:FORTRANParser.Write_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRANParser#expr.
    def enterExpr(self, ctx:FORTRANParser.ExprContext):
        pass

    # Exit a parse tree produced by FORTRANParser#expr.
    def exitExpr(self, ctx:FORTRANParser.ExprContext):
        pass


    # Enter a parse tree produced by FORTRANParser#RelationalExpression.
    def enterRelationalExpression(self, ctx:FORTRANParser.RelationalExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANParser#RelationalExpression.
    def exitRelationalExpression(self, ctx:FORTRANParser.RelationalExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANParser#RelationalPrimary.
    def enterRelationalPrimary(self, ctx:FORTRANParser.RelationalPrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANParser#RelationalPrimary.
    def exitRelationalPrimary(self, ctx:FORTRANParser.RelationalPrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANParser#relational_op.
    def enterRelational_op(self, ctx:FORTRANParser.Relational_opContext):
        pass

    # Exit a parse tree produced by FORTRANParser#relational_op.
    def exitRelational_op(self, ctx:FORTRANParser.Relational_opContext):
        pass


    # Enter a parse tree produced by FORTRANParser#AdditiveExpression.
    def enterAdditiveExpression(self, ctx:FORTRANParser.AdditiveExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANParser#AdditiveExpression.
    def exitAdditiveExpression(self, ctx:FORTRANParser.AdditiveExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANParser#AdditivePrimary.
    def enterAdditivePrimary(self, ctx:FORTRANParser.AdditivePrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANParser#AdditivePrimary.
    def exitAdditivePrimary(self, ctx:FORTRANParser.AdditivePrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANParser#additive_op.
    def enterAdditive_op(self, ctx:FORTRANParser.Additive_opContext):
        pass

    # Exit a parse tree produced by FORTRANParser#additive_op.
    def exitAdditive_op(self, ctx:FORTRANParser.Additive_opContext):
        pass


    # Enter a parse tree produced by FORTRANParser#MultiplicativePrimary.
    def enterMultiplicativePrimary(self, ctx:FORTRANParser.MultiplicativePrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANParser#MultiplicativePrimary.
    def exitMultiplicativePrimary(self, ctx:FORTRANParser.MultiplicativePrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANParser#MultiplicativeExpression.
    def enterMultiplicativeExpression(self, ctx:FORTRANParser.MultiplicativeExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANParser#MultiplicativeExpression.
    def exitMultiplicativeExpression(self, ctx:FORTRANParser.MultiplicativeExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANParser#multiplicative_op.
    def enterMultiplicative_op(self, ctx:FORTRANParser.Multiplicative_opContext):
        pass

    # Exit a parse tree produced by FORTRANParser#multiplicative_op.
    def exitMultiplicative_op(self, ctx:FORTRANParser.Multiplicative_opContext):
        pass


    # Enter a parse tree produced by FORTRANParser#UnaryExpression.
    def enterUnaryExpression(self, ctx:FORTRANParser.UnaryExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANParser#UnaryExpression.
    def exitUnaryExpression(self, ctx:FORTRANParser.UnaryExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANParser#UnaryPrimary.
    def enterUnaryPrimary(self, ctx:FORTRANParser.UnaryPrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANParser#UnaryPrimary.
    def exitUnaryPrimary(self, ctx:FORTRANParser.UnaryPrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANParser#unary_op.
    def enterUnary_op(self, ctx:FORTRANParser.Unary_opContext):
        pass

    # Exit a parse tree produced by FORTRANParser#unary_op.
    def exitUnary_op(self, ctx:FORTRANParser.Unary_opContext):
        pass


    # Enter a parse tree produced by FORTRANParser#PowerExpression.
    def enterPowerExpression(self, ctx:FORTRANParser.PowerExpressionContext):
        pass

    # Exit a parse tree produced by FORTRANParser#PowerExpression.
    def exitPowerExpression(self, ctx:FORTRANParser.PowerExpressionContext):
        pass


    # Enter a parse tree produced by FORTRANParser#PowerPrimary.
    def enterPowerPrimary(self, ctx:FORTRANParser.PowerPrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANParser#PowerPrimary.
    def exitPowerPrimary(self, ctx:FORTRANParser.PowerPrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANParser#primary.
    def enterPrimary(self, ctx:FORTRANParser.PrimaryContext):
        pass

    # Exit a parse tree produced by FORTRANParser#primary.
    def exitPrimary(self, ctx:FORTRANParser.PrimaryContext):
        pass


    # Enter a parse tree produced by FORTRANParser#literal.
    def enterLiteral(self, ctx:FORTRANParser.LiteralContext):
        pass

    # Exit a parse tree produced by FORTRANParser#literal.
    def exitLiteral(self, ctx:FORTRANParser.LiteralContext):
        pass


    # Enter a parse tree produced by FORTRANParser#variable.
    def enterVariable(self, ctx:FORTRANParser.VariableContext):
        pass

    # Exit a parse tree produced by FORTRANParser#variable.
    def exitVariable(self, ctx:FORTRANParser.VariableContext):
        pass


    # Enter a parse tree produced by FORTRANParser#label.
    def enterLabel(self, ctx:FORTRANParser.LabelContext):
        pass

    # Exit a parse tree produced by FORTRANParser#label.
    def exitLabel(self, ctx:FORTRANParser.LabelContext):
        pass


    # Enter a parse tree produced by FORTRANParser#expr_list.
    def enterExpr_list(self, ctx:FORTRANParser.Expr_listContext):
        pass

    # Exit a parse tree produced by FORTRANParser#expr_list.
    def exitExpr_list(self, ctx:FORTRANParser.Expr_listContext):
        pass


    # Enter a parse tree produced by FORTRANParser#input_list.
    def enterInput_list(self, ctx:FORTRANParser.Input_listContext):
        pass

    # Exit a parse tree produced by FORTRANParser#input_list.
    def exitInput_list(self, ctx:FORTRANParser.Input_listContext):
        pass


    # Enter a parse tree produced by FORTRANParser#output_list.
    def enterOutput_list(self, ctx:FORTRANParser.Output_listContext):
        pass

    # Exit a parse tree produced by FORTRANParser#output_list.
    def exitOutput_list(self, ctx:FORTRANParser.Output_listContext):
        pass



del FORTRANParser