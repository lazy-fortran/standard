# Generated from FORTRAN77Parser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .FORTRAN77Parser import FORTRAN77Parser
else:
    from FORTRAN77Parser import FORTRAN77Parser

# This class defines a complete listener for a parse tree produced by FORTRAN77Parser.
class FORTRAN77ParserListener(ParseTreeListener):

    # Enter a parse tree produced by FORTRAN77Parser#main_program.
    def enterMain_program(self, ctx:FORTRAN77Parser.Main_programContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#main_program.
    def exitMain_program(self, ctx:FORTRAN77Parser.Main_programContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#program_stmt.
    def enterProgram_stmt(self, ctx:FORTRAN77Parser.Program_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#program_stmt.
    def exitProgram_stmt(self, ctx:FORTRAN77Parser.Program_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#entry_stmt.
    def enterEntry_stmt(self, ctx:FORTRAN77Parser.Entry_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#entry_stmt.
    def exitEntry_stmt(self, ctx:FORTRAN77Parser.Entry_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#entry_dummy_arg_list.
    def enterEntry_dummy_arg_list(self, ctx:FORTRAN77Parser.Entry_dummy_arg_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#entry_dummy_arg_list.
    def exitEntry_dummy_arg_list(self, ctx:FORTRAN77Parser.Entry_dummy_arg_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#entry_dummy_arg.
    def enterEntry_dummy_arg(self, ctx:FORTRAN77Parser.Entry_dummy_argContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#entry_dummy_arg.
    def exitEntry_dummy_arg(self, ctx:FORTRAN77Parser.Entry_dummy_argContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#type_spec.
    def enterType_spec(self, ctx:FORTRAN77Parser.Type_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#type_spec.
    def exitType_spec(self, ctx:FORTRAN77Parser.Type_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_length.
    def enterCharacter_length(self, ctx:FORTRAN77Parser.Character_lengthContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_length.
    def exitCharacter_length(self, ctx:FORTRAN77Parser.Character_lengthContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#statement_body.
    def enterStatement_body(self, ctx:FORTRAN77Parser.Statement_bodyContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#statement_body.
    def exitStatement_body(self, ctx:FORTRAN77Parser.Statement_bodyContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#block_if_construct.
    def enterBlock_if_construct(self, ctx:FORTRAN77Parser.Block_if_constructContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#block_if_construct.
    def exitBlock_if_construct(self, ctx:FORTRAN77Parser.Block_if_constructContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_then_stmt.
    def enterIf_then_stmt(self, ctx:FORTRAN77Parser.If_then_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_then_stmt.
    def exitIf_then_stmt(self, ctx:FORTRAN77Parser.If_then_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#else_if_part.
    def enterElse_if_part(self, ctx:FORTRAN77Parser.Else_if_partContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#else_if_part.
    def exitElse_if_part(self, ctx:FORTRAN77Parser.Else_if_partContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#else_if_stmt.
    def enterElse_if_stmt(self, ctx:FORTRAN77Parser.Else_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#else_if_stmt.
    def exitElse_if_stmt(self, ctx:FORTRAN77Parser.Else_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#else_part.
    def enterElse_part(self, ctx:FORTRAN77Parser.Else_partContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#else_part.
    def exitElse_part(self, ctx:FORTRAN77Parser.Else_partContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#else_stmt.
    def enterElse_stmt(self, ctx:FORTRAN77Parser.Else_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#else_stmt.
    def exitElse_stmt(self, ctx:FORTRAN77Parser.Else_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#end_if_stmt.
    def enterEnd_if_stmt(self, ctx:FORTRAN77Parser.End_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#end_if_stmt.
    def exitEnd_if_stmt(self, ctx:FORTRAN77Parser.End_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#execution_part_construct.
    def enterExecution_part_construct(self, ctx:FORTRAN77Parser.Execution_part_constructContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#execution_part_construct.
    def exitExecution_part_construct(self, ctx:FORTRAN77Parser.Execution_part_constructContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#executable_construct.
    def enterExecutable_construct(self, ctx:FORTRAN77Parser.Executable_constructContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#executable_construct.
    def exitExecutable_construct(self, ctx:FORTRAN77Parser.Executable_constructContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#read_stmt.
    def enterRead_stmt(self, ctx:FORTRAN77Parser.Read_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#read_stmt.
    def exitRead_stmt(self, ctx:FORTRAN77Parser.Read_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#input_item_list_f77.
    def enterInput_item_list_f77(self, ctx:FORTRAN77Parser.Input_item_list_f77Context):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#input_item_list_f77.
    def exitInput_item_list_f77(self, ctx:FORTRAN77Parser.Input_item_list_f77Context):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#input_item_f77.
    def enterInput_item_f77(self, ctx:FORTRAN77Parser.Input_item_f77Context):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#input_item_f77.
    def exitInput_item_f77(self, ctx:FORTRAN77Parser.Input_item_f77Context):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#implied_do_input.
    def enterImplied_do_input(self, ctx:FORTRAN77Parser.Implied_do_inputContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#implied_do_input.
    def exitImplied_do_input(self, ctx:FORTRAN77Parser.Implied_do_inputContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#print_stmt.
    def enterPrint_stmt(self, ctx:FORTRAN77Parser.Print_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#print_stmt.
    def exitPrint_stmt(self, ctx:FORTRAN77Parser.Print_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#write_stmt.
    def enterWrite_stmt(self, ctx:FORTRAN77Parser.Write_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#write_stmt.
    def exitWrite_stmt(self, ctx:FORTRAN77Parser.Write_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#control_info_list.
    def enterControl_info_list(self, ctx:FORTRAN77Parser.Control_info_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#control_info_list.
    def exitControl_info_list(self, ctx:FORTRAN77Parser.Control_info_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#control_info_item.
    def enterControl_info_item(self, ctx:FORTRAN77Parser.Control_info_itemContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#control_info_item.
    def exitControl_info_item(self, ctx:FORTRAN77Parser.Control_info_itemContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_identifier.
    def enterFormat_identifier(self, ctx:FORTRAN77Parser.Format_identifierContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_identifier.
    def exitFormat_identifier(self, ctx:FORTRAN77Parser.Format_identifierContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#output_item_list.
    def enterOutput_item_list(self, ctx:FORTRAN77Parser.Output_item_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#output_item_list.
    def exitOutput_item_list(self, ctx:FORTRAN77Parser.Output_item_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#output_item.
    def enterOutput_item(self, ctx:FORTRAN77Parser.Output_itemContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#output_item.
    def exitOutput_item(self, ctx:FORTRAN77Parser.Output_itemContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#implied_do.
    def enterImplied_do(self, ctx:FORTRAN77Parser.Implied_doContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#implied_do.
    def exitImplied_do(self, ctx:FORTRAN77Parser.Implied_doContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#dlist.
    def enterDlist(self, ctx:FORTRAN77Parser.DlistContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#dlist.
    def exitDlist(self, ctx:FORTRAN77Parser.DlistContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#do_stmt.
    def enterDo_stmt(self, ctx:FORTRAN77Parser.Do_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#do_stmt.
    def exitDo_stmt(self, ctx:FORTRAN77Parser.Do_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#do_variable.
    def enterDo_variable(self, ctx:FORTRAN77Parser.Do_variableContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#do_variable.
    def exitDo_variable(self, ctx:FORTRAN77Parser.Do_variableContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#initial_expr.
    def enterInitial_expr(self, ctx:FORTRAN77Parser.Initial_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#initial_expr.
    def exitInitial_expr(self, ctx:FORTRAN77Parser.Initial_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#final_expr.
    def enterFinal_expr(self, ctx:FORTRAN77Parser.Final_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#final_expr.
    def exitFinal_expr(self, ctx:FORTRAN77Parser.Final_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#increment_expr.
    def enterIncrement_expr(self, ctx:FORTRAN77Parser.Increment_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#increment_expr.
    def exitIncrement_expr(self, ctx:FORTRAN77Parser.Increment_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#save_stmt.
    def enterSave_stmt(self, ctx:FORTRAN77Parser.Save_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#save_stmt.
    def exitSave_stmt(self, ctx:FORTRAN77Parser.Save_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#save_list.
    def enterSave_list(self, ctx:FORTRAN77Parser.Save_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#save_list.
    def exitSave_list(self, ctx:FORTRAN77Parser.Save_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#save_item.
    def enterSave_item(self, ctx:FORTRAN77Parser.Save_itemContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#save_item.
    def exitSave_item(self, ctx:FORTRAN77Parser.Save_itemContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#common_block_name.
    def enterCommon_block_name(self, ctx:FORTRAN77Parser.Common_block_nameContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#common_block_name.
    def exitCommon_block_name(self, ctx:FORTRAN77Parser.Common_block_nameContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#intrinsic_stmt.
    def enterIntrinsic_stmt(self, ctx:FORTRAN77Parser.Intrinsic_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#intrinsic_stmt.
    def exitIntrinsic_stmt(self, ctx:FORTRAN77Parser.Intrinsic_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#intrinsic_procedure_list.
    def enterIntrinsic_procedure_list(self, ctx:FORTRAN77Parser.Intrinsic_procedure_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#intrinsic_procedure_list.
    def exitIntrinsic_procedure_list(self, ctx:FORTRAN77Parser.Intrinsic_procedure_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#external_stmt.
    def enterExternal_stmt(self, ctx:FORTRAN77Parser.External_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#external_stmt.
    def exitExternal_stmt(self, ctx:FORTRAN77Parser.External_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#external_name_list.
    def enterExternal_name_list(self, ctx:FORTRAN77Parser.External_name_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#external_name_list.
    def exitExternal_name_list(self, ctx:FORTRAN77Parser.External_name_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#implicit_stmt.
    def enterImplicit_stmt(self, ctx:FORTRAN77Parser.Implicit_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#implicit_stmt.
    def exitImplicit_stmt(self, ctx:FORTRAN77Parser.Implicit_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#implicit_spec_list.
    def enterImplicit_spec_list(self, ctx:FORTRAN77Parser.Implicit_spec_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#implicit_spec_list.
    def exitImplicit_spec_list(self, ctx:FORTRAN77Parser.Implicit_spec_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#implicit_spec.
    def enterImplicit_spec(self, ctx:FORTRAN77Parser.Implicit_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#implicit_spec.
    def exitImplicit_spec(self, ctx:FORTRAN77Parser.Implicit_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#letter_spec_list.
    def enterLetter_spec_list(self, ctx:FORTRAN77Parser.Letter_spec_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#letter_spec_list.
    def exitLetter_spec_list(self, ctx:FORTRAN77Parser.Letter_spec_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#letter_spec.
    def enterLetter_spec(self, ctx:FORTRAN77Parser.Letter_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#letter_spec.
    def exitLetter_spec(self, ctx:FORTRAN77Parser.Letter_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#parameter_stmt.
    def enterParameter_stmt(self, ctx:FORTRAN77Parser.Parameter_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#parameter_stmt.
    def exitParameter_stmt(self, ctx:FORTRAN77Parser.Parameter_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#parameter_assignment_list.
    def enterParameter_assignment_list(self, ctx:FORTRAN77Parser.Parameter_assignment_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#parameter_assignment_list.
    def exitParameter_assignment_list(self, ctx:FORTRAN77Parser.Parameter_assignment_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#parameter_assignment.
    def enterParameter_assignment(self, ctx:FORTRAN77Parser.Parameter_assignmentContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#parameter_assignment.
    def exitParameter_assignment(self, ctx:FORTRAN77Parser.Parameter_assignmentContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#constant_expr.
    def enterConstant_expr(self, ctx:FORTRAN77Parser.Constant_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#constant_expr.
    def exitConstant_expr(self, ctx:FORTRAN77Parser.Constant_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt.
    def enterData_stmt(self, ctx:FORTRAN77Parser.Data_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt.
    def exitData_stmt(self, ctx:FORTRAN77Parser.Data_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_set.
    def enterData_stmt_set(self, ctx:FORTRAN77Parser.Data_stmt_setContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_set.
    def exitData_stmt_set(self, ctx:FORTRAN77Parser.Data_stmt_setContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_constant_list.
    def enterData_constant_list(self, ctx:FORTRAN77Parser.Data_constant_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_constant_list.
    def exitData_constant_list(self, ctx:FORTRAN77Parser.Data_constant_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_constant.
    def enterData_constant(self, ctx:FORTRAN77Parser.Data_constantContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_constant.
    def exitData_constant(self, ctx:FORTRAN77Parser.Data_constantContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#variable_list.
    def enterVariable_list(self, ctx:FORTRAN77Parser.Variable_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#variable_list.
    def exitVariable_list(self, ctx:FORTRAN77Parser.Variable_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#literal.
    def enterLiteral(self, ctx:FORTRAN77Parser.LiteralContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#literal.
    def exitLiteral(self, ctx:FORTRAN77Parser.LiteralContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_literal.
    def enterLogical_literal(self, ctx:FORTRAN77Parser.Logical_literalContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_literal.
    def exitLogical_literal(self, ctx:FORTRAN77Parser.Logical_literalContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_expr.
    def enterCharacter_expr(self, ctx:FORTRAN77Parser.Character_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_expr.
    def exitCharacter_expr(self, ctx:FORTRAN77Parser.Character_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_operand.
    def enterCharacter_operand(self, ctx:FORTRAN77Parser.Character_operandContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_operand.
    def exitCharacter_operand(self, ctx:FORTRAN77Parser.Character_operandContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_primary.
    def enterCharacter_primary(self, ctx:FORTRAN77Parser.Character_primaryContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_primary.
    def exitCharacter_primary(self, ctx:FORTRAN77Parser.Character_primaryContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_literal_constant.
    def enterCharacter_literal_constant(self, ctx:FORTRAN77Parser.Character_literal_constantContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_literal_constant.
    def exitCharacter_literal_constant(self, ctx:FORTRAN77Parser.Character_literal_constantContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_variable.
    def enterCharacter_variable(self, ctx:FORTRAN77Parser.Character_variableContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_variable.
    def exitCharacter_variable(self, ctx:FORTRAN77Parser.Character_variableContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#substring_range.
    def enterSubstring_range(self, ctx:FORTRAN77Parser.Substring_rangeContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#substring_range.
    def exitSubstring_range(self, ctx:FORTRAN77Parser.Substring_rangeContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#substr_start_expr.
    def enterSubstr_start_expr(self, ctx:FORTRAN77Parser.Substr_start_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#substr_start_expr.
    def exitSubstr_start_expr(self, ctx:FORTRAN77Parser.Substr_start_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#substr_end_expr.
    def enterSubstr_end_expr(self, ctx:FORTRAN77Parser.Substr_end_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#substr_end_expr.
    def exitSubstr_end_expr(self, ctx:FORTRAN77Parser.Substr_end_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#character_function_reference.
    def enterCharacter_function_reference(self, ctx:FORTRAN77Parser.Character_function_referenceContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#character_function_reference.
    def exitCharacter_function_reference(self, ctx:FORTRAN77Parser.Character_function_referenceContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#actual_arg_spec_list.
    def enterActual_arg_spec_list(self, ctx:FORTRAN77Parser.Actual_arg_spec_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#actual_arg_spec_list.
    def exitActual_arg_spec_list(self, ctx:FORTRAN77Parser.Actual_arg_spec_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#actual_arg_spec.
    def enterActual_arg_spec(self, ctx:FORTRAN77Parser.Actual_arg_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#actual_arg_spec.
    def exitActual_arg_spec(self, ctx:FORTRAN77Parser.Actual_arg_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#integer_variable.
    def enterInteger_variable(self, ctx:FORTRAN77Parser.Integer_variableContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#integer_variable.
    def exitInteger_variable(self, ctx:FORTRAN77Parser.Integer_variableContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#real_variable.
    def enterReal_variable(self, ctx:FORTRAN77Parser.Real_variableContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#real_variable.
    def exitReal_variable(self, ctx:FORTRAN77Parser.Real_variableContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#real_expr.
    def enterReal_expr(self, ctx:FORTRAN77Parser.Real_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#real_expr.
    def exitReal_expr(self, ctx:FORTRAN77Parser.Real_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#open_stmt.
    def enterOpen_stmt(self, ctx:FORTRAN77Parser.Open_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#open_stmt.
    def exitOpen_stmt(self, ctx:FORTRAN77Parser.Open_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#close_stmt.
    def enterClose_stmt(self, ctx:FORTRAN77Parser.Close_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#close_stmt.
    def exitClose_stmt(self, ctx:FORTRAN77Parser.Close_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#inquire_stmt.
    def enterInquire_stmt(self, ctx:FORTRAN77Parser.Inquire_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#inquire_stmt.
    def exitInquire_stmt(self, ctx:FORTRAN77Parser.Inquire_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#connect_spec_list.
    def enterConnect_spec_list(self, ctx:FORTRAN77Parser.Connect_spec_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#connect_spec_list.
    def exitConnect_spec_list(self, ctx:FORTRAN77Parser.Connect_spec_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#connect_spec.
    def enterConnect_spec(self, ctx:FORTRAN77Parser.Connect_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#connect_spec.
    def exitConnect_spec(self, ctx:FORTRAN77Parser.Connect_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#close_spec_list.
    def enterClose_spec_list(self, ctx:FORTRAN77Parser.Close_spec_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#close_spec_list.
    def exitClose_spec_list(self, ctx:FORTRAN77Parser.Close_spec_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#close_spec.
    def enterClose_spec(self, ctx:FORTRAN77Parser.Close_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#close_spec.
    def exitClose_spec(self, ctx:FORTRAN77Parser.Close_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#inquire_spec_list.
    def enterInquire_spec_list(self, ctx:FORTRAN77Parser.Inquire_spec_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#inquire_spec_list.
    def exitInquire_spec_list(self, ctx:FORTRAN77Parser.Inquire_spec_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#inquire_spec.
    def enterInquire_spec(self, ctx:FORTRAN77Parser.Inquire_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#inquire_spec.
    def exitInquire_spec(self, ctx:FORTRAN77Parser.Inquire_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#unit_spec.
    def enterUnit_spec(self, ctx:FORTRAN77Parser.Unit_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#unit_spec.
    def exitUnit_spec(self, ctx:FORTRAN77Parser.Unit_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#file_spec.
    def enterFile_spec(self, ctx:FORTRAN77Parser.File_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#file_spec.
    def exitFile_spec(self, ctx:FORTRAN77Parser.File_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#status_spec.
    def enterStatus_spec(self, ctx:FORTRAN77Parser.Status_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#status_spec.
    def exitStatus_spec(self, ctx:FORTRAN77Parser.Status_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#access_spec.
    def enterAccess_spec(self, ctx:FORTRAN77Parser.Access_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#access_spec.
    def exitAccess_spec(self, ctx:FORTRAN77Parser.Access_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#form_spec.
    def enterForm_spec(self, ctx:FORTRAN77Parser.Form_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#form_spec.
    def exitForm_spec(self, ctx:FORTRAN77Parser.Form_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#recl_spec.
    def enterRecl_spec(self, ctx:FORTRAN77Parser.Recl_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#recl_spec.
    def exitRecl_spec(self, ctx:FORTRAN77Parser.Recl_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#blank_spec.
    def enterBlank_spec(self, ctx:FORTRAN77Parser.Blank_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#blank_spec.
    def exitBlank_spec(self, ctx:FORTRAN77Parser.Blank_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#iostat_spec.
    def enterIostat_spec(self, ctx:FORTRAN77Parser.Iostat_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#iostat_spec.
    def exitIostat_spec(self, ctx:FORTRAN77Parser.Iostat_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#err_spec.
    def enterErr_spec(self, ctx:FORTRAN77Parser.Err_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#err_spec.
    def exitErr_spec(self, ctx:FORTRAN77Parser.Err_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#exist_spec.
    def enterExist_spec(self, ctx:FORTRAN77Parser.Exist_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#exist_spec.
    def exitExist_spec(self, ctx:FORTRAN77Parser.Exist_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#opened_spec.
    def enterOpened_spec(self, ctx:FORTRAN77Parser.Opened_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#opened_spec.
    def exitOpened_spec(self, ctx:FORTRAN77Parser.Opened_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#number_spec.
    def enterNumber_spec(self, ctx:FORTRAN77Parser.Number_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#number_spec.
    def exitNumber_spec(self, ctx:FORTRAN77Parser.Number_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#named_spec.
    def enterNamed_spec(self, ctx:FORTRAN77Parser.Named_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#named_spec.
    def exitNamed_spec(self, ctx:FORTRAN77Parser.Named_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#name_spec.
    def enterName_spec(self, ctx:FORTRAN77Parser.Name_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#name_spec.
    def exitName_spec(self, ctx:FORTRAN77Parser.Name_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#sequential_spec.
    def enterSequential_spec(self, ctx:FORTRAN77Parser.Sequential_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#sequential_spec.
    def exitSequential_spec(self, ctx:FORTRAN77Parser.Sequential_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#direct_spec.
    def enterDirect_spec(self, ctx:FORTRAN77Parser.Direct_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#direct_spec.
    def exitDirect_spec(self, ctx:FORTRAN77Parser.Direct_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#formatted_spec.
    def enterFormatted_spec(self, ctx:FORTRAN77Parser.Formatted_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#formatted_spec.
    def exitFormatted_spec(self, ctx:FORTRAN77Parser.Formatted_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#unformatted_spec.
    def enterUnformatted_spec(self, ctx:FORTRAN77Parser.Unformatted_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#unformatted_spec.
    def exitUnformatted_spec(self, ctx:FORTRAN77Parser.Unformatted_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#nextrec_spec.
    def enterNextrec_spec(self, ctx:FORTRAN77Parser.Nextrec_specContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#nextrec_spec.
    def exitNextrec_spec(self, ctx:FORTRAN77Parser.Nextrec_specContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_expr.
    def enterLogical_expr(self, ctx:FORTRAN77Parser.Logical_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_expr.
    def exitLogical_expr(self, ctx:FORTRAN77Parser.Logical_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_term.
    def enterLogical_term(self, ctx:FORTRAN77Parser.Logical_termContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_term.
    def exitLogical_term(self, ctx:FORTRAN77Parser.Logical_termContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_factor.
    def enterLogical_factor(self, ctx:FORTRAN77Parser.Logical_factorContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_factor.
    def exitLogical_factor(self, ctx:FORTRAN77Parser.Logical_factorContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_primary.
    def enterLogical_primary(self, ctx:FORTRAN77Parser.Logical_primaryContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_primary.
    def exitLogical_primary(self, ctx:FORTRAN77Parser.Logical_primaryContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_variable.
    def enterLogical_variable(self, ctx:FORTRAN77Parser.Logical_variableContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_variable.
    def exitLogical_variable(self, ctx:FORTRAN77Parser.Logical_variableContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#relational_expr.
    def enterRelational_expr(self, ctx:FORTRAN77Parser.Relational_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#relational_expr.
    def exitRelational_expr(self, ctx:FORTRAN77Parser.Relational_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#relational_op.
    def enterRelational_op(self, ctx:FORTRAN77Parser.Relational_opContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#relational_op.
    def exitRelational_op(self, ctx:FORTRAN77Parser.Relational_opContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#logical_if_stmt.
    def enterLogical_if_stmt(self, ctx:FORTRAN77Parser.Logical_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#logical_if_stmt.
    def exitLogical_if_stmt(self, ctx:FORTRAN77Parser.Logical_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#fortran66_program.
    def enterFortran66_program(self, ctx:FORTRAN77Parser.Fortran66_programContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#fortran66_program.
    def exitFortran66_program(self, ctx:FORTRAN77Parser.Fortran66_programContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#program_unit.
    def enterProgram_unit(self, ctx:FORTRAN77Parser.Program_unitContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#program_unit.
    def exitProgram_unit(self, ctx:FORTRAN77Parser.Program_unitContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#subprogram.
    def enterSubprogram(self, ctx:FORTRAN77Parser.SubprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#subprogram.
    def exitSubprogram(self, ctx:FORTRAN77Parser.SubprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#subroutine_subprogram.
    def enterSubroutine_subprogram(self, ctx:FORTRAN77Parser.Subroutine_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#subroutine_subprogram.
    def exitSubroutine_subprogram(self, ctx:FORTRAN77Parser.Subroutine_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#function_subprogram.
    def enterFunction_subprogram(self, ctx:FORTRAN77Parser.Function_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#function_subprogram.
    def exitFunction_subprogram(self, ctx:FORTRAN77Parser.Function_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#block_data_subprogram.
    def enterBlock_data_subprogram(self, ctx:FORTRAN77Parser.Block_data_subprogramContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#block_data_subprogram.
    def exitBlock_data_subprogram(self, ctx:FORTRAN77Parser.Block_data_subprogramContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#block_data_name.
    def enterBlock_data_name(self, ctx:FORTRAN77Parser.Block_data_nameContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#block_data_name.
    def exitBlock_data_name(self, ctx:FORTRAN77Parser.Block_data_nameContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_initialization_part.
    def enterData_initialization_part(self, ctx:FORTRAN77Parser.Data_initialization_partContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_initialization_part.
    def exitData_initialization_part(self, ctx:FORTRAN77Parser.Data_initialization_partContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_initialization_statement.
    def enterData_initialization_statement(self, ctx:FORTRAN77Parser.Data_initialization_statementContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_initialization_statement.
    def exitData_initialization_statement(self, ctx:FORTRAN77Parser.Data_initialization_statementContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_initialization_body.
    def enterData_initialization_body(self, ctx:FORTRAN77Parser.Data_initialization_bodyContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_initialization_body.
    def exitData_initialization_body(self, ctx:FORTRAN77Parser.Data_initialization_bodyContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#type_declaration.
    def enterType_declaration(self, ctx:FORTRAN77Parser.Type_declarationContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#type_declaration.
    def exitType_declaration(self, ctx:FORTRAN77Parser.Type_declarationContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#statement_list.
    def enterStatement_list(self, ctx:FORTRAN77Parser.Statement_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#statement_list.
    def exitStatement_list(self, ctx:FORTRAN77Parser.Statement_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#statement.
    def enterStatement(self, ctx:FORTRAN77Parser.StatementContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#statement.
    def exitStatement(self, ctx:FORTRAN77Parser.StatementContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#label.
    def enterLabel(self, ctx:FORTRAN77Parser.LabelContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#label.
    def exitLabel(self, ctx:FORTRAN77Parser.LabelContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#rewind_stmt.
    def enterRewind_stmt(self, ctx:FORTRAN77Parser.Rewind_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#rewind_stmt.
    def exitRewind_stmt(self, ctx:FORTRAN77Parser.Rewind_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#backspace_stmt.
    def enterBackspace_stmt(self, ctx:FORTRAN77Parser.Backspace_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#backspace_stmt.
    def exitBackspace_stmt(self, ctx:FORTRAN77Parser.Backspace_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#endfile_stmt.
    def enterEndfile_stmt(self, ctx:FORTRAN77Parser.Endfile_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#endfile_stmt.
    def exitEndfile_stmt(self, ctx:FORTRAN77Parser.Endfile_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#identifier_list.
    def enterIdentifier_list(self, ctx:FORTRAN77Parser.Identifier_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#identifier_list.
    def exitIdentifier_list(self, ctx:FORTRAN77Parser.Identifier_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_object_list.
    def enterData_stmt_object_list(self, ctx:FORTRAN77Parser.Data_stmt_object_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_object_list.
    def exitData_stmt_object_list(self, ctx:FORTRAN77Parser.Data_stmt_object_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_object.
    def enterData_stmt_object(self, ctx:FORTRAN77Parser.Data_stmt_objectContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_object.
    def exitData_stmt_object(self, ctx:FORTRAN77Parser.Data_stmt_objectContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_implied_do.
    def enterData_implied_do(self, ctx:FORTRAN77Parser.Data_implied_doContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_implied_do.
    def exitData_implied_do(self, ctx:FORTRAN77Parser.Data_implied_doContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_value_list.
    def enterData_stmt_value_list(self, ctx:FORTRAN77Parser.Data_stmt_value_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_value_list.
    def exitData_stmt_value_list(self, ctx:FORTRAN77Parser.Data_stmt_value_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_value.
    def enterData_stmt_value(self, ctx:FORTRAN77Parser.Data_stmt_valueContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_value.
    def exitData_stmt_value(self, ctx:FORTRAN77Parser.Data_stmt_valueContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_repeat.
    def enterData_stmt_repeat(self, ctx:FORTRAN77Parser.Data_stmt_repeatContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_repeat.
    def exitData_stmt_repeat(self, ctx:FORTRAN77Parser.Data_stmt_repeatContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#unsigned_int.
    def enterUnsigned_int(self, ctx:FORTRAN77Parser.Unsigned_intContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#unsigned_int.
    def exitUnsigned_int(self, ctx:FORTRAN77Parser.Unsigned_intContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#data_stmt_constant.
    def enterData_stmt_constant(self, ctx:FORTRAN77Parser.Data_stmt_constantContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#data_stmt_constant.
    def exitData_stmt_constant(self, ctx:FORTRAN77Parser.Data_stmt_constantContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#assign_stmt.
    def enterAssign_stmt(self, ctx:FORTRAN77Parser.Assign_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#assign_stmt.
    def exitAssign_stmt(self, ctx:FORTRAN77Parser.Assign_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#assigned_goto_stmt.
    def enterAssigned_goto_stmt(self, ctx:FORTRAN77Parser.Assigned_goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#assigned_goto_stmt.
    def exitAssigned_goto_stmt(self, ctx:FORTRAN77Parser.Assigned_goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_item.
    def enterFormat_item(self, ctx:FORTRAN77Parser.Format_itemContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_item.
    def exitFormat_item(self, ctx:FORTRAN77Parser.Format_itemContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_repeat_count.
    def enterFormat_repeat_count(self, ctx:FORTRAN77Parser.Format_repeat_countContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_repeat_count.
    def exitFormat_repeat_count(self, ctx:FORTRAN77Parser.Format_repeat_countContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_descriptor_full.
    def enterFormat_descriptor_full(self, ctx:FORTRAN77Parser.Format_descriptor_fullContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_descriptor_full.
    def exitFormat_descriptor_full(self, ctx:FORTRAN77Parser.Format_descriptor_fullContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_decimal_part.
    def enterFormat_decimal_part(self, ctx:FORTRAN77Parser.Format_decimal_partContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_decimal_part.
    def exitFormat_decimal_part(self, ctx:FORTRAN77Parser.Format_decimal_partContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#fortran_program.
    def enterFortran_program(self, ctx:FORTRAN77Parser.Fortran_programContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#fortran_program.
    def exitFortran_program(self, ctx:FORTRAN77Parser.Fortran_programContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#parameter_list.
    def enterParameter_list(self, ctx:FORTRAN77Parser.Parameter_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#parameter_list.
    def exitParameter_list(self, ctx:FORTRAN77Parser.Parameter_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#call_stmt.
    def enterCall_stmt(self, ctx:FORTRAN77Parser.Call_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#call_stmt.
    def exitCall_stmt(self, ctx:FORTRAN77Parser.Call_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#statement_function_stmt.
    def enterStatement_function_stmt(self, ctx:FORTRAN77Parser.Statement_function_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#statement_function_stmt.
    def exitStatement_function_stmt(self, ctx:FORTRAN77Parser.Statement_function_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#statement_function_dummy_arg_list.
    def enterStatement_function_dummy_arg_list(self, ctx:FORTRAN77Parser.Statement_function_dummy_arg_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#statement_function_dummy_arg_list.
    def exitStatement_function_dummy_arg_list(self, ctx:FORTRAN77Parser.Statement_function_dummy_arg_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#assignment_stmt.
    def enterAssignment_stmt(self, ctx:FORTRAN77Parser.Assignment_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#assignment_stmt.
    def exitAssignment_stmt(self, ctx:FORTRAN77Parser.Assignment_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#goto_stmt.
    def enterGoto_stmt(self, ctx:FORTRAN77Parser.Goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#goto_stmt.
    def exitGoto_stmt(self, ctx:FORTRAN77Parser.Goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#computed_goto_stmt.
    def enterComputed_goto_stmt(self, ctx:FORTRAN77Parser.Computed_goto_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#computed_goto_stmt.
    def exitComputed_goto_stmt(self, ctx:FORTRAN77Parser.Computed_goto_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#arithmetic_if_stmt.
    def enterArithmetic_if_stmt(self, ctx:FORTRAN77Parser.Arithmetic_if_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#arithmetic_if_stmt.
    def exitArithmetic_if_stmt(self, ctx:FORTRAN77Parser.Arithmetic_if_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#continue_stmt.
    def enterContinue_stmt(self, ctx:FORTRAN77Parser.Continue_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#continue_stmt.
    def exitContinue_stmt(self, ctx:FORTRAN77Parser.Continue_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#stop_stmt.
    def enterStop_stmt(self, ctx:FORTRAN77Parser.Stop_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#stop_stmt.
    def exitStop_stmt(self, ctx:FORTRAN77Parser.Stop_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#pause_stmt.
    def enterPause_stmt(self, ctx:FORTRAN77Parser.Pause_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#pause_stmt.
    def exitPause_stmt(self, ctx:FORTRAN77Parser.Pause_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#return_stmt.
    def enterReturn_stmt(self, ctx:FORTRAN77Parser.Return_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#return_stmt.
    def exitReturn_stmt(self, ctx:FORTRAN77Parser.Return_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#end_stmt.
    def enterEnd_stmt(self, ctx:FORTRAN77Parser.End_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#end_stmt.
    def exitEnd_stmt(self, ctx:FORTRAN77Parser.End_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#punch_stmt.
    def enterPunch_stmt(self, ctx:FORTRAN77Parser.Punch_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#punch_stmt.
    def exitPunch_stmt(self, ctx:FORTRAN77Parser.Punch_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_stmt.
    def enterFormat_stmt(self, ctx:FORTRAN77Parser.Format_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_stmt.
    def exitFormat_stmt(self, ctx:FORTRAN77Parser.Format_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_specification.
    def enterFormat_specification(self, ctx:FORTRAN77Parser.Format_specificationContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_specification.
    def exitFormat_specification(self, ctx:FORTRAN77Parser.Format_specificationContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#format_descriptor.
    def enterFormat_descriptor(self, ctx:FORTRAN77Parser.Format_descriptorContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#format_descriptor.
    def exitFormat_descriptor(self, ctx:FORTRAN77Parser.Format_descriptorContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#dimension_stmt.
    def enterDimension_stmt(self, ctx:FORTRAN77Parser.Dimension_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#dimension_stmt.
    def exitDimension_stmt(self, ctx:FORTRAN77Parser.Dimension_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#array_declarator.
    def enterArray_declarator(self, ctx:FORTRAN77Parser.Array_declaratorContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#array_declarator.
    def exitArray_declarator(self, ctx:FORTRAN77Parser.Array_declaratorContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#dimension_list.
    def enterDimension_list(self, ctx:FORTRAN77Parser.Dimension_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#dimension_list.
    def exitDimension_list(self, ctx:FORTRAN77Parser.Dimension_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#equivalence_stmt.
    def enterEquivalence_stmt(self, ctx:FORTRAN77Parser.Equivalence_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#equivalence_stmt.
    def exitEquivalence_stmt(self, ctx:FORTRAN77Parser.Equivalence_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#equivalence_set.
    def enterEquivalence_set(self, ctx:FORTRAN77Parser.Equivalence_setContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#equivalence_set.
    def exitEquivalence_set(self, ctx:FORTRAN77Parser.Equivalence_setContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#frequency_stmt.
    def enterFrequency_stmt(self, ctx:FORTRAN77Parser.Frequency_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#frequency_stmt.
    def exitFrequency_stmt(self, ctx:FORTRAN77Parser.Frequency_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#common_stmt.
    def enterCommon_stmt(self, ctx:FORTRAN77Parser.Common_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#common_stmt.
    def exitCommon_stmt(self, ctx:FORTRAN77Parser.Common_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#expr.
    def enterExpr(self, ctx:FORTRAN77Parser.ExprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#expr.
    def exitExpr(self, ctx:FORTRAN77Parser.ExprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#additive_expr.
    def enterAdditive_expr(self, ctx:FORTRAN77Parser.Additive_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#additive_expr.
    def exitAdditive_expr(self, ctx:FORTRAN77Parser.Additive_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#additive_op.
    def enterAdditive_op(self, ctx:FORTRAN77Parser.Additive_opContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#additive_op.
    def exitAdditive_op(self, ctx:FORTRAN77Parser.Additive_opContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#multiplicative_expr.
    def enterMultiplicative_expr(self, ctx:FORTRAN77Parser.Multiplicative_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#multiplicative_expr.
    def exitMultiplicative_expr(self, ctx:FORTRAN77Parser.Multiplicative_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#multiplicative_op.
    def enterMultiplicative_op(self, ctx:FORTRAN77Parser.Multiplicative_opContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#multiplicative_op.
    def exitMultiplicative_op(self, ctx:FORTRAN77Parser.Multiplicative_opContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#unary_expr.
    def enterUnary_expr(self, ctx:FORTRAN77Parser.Unary_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#unary_expr.
    def exitUnary_expr(self, ctx:FORTRAN77Parser.Unary_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#unary_op.
    def enterUnary_op(self, ctx:FORTRAN77Parser.Unary_opContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#unary_op.
    def exitUnary_op(self, ctx:FORTRAN77Parser.Unary_opContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#power_expr.
    def enterPower_expr(self, ctx:FORTRAN77Parser.Power_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#power_expr.
    def exitPower_expr(self, ctx:FORTRAN77Parser.Power_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#primary.
    def enterPrimary(self, ctx:FORTRAN77Parser.PrimaryContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#primary.
    def exitPrimary(self, ctx:FORTRAN77Parser.PrimaryContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#variable.
    def enterVariable(self, ctx:FORTRAN77Parser.VariableContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#variable.
    def exitVariable(self, ctx:FORTRAN77Parser.VariableContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#label_list.
    def enterLabel_list(self, ctx:FORTRAN77Parser.Label_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#label_list.
    def exitLabel_list(self, ctx:FORTRAN77Parser.Label_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#input_list.
    def enterInput_list(self, ctx:FORTRAN77Parser.Input_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#input_list.
    def exitInput_list(self, ctx:FORTRAN77Parser.Input_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#output_list.
    def enterOutput_list(self, ctx:FORTRAN77Parser.Output_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#output_list.
    def exitOutput_list(self, ctx:FORTRAN77Parser.Output_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#expr_list.
    def enterExpr_list(self, ctx:FORTRAN77Parser.Expr_listContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#expr_list.
    def exitExpr_list(self, ctx:FORTRAN77Parser.Expr_listContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#integer_expr.
    def enterInteger_expr(self, ctx:FORTRAN77Parser.Integer_exprContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#integer_expr.
    def exitInteger_expr(self, ctx:FORTRAN77Parser.Integer_exprContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#program_unit_core.
    def enterProgram_unit_core(self, ctx:FORTRAN77Parser.Program_unit_coreContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#program_unit_core.
    def exitProgram_unit_core(self, ctx:FORTRAN77Parser.Program_unit_coreContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_stmt_arithmetic.
    def enterIf_stmt_arithmetic(self, ctx:FORTRAN77Parser.If_stmt_arithmeticContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_stmt_arithmetic.
    def exitIf_stmt_arithmetic(self, ctx:FORTRAN77Parser.If_stmt_arithmeticContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_stmt_sense_light.
    def enterIf_stmt_sense_light(self, ctx:FORTRAN77Parser.If_stmt_sense_lightContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_stmt_sense_light.
    def exitIf_stmt_sense_light(self, ctx:FORTRAN77Parser.If_stmt_sense_lightContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_stmt_sense_switch.
    def enterIf_stmt_sense_switch(self, ctx:FORTRAN77Parser.If_stmt_sense_switchContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_stmt_sense_switch.
    def exitIf_stmt_sense_switch(self, ctx:FORTRAN77Parser.If_stmt_sense_switchContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_stmt_accumulator_overflow.
    def enterIf_stmt_accumulator_overflow(self, ctx:FORTRAN77Parser.If_stmt_accumulator_overflowContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_stmt_accumulator_overflow.
    def exitIf_stmt_accumulator_overflow(self, ctx:FORTRAN77Parser.If_stmt_accumulator_overflowContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_stmt_quotient_overflow.
    def enterIf_stmt_quotient_overflow(self, ctx:FORTRAN77Parser.If_stmt_quotient_overflowContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_stmt_quotient_overflow.
    def exitIf_stmt_quotient_overflow(self, ctx:FORTRAN77Parser.If_stmt_quotient_overflowContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#if_stmt_divide_check.
    def enterIf_stmt_divide_check(self, ctx:FORTRAN77Parser.If_stmt_divide_checkContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#if_stmt_divide_check.
    def exitIf_stmt_divide_check(self, ctx:FORTRAN77Parser.If_stmt_divide_checkContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#sense_light_stmt.
    def enterSense_light_stmt(self, ctx:FORTRAN77Parser.Sense_light_stmtContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#sense_light_stmt.
    def exitSense_light_stmt(self, ctx:FORTRAN77Parser.Sense_light_stmtContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#do_stmt_basic.
    def enterDo_stmt_basic(self, ctx:FORTRAN77Parser.Do_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#do_stmt_basic.
    def exitDo_stmt_basic(self, ctx:FORTRAN77Parser.Do_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#read_stmt_basic.
    def enterRead_stmt_basic(self, ctx:FORTRAN77Parser.Read_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#read_stmt_basic.
    def exitRead_stmt_basic(self, ctx:FORTRAN77Parser.Read_stmt_basicContext):
        pass


    # Enter a parse tree produced by FORTRAN77Parser#write_stmt_basic.
    def enterWrite_stmt_basic(self, ctx:FORTRAN77Parser.Write_stmt_basicContext):
        pass

    # Exit a parse tree produced by FORTRAN77Parser#write_stmt_basic.
    def exitWrite_stmt_basic(self, ctx:FORTRAN77Parser.Write_stmt_basicContext):
        pass



del FORTRAN77Parser