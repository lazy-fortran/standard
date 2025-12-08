# Generated from Fortran90Parser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .Fortran90Parser import Fortran90Parser
else:
    from Fortran90Parser import Fortran90Parser

# This class defines a complete listener for a parse tree produced by Fortran90Parser.
class Fortran90ParserListener(ParseTreeListener):

    # Enter a parse tree produced by Fortran90Parser#program_unit_f90.
    def enterProgram_unit_f90(self, ctx:Fortran90Parser.Program_unit_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#program_unit_f90.
    def exitProgram_unit_f90(self, ctx:Fortran90Parser.Program_unit_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#main_program.
    def enterMain_program(self, ctx:Fortran90Parser.Main_programContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#main_program.
    def exitMain_program(self, ctx:Fortran90Parser.Main_programContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#program_stmt.
    def enterProgram_stmt(self, ctx:Fortran90Parser.Program_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#program_stmt.
    def exitProgram_stmt(self, ctx:Fortran90Parser.Program_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_program_stmt.
    def enterEnd_program_stmt(self, ctx:Fortran90Parser.End_program_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_program_stmt.
    def exitEnd_program_stmt(self, ctx:Fortran90Parser.End_program_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#specification_part.
    def enterSpecification_part(self, ctx:Fortran90Parser.Specification_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#specification_part.
    def exitSpecification_part(self, ctx:Fortran90Parser.Specification_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#declaration_construct.
    def enterDeclaration_construct(self, ctx:Fortran90Parser.Declaration_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#declaration_construct.
    def exitDeclaration_construct(self, ctx:Fortran90Parser.Declaration_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#execution_part.
    def enterExecution_part(self, ctx:Fortran90Parser.Execution_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#execution_part.
    def exitExecution_part(self, ctx:Fortran90Parser.Execution_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#executable_construct.
    def enterExecutable_construct(self, ctx:Fortran90Parser.Executable_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#executable_construct.
    def exitExecutable_construct(self, ctx:Fortran90Parser.Executable_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#executable_stmt.
    def enterExecutable_stmt(self, ctx:Fortran90Parser.Executable_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#executable_stmt.
    def exitExecutable_stmt(self, ctx:Fortran90Parser.Executable_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#construct.
    def enterConstruct(self, ctx:Fortran90Parser.ConstructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#construct.
    def exitConstruct(self, ctx:Fortran90Parser.ConstructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#entry_stmt.
    def enterEntry_stmt(self, ctx:Fortran90Parser.Entry_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#entry_stmt.
    def exitEntry_stmt(self, ctx:Fortran90Parser.Entry_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#entry_dummy_arg_list.
    def enterEntry_dummy_arg_list(self, ctx:Fortran90Parser.Entry_dummy_arg_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#entry_dummy_arg_list.
    def exitEntry_dummy_arg_list(self, ctx:Fortran90Parser.Entry_dummy_arg_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#entry_dummy_arg.
    def enterEntry_dummy_arg(self, ctx:Fortran90Parser.Entry_dummy_argContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#entry_dummy_arg.
    def exitEntry_dummy_arg(self, ctx:Fortran90Parser.Entry_dummy_argContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#type_spec.
    def enterType_spec(self, ctx:Fortran90Parser.Type_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#type_spec.
    def exitType_spec(self, ctx:Fortran90Parser.Type_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_length.
    def enterCharacter_length(self, ctx:Fortran90Parser.Character_lengthContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_length.
    def exitCharacter_length(self, ctx:Fortran90Parser.Character_lengthContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#statement_body.
    def enterStatement_body(self, ctx:Fortran90Parser.Statement_bodyContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#statement_body.
    def exitStatement_body(self, ctx:Fortran90Parser.Statement_bodyContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#block_if_construct.
    def enterBlock_if_construct(self, ctx:Fortran90Parser.Block_if_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#block_if_construct.
    def exitBlock_if_construct(self, ctx:Fortran90Parser.Block_if_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_then_stmt.
    def enterIf_then_stmt(self, ctx:Fortran90Parser.If_then_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_then_stmt.
    def exitIf_then_stmt(self, ctx:Fortran90Parser.If_then_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#else_if_part.
    def enterElse_if_part(self, ctx:Fortran90Parser.Else_if_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#else_if_part.
    def exitElse_if_part(self, ctx:Fortran90Parser.Else_if_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#else_if_stmt.
    def enterElse_if_stmt(self, ctx:Fortran90Parser.Else_if_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#else_if_stmt.
    def exitElse_if_stmt(self, ctx:Fortran90Parser.Else_if_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#else_part.
    def enterElse_part(self, ctx:Fortran90Parser.Else_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#else_part.
    def exitElse_part(self, ctx:Fortran90Parser.Else_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#else_stmt.
    def enterElse_stmt(self, ctx:Fortran90Parser.Else_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#else_stmt.
    def exitElse_stmt(self, ctx:Fortran90Parser.Else_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_if_stmt.
    def enterEnd_if_stmt(self, ctx:Fortran90Parser.End_if_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_if_stmt.
    def exitEnd_if_stmt(self, ctx:Fortran90Parser.End_if_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#execution_part_construct.
    def enterExecution_part_construct(self, ctx:Fortran90Parser.Execution_part_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#execution_part_construct.
    def exitExecution_part_construct(self, ctx:Fortran90Parser.Execution_part_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#read_stmt.
    def enterRead_stmt(self, ctx:Fortran90Parser.Read_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#read_stmt.
    def exitRead_stmt(self, ctx:Fortran90Parser.Read_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#input_item_list_f77.
    def enterInput_item_list_f77(self, ctx:Fortran90Parser.Input_item_list_f77Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#input_item_list_f77.
    def exitInput_item_list_f77(self, ctx:Fortran90Parser.Input_item_list_f77Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#input_item_f77.
    def enterInput_item_f77(self, ctx:Fortran90Parser.Input_item_f77Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#input_item_f77.
    def exitInput_item_f77(self, ctx:Fortran90Parser.Input_item_f77Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#implied_do_input.
    def enterImplied_do_input(self, ctx:Fortran90Parser.Implied_do_inputContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#implied_do_input.
    def exitImplied_do_input(self, ctx:Fortran90Parser.Implied_do_inputContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#print_stmt.
    def enterPrint_stmt(self, ctx:Fortran90Parser.Print_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#print_stmt.
    def exitPrint_stmt(self, ctx:Fortran90Parser.Print_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#write_stmt.
    def enterWrite_stmt(self, ctx:Fortran90Parser.Write_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#write_stmt.
    def exitWrite_stmt(self, ctx:Fortran90Parser.Write_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#control_info_list.
    def enterControl_info_list(self, ctx:Fortran90Parser.Control_info_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#control_info_list.
    def exitControl_info_list(self, ctx:Fortran90Parser.Control_info_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#control_info_item.
    def enterControl_info_item(self, ctx:Fortran90Parser.Control_info_itemContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#control_info_item.
    def exitControl_info_item(self, ctx:Fortran90Parser.Control_info_itemContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_identifier.
    def enterFormat_identifier(self, ctx:Fortran90Parser.Format_identifierContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_identifier.
    def exitFormat_identifier(self, ctx:Fortran90Parser.Format_identifierContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#output_item_list.
    def enterOutput_item_list(self, ctx:Fortran90Parser.Output_item_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#output_item_list.
    def exitOutput_item_list(self, ctx:Fortran90Parser.Output_item_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#output_item.
    def enterOutput_item(self, ctx:Fortran90Parser.Output_itemContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#output_item.
    def exitOutput_item(self, ctx:Fortran90Parser.Output_itemContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#implied_do.
    def enterImplied_do(self, ctx:Fortran90Parser.Implied_doContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#implied_do.
    def exitImplied_do(self, ctx:Fortran90Parser.Implied_doContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#dlist.
    def enterDlist(self, ctx:Fortran90Parser.DlistContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#dlist.
    def exitDlist(self, ctx:Fortran90Parser.DlistContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#do_stmt.
    def enterDo_stmt(self, ctx:Fortran90Parser.Do_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#do_stmt.
    def exitDo_stmt(self, ctx:Fortran90Parser.Do_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#do_variable.
    def enterDo_variable(self, ctx:Fortran90Parser.Do_variableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#do_variable.
    def exitDo_variable(self, ctx:Fortran90Parser.Do_variableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#initial_expr.
    def enterInitial_expr(self, ctx:Fortran90Parser.Initial_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#initial_expr.
    def exitInitial_expr(self, ctx:Fortran90Parser.Initial_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#final_expr.
    def enterFinal_expr(self, ctx:Fortran90Parser.Final_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#final_expr.
    def exitFinal_expr(self, ctx:Fortran90Parser.Final_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#increment_expr.
    def enterIncrement_expr(self, ctx:Fortran90Parser.Increment_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#increment_expr.
    def exitIncrement_expr(self, ctx:Fortran90Parser.Increment_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#save_stmt.
    def enterSave_stmt(self, ctx:Fortran90Parser.Save_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#save_stmt.
    def exitSave_stmt(self, ctx:Fortran90Parser.Save_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#save_list.
    def enterSave_list(self, ctx:Fortran90Parser.Save_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#save_list.
    def exitSave_list(self, ctx:Fortran90Parser.Save_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#save_item.
    def enterSave_item(self, ctx:Fortran90Parser.Save_itemContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#save_item.
    def exitSave_item(self, ctx:Fortran90Parser.Save_itemContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#common_block_name.
    def enterCommon_block_name(self, ctx:Fortran90Parser.Common_block_nameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#common_block_name.
    def exitCommon_block_name(self, ctx:Fortran90Parser.Common_block_nameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#intrinsic_stmt.
    def enterIntrinsic_stmt(self, ctx:Fortran90Parser.Intrinsic_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#intrinsic_stmt.
    def exitIntrinsic_stmt(self, ctx:Fortran90Parser.Intrinsic_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#intrinsic_procedure_list.
    def enterIntrinsic_procedure_list(self, ctx:Fortran90Parser.Intrinsic_procedure_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#intrinsic_procedure_list.
    def exitIntrinsic_procedure_list(self, ctx:Fortran90Parser.Intrinsic_procedure_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#external_stmt.
    def enterExternal_stmt(self, ctx:Fortran90Parser.External_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#external_stmt.
    def exitExternal_stmt(self, ctx:Fortran90Parser.External_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#external_name_list.
    def enterExternal_name_list(self, ctx:Fortran90Parser.External_name_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#external_name_list.
    def exitExternal_name_list(self, ctx:Fortran90Parser.External_name_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#implicit_stmt.
    def enterImplicit_stmt(self, ctx:Fortran90Parser.Implicit_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#implicit_stmt.
    def exitImplicit_stmt(self, ctx:Fortran90Parser.Implicit_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#implicit_spec_list.
    def enterImplicit_spec_list(self, ctx:Fortran90Parser.Implicit_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#implicit_spec_list.
    def exitImplicit_spec_list(self, ctx:Fortran90Parser.Implicit_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#implicit_spec.
    def enterImplicit_spec(self, ctx:Fortran90Parser.Implicit_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#implicit_spec.
    def exitImplicit_spec(self, ctx:Fortran90Parser.Implicit_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#letter_spec_list.
    def enterLetter_spec_list(self, ctx:Fortran90Parser.Letter_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#letter_spec_list.
    def exitLetter_spec_list(self, ctx:Fortran90Parser.Letter_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#letter_spec.
    def enterLetter_spec(self, ctx:Fortran90Parser.Letter_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#letter_spec.
    def exitLetter_spec(self, ctx:Fortran90Parser.Letter_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#parameter_stmt.
    def enterParameter_stmt(self, ctx:Fortran90Parser.Parameter_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#parameter_stmt.
    def exitParameter_stmt(self, ctx:Fortran90Parser.Parameter_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#parameter_assignment_list.
    def enterParameter_assignment_list(self, ctx:Fortran90Parser.Parameter_assignment_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#parameter_assignment_list.
    def exitParameter_assignment_list(self, ctx:Fortran90Parser.Parameter_assignment_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#parameter_assignment.
    def enterParameter_assignment(self, ctx:Fortran90Parser.Parameter_assignmentContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#parameter_assignment.
    def exitParameter_assignment(self, ctx:Fortran90Parser.Parameter_assignmentContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#constant_expr.
    def enterConstant_expr(self, ctx:Fortran90Parser.Constant_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#constant_expr.
    def exitConstant_expr(self, ctx:Fortran90Parser.Constant_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt.
    def enterData_stmt(self, ctx:Fortran90Parser.Data_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt.
    def exitData_stmt(self, ctx:Fortran90Parser.Data_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_set.
    def enterData_stmt_set(self, ctx:Fortran90Parser.Data_stmt_setContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_set.
    def exitData_stmt_set(self, ctx:Fortran90Parser.Data_stmt_setContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_constant_list.
    def enterData_constant_list(self, ctx:Fortran90Parser.Data_constant_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_constant_list.
    def exitData_constant_list(self, ctx:Fortran90Parser.Data_constant_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_constant.
    def enterData_constant(self, ctx:Fortran90Parser.Data_constantContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_constant.
    def exitData_constant(self, ctx:Fortran90Parser.Data_constantContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#variable_list.
    def enterVariable_list(self, ctx:Fortran90Parser.Variable_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#variable_list.
    def exitVariable_list(self, ctx:Fortran90Parser.Variable_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#literal.
    def enterLiteral(self, ctx:Fortran90Parser.LiteralContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#literal.
    def exitLiteral(self, ctx:Fortran90Parser.LiteralContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_literal.
    def enterLogical_literal(self, ctx:Fortran90Parser.Logical_literalContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_literal.
    def exitLogical_literal(self, ctx:Fortran90Parser.Logical_literalContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_expr.
    def enterCharacter_expr(self, ctx:Fortran90Parser.Character_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_expr.
    def exitCharacter_expr(self, ctx:Fortran90Parser.Character_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_operand.
    def enterCharacter_operand(self, ctx:Fortran90Parser.Character_operandContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_operand.
    def exitCharacter_operand(self, ctx:Fortran90Parser.Character_operandContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_primary.
    def enterCharacter_primary(self, ctx:Fortran90Parser.Character_primaryContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_primary.
    def exitCharacter_primary(self, ctx:Fortran90Parser.Character_primaryContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_literal_constant.
    def enterCharacter_literal_constant(self, ctx:Fortran90Parser.Character_literal_constantContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_literal_constant.
    def exitCharacter_literal_constant(self, ctx:Fortran90Parser.Character_literal_constantContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_variable.
    def enterCharacter_variable(self, ctx:Fortran90Parser.Character_variableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_variable.
    def exitCharacter_variable(self, ctx:Fortran90Parser.Character_variableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#substring_range.
    def enterSubstring_range(self, ctx:Fortran90Parser.Substring_rangeContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#substring_range.
    def exitSubstring_range(self, ctx:Fortran90Parser.Substring_rangeContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#substr_start_expr.
    def enterSubstr_start_expr(self, ctx:Fortran90Parser.Substr_start_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#substr_start_expr.
    def exitSubstr_start_expr(self, ctx:Fortran90Parser.Substr_start_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#substr_end_expr.
    def enterSubstr_end_expr(self, ctx:Fortran90Parser.Substr_end_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#substr_end_expr.
    def exitSubstr_end_expr(self, ctx:Fortran90Parser.Substr_end_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#character_function_reference.
    def enterCharacter_function_reference(self, ctx:Fortran90Parser.Character_function_referenceContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#character_function_reference.
    def exitCharacter_function_reference(self, ctx:Fortran90Parser.Character_function_referenceContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#actual_arg_spec_list.
    def enterActual_arg_spec_list(self, ctx:Fortran90Parser.Actual_arg_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#actual_arg_spec_list.
    def exitActual_arg_spec_list(self, ctx:Fortran90Parser.Actual_arg_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#actual_arg_spec.
    def enterActual_arg_spec(self, ctx:Fortran90Parser.Actual_arg_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#actual_arg_spec.
    def exitActual_arg_spec(self, ctx:Fortran90Parser.Actual_arg_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#integer_variable.
    def enterInteger_variable(self, ctx:Fortran90Parser.Integer_variableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#integer_variable.
    def exitInteger_variable(self, ctx:Fortran90Parser.Integer_variableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#real_variable.
    def enterReal_variable(self, ctx:Fortran90Parser.Real_variableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#real_variable.
    def exitReal_variable(self, ctx:Fortran90Parser.Real_variableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#real_expr.
    def enterReal_expr(self, ctx:Fortran90Parser.Real_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#real_expr.
    def exitReal_expr(self, ctx:Fortran90Parser.Real_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#open_stmt.
    def enterOpen_stmt(self, ctx:Fortran90Parser.Open_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#open_stmt.
    def exitOpen_stmt(self, ctx:Fortran90Parser.Open_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#close_stmt.
    def enterClose_stmt(self, ctx:Fortran90Parser.Close_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#close_stmt.
    def exitClose_stmt(self, ctx:Fortran90Parser.Close_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#inquire_stmt.
    def enterInquire_stmt(self, ctx:Fortran90Parser.Inquire_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#inquire_stmt.
    def exitInquire_stmt(self, ctx:Fortran90Parser.Inquire_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#connect_spec_list.
    def enterConnect_spec_list(self, ctx:Fortran90Parser.Connect_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#connect_spec_list.
    def exitConnect_spec_list(self, ctx:Fortran90Parser.Connect_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#connect_spec.
    def enterConnect_spec(self, ctx:Fortran90Parser.Connect_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#connect_spec.
    def exitConnect_spec(self, ctx:Fortran90Parser.Connect_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#close_spec_list.
    def enterClose_spec_list(self, ctx:Fortran90Parser.Close_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#close_spec_list.
    def exitClose_spec_list(self, ctx:Fortran90Parser.Close_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#close_spec.
    def enterClose_spec(self, ctx:Fortran90Parser.Close_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#close_spec.
    def exitClose_spec(self, ctx:Fortran90Parser.Close_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#inquire_spec_list.
    def enterInquire_spec_list(self, ctx:Fortran90Parser.Inquire_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#inquire_spec_list.
    def exitInquire_spec_list(self, ctx:Fortran90Parser.Inquire_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#inquire_spec.
    def enterInquire_spec(self, ctx:Fortran90Parser.Inquire_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#inquire_spec.
    def exitInquire_spec(self, ctx:Fortran90Parser.Inquire_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#unit_spec.
    def enterUnit_spec(self, ctx:Fortran90Parser.Unit_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#unit_spec.
    def exitUnit_spec(self, ctx:Fortran90Parser.Unit_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#file_spec.
    def enterFile_spec(self, ctx:Fortran90Parser.File_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#file_spec.
    def exitFile_spec(self, ctx:Fortran90Parser.File_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#status_spec.
    def enterStatus_spec(self, ctx:Fortran90Parser.Status_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#status_spec.
    def exitStatus_spec(self, ctx:Fortran90Parser.Status_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#access_spec.
    def enterAccess_spec(self, ctx:Fortran90Parser.Access_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#access_spec.
    def exitAccess_spec(self, ctx:Fortran90Parser.Access_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#form_spec.
    def enterForm_spec(self, ctx:Fortran90Parser.Form_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#form_spec.
    def exitForm_spec(self, ctx:Fortran90Parser.Form_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#recl_spec.
    def enterRecl_spec(self, ctx:Fortran90Parser.Recl_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#recl_spec.
    def exitRecl_spec(self, ctx:Fortran90Parser.Recl_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#blank_spec.
    def enterBlank_spec(self, ctx:Fortran90Parser.Blank_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#blank_spec.
    def exitBlank_spec(self, ctx:Fortran90Parser.Blank_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#iostat_spec.
    def enterIostat_spec(self, ctx:Fortran90Parser.Iostat_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#iostat_spec.
    def exitIostat_spec(self, ctx:Fortran90Parser.Iostat_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#err_spec.
    def enterErr_spec(self, ctx:Fortran90Parser.Err_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#err_spec.
    def exitErr_spec(self, ctx:Fortran90Parser.Err_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#exist_spec.
    def enterExist_spec(self, ctx:Fortran90Parser.Exist_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#exist_spec.
    def exitExist_spec(self, ctx:Fortran90Parser.Exist_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#opened_spec.
    def enterOpened_spec(self, ctx:Fortran90Parser.Opened_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#opened_spec.
    def exitOpened_spec(self, ctx:Fortran90Parser.Opened_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#number_spec.
    def enterNumber_spec(self, ctx:Fortran90Parser.Number_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#number_spec.
    def exitNumber_spec(self, ctx:Fortran90Parser.Number_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#named_spec.
    def enterNamed_spec(self, ctx:Fortran90Parser.Named_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#named_spec.
    def exitNamed_spec(self, ctx:Fortran90Parser.Named_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#name_spec.
    def enterName_spec(self, ctx:Fortran90Parser.Name_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#name_spec.
    def exitName_spec(self, ctx:Fortran90Parser.Name_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#sequential_spec.
    def enterSequential_spec(self, ctx:Fortran90Parser.Sequential_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#sequential_spec.
    def exitSequential_spec(self, ctx:Fortran90Parser.Sequential_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#direct_spec.
    def enterDirect_spec(self, ctx:Fortran90Parser.Direct_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#direct_spec.
    def exitDirect_spec(self, ctx:Fortran90Parser.Direct_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#formatted_spec.
    def enterFormatted_spec(self, ctx:Fortran90Parser.Formatted_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#formatted_spec.
    def exitFormatted_spec(self, ctx:Fortran90Parser.Formatted_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#unformatted_spec.
    def enterUnformatted_spec(self, ctx:Fortran90Parser.Unformatted_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#unformatted_spec.
    def exitUnformatted_spec(self, ctx:Fortran90Parser.Unformatted_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#nextrec_spec.
    def enterNextrec_spec(self, ctx:Fortran90Parser.Nextrec_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#nextrec_spec.
    def exitNextrec_spec(self, ctx:Fortran90Parser.Nextrec_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_expr.
    def enterLogical_expr(self, ctx:Fortran90Parser.Logical_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_expr.
    def exitLogical_expr(self, ctx:Fortran90Parser.Logical_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_term.
    def enterLogical_term(self, ctx:Fortran90Parser.Logical_termContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_term.
    def exitLogical_term(self, ctx:Fortran90Parser.Logical_termContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_factor.
    def enterLogical_factor(self, ctx:Fortran90Parser.Logical_factorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_factor.
    def exitLogical_factor(self, ctx:Fortran90Parser.Logical_factorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_primary.
    def enterLogical_primary(self, ctx:Fortran90Parser.Logical_primaryContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_primary.
    def exitLogical_primary(self, ctx:Fortran90Parser.Logical_primaryContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_variable.
    def enterLogical_variable(self, ctx:Fortran90Parser.Logical_variableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_variable.
    def exitLogical_variable(self, ctx:Fortran90Parser.Logical_variableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#relational_expr.
    def enterRelational_expr(self, ctx:Fortran90Parser.Relational_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#relational_expr.
    def exitRelational_expr(self, ctx:Fortran90Parser.Relational_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#relational_op.
    def enterRelational_op(self, ctx:Fortran90Parser.Relational_opContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#relational_op.
    def exitRelational_op(self, ctx:Fortran90Parser.Relational_opContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_if_stmt.
    def enterLogical_if_stmt(self, ctx:Fortran90Parser.Logical_if_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_if_stmt.
    def exitLogical_if_stmt(self, ctx:Fortran90Parser.Logical_if_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#fortran66_program.
    def enterFortran66_program(self, ctx:Fortran90Parser.Fortran66_programContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#fortran66_program.
    def exitFortran66_program(self, ctx:Fortran90Parser.Fortran66_programContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#program_unit.
    def enterProgram_unit(self, ctx:Fortran90Parser.Program_unitContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#program_unit.
    def exitProgram_unit(self, ctx:Fortran90Parser.Program_unitContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#subprogram.
    def enterSubprogram(self, ctx:Fortran90Parser.SubprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#subprogram.
    def exitSubprogram(self, ctx:Fortran90Parser.SubprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#subroutine_subprogram.
    def enterSubroutine_subprogram(self, ctx:Fortran90Parser.Subroutine_subprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#subroutine_subprogram.
    def exitSubroutine_subprogram(self, ctx:Fortran90Parser.Subroutine_subprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#function_subprogram.
    def enterFunction_subprogram(self, ctx:Fortran90Parser.Function_subprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#function_subprogram.
    def exitFunction_subprogram(self, ctx:Fortran90Parser.Function_subprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#block_data_subprogram.
    def enterBlock_data_subprogram(self, ctx:Fortran90Parser.Block_data_subprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#block_data_subprogram.
    def exitBlock_data_subprogram(self, ctx:Fortran90Parser.Block_data_subprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#block_data_name.
    def enterBlock_data_name(self, ctx:Fortran90Parser.Block_data_nameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#block_data_name.
    def exitBlock_data_name(self, ctx:Fortran90Parser.Block_data_nameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_initialization_part.
    def enterData_initialization_part(self, ctx:Fortran90Parser.Data_initialization_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_initialization_part.
    def exitData_initialization_part(self, ctx:Fortran90Parser.Data_initialization_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_initialization_statement.
    def enterData_initialization_statement(self, ctx:Fortran90Parser.Data_initialization_statementContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_initialization_statement.
    def exitData_initialization_statement(self, ctx:Fortran90Parser.Data_initialization_statementContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_initialization_body.
    def enterData_initialization_body(self, ctx:Fortran90Parser.Data_initialization_bodyContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_initialization_body.
    def exitData_initialization_body(self, ctx:Fortran90Parser.Data_initialization_bodyContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#type_declaration.
    def enterType_declaration(self, ctx:Fortran90Parser.Type_declarationContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#type_declaration.
    def exitType_declaration(self, ctx:Fortran90Parser.Type_declarationContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#statement_list.
    def enterStatement_list(self, ctx:Fortran90Parser.Statement_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#statement_list.
    def exitStatement_list(self, ctx:Fortran90Parser.Statement_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#statement.
    def enterStatement(self, ctx:Fortran90Parser.StatementContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#statement.
    def exitStatement(self, ctx:Fortran90Parser.StatementContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#label.
    def enterLabel(self, ctx:Fortran90Parser.LabelContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#label.
    def exitLabel(self, ctx:Fortran90Parser.LabelContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#rewind_stmt.
    def enterRewind_stmt(self, ctx:Fortran90Parser.Rewind_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#rewind_stmt.
    def exitRewind_stmt(self, ctx:Fortran90Parser.Rewind_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#backspace_stmt.
    def enterBackspace_stmt(self, ctx:Fortran90Parser.Backspace_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#backspace_stmt.
    def exitBackspace_stmt(self, ctx:Fortran90Parser.Backspace_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#endfile_stmt.
    def enterEndfile_stmt(self, ctx:Fortran90Parser.Endfile_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#endfile_stmt.
    def exitEndfile_stmt(self, ctx:Fortran90Parser.Endfile_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#identifier_list.
    def enterIdentifier_list(self, ctx:Fortran90Parser.Identifier_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#identifier_list.
    def exitIdentifier_list(self, ctx:Fortran90Parser.Identifier_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_object_list.
    def enterData_stmt_object_list(self, ctx:Fortran90Parser.Data_stmt_object_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_object_list.
    def exitData_stmt_object_list(self, ctx:Fortran90Parser.Data_stmt_object_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_object.
    def enterData_stmt_object(self, ctx:Fortran90Parser.Data_stmt_objectContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_object.
    def exitData_stmt_object(self, ctx:Fortran90Parser.Data_stmt_objectContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_implied_do.
    def enterData_implied_do(self, ctx:Fortran90Parser.Data_implied_doContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_implied_do.
    def exitData_implied_do(self, ctx:Fortran90Parser.Data_implied_doContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_value_list.
    def enterData_stmt_value_list(self, ctx:Fortran90Parser.Data_stmt_value_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_value_list.
    def exitData_stmt_value_list(self, ctx:Fortran90Parser.Data_stmt_value_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_value.
    def enterData_stmt_value(self, ctx:Fortran90Parser.Data_stmt_valueContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_value.
    def exitData_stmt_value(self, ctx:Fortran90Parser.Data_stmt_valueContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_repeat.
    def enterData_stmt_repeat(self, ctx:Fortran90Parser.Data_stmt_repeatContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_repeat.
    def exitData_stmt_repeat(self, ctx:Fortran90Parser.Data_stmt_repeatContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#unsigned_int.
    def enterUnsigned_int(self, ctx:Fortran90Parser.Unsigned_intContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#unsigned_int.
    def exitUnsigned_int(self, ctx:Fortran90Parser.Unsigned_intContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#data_stmt_constant.
    def enterData_stmt_constant(self, ctx:Fortran90Parser.Data_stmt_constantContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#data_stmt_constant.
    def exitData_stmt_constant(self, ctx:Fortran90Parser.Data_stmt_constantContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assign_stmt.
    def enterAssign_stmt(self, ctx:Fortran90Parser.Assign_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#assign_stmt.
    def exitAssign_stmt(self, ctx:Fortran90Parser.Assign_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assigned_goto_stmt.
    def enterAssigned_goto_stmt(self, ctx:Fortran90Parser.Assigned_goto_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#assigned_goto_stmt.
    def exitAssigned_goto_stmt(self, ctx:Fortran90Parser.Assigned_goto_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_item.
    def enterFormat_item(self, ctx:Fortran90Parser.Format_itemContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_item.
    def exitFormat_item(self, ctx:Fortran90Parser.Format_itemContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_repeat_count.
    def enterFormat_repeat_count(self, ctx:Fortran90Parser.Format_repeat_countContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_repeat_count.
    def exitFormat_repeat_count(self, ctx:Fortran90Parser.Format_repeat_countContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_descriptor_full.
    def enterFormat_descriptor_full(self, ctx:Fortran90Parser.Format_descriptor_fullContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_descriptor_full.
    def exitFormat_descriptor_full(self, ctx:Fortran90Parser.Format_descriptor_fullContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_decimal_part.
    def enterFormat_decimal_part(self, ctx:Fortran90Parser.Format_decimal_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_decimal_part.
    def exitFormat_decimal_part(self, ctx:Fortran90Parser.Format_decimal_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#fortran_program.
    def enterFortran_program(self, ctx:Fortran90Parser.Fortran_programContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#fortran_program.
    def exitFortran_program(self, ctx:Fortran90Parser.Fortran_programContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#parameter_list.
    def enterParameter_list(self, ctx:Fortran90Parser.Parameter_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#parameter_list.
    def exitParameter_list(self, ctx:Fortran90Parser.Parameter_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#call_stmt.
    def enterCall_stmt(self, ctx:Fortran90Parser.Call_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#call_stmt.
    def exitCall_stmt(self, ctx:Fortran90Parser.Call_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#statement_function_stmt.
    def enterStatement_function_stmt(self, ctx:Fortran90Parser.Statement_function_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#statement_function_stmt.
    def exitStatement_function_stmt(self, ctx:Fortran90Parser.Statement_function_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#statement_function_dummy_arg_list.
    def enterStatement_function_dummy_arg_list(self, ctx:Fortran90Parser.Statement_function_dummy_arg_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#statement_function_dummy_arg_list.
    def exitStatement_function_dummy_arg_list(self, ctx:Fortran90Parser.Statement_function_dummy_arg_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assignment_stmt.
    def enterAssignment_stmt(self, ctx:Fortran90Parser.Assignment_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#assignment_stmt.
    def exitAssignment_stmt(self, ctx:Fortran90Parser.Assignment_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#goto_stmt.
    def enterGoto_stmt(self, ctx:Fortran90Parser.Goto_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#goto_stmt.
    def exitGoto_stmt(self, ctx:Fortran90Parser.Goto_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#computed_goto_stmt.
    def enterComputed_goto_stmt(self, ctx:Fortran90Parser.Computed_goto_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#computed_goto_stmt.
    def exitComputed_goto_stmt(self, ctx:Fortran90Parser.Computed_goto_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#arithmetic_if_stmt.
    def enterArithmetic_if_stmt(self, ctx:Fortran90Parser.Arithmetic_if_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#arithmetic_if_stmt.
    def exitArithmetic_if_stmt(self, ctx:Fortran90Parser.Arithmetic_if_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#continue_stmt.
    def enterContinue_stmt(self, ctx:Fortran90Parser.Continue_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#continue_stmt.
    def exitContinue_stmt(self, ctx:Fortran90Parser.Continue_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#stop_stmt.
    def enterStop_stmt(self, ctx:Fortran90Parser.Stop_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#stop_stmt.
    def exitStop_stmt(self, ctx:Fortran90Parser.Stop_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#pause_stmt.
    def enterPause_stmt(self, ctx:Fortran90Parser.Pause_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pause_stmt.
    def exitPause_stmt(self, ctx:Fortran90Parser.Pause_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#return_stmt.
    def enterReturn_stmt(self, ctx:Fortran90Parser.Return_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#return_stmt.
    def exitReturn_stmt(self, ctx:Fortran90Parser.Return_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_stmt.
    def enterEnd_stmt(self, ctx:Fortran90Parser.End_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_stmt.
    def exitEnd_stmt(self, ctx:Fortran90Parser.End_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#punch_stmt.
    def enterPunch_stmt(self, ctx:Fortran90Parser.Punch_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#punch_stmt.
    def exitPunch_stmt(self, ctx:Fortran90Parser.Punch_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_stmt.
    def enterFormat_stmt(self, ctx:Fortran90Parser.Format_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_stmt.
    def exitFormat_stmt(self, ctx:Fortran90Parser.Format_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_specification.
    def enterFormat_specification(self, ctx:Fortran90Parser.Format_specificationContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_specification.
    def exitFormat_specification(self, ctx:Fortran90Parser.Format_specificationContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_descriptor.
    def enterFormat_descriptor(self, ctx:Fortran90Parser.Format_descriptorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_descriptor.
    def exitFormat_descriptor(self, ctx:Fortran90Parser.Format_descriptorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#dimension_stmt.
    def enterDimension_stmt(self, ctx:Fortran90Parser.Dimension_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#dimension_stmt.
    def exitDimension_stmt(self, ctx:Fortran90Parser.Dimension_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#array_declarator.
    def enterArray_declarator(self, ctx:Fortran90Parser.Array_declaratorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#array_declarator.
    def exitArray_declarator(self, ctx:Fortran90Parser.Array_declaratorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#dimension_list.
    def enterDimension_list(self, ctx:Fortran90Parser.Dimension_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#dimension_list.
    def exitDimension_list(self, ctx:Fortran90Parser.Dimension_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#equivalence_stmt.
    def enterEquivalence_stmt(self, ctx:Fortran90Parser.Equivalence_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#equivalence_stmt.
    def exitEquivalence_stmt(self, ctx:Fortran90Parser.Equivalence_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#equivalence_set.
    def enterEquivalence_set(self, ctx:Fortran90Parser.Equivalence_setContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#equivalence_set.
    def exitEquivalence_set(self, ctx:Fortran90Parser.Equivalence_setContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#frequency_stmt.
    def enterFrequency_stmt(self, ctx:Fortran90Parser.Frequency_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#frequency_stmt.
    def exitFrequency_stmt(self, ctx:Fortran90Parser.Frequency_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#common_stmt.
    def enterCommon_stmt(self, ctx:Fortran90Parser.Common_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#common_stmt.
    def exitCommon_stmt(self, ctx:Fortran90Parser.Common_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#expr.
    def enterExpr(self, ctx:Fortran90Parser.ExprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#expr.
    def exitExpr(self, ctx:Fortran90Parser.ExprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#additive_expr.
    def enterAdditive_expr(self, ctx:Fortran90Parser.Additive_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#additive_expr.
    def exitAdditive_expr(self, ctx:Fortran90Parser.Additive_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#additive_op.
    def enterAdditive_op(self, ctx:Fortran90Parser.Additive_opContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#additive_op.
    def exitAdditive_op(self, ctx:Fortran90Parser.Additive_opContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#multiplicative_expr.
    def enterMultiplicative_expr(self, ctx:Fortran90Parser.Multiplicative_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#multiplicative_expr.
    def exitMultiplicative_expr(self, ctx:Fortran90Parser.Multiplicative_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#multiplicative_op.
    def enterMultiplicative_op(self, ctx:Fortran90Parser.Multiplicative_opContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#multiplicative_op.
    def exitMultiplicative_op(self, ctx:Fortran90Parser.Multiplicative_opContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#unary_expr.
    def enterUnary_expr(self, ctx:Fortran90Parser.Unary_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#unary_expr.
    def exitUnary_expr(self, ctx:Fortran90Parser.Unary_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#unary_op.
    def enterUnary_op(self, ctx:Fortran90Parser.Unary_opContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#unary_op.
    def exitUnary_op(self, ctx:Fortran90Parser.Unary_opContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#power_expr.
    def enterPower_expr(self, ctx:Fortran90Parser.Power_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#power_expr.
    def exitPower_expr(self, ctx:Fortran90Parser.Power_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#primary.
    def enterPrimary(self, ctx:Fortran90Parser.PrimaryContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#primary.
    def exitPrimary(self, ctx:Fortran90Parser.PrimaryContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#variable.
    def enterVariable(self, ctx:Fortran90Parser.VariableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#variable.
    def exitVariable(self, ctx:Fortran90Parser.VariableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#label_list.
    def enterLabel_list(self, ctx:Fortran90Parser.Label_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#label_list.
    def exitLabel_list(self, ctx:Fortran90Parser.Label_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#input_list.
    def enterInput_list(self, ctx:Fortran90Parser.Input_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#input_list.
    def exitInput_list(self, ctx:Fortran90Parser.Input_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#output_list.
    def enterOutput_list(self, ctx:Fortran90Parser.Output_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#output_list.
    def exitOutput_list(self, ctx:Fortran90Parser.Output_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#expr_list.
    def enterExpr_list(self, ctx:Fortran90Parser.Expr_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#expr_list.
    def exitExpr_list(self, ctx:Fortran90Parser.Expr_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#integer_expr.
    def enterInteger_expr(self, ctx:Fortran90Parser.Integer_exprContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#integer_expr.
    def exitInteger_expr(self, ctx:Fortran90Parser.Integer_exprContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#program_unit_core.
    def enterProgram_unit_core(self, ctx:Fortran90Parser.Program_unit_coreContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#program_unit_core.
    def exitProgram_unit_core(self, ctx:Fortran90Parser.Program_unit_coreContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_stmt_arithmetic.
    def enterIf_stmt_arithmetic(self, ctx:Fortran90Parser.If_stmt_arithmeticContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_stmt_arithmetic.
    def exitIf_stmt_arithmetic(self, ctx:Fortran90Parser.If_stmt_arithmeticContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_stmt_sense_light.
    def enterIf_stmt_sense_light(self, ctx:Fortran90Parser.If_stmt_sense_lightContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_stmt_sense_light.
    def exitIf_stmt_sense_light(self, ctx:Fortran90Parser.If_stmt_sense_lightContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_stmt_sense_switch.
    def enterIf_stmt_sense_switch(self, ctx:Fortran90Parser.If_stmt_sense_switchContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_stmt_sense_switch.
    def exitIf_stmt_sense_switch(self, ctx:Fortran90Parser.If_stmt_sense_switchContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_stmt_accumulator_overflow.
    def enterIf_stmt_accumulator_overflow(self, ctx:Fortran90Parser.If_stmt_accumulator_overflowContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_stmt_accumulator_overflow.
    def exitIf_stmt_accumulator_overflow(self, ctx:Fortran90Parser.If_stmt_accumulator_overflowContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_stmt_quotient_overflow.
    def enterIf_stmt_quotient_overflow(self, ctx:Fortran90Parser.If_stmt_quotient_overflowContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_stmt_quotient_overflow.
    def exitIf_stmt_quotient_overflow(self, ctx:Fortran90Parser.If_stmt_quotient_overflowContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_stmt_divide_check.
    def enterIf_stmt_divide_check(self, ctx:Fortran90Parser.If_stmt_divide_checkContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_stmt_divide_check.
    def exitIf_stmt_divide_check(self, ctx:Fortran90Parser.If_stmt_divide_checkContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#sense_light_stmt.
    def enterSense_light_stmt(self, ctx:Fortran90Parser.Sense_light_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#sense_light_stmt.
    def exitSense_light_stmt(self, ctx:Fortran90Parser.Sense_light_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#do_stmt_basic.
    def enterDo_stmt_basic(self, ctx:Fortran90Parser.Do_stmt_basicContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#do_stmt_basic.
    def exitDo_stmt_basic(self, ctx:Fortran90Parser.Do_stmt_basicContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#read_stmt_basic.
    def enterRead_stmt_basic(self, ctx:Fortran90Parser.Read_stmt_basicContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#read_stmt_basic.
    def exitRead_stmt_basic(self, ctx:Fortran90Parser.Read_stmt_basicContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#write_stmt_basic.
    def enterWrite_stmt_basic(self, ctx:Fortran90Parser.Write_stmt_basicContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#write_stmt_basic.
    def exitWrite_stmt_basic(self, ctx:Fortran90Parser.Write_stmt_basicContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#module.
    def enterModule(self, ctx:Fortran90Parser.ModuleContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#module.
    def exitModule(self, ctx:Fortran90Parser.ModuleContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#module_stmt.
    def enterModule_stmt(self, ctx:Fortran90Parser.Module_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#module_stmt.
    def exitModule_stmt(self, ctx:Fortran90Parser.Module_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_module_stmt.
    def enterEnd_module_stmt(self, ctx:Fortran90Parser.End_module_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_module_stmt.
    def exitEnd_module_stmt(self, ctx:Fortran90Parser.End_module_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#module_subprogram_part.
    def enterModule_subprogram_part(self, ctx:Fortran90Parser.Module_subprogram_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#module_subprogram_part.
    def exitModule_subprogram_part(self, ctx:Fortran90Parser.Module_subprogram_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#module_subprogram.
    def enterModule_subprogram(self, ctx:Fortran90Parser.Module_subprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#module_subprogram.
    def exitModule_subprogram(self, ctx:Fortran90Parser.Module_subprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#contains_stmt.
    def enterContains_stmt(self, ctx:Fortran90Parser.Contains_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#contains_stmt.
    def exitContains_stmt(self, ctx:Fortran90Parser.Contains_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#use_stmt.
    def enterUse_stmt(self, ctx:Fortran90Parser.Use_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#use_stmt.
    def exitUse_stmt(self, ctx:Fortran90Parser.Use_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#module_name.
    def enterModule_name(self, ctx:Fortran90Parser.Module_nameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#module_name.
    def exitModule_name(self, ctx:Fortran90Parser.Module_nameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#rename_list.
    def enterRename_list(self, ctx:Fortran90Parser.Rename_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#rename_list.
    def exitRename_list(self, ctx:Fortran90Parser.Rename_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#rename.
    def enterRename(self, ctx:Fortran90Parser.RenameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#rename.
    def exitRename(self, ctx:Fortran90Parser.RenameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#only_list.
    def enterOnly_list(self, ctx:Fortran90Parser.Only_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#only_list.
    def exitOnly_list(self, ctx:Fortran90Parser.Only_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#only_item.
    def enterOnly_item(self, ctx:Fortran90Parser.Only_itemContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#only_item.
    def exitOnly_item(self, ctx:Fortran90Parser.Only_itemContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#operator_token.
    def enterOperator_token(self, ctx:Fortran90Parser.Operator_tokenContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#operator_token.
    def exitOperator_token(self, ctx:Fortran90Parser.Operator_tokenContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#interface_block.
    def enterInterface_block(self, ctx:Fortran90Parser.Interface_blockContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#interface_block.
    def exitInterface_block(self, ctx:Fortran90Parser.Interface_blockContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#interface_stmt.
    def enterInterface_stmt(self, ctx:Fortran90Parser.Interface_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#interface_stmt.
    def exitInterface_stmt(self, ctx:Fortran90Parser.Interface_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#generic_spec.
    def enterGeneric_spec(self, ctx:Fortran90Parser.Generic_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#generic_spec.
    def exitGeneric_spec(self, ctx:Fortran90Parser.Generic_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#interface_specification.
    def enterInterface_specification(self, ctx:Fortran90Parser.Interface_specificationContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#interface_specification.
    def exitInterface_specification(self, ctx:Fortran90Parser.Interface_specificationContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#interface_body.
    def enterInterface_body(self, ctx:Fortran90Parser.Interface_bodyContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#interface_body.
    def exitInterface_body(self, ctx:Fortran90Parser.Interface_bodyContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_interface_stmt.
    def enterEnd_interface_stmt(self, ctx:Fortran90Parser.End_interface_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_interface_stmt.
    def exitEnd_interface_stmt(self, ctx:Fortran90Parser.End_interface_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#import_stmt.
    def enterImport_stmt(self, ctx:Fortran90Parser.Import_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#import_stmt.
    def exitImport_stmt(self, ctx:Fortran90Parser.Import_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#import_name_list.
    def enterImport_name_list(self, ctx:Fortran90Parser.Import_name_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#import_name_list.
    def exitImport_name_list(self, ctx:Fortran90Parser.Import_name_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#derived_type_def.
    def enterDerived_type_def(self, ctx:Fortran90Parser.Derived_type_defContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#derived_type_def.
    def exitDerived_type_def(self, ctx:Fortran90Parser.Derived_type_defContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#derived_type_stmt.
    def enterDerived_type_stmt(self, ctx:Fortran90Parser.Derived_type_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#derived_type_stmt.
    def exitDerived_type_stmt(self, ctx:Fortran90Parser.Derived_type_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#type_name.
    def enterType_name(self, ctx:Fortran90Parser.Type_nameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#type_name.
    def exitType_name(self, ctx:Fortran90Parser.Type_nameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#component_def_stmt.
    def enterComponent_def_stmt(self, ctx:Fortran90Parser.Component_def_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#component_def_stmt.
    def exitComponent_def_stmt(self, ctx:Fortran90Parser.Component_def_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#private_sequence_stmt.
    def enterPrivate_sequence_stmt(self, ctx:Fortran90Parser.Private_sequence_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#private_sequence_stmt.
    def exitPrivate_sequence_stmt(self, ctx:Fortran90Parser.Private_sequence_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_type_stmt.
    def enterEnd_type_stmt(self, ctx:Fortran90Parser.End_type_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_type_stmt.
    def exitEnd_type_stmt(self, ctx:Fortran90Parser.End_type_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#structure_constructor.
    def enterStructure_constructor(self, ctx:Fortran90Parser.Structure_constructorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#structure_constructor.
    def exitStructure_constructor(self, ctx:Fortran90Parser.Structure_constructorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#component_spec_list.
    def enterComponent_spec_list(self, ctx:Fortran90Parser.Component_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#component_spec_list.
    def exitComponent_spec_list(self, ctx:Fortran90Parser.Component_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#component_spec.
    def enterComponent_spec(self, ctx:Fortran90Parser.Component_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#component_spec.
    def exitComponent_spec(self, ctx:Fortran90Parser.Component_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#type_declaration_stmt_f90.
    def enterType_declaration_stmt_f90(self, ctx:Fortran90Parser.Type_declaration_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#type_declaration_stmt_f90.
    def exitType_declaration_stmt_f90(self, ctx:Fortran90Parser.Type_declaration_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#type_spec_f90.
    def enterType_spec_f90(self, ctx:Fortran90Parser.Type_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#type_spec_f90.
    def exitType_spec_f90(self, ctx:Fortran90Parser.Type_spec_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#intrinsic_type_spec_f90.
    def enterIntrinsic_type_spec_f90(self, ctx:Fortran90Parser.Intrinsic_type_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#intrinsic_type_spec_f90.
    def exitIntrinsic_type_spec_f90(self, ctx:Fortran90Parser.Intrinsic_type_spec_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#derived_type_spec_f90.
    def enterDerived_type_spec_f90(self, ctx:Fortran90Parser.Derived_type_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#derived_type_spec_f90.
    def exitDerived_type_spec_f90(self, ctx:Fortran90Parser.Derived_type_spec_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#kind_selector.
    def enterKind_selector(self, ctx:Fortran90Parser.Kind_selectorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#kind_selector.
    def exitKind_selector(self, ctx:Fortran90Parser.Kind_selectorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#char_selector.
    def enterChar_selector(self, ctx:Fortran90Parser.Char_selectorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#char_selector.
    def exitChar_selector(self, ctx:Fortran90Parser.Char_selectorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#attr_spec_f90.
    def enterAttr_spec_f90(self, ctx:Fortran90Parser.Attr_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#attr_spec_f90.
    def exitAttr_spec_f90(self, ctx:Fortran90Parser.Attr_spec_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#intent_spec.
    def enterIntent_spec(self, ctx:Fortran90Parser.Intent_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#intent_spec.
    def exitIntent_spec(self, ctx:Fortran90Parser.Intent_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#array_spec_f90.
    def enterArray_spec_f90(self, ctx:Fortran90Parser.Array_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#array_spec_f90.
    def exitArray_spec_f90(self, ctx:Fortran90Parser.Array_spec_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#explicit_shape_spec_list.
    def enterExplicit_shape_spec_list(self, ctx:Fortran90Parser.Explicit_shape_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#explicit_shape_spec_list.
    def exitExplicit_shape_spec_list(self, ctx:Fortran90Parser.Explicit_shape_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#explicit_shape_spec.
    def enterExplicit_shape_spec(self, ctx:Fortran90Parser.Explicit_shape_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#explicit_shape_spec.
    def exitExplicit_shape_spec(self, ctx:Fortran90Parser.Explicit_shape_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assumed_shape_spec_list.
    def enterAssumed_shape_spec_list(self, ctx:Fortran90Parser.Assumed_shape_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#assumed_shape_spec_list.
    def exitAssumed_shape_spec_list(self, ctx:Fortran90Parser.Assumed_shape_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assumed_shape_spec.
    def enterAssumed_shape_spec(self, ctx:Fortran90Parser.Assumed_shape_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#assumed_shape_spec.
    def exitAssumed_shape_spec(self, ctx:Fortran90Parser.Assumed_shape_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#deferred_shape_spec_list.
    def enterDeferred_shape_spec_list(self, ctx:Fortran90Parser.Deferred_shape_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#deferred_shape_spec_list.
    def exitDeferred_shape_spec_list(self, ctx:Fortran90Parser.Deferred_shape_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#deferred_shape_spec.
    def enterDeferred_shape_spec(self, ctx:Fortran90Parser.Deferred_shape_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#deferred_shape_spec.
    def exitDeferred_shape_spec(self, ctx:Fortran90Parser.Deferred_shape_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assumed_size_spec.
    def enterAssumed_size_spec(self, ctx:Fortran90Parser.Assumed_size_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#assumed_size_spec.
    def exitAssumed_size_spec(self, ctx:Fortran90Parser.Assumed_size_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#entity_decl_list_f90.
    def enterEntity_decl_list_f90(self, ctx:Fortran90Parser.Entity_decl_list_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#entity_decl_list_f90.
    def exitEntity_decl_list_f90(self, ctx:Fortran90Parser.Entity_decl_list_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#entity_decl_f90.
    def enterEntity_decl_f90(self, ctx:Fortran90Parser.Entity_decl_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#entity_decl_f90.
    def exitEntity_decl_f90(self, ctx:Fortran90Parser.Entity_decl_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#char_length.
    def enterChar_length(self, ctx:Fortran90Parser.Char_lengthContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#char_length.
    def exitChar_length(self, ctx:Fortran90Parser.Char_lengthContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#identifier_or_keyword.
    def enterIdentifier_or_keyword(self, ctx:Fortran90Parser.Identifier_or_keywordContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#identifier_or_keyword.
    def exitIdentifier_or_keyword(self, ctx:Fortran90Parser.Identifier_or_keywordContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#select_case_construct.
    def enterSelect_case_construct(self, ctx:Fortran90Parser.Select_case_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#select_case_construct.
    def exitSelect_case_construct(self, ctx:Fortran90Parser.Select_case_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#select_case_stmt.
    def enterSelect_case_stmt(self, ctx:Fortran90Parser.Select_case_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#select_case_stmt.
    def exitSelect_case_stmt(self, ctx:Fortran90Parser.Select_case_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#case_construct.
    def enterCase_construct(self, ctx:Fortran90Parser.Case_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#case_construct.
    def exitCase_construct(self, ctx:Fortran90Parser.Case_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#case_stmt.
    def enterCase_stmt(self, ctx:Fortran90Parser.Case_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#case_stmt.
    def exitCase_stmt(self, ctx:Fortran90Parser.Case_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#case_selector.
    def enterCase_selector(self, ctx:Fortran90Parser.Case_selectorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#case_selector.
    def exitCase_selector(self, ctx:Fortran90Parser.Case_selectorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#case_value_range_list.
    def enterCase_value_range_list(self, ctx:Fortran90Parser.Case_value_range_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#case_value_range_list.
    def exitCase_value_range_list(self, ctx:Fortran90Parser.Case_value_range_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#case_value_range.
    def enterCase_value_range(self, ctx:Fortran90Parser.Case_value_rangeContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#case_value_range.
    def exitCase_value_range(self, ctx:Fortran90Parser.Case_value_rangeContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_select_stmt.
    def enterEnd_select_stmt(self, ctx:Fortran90Parser.End_select_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_select_stmt.
    def exitEnd_select_stmt(self, ctx:Fortran90Parser.End_select_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#where_construct.
    def enterWhere_construct(self, ctx:Fortran90Parser.Where_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#where_construct.
    def exitWhere_construct(self, ctx:Fortran90Parser.Where_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#where_construct_stmt.
    def enterWhere_construct_stmt(self, ctx:Fortran90Parser.Where_construct_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#where_construct_stmt.
    def exitWhere_construct_stmt(self, ctx:Fortran90Parser.Where_construct_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#elsewhere_stmt.
    def enterElsewhere_stmt(self, ctx:Fortran90Parser.Elsewhere_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#elsewhere_stmt.
    def exitElsewhere_stmt(self, ctx:Fortran90Parser.Elsewhere_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_where_stmt.
    def enterEnd_where_stmt(self, ctx:Fortran90Parser.End_where_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_where_stmt.
    def exitEnd_where_stmt(self, ctx:Fortran90Parser.End_where_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_expr_f90.
    def enterLogical_expr_f90(self, ctx:Fortran90Parser.Logical_expr_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_expr_f90.
    def exitLogical_expr_f90(self, ctx:Fortran90Parser.Logical_expr_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#where_stmt.
    def enterWhere_stmt(self, ctx:Fortran90Parser.Where_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#where_stmt.
    def exitWhere_stmt(self, ctx:Fortran90Parser.Where_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#do_construct_f90.
    def enterDo_construct_f90(self, ctx:Fortran90Parser.Do_construct_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#do_construct_f90.
    def exitDo_construct_f90(self, ctx:Fortran90Parser.Do_construct_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#do_stmt_f90.
    def enterDo_stmt_f90(self, ctx:Fortran90Parser.Do_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#do_stmt_f90.
    def exitDo_stmt_f90(self, ctx:Fortran90Parser.Do_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#loop_control.
    def enterLoop_control(self, ctx:Fortran90Parser.Loop_controlContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#loop_control.
    def exitLoop_control(self, ctx:Fortran90Parser.Loop_controlContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_do_stmt.
    def enterEnd_do_stmt(self, ctx:Fortran90Parser.End_do_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_do_stmt.
    def exitEnd_do_stmt(self, ctx:Fortran90Parser.End_do_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#cycle_stmt.
    def enterCycle_stmt(self, ctx:Fortran90Parser.Cycle_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#cycle_stmt.
    def exitCycle_stmt(self, ctx:Fortran90Parser.Cycle_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#exit_stmt.
    def enterExit_stmt(self, ctx:Fortran90Parser.Exit_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#exit_stmt.
    def exitExit_stmt(self, ctx:Fortran90Parser.Exit_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#if_construct.
    def enterIf_construct(self, ctx:Fortran90Parser.If_constructContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#if_construct.
    def exitIf_construct(self, ctx:Fortran90Parser.If_constructContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#namelist_stmt.
    def enterNamelist_stmt(self, ctx:Fortran90Parser.Namelist_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#namelist_stmt.
    def exitNamelist_stmt(self, ctx:Fortran90Parser.Namelist_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#namelist_item_list.
    def enterNamelist_item_list(self, ctx:Fortran90Parser.Namelist_item_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#namelist_item_list.
    def exitNamelist_item_list(self, ctx:Fortran90Parser.Namelist_item_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#read_stmt_f90.
    def enterRead_stmt_f90(self, ctx:Fortran90Parser.Read_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#read_stmt_f90.
    def exitRead_stmt_f90(self, ctx:Fortran90Parser.Read_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#write_stmt_f90.
    def enterWrite_stmt_f90(self, ctx:Fortran90Parser.Write_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#write_stmt_f90.
    def exitWrite_stmt_f90(self, ctx:Fortran90Parser.Write_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#print_stmt_f90.
    def enterPrint_stmt_f90(self, ctx:Fortran90Parser.Print_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#print_stmt_f90.
    def exitPrint_stmt_f90(self, ctx:Fortran90Parser.Print_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#io_control_spec_list.
    def enterIo_control_spec_list(self, ctx:Fortran90Parser.Io_control_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#io_control_spec_list.
    def exitIo_control_spec_list(self, ctx:Fortran90Parser.Io_control_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#io_control_spec.
    def enterIo_control_spec(self, ctx:Fortran90Parser.Io_control_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#io_control_spec.
    def exitIo_control_spec(self, ctx:Fortran90Parser.Io_control_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format_spec.
    def enterFormat_spec(self, ctx:Fortran90Parser.Format_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format_spec.
    def exitFormat_spec(self, ctx:Fortran90Parser.Format_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#namelist_name.
    def enterNamelist_name(self, ctx:Fortran90Parser.Namelist_nameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#namelist_name.
    def exitNamelist_name(self, ctx:Fortran90Parser.Namelist_nameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#format.
    def enterFormat(self, ctx:Fortran90Parser.FormatContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#format.
    def exitFormat(self, ctx:Fortran90Parser.FormatContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#input_item_list.
    def enterInput_item_list(self, ctx:Fortran90Parser.Input_item_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#input_item_list.
    def exitInput_item_list(self, ctx:Fortran90Parser.Input_item_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#input_item.
    def enterInput_item(self, ctx:Fortran90Parser.Input_itemContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#input_item.
    def exitInput_item(self, ctx:Fortran90Parser.Input_itemContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#io_implied_do.
    def enterIo_implied_do(self, ctx:Fortran90Parser.Io_implied_doContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#io_implied_do.
    def exitIo_implied_do(self, ctx:Fortran90Parser.Io_implied_doContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#LogicalAndExprF90.
    def enterLogicalAndExprF90(self, ctx:Fortran90Parser.LogicalAndExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#LogicalAndExprF90.
    def exitLogicalAndExprF90(self, ctx:Fortran90Parser.LogicalAndExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#NotEquivalenceExprF90.
    def enterNotEquivalenceExprF90(self, ctx:Fortran90Parser.NotEquivalenceExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#NotEquivalenceExprF90.
    def exitNotEquivalenceExprF90(self, ctx:Fortran90Parser.NotEquivalenceExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#LogicalOrExprF90.
    def enterLogicalOrExprF90(self, ctx:Fortran90Parser.LogicalOrExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#LogicalOrExprF90.
    def exitLogicalOrExprF90(self, ctx:Fortran90Parser.LogicalOrExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#GreaterEqualExprF90.
    def enterGreaterEqualExprF90(self, ctx:Fortran90Parser.GreaterEqualExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#GreaterEqualExprF90.
    def exitGreaterEqualExprF90(self, ctx:Fortran90Parser.GreaterEqualExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#PowerExprF90.
    def enterPowerExprF90(self, ctx:Fortran90Parser.PowerExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#PowerExprF90.
    def exitPowerExprF90(self, ctx:Fortran90Parser.PowerExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#LessEqualExprF90.
    def enterLessEqualExprF90(self, ctx:Fortran90Parser.LessEqualExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#LessEqualExprF90.
    def exitLessEqualExprF90(self, ctx:Fortran90Parser.LessEqualExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#EquivalenceExprF90.
    def enterEquivalenceExprF90(self, ctx:Fortran90Parser.EquivalenceExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#EquivalenceExprF90.
    def exitEquivalenceExprF90(self, ctx:Fortran90Parser.EquivalenceExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#NotEqualExprF90.
    def enterNotEqualExprF90(self, ctx:Fortran90Parser.NotEqualExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#NotEqualExprF90.
    def exitNotEqualExprF90(self, ctx:Fortran90Parser.NotEqualExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#EqualExprF90.
    def enterEqualExprF90(self, ctx:Fortran90Parser.EqualExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#EqualExprF90.
    def exitEqualExprF90(self, ctx:Fortran90Parser.EqualExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#ConcatExprF90.
    def enterConcatExprF90(self, ctx:Fortran90Parser.ConcatExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#ConcatExprF90.
    def exitConcatExprF90(self, ctx:Fortran90Parser.ConcatExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#AddSubExprF90.
    def enterAddSubExprF90(self, ctx:Fortran90Parser.AddSubExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#AddSubExprF90.
    def exitAddSubExprF90(self, ctx:Fortran90Parser.AddSubExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#LogicalNotExprF90.
    def enterLogicalNotExprF90(self, ctx:Fortran90Parser.LogicalNotExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#LogicalNotExprF90.
    def exitLogicalNotExprF90(self, ctx:Fortran90Parser.LogicalNotExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#PrimaryExprF90.
    def enterPrimaryExprF90(self, ctx:Fortran90Parser.PrimaryExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#PrimaryExprF90.
    def exitPrimaryExprF90(self, ctx:Fortran90Parser.PrimaryExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#UnaryExprF90.
    def enterUnaryExprF90(self, ctx:Fortran90Parser.UnaryExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#UnaryExprF90.
    def exitUnaryExprF90(self, ctx:Fortran90Parser.UnaryExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#GreaterExprF90.
    def enterGreaterExprF90(self, ctx:Fortran90Parser.GreaterExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#GreaterExprF90.
    def exitGreaterExprF90(self, ctx:Fortran90Parser.GreaterExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#LessExprF90.
    def enterLessExprF90(self, ctx:Fortran90Parser.LessExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#LessExprF90.
    def exitLessExprF90(self, ctx:Fortran90Parser.LessExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#MultDivExprF90.
    def enterMultDivExprF90(self, ctx:Fortran90Parser.MultDivExprF90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#MultDivExprF90.
    def exitMultDivExprF90(self, ctx:Fortran90Parser.MultDivExprF90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#primary_f90.
    def enterPrimary_f90(self, ctx:Fortran90Parser.Primary_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#primary_f90.
    def exitPrimary_f90(self, ctx:Fortran90Parser.Primary_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#intrinsic_function_f90.
    def enterIntrinsic_function_f90(self, ctx:Fortran90Parser.Intrinsic_function_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#intrinsic_function_f90.
    def exitIntrinsic_function_f90(self, ctx:Fortran90Parser.Intrinsic_function_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#variable_f90.
    def enterVariable_f90(self, ctx:Fortran90Parser.Variable_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#variable_f90.
    def exitVariable_f90(self, ctx:Fortran90Parser.Variable_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#section_subscript_list.
    def enterSection_subscript_list(self, ctx:Fortran90Parser.Section_subscript_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#section_subscript_list.
    def exitSection_subscript_list(self, ctx:Fortran90Parser.Section_subscript_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#section_subscript.
    def enterSection_subscript(self, ctx:Fortran90Parser.Section_subscriptContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#section_subscript.
    def exitSection_subscript(self, ctx:Fortran90Parser.Section_subscriptContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#subscript_triplet.
    def enterSubscript_triplet(self, ctx:Fortran90Parser.Subscript_tripletContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#subscript_triplet.
    def exitSubscript_triplet(self, ctx:Fortran90Parser.Subscript_tripletContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#array_constructor_f90.
    def enterArray_constructor_f90(self, ctx:Fortran90Parser.Array_constructor_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#array_constructor_f90.
    def exitArray_constructor_f90(self, ctx:Fortran90Parser.Array_constructor_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#ac_spec.
    def enterAc_spec(self, ctx:Fortran90Parser.Ac_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#ac_spec.
    def exitAc_spec(self, ctx:Fortran90Parser.Ac_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#ac_value_list.
    def enterAc_value_list(self, ctx:Fortran90Parser.Ac_value_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#ac_value_list.
    def exitAc_value_list(self, ctx:Fortran90Parser.Ac_value_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#ac_value.
    def enterAc_value(self, ctx:Fortran90Parser.Ac_valueContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#ac_value.
    def exitAc_value(self, ctx:Fortran90Parser.Ac_valueContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#ac_implied_do.
    def enterAc_implied_do(self, ctx:Fortran90Parser.Ac_implied_doContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#ac_implied_do.
    def exitAc_implied_do(self, ctx:Fortran90Parser.Ac_implied_doContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#literal_f90.
    def enterLiteral_f90(self, ctx:Fortran90Parser.Literal_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#literal_f90.
    def exitLiteral_f90(self, ctx:Fortran90Parser.Literal_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#logical_literal_f90.
    def enterLogical_literal_f90(self, ctx:Fortran90Parser.Logical_literal_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#logical_literal_f90.
    def exitLogical_literal_f90(self, ctx:Fortran90Parser.Logical_literal_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#boz_literal_constant.
    def enterBoz_literal_constant(self, ctx:Fortran90Parser.Boz_literal_constantContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#boz_literal_constant.
    def exitBoz_literal_constant(self, ctx:Fortran90Parser.Boz_literal_constantContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#assignment_stmt_f90.
    def enterAssignment_stmt_f90(self, ctx:Fortran90Parser.Assignment_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#assignment_stmt_f90.
    def exitAssignment_stmt_f90(self, ctx:Fortran90Parser.Assignment_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#pointer_assignment_stmt.
    def enterPointer_assignment_stmt(self, ctx:Fortran90Parser.Pointer_assignment_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pointer_assignment_stmt.
    def exitPointer_assignment_stmt(self, ctx:Fortran90Parser.Pointer_assignment_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#function_reference_f90.
    def enterFunction_reference_f90(self, ctx:Fortran90Parser.Function_reference_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#function_reference_f90.
    def exitFunction_reference_f90(self, ctx:Fortran90Parser.Function_reference_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocate_stmt.
    def enterAllocate_stmt(self, ctx:Fortran90Parser.Allocate_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocate_stmt.
    def exitAllocate_stmt(self, ctx:Fortran90Parser.Allocate_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocation_list.
    def enterAllocation_list(self, ctx:Fortran90Parser.Allocation_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocation_list.
    def exitAllocation_list(self, ctx:Fortran90Parser.Allocation_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocation.
    def enterAllocation(self, ctx:Fortran90Parser.AllocationContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocation.
    def exitAllocation(self, ctx:Fortran90Parser.AllocationContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocate_object.
    def enterAllocate_object(self, ctx:Fortran90Parser.Allocate_objectContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocate_object.
    def exitAllocate_object(self, ctx:Fortran90Parser.Allocate_objectContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocate_shape_spec_list.
    def enterAllocate_shape_spec_list(self, ctx:Fortran90Parser.Allocate_shape_spec_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocate_shape_spec_list.
    def exitAllocate_shape_spec_list(self, ctx:Fortran90Parser.Allocate_shape_spec_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocate_shape_spec.
    def enterAllocate_shape_spec(self, ctx:Fortran90Parser.Allocate_shape_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocate_shape_spec.
    def exitAllocate_shape_spec(self, ctx:Fortran90Parser.Allocate_shape_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#deallocate_stmt.
    def enterDeallocate_stmt(self, ctx:Fortran90Parser.Deallocate_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#deallocate_stmt.
    def exitDeallocate_stmt(self, ctx:Fortran90Parser.Deallocate_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#deallocate_list.
    def enterDeallocate_list(self, ctx:Fortran90Parser.Deallocate_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#deallocate_list.
    def exitDeallocate_list(self, ctx:Fortran90Parser.Deallocate_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#nullify_stmt.
    def enterNullify_stmt(self, ctx:Fortran90Parser.Nullify_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#nullify_stmt.
    def exitNullify_stmt(self, ctx:Fortran90Parser.Nullify_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#pointer_object_list.
    def enterPointer_object_list(self, ctx:Fortran90Parser.Pointer_object_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pointer_object_list.
    def exitPointer_object_list(self, ctx:Fortran90Parser.Pointer_object_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#pointer_object.
    def enterPointer_object(self, ctx:Fortran90Parser.Pointer_objectContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pointer_object.
    def exitPointer_object(self, ctx:Fortran90Parser.Pointer_objectContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#stat_variable.
    def enterStat_variable(self, ctx:Fortran90Parser.Stat_variableContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#stat_variable.
    def exitStat_variable(self, ctx:Fortran90Parser.Stat_variableContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#function_stmt.
    def enterFunction_stmt(self, ctx:Fortran90Parser.Function_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#function_stmt.
    def exitFunction_stmt(self, ctx:Fortran90Parser.Function_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#subroutine_stmt.
    def enterSubroutine_stmt(self, ctx:Fortran90Parser.Subroutine_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#subroutine_stmt.
    def exitSubroutine_stmt(self, ctx:Fortran90Parser.Subroutine_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#prefix.
    def enterPrefix(self, ctx:Fortran90Parser.PrefixContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#prefix.
    def exitPrefix(self, ctx:Fortran90Parser.PrefixContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#prefix_spec.
    def enterPrefix_spec(self, ctx:Fortran90Parser.Prefix_specContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#prefix_spec.
    def exitPrefix_spec(self, ctx:Fortran90Parser.Prefix_specContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#suffix.
    def enterSuffix(self, ctx:Fortran90Parser.SuffixContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#suffix.
    def exitSuffix(self, ctx:Fortran90Parser.SuffixContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#dummy_arg_name_list.
    def enterDummy_arg_name_list(self, ctx:Fortran90Parser.Dummy_arg_name_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#dummy_arg_name_list.
    def exitDummy_arg_name_list(self, ctx:Fortran90Parser.Dummy_arg_name_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#call_stmt_f90.
    def enterCall_stmt_f90(self, ctx:Fortran90Parser.Call_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#call_stmt_f90.
    def exitCall_stmt_f90(self, ctx:Fortran90Parser.Call_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#procedure_designator.
    def enterProcedure_designator(self, ctx:Fortran90Parser.Procedure_designatorContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#procedure_designator.
    def exitProcedure_designator(self, ctx:Fortran90Parser.Procedure_designatorContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#procedure_stmt.
    def enterProcedure_stmt(self, ctx:Fortran90Parser.Procedure_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#procedure_stmt.
    def exitProcedure_stmt(self, ctx:Fortran90Parser.Procedure_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#external_subprogram.
    def enterExternal_subprogram(self, ctx:Fortran90Parser.External_subprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#external_subprogram.
    def exitExternal_subprogram(self, ctx:Fortran90Parser.External_subprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_function_stmt.
    def enterEnd_function_stmt(self, ctx:Fortran90Parser.End_function_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_function_stmt.
    def exitEnd_function_stmt(self, ctx:Fortran90Parser.End_function_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#end_subroutine_stmt.
    def enterEnd_subroutine_stmt(self, ctx:Fortran90Parser.End_subroutine_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#end_subroutine_stmt.
    def exitEnd_subroutine_stmt(self, ctx:Fortran90Parser.End_subroutine_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#internal_subprogram_part.
    def enterInternal_subprogram_part(self, ctx:Fortran90Parser.Internal_subprogram_partContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#internal_subprogram_part.
    def exitInternal_subprogram_part(self, ctx:Fortran90Parser.Internal_subprogram_partContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#internal_subprogram.
    def enterInternal_subprogram(self, ctx:Fortran90Parser.Internal_subprogramContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#internal_subprogram.
    def exitInternal_subprogram(self, ctx:Fortran90Parser.Internal_subprogramContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#common_block_object_list.
    def enterCommon_block_object_list(self, ctx:Fortran90Parser.Common_block_object_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#common_block_object_list.
    def exitCommon_block_object_list(self, ctx:Fortran90Parser.Common_block_object_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#common_block_object.
    def enterCommon_block_object(self, ctx:Fortran90Parser.Common_block_objectContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#common_block_object.
    def exitCommon_block_object(self, ctx:Fortran90Parser.Common_block_objectContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#variable_name.
    def enterVariable_name(self, ctx:Fortran90Parser.Variable_nameContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#variable_name.
    def exitVariable_name(self, ctx:Fortran90Parser.Variable_nameContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#equivalence_set_list.
    def enterEquivalence_set_list(self, ctx:Fortran90Parser.Equivalence_set_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#equivalence_set_list.
    def exitEquivalence_set_list(self, ctx:Fortran90Parser.Equivalence_set_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#equivalence_object_list.
    def enterEquivalence_object_list(self, ctx:Fortran90Parser.Equivalence_object_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#equivalence_object_list.
    def exitEquivalence_object_list(self, ctx:Fortran90Parser.Equivalence_object_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#equivalence_object.
    def enterEquivalence_object(self, ctx:Fortran90Parser.Equivalence_objectContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#equivalence_object.
    def exitEquivalence_object(self, ctx:Fortran90Parser.Equivalence_objectContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#array_declarator_list.
    def enterArray_declarator_list(self, ctx:Fortran90Parser.Array_declarator_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#array_declarator_list.
    def exitArray_declarator_list(self, ctx:Fortran90Parser.Array_declarator_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#saved_entity_list.
    def enterSaved_entity_list(self, ctx:Fortran90Parser.Saved_entity_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#saved_entity_list.
    def exitSaved_entity_list(self, ctx:Fortran90Parser.Saved_entity_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#saved_entity.
    def enterSaved_entity(self, ctx:Fortran90Parser.Saved_entityContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#saved_entity.
    def exitSaved_entity(self, ctx:Fortran90Parser.Saved_entityContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#intrinsic_name_list.
    def enterIntrinsic_name_list(self, ctx:Fortran90Parser.Intrinsic_name_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#intrinsic_name_list.
    def exitIntrinsic_name_list(self, ctx:Fortran90Parser.Intrinsic_name_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocatable_stmt.
    def enterAllocatable_stmt(self, ctx:Fortran90Parser.Allocatable_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocatable_stmt.
    def exitAllocatable_stmt(self, ctx:Fortran90Parser.Allocatable_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#pointer_stmt.
    def enterPointer_stmt(self, ctx:Fortran90Parser.Pointer_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pointer_stmt.
    def exitPointer_stmt(self, ctx:Fortran90Parser.Pointer_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#target_stmt.
    def enterTarget_stmt(self, ctx:Fortran90Parser.Target_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#target_stmt.
    def exitTarget_stmt(self, ctx:Fortran90Parser.Target_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#optional_stmt.
    def enterOptional_stmt(self, ctx:Fortran90Parser.Optional_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#optional_stmt.
    def exitOptional_stmt(self, ctx:Fortran90Parser.Optional_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#intent_stmt.
    def enterIntent_stmt(self, ctx:Fortran90Parser.Intent_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#intent_stmt.
    def exitIntent_stmt(self, ctx:Fortran90Parser.Intent_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#public_stmt.
    def enterPublic_stmt(self, ctx:Fortran90Parser.Public_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#public_stmt.
    def exitPublic_stmt(self, ctx:Fortran90Parser.Public_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#private_stmt.
    def enterPrivate_stmt(self, ctx:Fortran90Parser.Private_stmtContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#private_stmt.
    def exitPrivate_stmt(self, ctx:Fortran90Parser.Private_stmtContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#access_id_list.
    def enterAccess_id_list(self, ctx:Fortran90Parser.Access_id_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#access_id_list.
    def exitAccess_id_list(self, ctx:Fortran90Parser.Access_id_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#access_id.
    def enterAccess_id(self, ctx:Fortran90Parser.Access_idContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#access_id.
    def exitAccess_id(self, ctx:Fortran90Parser.Access_idContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocatable_decl_list.
    def enterAllocatable_decl_list(self, ctx:Fortran90Parser.Allocatable_decl_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocatable_decl_list.
    def exitAllocatable_decl_list(self, ctx:Fortran90Parser.Allocatable_decl_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#allocatable_decl.
    def enterAllocatable_decl(self, ctx:Fortran90Parser.Allocatable_declContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#allocatable_decl.
    def exitAllocatable_decl(self, ctx:Fortran90Parser.Allocatable_declContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#pointer_decl_list.
    def enterPointer_decl_list(self, ctx:Fortran90Parser.Pointer_decl_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pointer_decl_list.
    def exitPointer_decl_list(self, ctx:Fortran90Parser.Pointer_decl_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#pointer_decl.
    def enterPointer_decl(self, ctx:Fortran90Parser.Pointer_declContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#pointer_decl.
    def exitPointer_decl(self, ctx:Fortran90Parser.Pointer_declContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#target_decl_list.
    def enterTarget_decl_list(self, ctx:Fortran90Parser.Target_decl_listContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#target_decl_list.
    def exitTarget_decl_list(self, ctx:Fortran90Parser.Target_decl_listContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#target_decl.
    def enterTarget_decl(self, ctx:Fortran90Parser.Target_declContext):
        pass

    # Exit a parse tree produced by Fortran90Parser#target_decl.
    def exitTarget_decl(self, ctx:Fortran90Parser.Target_declContext):
        pass


    # Enter a parse tree produced by Fortran90Parser#implicit_stmt_f90.
    def enterImplicit_stmt_f90(self, ctx:Fortran90Parser.Implicit_stmt_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#implicit_stmt_f90.
    def exitImplicit_stmt_f90(self, ctx:Fortran90Parser.Implicit_stmt_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#implicit_spec_list_f90.
    def enterImplicit_spec_list_f90(self, ctx:Fortran90Parser.Implicit_spec_list_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#implicit_spec_list_f90.
    def exitImplicit_spec_list_f90(self, ctx:Fortran90Parser.Implicit_spec_list_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#implicit_spec_f90.
    def enterImplicit_spec_f90(self, ctx:Fortran90Parser.Implicit_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#implicit_spec_f90.
    def exitImplicit_spec_f90(self, ctx:Fortran90Parser.Implicit_spec_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#letter_spec_list_f90.
    def enterLetter_spec_list_f90(self, ctx:Fortran90Parser.Letter_spec_list_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#letter_spec_list_f90.
    def exitLetter_spec_list_f90(self, ctx:Fortran90Parser.Letter_spec_list_f90Context):
        pass


    # Enter a parse tree produced by Fortran90Parser#letter_spec_f90.
    def enterLetter_spec_f90(self, ctx:Fortran90Parser.Letter_spec_f90Context):
        pass

    # Exit a parse tree produced by Fortran90Parser#letter_spec_f90.
    def exitLetter_spec_f90(self, ctx:Fortran90Parser.Letter_spec_f90Context):
        pass



del Fortran90Parser