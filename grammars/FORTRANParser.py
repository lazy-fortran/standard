# Generated from ../../grammars/FORTRANParser.g4 by ANTLR 4.13.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,41,218,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,
        2,14,7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,
        7,20,2,21,7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,
        1,0,1,0,1,0,1,1,5,1,59,8,1,10,1,12,1,62,9,1,1,2,3,2,65,8,2,1,2,1,
        2,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,78,8,3,1,4,1,4,1,4,1,4,
        1,5,1,5,1,5,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,7,1,7,1,7,
        1,7,1,7,1,7,1,7,1,7,1,7,3,7,106,8,7,1,8,1,8,1,8,1,9,1,9,1,9,1,10,
        1,10,1,11,1,11,1,11,1,11,1,11,1,11,1,11,5,11,123,8,11,10,11,12,11,
        126,9,11,1,12,1,12,1,13,1,13,1,13,1,13,1,13,1,13,1,13,5,13,137,8,
        13,10,13,12,13,140,9,13,1,14,1,14,1,15,1,15,1,15,1,15,1,15,1,15,
        1,15,5,15,151,8,15,10,15,12,15,154,9,15,1,16,1,16,1,17,1,17,1,17,
        1,17,3,17,162,8,17,1,18,1,18,1,19,1,19,1,19,1,19,1,19,3,19,171,8,
        19,1,20,1,20,1,20,1,20,1,20,1,20,3,20,179,8,20,1,21,1,21,1,22,1,
        22,1,22,1,22,1,22,3,22,188,8,22,1,23,1,23,1,24,1,24,1,24,5,24,195,
        8,24,10,24,12,24,198,9,24,3,24,200,8,24,1,25,1,25,1,25,5,25,205,
        8,25,10,25,12,25,208,9,25,1,26,1,26,1,26,5,26,213,8,26,10,26,12,
        26,216,9,26,1,26,0,3,22,26,30,27,0,2,4,6,8,10,12,14,16,18,20,22,
        24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,0,4,1,0,27,32,1,0,22,
        23,1,0,24,25,1,0,37,38,213,0,54,1,0,0,0,2,60,1,0,0,0,4,64,1,0,0,
        0,6,77,1,0,0,0,8,79,1,0,0,0,10,83,1,0,0,0,12,86,1,0,0,0,14,96,1,
        0,0,0,16,107,1,0,0,0,18,110,1,0,0,0,20,113,1,0,0,0,22,115,1,0,0,
        0,24,127,1,0,0,0,26,129,1,0,0,0,28,141,1,0,0,0,30,143,1,0,0,0,32,
        155,1,0,0,0,34,161,1,0,0,0,36,163,1,0,0,0,38,170,1,0,0,0,40,178,
        1,0,0,0,42,180,1,0,0,0,44,182,1,0,0,0,46,189,1,0,0,0,48,199,1,0,
        0,0,50,201,1,0,0,0,52,209,1,0,0,0,54,55,3,2,1,0,55,56,5,0,0,1,56,
        1,1,0,0,0,57,59,3,4,2,0,58,57,1,0,0,0,59,62,1,0,0,0,60,58,1,0,0,
        0,60,61,1,0,0,0,61,3,1,0,0,0,62,60,1,0,0,0,63,65,3,46,23,0,64,63,
        1,0,0,0,64,65,1,0,0,0,65,66,1,0,0,0,66,67,3,6,3,0,67,5,1,0,0,0,68,
        78,3,8,4,0,69,78,3,10,5,0,70,78,3,12,6,0,71,78,3,14,7,0,72,78,3,
        16,8,0,73,78,3,18,9,0,74,78,5,5,0,0,75,78,5,6,0,0,76,78,5,4,0,0,
        77,68,1,0,0,0,77,69,1,0,0,0,77,70,1,0,0,0,77,71,1,0,0,0,77,72,1,
        0,0,0,77,73,1,0,0,0,77,74,1,0,0,0,77,75,1,0,0,0,77,76,1,0,0,0,78,
        7,1,0,0,0,79,80,3,44,22,0,80,81,5,21,0,0,81,82,3,20,10,0,82,9,1,
        0,0,0,83,84,5,2,0,0,84,85,3,46,23,0,85,11,1,0,0,0,86,87,5,1,0,0,
        87,88,5,33,0,0,88,89,3,20,10,0,89,90,5,34,0,0,90,91,3,46,23,0,91,
        92,5,35,0,0,92,93,3,46,23,0,93,94,5,35,0,0,94,95,3,46,23,0,95,13,
        1,0,0,0,96,97,5,3,0,0,97,98,3,46,23,0,98,99,3,44,22,0,99,100,5,21,
        0,0,100,101,3,20,10,0,101,102,5,35,0,0,102,105,3,20,10,0,103,104,
        5,35,0,0,104,106,3,20,10,0,105,103,1,0,0,0,105,106,1,0,0,0,106,15,
        1,0,0,0,107,108,5,7,0,0,108,109,3,50,25,0,109,17,1,0,0,0,110,111,
        5,8,0,0,111,112,3,52,26,0,112,19,1,0,0,0,113,114,3,22,11,0,114,21,
        1,0,0,0,115,116,6,11,-1,0,116,117,3,26,13,0,117,124,1,0,0,0,118,
        119,10,2,0,0,119,120,3,24,12,0,120,121,3,26,13,0,121,123,1,0,0,0,
        122,118,1,0,0,0,123,126,1,0,0,0,124,122,1,0,0,0,124,125,1,0,0,0,
        125,23,1,0,0,0,126,124,1,0,0,0,127,128,7,0,0,0,128,25,1,0,0,0,129,
        130,6,13,-1,0,130,131,3,30,15,0,131,138,1,0,0,0,132,133,10,2,0,0,
        133,134,3,28,14,0,134,135,3,30,15,0,135,137,1,0,0,0,136,132,1,0,
        0,0,137,140,1,0,0,0,138,136,1,0,0,0,138,139,1,0,0,0,139,27,1,0,0,
        0,140,138,1,0,0,0,141,142,7,1,0,0,142,29,1,0,0,0,143,144,6,15,-1,
        0,144,145,3,34,17,0,145,152,1,0,0,0,146,147,10,2,0,0,147,148,3,32,
        16,0,148,149,3,34,17,0,149,151,1,0,0,0,150,146,1,0,0,0,151,154,1,
        0,0,0,152,150,1,0,0,0,152,153,1,0,0,0,153,31,1,0,0,0,154,152,1,0,
        0,0,155,156,7,2,0,0,156,33,1,0,0,0,157,158,3,36,18,0,158,159,3,34,
        17,0,159,162,1,0,0,0,160,162,3,38,19,0,161,157,1,0,0,0,161,160,1,
        0,0,0,162,35,1,0,0,0,163,164,7,1,0,0,164,37,1,0,0,0,165,166,3,40,
        20,0,166,167,5,26,0,0,167,168,3,38,19,0,168,171,1,0,0,0,169,171,
        3,40,20,0,170,165,1,0,0,0,170,169,1,0,0,0,171,39,1,0,0,0,172,179,
        3,42,21,0,173,179,3,44,22,0,174,175,5,33,0,0,175,176,3,20,10,0,176,
        177,5,34,0,0,177,179,1,0,0,0,178,172,1,0,0,0,178,173,1,0,0,0,178,
        174,1,0,0,0,179,41,1,0,0,0,180,181,7,3,0,0,181,43,1,0,0,0,182,187,
        5,39,0,0,183,184,5,33,0,0,184,185,3,48,24,0,185,186,5,34,0,0,186,
        188,1,0,0,0,187,183,1,0,0,0,187,188,1,0,0,0,188,45,1,0,0,0,189,190,
        5,37,0,0,190,47,1,0,0,0,191,196,3,20,10,0,192,193,5,35,0,0,193,195,
        3,20,10,0,194,192,1,0,0,0,195,198,1,0,0,0,196,194,1,0,0,0,196,197,
        1,0,0,0,197,200,1,0,0,0,198,196,1,0,0,0,199,191,1,0,0,0,199,200,
        1,0,0,0,200,49,1,0,0,0,201,206,3,44,22,0,202,203,5,35,0,0,203,205,
        3,44,22,0,204,202,1,0,0,0,205,208,1,0,0,0,206,204,1,0,0,0,206,207,
        1,0,0,0,207,51,1,0,0,0,208,206,1,0,0,0,209,214,3,20,10,0,210,211,
        5,35,0,0,211,213,3,20,10,0,212,210,1,0,0,0,213,216,1,0,0,0,214,212,
        1,0,0,0,214,215,1,0,0,0,215,53,1,0,0,0,216,214,1,0,0,0,15,60,64,
        77,105,124,138,152,161,170,178,187,196,199,206,214
    ]

class FORTRANParser ( Parser ):

    grammarFileName = "FORTRANParser.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "'='", "'+'", "'-'", "'*'", "'/'", "'**'", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "'('", "')'", "','", "':'" ]

    symbolicNames = [ "<INVALID>", "IF", "GOTO", "DO", "END", "CONTINUE", 
                      "STOP", "READ", "WRITE", "PRINT", "PUNCH", "DIMENSION", 
                      "EQUIVALENCE", "FORMAT", "COMMON", "PAUSE", "FREQUENCY", 
                      "ASSIGN", "INTEGER", "REAL", "IMPLICIT", "EQUALS", 
                      "PLUS", "MINUS", "MULTIPLY", "DIVIDE", "POWER", "EQ", 
                      "NE", "LT", "LE", "GT", "GE", "LPAREN", "RPAREN", 
                      "COMMA", "COLON", "INTEGER_LITERAL", "REAL_LITERAL", 
                      "IDENTIFIER", "WS", "COMMENT" ]

    RULE_program_unit_core = 0
    RULE_statement_list = 1
    RULE_statement = 2
    RULE_statement_body = 3
    RULE_assignment_stmt = 4
    RULE_goto_stmt = 5
    RULE_if_stmt_arithmetic = 6
    RULE_do_stmt_basic = 7
    RULE_read_stmt_basic = 8
    RULE_write_stmt_basic = 9
    RULE_expr = 10
    RULE_relational_expr = 11
    RULE_relational_op = 12
    RULE_additive_expr = 13
    RULE_additive_op = 14
    RULE_multiplicative_expr = 15
    RULE_multiplicative_op = 16
    RULE_unary_expr = 17
    RULE_unary_op = 18
    RULE_power_expr = 19
    RULE_primary = 20
    RULE_literal = 21
    RULE_variable = 22
    RULE_label = 23
    RULE_expr_list = 24
    RULE_input_list = 25
    RULE_output_list = 26

    ruleNames =  [ "program_unit_core", "statement_list", "statement", "statement_body", 
                   "assignment_stmt", "goto_stmt", "if_stmt_arithmetic", 
                   "do_stmt_basic", "read_stmt_basic", "write_stmt_basic", 
                   "expr", "relational_expr", "relational_op", "additive_expr", 
                   "additive_op", "multiplicative_expr", "multiplicative_op", 
                   "unary_expr", "unary_op", "power_expr", "primary", "literal", 
                   "variable", "label", "expr_list", "input_list", "output_list" ]

    EOF = Token.EOF
    IF=1
    GOTO=2
    DO=3
    END=4
    CONTINUE=5
    STOP=6
    READ=7
    WRITE=8
    PRINT=9
    PUNCH=10
    DIMENSION=11
    EQUIVALENCE=12
    FORMAT=13
    COMMON=14
    PAUSE=15
    FREQUENCY=16
    ASSIGN=17
    INTEGER=18
    REAL=19
    IMPLICIT=20
    EQUALS=21
    PLUS=22
    MINUS=23
    MULTIPLY=24
    DIVIDE=25
    POWER=26
    EQ=27
    NE=28
    LT=29
    LE=30
    GT=31
    GE=32
    LPAREN=33
    RPAREN=34
    COMMA=35
    COLON=36
    INTEGER_LITERAL=37
    REAL_LITERAL=38
    IDENTIFIER=39
    WS=40
    COMMENT=41

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class Program_unit_coreContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def statement_list(self):
            return self.getTypedRuleContext(FORTRANParser.Statement_listContext,0)


        def EOF(self):
            return self.getToken(FORTRANParser.EOF, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_program_unit_core

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterProgram_unit_core" ):
                listener.enterProgram_unit_core(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitProgram_unit_core" ):
                listener.exitProgram_unit_core(self)




    def program_unit_core(self):

        localctx = FORTRANParser.Program_unit_coreContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_program_unit_core)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 54
            self.statement_list()
            self.state = 55
            self.match(FORTRANParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Statement_listContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def statement(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(FORTRANParser.StatementContext)
            else:
                return self.getTypedRuleContext(FORTRANParser.StatementContext,i)


        def getRuleIndex(self):
            return FORTRANParser.RULE_statement_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterStatement_list" ):
                listener.enterStatement_list(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitStatement_list" ):
                listener.exitStatement_list(self)




    def statement_list(self):

        localctx = FORTRANParser.Statement_listContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_statement_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 60
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 687194767870) != 0):
                self.state = 57
                self.statement()
                self.state = 62
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class StatementContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def statement_body(self):
            return self.getTypedRuleContext(FORTRANParser.Statement_bodyContext,0)


        def label(self):
            return self.getTypedRuleContext(FORTRANParser.LabelContext,0)


        def getRuleIndex(self):
            return FORTRANParser.RULE_statement

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterStatement" ):
                listener.enterStatement(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitStatement" ):
                listener.exitStatement(self)




    def statement(self):

        localctx = FORTRANParser.StatementContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_statement)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 64
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==37:
                self.state = 63
                self.label()


            self.state = 66
            self.statement_body()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Statement_bodyContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def assignment_stmt(self):
            return self.getTypedRuleContext(FORTRANParser.Assignment_stmtContext,0)


        def goto_stmt(self):
            return self.getTypedRuleContext(FORTRANParser.Goto_stmtContext,0)


        def if_stmt_arithmetic(self):
            return self.getTypedRuleContext(FORTRANParser.If_stmt_arithmeticContext,0)


        def do_stmt_basic(self):
            return self.getTypedRuleContext(FORTRANParser.Do_stmt_basicContext,0)


        def read_stmt_basic(self):
            return self.getTypedRuleContext(FORTRANParser.Read_stmt_basicContext,0)


        def write_stmt_basic(self):
            return self.getTypedRuleContext(FORTRANParser.Write_stmt_basicContext,0)


        def CONTINUE(self):
            return self.getToken(FORTRANParser.CONTINUE, 0)

        def STOP(self):
            return self.getToken(FORTRANParser.STOP, 0)

        def END(self):
            return self.getToken(FORTRANParser.END, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_statement_body

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterStatement_body" ):
                listener.enterStatement_body(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitStatement_body" ):
                listener.exitStatement_body(self)




    def statement_body(self):

        localctx = FORTRANParser.Statement_bodyContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_statement_body)
        try:
            self.state = 77
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [39]:
                self.enterOuterAlt(localctx, 1)
                self.state = 68
                self.assignment_stmt()
                pass
            elif token in [2]:
                self.enterOuterAlt(localctx, 2)
                self.state = 69
                self.goto_stmt()
                pass
            elif token in [1]:
                self.enterOuterAlt(localctx, 3)
                self.state = 70
                self.if_stmt_arithmetic()
                pass
            elif token in [3]:
                self.enterOuterAlt(localctx, 4)
                self.state = 71
                self.do_stmt_basic()
                pass
            elif token in [7]:
                self.enterOuterAlt(localctx, 5)
                self.state = 72
                self.read_stmt_basic()
                pass
            elif token in [8]:
                self.enterOuterAlt(localctx, 6)
                self.state = 73
                self.write_stmt_basic()
                pass
            elif token in [5]:
                self.enterOuterAlt(localctx, 7)
                self.state = 74
                self.match(FORTRANParser.CONTINUE)
                pass
            elif token in [6]:
                self.enterOuterAlt(localctx, 8)
                self.state = 75
                self.match(FORTRANParser.STOP)
                pass
            elif token in [4]:
                self.enterOuterAlt(localctx, 9)
                self.state = 76
                self.match(FORTRANParser.END)
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Assignment_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def variable(self):
            return self.getTypedRuleContext(FORTRANParser.VariableContext,0)


        def EQUALS(self):
            return self.getToken(FORTRANParser.EQUALS, 0)

        def expr(self):
            return self.getTypedRuleContext(FORTRANParser.ExprContext,0)


        def getRuleIndex(self):
            return FORTRANParser.RULE_assignment_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAssignment_stmt" ):
                listener.enterAssignment_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAssignment_stmt" ):
                listener.exitAssignment_stmt(self)




    def assignment_stmt(self):

        localctx = FORTRANParser.Assignment_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_assignment_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 79
            self.variable()
            self.state = 80
            self.match(FORTRANParser.EQUALS)
            self.state = 81
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Goto_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def GOTO(self):
            return self.getToken(FORTRANParser.GOTO, 0)

        def label(self):
            return self.getTypedRuleContext(FORTRANParser.LabelContext,0)


        def getRuleIndex(self):
            return FORTRANParser.RULE_goto_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterGoto_stmt" ):
                listener.enterGoto_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitGoto_stmt" ):
                listener.exitGoto_stmt(self)




    def goto_stmt(self):

        localctx = FORTRANParser.Goto_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_goto_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 83
            self.match(FORTRANParser.GOTO)
            self.state = 84
            self.label()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class If_stmt_arithmeticContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IF(self):
            return self.getToken(FORTRANParser.IF, 0)

        def LPAREN(self):
            return self.getToken(FORTRANParser.LPAREN, 0)

        def expr(self):
            return self.getTypedRuleContext(FORTRANParser.ExprContext,0)


        def RPAREN(self):
            return self.getToken(FORTRANParser.RPAREN, 0)

        def label(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(FORTRANParser.LabelContext)
            else:
                return self.getTypedRuleContext(FORTRANParser.LabelContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(FORTRANParser.COMMA)
            else:
                return self.getToken(FORTRANParser.COMMA, i)

        def getRuleIndex(self):
            return FORTRANParser.RULE_if_stmt_arithmetic

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterIf_stmt_arithmetic" ):
                listener.enterIf_stmt_arithmetic(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitIf_stmt_arithmetic" ):
                listener.exitIf_stmt_arithmetic(self)




    def if_stmt_arithmetic(self):

        localctx = FORTRANParser.If_stmt_arithmeticContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_if_stmt_arithmetic)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 86
            self.match(FORTRANParser.IF)
            self.state = 87
            self.match(FORTRANParser.LPAREN)
            self.state = 88
            self.expr()
            self.state = 89
            self.match(FORTRANParser.RPAREN)
            self.state = 90
            self.label()
            self.state = 91
            self.match(FORTRANParser.COMMA)
            self.state = 92
            self.label()
            self.state = 93
            self.match(FORTRANParser.COMMA)
            self.state = 94
            self.label()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Do_stmt_basicContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def DO(self):
            return self.getToken(FORTRANParser.DO, 0)

        def label(self):
            return self.getTypedRuleContext(FORTRANParser.LabelContext,0)


        def variable(self):
            return self.getTypedRuleContext(FORTRANParser.VariableContext,0)


        def EQUALS(self):
            return self.getToken(FORTRANParser.EQUALS, 0)

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(FORTRANParser.ExprContext)
            else:
                return self.getTypedRuleContext(FORTRANParser.ExprContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(FORTRANParser.COMMA)
            else:
                return self.getToken(FORTRANParser.COMMA, i)

        def getRuleIndex(self):
            return FORTRANParser.RULE_do_stmt_basic

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterDo_stmt_basic" ):
                listener.enterDo_stmt_basic(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitDo_stmt_basic" ):
                listener.exitDo_stmt_basic(self)




    def do_stmt_basic(self):

        localctx = FORTRANParser.Do_stmt_basicContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_do_stmt_basic)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 96
            self.match(FORTRANParser.DO)
            self.state = 97
            self.label()
            self.state = 98
            self.variable()
            self.state = 99
            self.match(FORTRANParser.EQUALS)
            self.state = 100
            self.expr()
            self.state = 101
            self.match(FORTRANParser.COMMA)
            self.state = 102
            self.expr()
            self.state = 105
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==35:
                self.state = 103
                self.match(FORTRANParser.COMMA)
                self.state = 104
                self.expr()


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Read_stmt_basicContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def READ(self):
            return self.getToken(FORTRANParser.READ, 0)

        def input_list(self):
            return self.getTypedRuleContext(FORTRANParser.Input_listContext,0)


        def getRuleIndex(self):
            return FORTRANParser.RULE_read_stmt_basic

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRead_stmt_basic" ):
                listener.enterRead_stmt_basic(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRead_stmt_basic" ):
                listener.exitRead_stmt_basic(self)




    def read_stmt_basic(self):

        localctx = FORTRANParser.Read_stmt_basicContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_read_stmt_basic)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 107
            self.match(FORTRANParser.READ)
            self.state = 108
            self.input_list()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Write_stmt_basicContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def WRITE(self):
            return self.getToken(FORTRANParser.WRITE, 0)

        def output_list(self):
            return self.getTypedRuleContext(FORTRANParser.Output_listContext,0)


        def getRuleIndex(self):
            return FORTRANParser.RULE_write_stmt_basic

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterWrite_stmt_basic" ):
                listener.enterWrite_stmt_basic(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitWrite_stmt_basic" ):
                listener.exitWrite_stmt_basic(self)




    def write_stmt_basic(self):

        localctx = FORTRANParser.Write_stmt_basicContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_write_stmt_basic)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 110
            self.match(FORTRANParser.WRITE)
            self.state = 111
            self.output_list()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def relational_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Relational_exprContext,0)


        def getRuleIndex(self):
            return FORTRANParser.RULE_expr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExpr" ):
                listener.enterExpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExpr" ):
                listener.exitExpr(self)




    def expr(self):

        localctx = FORTRANParser.ExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_expr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 113
            self.relational_expr(0)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Relational_exprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return FORTRANParser.RULE_relational_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)


    class RelationalExpressionContext(Relational_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Relational_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def relational_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Relational_exprContext,0)

        def relational_op(self):
            return self.getTypedRuleContext(FORTRANParser.Relational_opContext,0)

        def additive_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Additive_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRelationalExpression" ):
                listener.enterRelationalExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRelationalExpression" ):
                listener.exitRelationalExpression(self)


    class RelationalPrimaryContext(Relational_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Relational_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def additive_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Additive_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRelationalPrimary" ):
                listener.enterRelationalPrimary(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRelationalPrimary" ):
                listener.exitRelationalPrimary(self)



    def relational_expr(self, _p:int=0):
        _parentctx = self._ctx
        _parentState = self.state
        localctx = FORTRANParser.Relational_exprContext(self, self._ctx, _parentState)
        _prevctx = localctx
        _startState = 22
        self.enterRecursionRule(localctx, 22, self.RULE_relational_expr, _p)
        try:
            self.enterOuterAlt(localctx, 1)
            localctx = FORTRANParser.RelationalPrimaryContext(self, localctx)
            self._ctx = localctx
            _prevctx = localctx

            self.state = 116
            self.additive_expr(0)
            self._ctx.stop = self._input.LT(-1)
            self.state = 124
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,4,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    if self._parseListeners is not None:
                        self.triggerExitRuleEvent()
                    _prevctx = localctx
                    localctx = FORTRANParser.RelationalExpressionContext(self, FORTRANParser.Relational_exprContext(self, _parentctx, _parentState))
                    self.pushNewRecursionContext(localctx, _startState, self.RULE_relational_expr)
                    self.state = 118
                    if not self.precpred(self._ctx, 2):
                        from antlr4.error.Errors import FailedPredicateException
                        raise FailedPredicateException(self, "self.precpred(self._ctx, 2)")
                    self.state = 119
                    self.relational_op()
                    self.state = 120
                    self.additive_expr(0) 
                self.state = 126
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,4,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.unrollRecursionContexts(_parentctx)
        return localctx


    class Relational_opContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def EQ(self):
            return self.getToken(FORTRANParser.EQ, 0)

        def NE(self):
            return self.getToken(FORTRANParser.NE, 0)

        def LT(self):
            return self.getToken(FORTRANParser.LT, 0)

        def LE(self):
            return self.getToken(FORTRANParser.LE, 0)

        def GT(self):
            return self.getToken(FORTRANParser.GT, 0)

        def GE(self):
            return self.getToken(FORTRANParser.GE, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_relational_op

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRelational_op" ):
                listener.enterRelational_op(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRelational_op" ):
                listener.exitRelational_op(self)




    def relational_op(self):

        localctx = FORTRANParser.Relational_opContext(self, self._ctx, self.state)
        self.enterRule(localctx, 24, self.RULE_relational_op)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 127
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 8455716864) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Additive_exprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return FORTRANParser.RULE_additive_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)


    class AdditiveExpressionContext(Additive_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Additive_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def additive_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Additive_exprContext,0)

        def additive_op(self):
            return self.getTypedRuleContext(FORTRANParser.Additive_opContext,0)

        def multiplicative_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Multiplicative_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAdditiveExpression" ):
                listener.enterAdditiveExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAdditiveExpression" ):
                listener.exitAdditiveExpression(self)


    class AdditivePrimaryContext(Additive_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Additive_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def multiplicative_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Multiplicative_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAdditivePrimary" ):
                listener.enterAdditivePrimary(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAdditivePrimary" ):
                listener.exitAdditivePrimary(self)



    def additive_expr(self, _p:int=0):
        _parentctx = self._ctx
        _parentState = self.state
        localctx = FORTRANParser.Additive_exprContext(self, self._ctx, _parentState)
        _prevctx = localctx
        _startState = 26
        self.enterRecursionRule(localctx, 26, self.RULE_additive_expr, _p)
        try:
            self.enterOuterAlt(localctx, 1)
            localctx = FORTRANParser.AdditivePrimaryContext(self, localctx)
            self._ctx = localctx
            _prevctx = localctx

            self.state = 130
            self.multiplicative_expr(0)
            self._ctx.stop = self._input.LT(-1)
            self.state = 138
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,5,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    if self._parseListeners is not None:
                        self.triggerExitRuleEvent()
                    _prevctx = localctx
                    localctx = FORTRANParser.AdditiveExpressionContext(self, FORTRANParser.Additive_exprContext(self, _parentctx, _parentState))
                    self.pushNewRecursionContext(localctx, _startState, self.RULE_additive_expr)
                    self.state = 132
                    if not self.precpred(self._ctx, 2):
                        from antlr4.error.Errors import FailedPredicateException
                        raise FailedPredicateException(self, "self.precpred(self._ctx, 2)")
                    self.state = 133
                    self.additive_op()
                    self.state = 134
                    self.multiplicative_expr(0) 
                self.state = 140
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,5,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.unrollRecursionContexts(_parentctx)
        return localctx


    class Additive_opContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def PLUS(self):
            return self.getToken(FORTRANParser.PLUS, 0)

        def MINUS(self):
            return self.getToken(FORTRANParser.MINUS, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_additive_op

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAdditive_op" ):
                listener.enterAdditive_op(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAdditive_op" ):
                listener.exitAdditive_op(self)




    def additive_op(self):

        localctx = FORTRANParser.Additive_opContext(self, self._ctx, self.state)
        self.enterRule(localctx, 28, self.RULE_additive_op)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 141
            _la = self._input.LA(1)
            if not(_la==22 or _la==23):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Multiplicative_exprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return FORTRANParser.RULE_multiplicative_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)


    class MultiplicativePrimaryContext(Multiplicative_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Multiplicative_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def unary_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Unary_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMultiplicativePrimary" ):
                listener.enterMultiplicativePrimary(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMultiplicativePrimary" ):
                listener.exitMultiplicativePrimary(self)


    class MultiplicativeExpressionContext(Multiplicative_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Multiplicative_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def multiplicative_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Multiplicative_exprContext,0)

        def multiplicative_op(self):
            return self.getTypedRuleContext(FORTRANParser.Multiplicative_opContext,0)

        def unary_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Unary_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMultiplicativeExpression" ):
                listener.enterMultiplicativeExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMultiplicativeExpression" ):
                listener.exitMultiplicativeExpression(self)



    def multiplicative_expr(self, _p:int=0):
        _parentctx = self._ctx
        _parentState = self.state
        localctx = FORTRANParser.Multiplicative_exprContext(self, self._ctx, _parentState)
        _prevctx = localctx
        _startState = 30
        self.enterRecursionRule(localctx, 30, self.RULE_multiplicative_expr, _p)
        try:
            self.enterOuterAlt(localctx, 1)
            localctx = FORTRANParser.MultiplicativePrimaryContext(self, localctx)
            self._ctx = localctx
            _prevctx = localctx

            self.state = 144
            self.unary_expr()
            self._ctx.stop = self._input.LT(-1)
            self.state = 152
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,6,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    if self._parseListeners is not None:
                        self.triggerExitRuleEvent()
                    _prevctx = localctx
                    localctx = FORTRANParser.MultiplicativeExpressionContext(self, FORTRANParser.Multiplicative_exprContext(self, _parentctx, _parentState))
                    self.pushNewRecursionContext(localctx, _startState, self.RULE_multiplicative_expr)
                    self.state = 146
                    if not self.precpred(self._ctx, 2):
                        from antlr4.error.Errors import FailedPredicateException
                        raise FailedPredicateException(self, "self.precpred(self._ctx, 2)")
                    self.state = 147
                    self.multiplicative_op()
                    self.state = 148
                    self.unary_expr() 
                self.state = 154
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,6,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.unrollRecursionContexts(_parentctx)
        return localctx


    class Multiplicative_opContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def MULTIPLY(self):
            return self.getToken(FORTRANParser.MULTIPLY, 0)

        def DIVIDE(self):
            return self.getToken(FORTRANParser.DIVIDE, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_multiplicative_op

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMultiplicative_op" ):
                listener.enterMultiplicative_op(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMultiplicative_op" ):
                listener.exitMultiplicative_op(self)




    def multiplicative_op(self):

        localctx = FORTRANParser.Multiplicative_opContext(self, self._ctx, self.state)
        self.enterRule(localctx, 32, self.RULE_multiplicative_op)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 155
            _la = self._input.LA(1)
            if not(_la==24 or _la==25):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Unary_exprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return FORTRANParser.RULE_unary_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)



    class UnaryPrimaryContext(Unary_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Unary_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def power_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Power_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterUnaryPrimary" ):
                listener.enterUnaryPrimary(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitUnaryPrimary" ):
                listener.exitUnaryPrimary(self)


    class UnaryExpressionContext(Unary_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Unary_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def unary_op(self):
            return self.getTypedRuleContext(FORTRANParser.Unary_opContext,0)

        def unary_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Unary_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterUnaryExpression" ):
                listener.enterUnaryExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitUnaryExpression" ):
                listener.exitUnaryExpression(self)



    def unary_expr(self):

        localctx = FORTRANParser.Unary_exprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 34, self.RULE_unary_expr)
        try:
            self.state = 161
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [22, 23]:
                localctx = FORTRANParser.UnaryExpressionContext(self, localctx)
                self.enterOuterAlt(localctx, 1)
                self.state = 157
                self.unary_op()
                self.state = 158
                self.unary_expr()
                pass
            elif token in [33, 37, 38, 39]:
                localctx = FORTRANParser.UnaryPrimaryContext(self, localctx)
                self.enterOuterAlt(localctx, 2)
                self.state = 160
                self.power_expr()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Unary_opContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def PLUS(self):
            return self.getToken(FORTRANParser.PLUS, 0)

        def MINUS(self):
            return self.getToken(FORTRANParser.MINUS, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_unary_op

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterUnary_op" ):
                listener.enterUnary_op(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitUnary_op" ):
                listener.exitUnary_op(self)




    def unary_op(self):

        localctx = FORTRANParser.Unary_opContext(self, self._ctx, self.state)
        self.enterRule(localctx, 36, self.RULE_unary_op)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 163
            _la = self._input.LA(1)
            if not(_la==22 or _la==23):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Power_exprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return FORTRANParser.RULE_power_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)



    class PowerPrimaryContext(Power_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Power_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def primary(self):
            return self.getTypedRuleContext(FORTRANParser.PrimaryContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterPowerPrimary" ):
                listener.enterPowerPrimary(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitPowerPrimary" ):
                listener.exitPowerPrimary(self)


    class PowerExpressionContext(Power_exprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a FORTRANParser.Power_exprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def primary(self):
            return self.getTypedRuleContext(FORTRANParser.PrimaryContext,0)

        def POWER(self):
            return self.getToken(FORTRANParser.POWER, 0)
        def power_expr(self):
            return self.getTypedRuleContext(FORTRANParser.Power_exprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterPowerExpression" ):
                listener.enterPowerExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitPowerExpression" ):
                listener.exitPowerExpression(self)



    def power_expr(self):

        localctx = FORTRANParser.Power_exprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 38, self.RULE_power_expr)
        try:
            self.state = 170
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,8,self._ctx)
            if la_ == 1:
                localctx = FORTRANParser.PowerExpressionContext(self, localctx)
                self.enterOuterAlt(localctx, 1)
                self.state = 165
                self.primary()
                self.state = 166
                self.match(FORTRANParser.POWER)
                self.state = 167
                self.power_expr()
                pass

            elif la_ == 2:
                localctx = FORTRANParser.PowerPrimaryContext(self, localctx)
                self.enterOuterAlt(localctx, 2)
                self.state = 169
                self.primary()
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PrimaryContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def literal(self):
            return self.getTypedRuleContext(FORTRANParser.LiteralContext,0)


        def variable(self):
            return self.getTypedRuleContext(FORTRANParser.VariableContext,0)


        def LPAREN(self):
            return self.getToken(FORTRANParser.LPAREN, 0)

        def expr(self):
            return self.getTypedRuleContext(FORTRANParser.ExprContext,0)


        def RPAREN(self):
            return self.getToken(FORTRANParser.RPAREN, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_primary

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterPrimary" ):
                listener.enterPrimary(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitPrimary" ):
                listener.exitPrimary(self)




    def primary(self):

        localctx = FORTRANParser.PrimaryContext(self, self._ctx, self.state)
        self.enterRule(localctx, 40, self.RULE_primary)
        try:
            self.state = 178
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [37, 38]:
                self.enterOuterAlt(localctx, 1)
                self.state = 172
                self.literal()
                pass
            elif token in [39]:
                self.enterOuterAlt(localctx, 2)
                self.state = 173
                self.variable()
                pass
            elif token in [33]:
                self.enterOuterAlt(localctx, 3)
                self.state = 174
                self.match(FORTRANParser.LPAREN)
                self.state = 175
                self.expr()
                self.state = 176
                self.match(FORTRANParser.RPAREN)
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LiteralContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def INTEGER_LITERAL(self):
            return self.getToken(FORTRANParser.INTEGER_LITERAL, 0)

        def REAL_LITERAL(self):
            return self.getToken(FORTRANParser.REAL_LITERAL, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_literal

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLiteral" ):
                listener.enterLiteral(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLiteral" ):
                listener.exitLiteral(self)




    def literal(self):

        localctx = FORTRANParser.LiteralContext(self, self._ctx, self.state)
        self.enterRule(localctx, 42, self.RULE_literal)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 180
            _la = self._input.LA(1)
            if not(_la==37 or _la==38):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class VariableContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IDENTIFIER(self):
            return self.getToken(FORTRANParser.IDENTIFIER, 0)

        def LPAREN(self):
            return self.getToken(FORTRANParser.LPAREN, 0)

        def expr_list(self):
            return self.getTypedRuleContext(FORTRANParser.Expr_listContext,0)


        def RPAREN(self):
            return self.getToken(FORTRANParser.RPAREN, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_variable

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterVariable" ):
                listener.enterVariable(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitVariable" ):
                listener.exitVariable(self)




    def variable(self):

        localctx = FORTRANParser.VariableContext(self, self._ctx, self.state)
        self.enterRule(localctx, 44, self.RULE_variable)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 182
            self.match(FORTRANParser.IDENTIFIER)
            self.state = 187
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,10,self._ctx)
            if la_ == 1:
                self.state = 183
                self.match(FORTRANParser.LPAREN)
                self.state = 184
                self.expr_list()
                self.state = 185
                self.match(FORTRANParser.RPAREN)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LabelContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def INTEGER_LITERAL(self):
            return self.getToken(FORTRANParser.INTEGER_LITERAL, 0)

        def getRuleIndex(self):
            return FORTRANParser.RULE_label

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLabel" ):
                listener.enterLabel(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLabel" ):
                listener.exitLabel(self)




    def label(self):

        localctx = FORTRANParser.LabelContext(self, self._ctx, self.state)
        self.enterRule(localctx, 46, self.RULE_label)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 189
            self.match(FORTRANParser.INTEGER_LITERAL)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Expr_listContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(FORTRANParser.ExprContext)
            else:
                return self.getTypedRuleContext(FORTRANParser.ExprContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(FORTRANParser.COMMA)
            else:
                return self.getToken(FORTRANParser.COMMA, i)

        def getRuleIndex(self):
            return FORTRANParser.RULE_expr_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExpr_list" ):
                listener.enterExpr_list(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExpr_list" ):
                listener.exitExpr_list(self)




    def expr_list(self):

        localctx = FORTRANParser.Expr_listContext(self, self._ctx, self.state)
        self.enterRule(localctx, 48, self.RULE_expr_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 199
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 970675191808) != 0):
                self.state = 191
                self.expr()
                self.state = 196
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==35:
                    self.state = 192
                    self.match(FORTRANParser.COMMA)
                    self.state = 193
                    self.expr()
                    self.state = 198
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)



        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Input_listContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def variable(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(FORTRANParser.VariableContext)
            else:
                return self.getTypedRuleContext(FORTRANParser.VariableContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(FORTRANParser.COMMA)
            else:
                return self.getToken(FORTRANParser.COMMA, i)

        def getRuleIndex(self):
            return FORTRANParser.RULE_input_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterInput_list" ):
                listener.enterInput_list(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitInput_list" ):
                listener.exitInput_list(self)




    def input_list(self):

        localctx = FORTRANParser.Input_listContext(self, self._ctx, self.state)
        self.enterRule(localctx, 50, self.RULE_input_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 201
            self.variable()
            self.state = 206
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==35:
                self.state = 202
                self.match(FORTRANParser.COMMA)
                self.state = 203
                self.variable()
                self.state = 208
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Output_listContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(FORTRANParser.ExprContext)
            else:
                return self.getTypedRuleContext(FORTRANParser.ExprContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(FORTRANParser.COMMA)
            else:
                return self.getToken(FORTRANParser.COMMA, i)

        def getRuleIndex(self):
            return FORTRANParser.RULE_output_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterOutput_list" ):
                listener.enterOutput_list(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitOutput_list" ):
                listener.exitOutput_list(self)




    def output_list(self):

        localctx = FORTRANParser.Output_listContext(self, self._ctx, self.state)
        self.enterRule(localctx, 52, self.RULE_output_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 209
            self.expr()
            self.state = 214
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==35:
                self.state = 210
                self.match(FORTRANParser.COMMA)
                self.state = 211
                self.expr()
                self.state = 216
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx



    def sempred(self, localctx:RuleContext, ruleIndex:int, predIndex:int):
        if self._predicates == None:
            self._predicates = dict()
        self._predicates[11] = self.relational_expr_sempred
        self._predicates[13] = self.additive_expr_sempred
        self._predicates[15] = self.multiplicative_expr_sempred
        pred = self._predicates.get(ruleIndex, None)
        if pred is None:
            raise Exception("No predicate with index:" + str(ruleIndex))
        else:
            return pred(localctx, predIndex)

    def relational_expr_sempred(self, localctx:Relational_exprContext, predIndex:int):
            if predIndex == 0:
                return self.precpred(self._ctx, 2)
         

    def additive_expr_sempred(self, localctx:Additive_exprContext, predIndex:int):
            if predIndex == 1:
                return self.precpred(self._ctx, 2)
         

    def multiplicative_expr_sempred(self, localctx:Multiplicative_exprContext, predIndex:int):
            if predIndex == 2:
                return self.precpred(self._ctx, 2)
         




