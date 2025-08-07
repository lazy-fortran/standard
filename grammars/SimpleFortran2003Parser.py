# Generated from ../../grammars/SimpleFortran2003Parser.g4 by ANTLR 4.13.2
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
        4,1,25,100,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,1,0,1,0,3,0,25,8,0,1,1,1,1,1,
        1,3,1,30,8,1,1,1,1,1,1,1,3,1,35,8,1,1,2,1,2,1,2,3,2,40,8,2,1,2,1,
        2,1,2,3,2,45,8,2,1,3,4,3,48,8,3,11,3,12,3,49,1,4,1,4,1,4,1,4,1,4,
        3,4,57,8,4,1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,3,5,68,8,5,1,6,1,
        6,1,6,1,6,1,6,1,6,1,6,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,8,1,8,1,8,1,
        8,1,9,1,9,1,9,1,9,1,10,1,10,1,10,5,10,95,8,10,10,10,12,10,98,9,10,
        1,10,0,0,11,0,2,4,6,8,10,12,14,16,18,20,0,0,101,0,24,1,0,0,0,2,26,
        1,0,0,0,4,36,1,0,0,0,6,47,1,0,0,0,8,56,1,0,0,0,10,67,1,0,0,0,12,
        69,1,0,0,0,14,76,1,0,0,0,16,83,1,0,0,0,18,87,1,0,0,0,20,91,1,0,0,
        0,22,25,3,2,1,0,23,25,3,4,2,0,24,22,1,0,0,0,24,23,1,0,0,0,25,1,1,
        0,0,0,26,27,5,13,0,0,27,29,5,22,0,0,28,30,3,6,3,0,29,28,1,0,0,0,
        29,30,1,0,0,0,30,31,1,0,0,0,31,32,5,14,0,0,32,34,5,13,0,0,33,35,
        5,22,0,0,34,33,1,0,0,0,34,35,1,0,0,0,35,3,1,0,0,0,36,37,5,15,0,0,
        37,39,5,22,0,0,38,40,3,6,3,0,39,38,1,0,0,0,39,40,1,0,0,0,40,41,1,
        0,0,0,41,42,5,14,0,0,42,44,5,15,0,0,43,45,5,22,0,0,44,43,1,0,0,0,
        44,45,1,0,0,0,45,5,1,0,0,0,46,48,3,8,4,0,47,46,1,0,0,0,48,49,1,0,
        0,0,49,47,1,0,0,0,49,50,1,0,0,0,50,7,1,0,0,0,51,57,3,10,5,0,52,57,
        3,12,6,0,53,57,3,14,7,0,54,57,3,16,8,0,55,57,3,18,9,0,56,51,1,0,
        0,0,56,52,1,0,0,0,56,53,1,0,0,0,56,54,1,0,0,0,56,55,1,0,0,0,57,9,
        1,0,0,0,58,59,5,10,0,0,59,60,5,16,0,0,60,68,3,20,10,0,61,62,5,11,
        0,0,62,63,5,16,0,0,63,68,3,20,10,0,64,65,5,12,0,0,65,66,5,16,0,0,
        66,68,3,20,10,0,67,58,1,0,0,0,67,61,1,0,0,0,67,64,1,0,0,0,68,11,
        1,0,0,0,69,70,5,4,0,0,70,71,5,19,0,0,71,72,5,22,0,0,72,73,5,20,0,
        0,73,74,5,16,0,0,74,75,3,20,10,0,75,13,1,0,0,0,76,77,5,5,0,0,77,
        78,5,19,0,0,78,79,5,22,0,0,79,80,5,20,0,0,80,81,5,16,0,0,81,82,3,
        20,10,0,82,15,1,0,0,0,83,84,5,6,0,0,84,85,5,16,0,0,85,86,3,20,10,
        0,86,17,1,0,0,0,87,88,5,7,0,0,88,89,5,16,0,0,89,90,3,20,10,0,90,
        19,1,0,0,0,91,96,5,22,0,0,92,93,5,21,0,0,93,95,5,22,0,0,94,92,1,
        0,0,0,95,98,1,0,0,0,96,94,1,0,0,0,96,97,1,0,0,0,97,21,1,0,0,0,98,
        96,1,0,0,0,9,24,29,34,39,44,49,56,67,96
    ]

class SimpleFortran2003Parser ( Parser ):

    grammarFileName = "SimpleFortran2003Parser.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "'::'", "'=>'", "'='", "'('", "')'", "','" ]

    symbolicNames = [ "<INVALID>", "ABSTRACT", "EXTENDS", "FINAL", "CLASS", 
                      "PROCEDURE", "VOLATILE", "PROTECTED", "ASSOCIATE", 
                      "BLOCK", "INTEGER", "REAL", "CHARACTER", "PROGRAM", 
                      "END", "MODULE", "DOUBLE_COLON", "ARROW", "ASSIGN", 
                      "LPAREN", "RPAREN", "COMMA", "IDENTIFIER", "INTEGER_LITERAL", 
                      "WS", "COMMENT" ]

    RULE_program_unit_f2003 = 0
    RULE_main_program_f2003 = 1
    RULE_module_f2003 = 2
    RULE_declaration_list = 3
    RULE_declaration = 4
    RULE_type_declaration_stmt = 5
    RULE_class_declaration_stmt = 6
    RULE_procedure_declaration_stmt = 7
    RULE_volatile_stmt = 8
    RULE_protected_stmt = 9
    RULE_entity_list = 10

    ruleNames =  [ "program_unit_f2003", "main_program_f2003", "module_f2003", 
                   "declaration_list", "declaration", "type_declaration_stmt", 
                   "class_declaration_stmt", "procedure_declaration_stmt", 
                   "volatile_stmt", "protected_stmt", "entity_list" ]

    EOF = Token.EOF
    ABSTRACT=1
    EXTENDS=2
    FINAL=3
    CLASS=4
    PROCEDURE=5
    VOLATILE=6
    PROTECTED=7
    ASSOCIATE=8
    BLOCK=9
    INTEGER=10
    REAL=11
    CHARACTER=12
    PROGRAM=13
    END=14
    MODULE=15
    DOUBLE_COLON=16
    ARROW=17
    ASSIGN=18
    LPAREN=19
    RPAREN=20
    COMMA=21
    IDENTIFIER=22
    INTEGER_LITERAL=23
    WS=24
    COMMENT=25

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class Program_unit_f2003Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def main_program_f2003(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Main_program_f2003Context,0)


        def module_f2003(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Module_f2003Context,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_program_unit_f2003

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterProgram_unit_f2003" ):
                listener.enterProgram_unit_f2003(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitProgram_unit_f2003" ):
                listener.exitProgram_unit_f2003(self)




    def program_unit_f2003(self):

        localctx = SimpleFortran2003Parser.Program_unit_f2003Context(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_program_unit_f2003)
        try:
            self.state = 24
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [13]:
                self.enterOuterAlt(localctx, 1)
                self.state = 22
                self.main_program_f2003()
                pass
            elif token in [15]:
                self.enterOuterAlt(localctx, 2)
                self.state = 23
                self.module_f2003()
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


    class Main_program_f2003Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def PROGRAM(self, i:int=None):
            if i is None:
                return self.getTokens(SimpleFortran2003Parser.PROGRAM)
            else:
                return self.getToken(SimpleFortran2003Parser.PROGRAM, i)

        def IDENTIFIER(self, i:int=None):
            if i is None:
                return self.getTokens(SimpleFortran2003Parser.IDENTIFIER)
            else:
                return self.getToken(SimpleFortran2003Parser.IDENTIFIER, i)

        def END(self):
            return self.getToken(SimpleFortran2003Parser.END, 0)

        def declaration_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Declaration_listContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_main_program_f2003

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMain_program_f2003" ):
                listener.enterMain_program_f2003(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMain_program_f2003" ):
                listener.exitMain_program_f2003(self)




    def main_program_f2003(self):

        localctx = SimpleFortran2003Parser.Main_program_f2003Context(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_main_program_f2003)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 26
            self.match(SimpleFortran2003Parser.PROGRAM)
            self.state = 27
            self.match(SimpleFortran2003Parser.IDENTIFIER)
            self.state = 29
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 7408) != 0):
                self.state = 28
                self.declaration_list()


            self.state = 31
            self.match(SimpleFortran2003Parser.END)
            self.state = 32
            self.match(SimpleFortran2003Parser.PROGRAM)
            self.state = 34
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==22:
                self.state = 33
                self.match(SimpleFortran2003Parser.IDENTIFIER)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Module_f2003Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def MODULE(self, i:int=None):
            if i is None:
                return self.getTokens(SimpleFortran2003Parser.MODULE)
            else:
                return self.getToken(SimpleFortran2003Parser.MODULE, i)

        def IDENTIFIER(self, i:int=None):
            if i is None:
                return self.getTokens(SimpleFortran2003Parser.IDENTIFIER)
            else:
                return self.getToken(SimpleFortran2003Parser.IDENTIFIER, i)

        def END(self):
            return self.getToken(SimpleFortran2003Parser.END, 0)

        def declaration_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Declaration_listContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_module_f2003

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterModule_f2003" ):
                listener.enterModule_f2003(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitModule_f2003" ):
                listener.exitModule_f2003(self)




    def module_f2003(self):

        localctx = SimpleFortran2003Parser.Module_f2003Context(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_module_f2003)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 36
            self.match(SimpleFortran2003Parser.MODULE)
            self.state = 37
            self.match(SimpleFortran2003Parser.IDENTIFIER)
            self.state = 39
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 7408) != 0):
                self.state = 38
                self.declaration_list()


            self.state = 41
            self.match(SimpleFortran2003Parser.END)
            self.state = 42
            self.match(SimpleFortran2003Parser.MODULE)
            self.state = 44
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==22:
                self.state = 43
                self.match(SimpleFortran2003Parser.IDENTIFIER)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Declaration_listContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def declaration(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(SimpleFortran2003Parser.DeclarationContext)
            else:
                return self.getTypedRuleContext(SimpleFortran2003Parser.DeclarationContext,i)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_declaration_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterDeclaration_list" ):
                listener.enterDeclaration_list(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitDeclaration_list" ):
                listener.exitDeclaration_list(self)




    def declaration_list(self):

        localctx = SimpleFortran2003Parser.Declaration_listContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_declaration_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 47 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 46
                self.declaration()
                self.state = 49 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 7408) != 0)):
                    break

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class DeclarationContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def type_declaration_stmt(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Type_declaration_stmtContext,0)


        def class_declaration_stmt(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Class_declaration_stmtContext,0)


        def procedure_declaration_stmt(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Procedure_declaration_stmtContext,0)


        def volatile_stmt(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Volatile_stmtContext,0)


        def protected_stmt(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Protected_stmtContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_declaration

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterDeclaration" ):
                listener.enterDeclaration(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitDeclaration" ):
                listener.exitDeclaration(self)




    def declaration(self):

        localctx = SimpleFortran2003Parser.DeclarationContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_declaration)
        try:
            self.state = 56
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [10, 11, 12]:
                self.enterOuterAlt(localctx, 1)
                self.state = 51
                self.type_declaration_stmt()
                pass
            elif token in [4]:
                self.enterOuterAlt(localctx, 2)
                self.state = 52
                self.class_declaration_stmt()
                pass
            elif token in [5]:
                self.enterOuterAlt(localctx, 3)
                self.state = 53
                self.procedure_declaration_stmt()
                pass
            elif token in [6]:
                self.enterOuterAlt(localctx, 4)
                self.state = 54
                self.volatile_stmt()
                pass
            elif token in [7]:
                self.enterOuterAlt(localctx, 5)
                self.state = 55
                self.protected_stmt()
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


    class Type_declaration_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def INTEGER(self):
            return self.getToken(SimpleFortran2003Parser.INTEGER, 0)

        def DOUBLE_COLON(self):
            return self.getToken(SimpleFortran2003Parser.DOUBLE_COLON, 0)

        def entity_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Entity_listContext,0)


        def REAL(self):
            return self.getToken(SimpleFortran2003Parser.REAL, 0)

        def CHARACTER(self):
            return self.getToken(SimpleFortran2003Parser.CHARACTER, 0)

        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_type_declaration_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterType_declaration_stmt" ):
                listener.enterType_declaration_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitType_declaration_stmt" ):
                listener.exitType_declaration_stmt(self)




    def type_declaration_stmt(self):

        localctx = SimpleFortran2003Parser.Type_declaration_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_type_declaration_stmt)
        try:
            self.state = 67
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [10]:
                self.enterOuterAlt(localctx, 1)
                self.state = 58
                self.match(SimpleFortran2003Parser.INTEGER)
                self.state = 59
                self.match(SimpleFortran2003Parser.DOUBLE_COLON)
                self.state = 60
                self.entity_list()
                pass
            elif token in [11]:
                self.enterOuterAlt(localctx, 2)
                self.state = 61
                self.match(SimpleFortran2003Parser.REAL)
                self.state = 62
                self.match(SimpleFortran2003Parser.DOUBLE_COLON)
                self.state = 63
                self.entity_list()
                pass
            elif token in [12]:
                self.enterOuterAlt(localctx, 3)
                self.state = 64
                self.match(SimpleFortran2003Parser.CHARACTER)
                self.state = 65
                self.match(SimpleFortran2003Parser.DOUBLE_COLON)
                self.state = 66
                self.entity_list()
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


    class Class_declaration_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def CLASS(self):
            return self.getToken(SimpleFortran2003Parser.CLASS, 0)

        def LPAREN(self):
            return self.getToken(SimpleFortran2003Parser.LPAREN, 0)

        def IDENTIFIER(self):
            return self.getToken(SimpleFortran2003Parser.IDENTIFIER, 0)

        def RPAREN(self):
            return self.getToken(SimpleFortran2003Parser.RPAREN, 0)

        def DOUBLE_COLON(self):
            return self.getToken(SimpleFortran2003Parser.DOUBLE_COLON, 0)

        def entity_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Entity_listContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_class_declaration_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterClass_declaration_stmt" ):
                listener.enterClass_declaration_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitClass_declaration_stmt" ):
                listener.exitClass_declaration_stmt(self)




    def class_declaration_stmt(self):

        localctx = SimpleFortran2003Parser.Class_declaration_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_class_declaration_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 69
            self.match(SimpleFortran2003Parser.CLASS)
            self.state = 70
            self.match(SimpleFortran2003Parser.LPAREN)
            self.state = 71
            self.match(SimpleFortran2003Parser.IDENTIFIER)
            self.state = 72
            self.match(SimpleFortran2003Parser.RPAREN)
            self.state = 73
            self.match(SimpleFortran2003Parser.DOUBLE_COLON)
            self.state = 74
            self.entity_list()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Procedure_declaration_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def PROCEDURE(self):
            return self.getToken(SimpleFortran2003Parser.PROCEDURE, 0)

        def LPAREN(self):
            return self.getToken(SimpleFortran2003Parser.LPAREN, 0)

        def IDENTIFIER(self):
            return self.getToken(SimpleFortran2003Parser.IDENTIFIER, 0)

        def RPAREN(self):
            return self.getToken(SimpleFortran2003Parser.RPAREN, 0)

        def DOUBLE_COLON(self):
            return self.getToken(SimpleFortran2003Parser.DOUBLE_COLON, 0)

        def entity_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Entity_listContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_procedure_declaration_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterProcedure_declaration_stmt" ):
                listener.enterProcedure_declaration_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitProcedure_declaration_stmt" ):
                listener.exitProcedure_declaration_stmt(self)




    def procedure_declaration_stmt(self):

        localctx = SimpleFortran2003Parser.Procedure_declaration_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_procedure_declaration_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 76
            self.match(SimpleFortran2003Parser.PROCEDURE)
            self.state = 77
            self.match(SimpleFortran2003Parser.LPAREN)
            self.state = 78
            self.match(SimpleFortran2003Parser.IDENTIFIER)
            self.state = 79
            self.match(SimpleFortran2003Parser.RPAREN)
            self.state = 80
            self.match(SimpleFortran2003Parser.DOUBLE_COLON)
            self.state = 81
            self.entity_list()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Volatile_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def VOLATILE(self):
            return self.getToken(SimpleFortran2003Parser.VOLATILE, 0)

        def DOUBLE_COLON(self):
            return self.getToken(SimpleFortran2003Parser.DOUBLE_COLON, 0)

        def entity_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Entity_listContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_volatile_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterVolatile_stmt" ):
                listener.enterVolatile_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitVolatile_stmt" ):
                listener.exitVolatile_stmt(self)




    def volatile_stmt(self):

        localctx = SimpleFortran2003Parser.Volatile_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_volatile_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 83
            self.match(SimpleFortran2003Parser.VOLATILE)
            self.state = 84
            self.match(SimpleFortran2003Parser.DOUBLE_COLON)
            self.state = 85
            self.entity_list()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Protected_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def PROTECTED(self):
            return self.getToken(SimpleFortran2003Parser.PROTECTED, 0)

        def DOUBLE_COLON(self):
            return self.getToken(SimpleFortran2003Parser.DOUBLE_COLON, 0)

        def entity_list(self):
            return self.getTypedRuleContext(SimpleFortran2003Parser.Entity_listContext,0)


        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_protected_stmt

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterProtected_stmt" ):
                listener.enterProtected_stmt(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitProtected_stmt" ):
                listener.exitProtected_stmt(self)




    def protected_stmt(self):

        localctx = SimpleFortran2003Parser.Protected_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_protected_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 87
            self.match(SimpleFortran2003Parser.PROTECTED)
            self.state = 88
            self.match(SimpleFortran2003Parser.DOUBLE_COLON)
            self.state = 89
            self.entity_list()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Entity_listContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IDENTIFIER(self, i:int=None):
            if i is None:
                return self.getTokens(SimpleFortran2003Parser.IDENTIFIER)
            else:
                return self.getToken(SimpleFortran2003Parser.IDENTIFIER, i)

        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(SimpleFortran2003Parser.COMMA)
            else:
                return self.getToken(SimpleFortran2003Parser.COMMA, i)

        def getRuleIndex(self):
            return SimpleFortran2003Parser.RULE_entity_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterEntity_list" ):
                listener.enterEntity_list(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitEntity_list" ):
                listener.exitEntity_list(self)




    def entity_list(self):

        localctx = SimpleFortran2003Parser.Entity_listContext(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_entity_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 91
            self.match(SimpleFortran2003Parser.IDENTIFIER)
            self.state = 96
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==21:
                self.state = 92
                self.match(SimpleFortran2003Parser.COMMA)
                self.state = 93
                self.match(SimpleFortran2003Parser.IDENTIFIER)
                self.state = 98
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





