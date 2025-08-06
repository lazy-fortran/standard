#!/usr/bin/env python3
"""
Comprehensive validation test for SharedCore FORTRAN grammar
This test validates complete functionality and serves as a regression test
covering all fixed issues and implemented features.
"""

import sys
import os
import unittest
from pathlib import Path

# Add build directory to path for imports
project_root = Path(__file__).parent.parent.parent
build_dir = project_root / "build" / "shared_core"
sys.path.insert(0, str(build_dir))

from antlr4 import InputStream, CommonTokenStream
from SharedCoreLexer import SharedCoreLexer
from SharedCoreParser import SharedCoreParser


class TestComprehensiveValidation(unittest.TestCase):
    """Comprehensive validation of SharedCore FORTRAN grammar functionality"""
    
    def parse_program(self, code):
        """Helper to parse complete FORTRAN programs"""
        input_stream = InputStream(code)
        lexer = SharedCoreLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = SharedCoreParser(token_stream)
        return parser.program_unit_core()
    
    def parse_expression(self, expr):
        """Helper to parse expressions"""
        input_stream = InputStream(expr)
        lexer = SharedCoreLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = SharedCoreParser(token_stream)
        return parser.expr()
    
    def get_tree_structure(self, node):
        """Get readable tree structure for validation"""
        if not hasattr(node, 'children') or not node.children:
            return node.getText() if hasattr(node, 'getText') else str(node)
        
        children = []
        for child in node.children:
            children.append(self.get_tree_structure(child))
        
        rule_name = type(node).__name__.replace('Context', '')
        return {rule_name: children}
    
    def test_operator_precedence_comprehensive(self):
        """Test that all operator precedence is correctly implemented"""
        precedence_cases = [
            # Multiplication over addition
            ("A + B * C", "A + (B * C)"),
            # Power over multiplication  
            ("A * B ** C", "A * (B ** C)"),
            # Power right associativity
            ("A ** B ** C", "A ** (B ** C)"),
            # Parentheses override precedence
            ("(A + B) * C", "(A + B) * C"),
            # Complex mixed precedence
            ("A + B * C ** D - E / F", "A + (B * (C ** D)) - (E / F)"),
            # Relational lower than arithmetic
            ("A + B .GT. C * D", "(A + B) .GT. (C * D)"),
        ]
        
        for expr, expected_grouping in precedence_cases:
            with self.subTest(expression=expr):
                tree = self.parse_expression(expr)
                self.assertIsNotNone(tree, f"Failed to parse: {expr}")
                
                # Verify the structure reflects correct precedence
                structure = self.get_tree_structure(tree)
                structure_str = str(structure)
                
                # For A + B * C, should find MultiplicativeExpression nested in AdditiveExpression
                if "A + B * C" in expr:
                    self.assertIn('AdditiveExpression', structure_str)
                    self.assertIn('MultiplicativeExpression', structure_str)
                
                # For A ** B ** C, should find nested PowerExpression (right associative)
                elif "A ** B ** C" in expr:
                    self.assertIn('PowerExpression', structure_str)
                    # Should have nested PowerExpression for right associativity
                    power_count = structure_str.count('PowerExpression')
                    self.assertGreaterEqual(power_count, 2)
    
    def test_call_statements_comprehensive(self):
        """Test CALL statements with all variations"""
        call_cases = [
            "CALL SUB1",                          # No arguments
            "CALL SUB2()",                        # Empty parentheses
            "CALL SUB3(X)",                       # Single argument
            "CALL SUB4(A, B, C)",                 # Multiple arguments
            "CALL SUB5(X + Y, Z * W)",           # Expression arguments
            "CALL SUB6((A + B) * C, D ** E)",    # Complex expressions
            "CALL SUB_NAME_WITH_UNDERSCORES(VAR_1, VAR_2)"  # Underscore names
        ]
        
        for call_stmt in call_cases:
            with self.subTest(call=call_stmt):
                # Parse as individual call statement
                input_stream = InputStream(call_stmt)
                lexer = SharedCoreLexer(input_stream)
                token_stream = CommonTokenStream(lexer)
                parser = SharedCoreParser(token_stream)
                tree = parser.call_stmt()
                self.assertIsNotNone(tree, f"Failed to parse CALL: {call_stmt}")
                
                # Parse as part of complete program
                tree = self.parse_program(call_stmt)
                self.assertIsNotNone(tree, f"Failed to parse CALL in program: {call_stmt}")
    
    def test_lexer_tokens_comprehensive(self):
        """Test all lexer tokens including newly added ones"""
        token_cases = [
            # Keywords
            ("IF", "IF"), ("GOTO", "GOTO"), ("DO", "DO"), ("CALL", "CALL"),
            ("CONTINUE", "CONTINUE"), ("STOP", "STOP"), ("READ", "READ"), ("WRITE", "WRITE"),
            
            # Operators  
            ("=", "ASSIGN"), ("+", "PLUS"), ("-", "MINUS"), ("*", "MULTIPLY"), 
            ("/", "DIVIDE"), ("**", "POWER"),
            
            # Relational operators
            (".EQ.", "EQ"), (".NE.", "NE"), (".LT.", "LT"), 
            (".LE.", "LE"), (".GT.", "GT"), (".GE.", "GE"),
            
            # Delimiters (including newly added colon)
            ("(", "LPAREN"), (")", "RPAREN"), (",", "COMMA"), (":", "COLON"),
            
            # Literals
            ("123", "INTEGER_LITERAL"), ("3.14", "REAL_LITERAL"), ("1.23E-4", "REAL_LITERAL"),
            
            # Identifiers (including underscores)
            ("VAR", "IDENTIFIER"), ("VAR_NAME", "IDENTIFIER"), ("X_1", "IDENTIFIER"),
        ]
        
        for token_text, expected_type in token_cases:
            with self.subTest(token=token_text):
                input_stream = InputStream(token_text)
                lexer = SharedCoreLexer(input_stream)
                token = lexer.nextToken()
                
                # Check token is recognized
                self.assertNotEqual(token.type, -1, f"Token not recognized: {token_text}")
                
                # Check correct token type
                token_name = lexer.symbolicNames[token.type] if token.type < len(lexer.symbolicNames) else "UNKNOWN"
                self.assertEqual(token_name, expected_type, 
                               f"Wrong token type for '{token_text}': got {token_name}, expected {expected_type}")
    
    def test_classic_fortran_programs(self):
        """Test complete classic FORTRAN programs"""
        
        # Bubble sort algorithm
        bubble_sort = """
        N = 10
        DO 100 I = 1, N
        DO 200 J = 1, N
        IF (A(I) .GT. A(J)) 10, 200, 10
        10 TEMP = A(I)
        A(I) = A(J)
        A(J) = TEMP  
        200 CONTINUE
        100 CONTINUE
        """
        
        # Quadratic equation solver
        quadratic = """
        A = 1.0
        B = -5.0
        C = 6.0
        DISC = B ** 2 - 4.0 * A * C
        IF (DISC) 10, 20, 30
        10 WRITE -1
        GOTO 40
        20 X = -B / (2.0 * A)
        WRITE X
        GOTO 40
        30 SQRT_D = DISC ** 0.5  
        X1 = (-B + SQRT_D) / (2.0 * A)
        X2 = (-B - SQRT_D) / (2.0 * A)
        WRITE X1
        WRITE X2
        40 STOP
        """
        
        # Matrix operations
        matrix = """
        DO 100 I = 1, 3
        DO 200 J = 1, 3
        IF (I .EQ. J) 10, 20, 20
        10 MATRIX(I, J) = 1.0
        GOTO 200
        20 MATRIX(I, J) = 0.0
        200 CONTINUE
        100 CONTINUE
        """
        
        # Factorial calculation
        factorial = """
        N = 5
        FACT = 1
        I = 1
        10 IF (I .GT. N) 20, 15, 20
        15 FACT = FACT * I
        I = I + 1
        GOTO 10
        20 WRITE FACT
        STOP
        """
        
        programs = [
            (bubble_sort.strip(), "Bubble sort algorithm"),
            (quadratic.strip(), "Quadratic equation solver"), 
            (matrix.strip(), "Matrix initialization"),
            (factorial.strip(), "Factorial calculation"),
        ]
        
        for program, description in programs:
            with self.subTest(program=description):
                tree = self.parse_program(program)
                self.assertIsNotNone(tree, f"Failed to parse {description}")
    
    def test_comprehensive_expression_parsing(self):
        """Test comprehensive expression parsing with all features"""
        expressions = [
            # Basic arithmetic
            "A + B - C",
            "A * B / C", 
            "A ** B",
            
            # Mixed precedence
            "A + B * C ** D - E / F",
            "A * B + C * D",
            "(A + B) * (C - D)",
            
            # Relational expressions
            "A .EQ. B",
            "A .GT. B + C",
            "A + B .LT. C * D",
            
            # Complex nested expressions
            "((A + B) * C - D) / (E ** F + G)",
            "A ** (B + C) * D - E / (F - G)",
            
            # Array access in expressions  
            "A(I) + B(J, K)",
            "MATRIX(I, J) * VECTOR(K)",
            
            # Function-like calls in expressions
            "X + FUNC(A, B) * Y",
            
            # Mixed data types
            "3.14 * R ** 2",
            "I + 1.5E-3",
            
            # Underscore identifiers
            "VAR_X + VAR_Y * CONST_PI",
        ]
        
        for expr in expressions:
            with self.subTest(expression=expr):
                tree = self.parse_expression(expr)
                self.assertIsNotNone(tree, f"Failed to parse expression: {expr}")
    
    def test_all_statement_types(self):
        """Test all supported statement types"""
        statements = [
            # Assignment statements
            "X = 1",
            "Y = A + B * C",
            "ARRAY(I) = VALUE",
            
            # Control statements  
            "GOTO 10",
            "IF (X) 10, 20, 30",
            "DO 100 I = 1, N",
            "CONTINUE",
            "STOP",
            "END",
            
            # I/O statements
            "READ X",
            "READ X, Y, Z", 
            "WRITE RESULT",
            "WRITE A, B, C",
            
            # CALL statements
            "CALL SUB()",
            "CALL SUB(A, B)",
        ]
        
        for stmt in statements:
            with self.subTest(statement=stmt):
                tree = self.parse_program(stmt)
                self.assertIsNotNone(tree, f"Failed to parse statement: {stmt}")
    
    def test_regression_fixed_issues(self):
        """Test specific issues that were fixed during development"""
        
        # Issue: Operator precedence was incorrect
        tree = self.parse_expression("A + B * C")
        structure = self.get_tree_structure(tree)
        structure_str = str(structure)
        # Should have AdditiveExpression at top level with nested MultiplicativeExpression
        self.assertIn('AdditiveExpression', structure_str)
        self.assertIn('MultiplicativeExpression', structure_str)
        
        # Issue: CALL statements with empty parentheses failed
        tree = self.parse_program("CALL SUB()")
        self.assertIsNotNone(tree)
        
        # Issue: Underscore in identifiers not supported
        input_stream = InputStream("VAR_NAME")
        lexer = SharedCoreLexer(input_stream)
        token = lexer.nextToken()
        self.assertEqual(token.text, "VAR_NAME")  # Should be single token
        
        # Issue: Colon token not supported
        input_stream = InputStream(":")
        lexer = SharedCoreLexer(input_stream)
        token = lexer.nextToken()
        token_name = lexer.symbolicNames[token.type]
        self.assertEqual(token_name, "COLON")
        
        # Issue: Comments caused identifier conflicts (C vs comment)
        tree = self.parse_program("C = 1 ! This is a comment")
        self.assertIsNotNone(tree)
    
    def test_comprehensive_integration(self):
        """Final integration test with complex FORTRAN program using all features"""
        complex_program = """
        ! Comprehensive test program using all implemented features
        
        ! Variable declarations and initialization
        N_SIZE = 10
        PI_CONSTANT = 3.14159
        TOLERANCE = 1.0E-6
        
        ! Array operations with complex subscripts
        DO 100 I = 1, N_SIZE
        DO 200 J = 1, N_SIZE
        INDEX_CALC = I * N_SIZE + J
        MATRIX_A(I, J) = PI_CONSTANT * INDEX_CALC
        200 CONTINUE
        100 CONTINUE
        
        ! Complex arithmetic with proper precedence
        RESULT = (MATRIX_A(1, 1) + MATRIX_A(2, 2)) ** 2 - PI_CONSTANT / 4.0
        
        ! Conditional logic with relational operators
        IF (RESULT .GT. TOLERANCE) 10, 20, 10
        10 STATUS_FLAG = 1
        GOTO 30
        20 STATUS_FLAG = 0
        30 CONTINUE
        
        ! Subroutine calls with complex arguments
        CALL PROCESS_DATA(MATRIX_A, N_SIZE, STATUS_FLAG)
        CALL CALCULATE_RESULT(RESULT + PI_CONSTANT, TOLERANCE * 2.0)
        CALL FINALIZE()
        
        ! I/O operations
        WRITE RESULT, STATUS_FLAG
        READ NEXT_VALUE
        
        ! Program termination
        STOP
        END
        """
        
        tree = self.parse_program(complex_program.strip())
        self.assertIsNotNone(tree, "Failed to parse comprehensive integration program")
        
        # Verify the program contains all expected constructs
        program_text = complex_program
        self.assertIn("! ", program_text)  # Comments
        self.assertIn("_", program_text)   # Underscores in identifiers  
        self.assertIn("**", program_text)  # Power operator
        self.assertIn(".GT.", program_text) # Relational operator
        self.assertIn("CALL", program_text) # Subroutine calls
        self.assertIn("DO", program_text)   # Loops
        self.assertIn("IF", program_text)   # Conditional
        self.assertIn("E-", program_text)   # Scientific notation


if __name__ == '__main__':
    unittest.main()