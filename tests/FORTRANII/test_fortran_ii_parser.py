#!/usr/bin/env python3
"""
Test suite for FORTRAN II Parser - FORTRAN II (1958)
Tests for FORTRAN II specific features
"""

import sys
import os
import unittest
from pathlib import Path

# Add tests root (for fixture_utils) and grammars directory to path for imports
ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT))
sys.path.append(str(ROOT.parent / "grammars"))

sys.path.insert(0, 'grammars')

try:
    from antlr4 import InputStream, CommonTokenStream
    from FORTRANIILexer import FORTRANIILexer
    from FORTRANIIParser import FORTRANIIParser
    from fixture_utils import load_fixture
except ImportError as e:
    print(f"Import error: {e}")
    FORTRANIIParser = None


class TestFORTRANIIParser(unittest.TestCase):
    """Test FORTRAN II (1958) parser rules"""
    
    def setUp(self):
        """Set up test fixtures"""
        if FORTRANIIParser is None:
            self.skipTest("FORTRANIIParser not available - grammar not yet implemented")
    
    def parse(self, text, rule_name='program_unit_core'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRANIILexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANIIParser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_call_statements(self):
        """Test CALL statements (introduced in FORTRAN II, 1958)"""
        test_cases = [
            ("CALL SUBRTN", "SUBRTN", 0),
            ("CALL CALC(X, Y, Z)", "CALC", 3),
            ("CALL PROCESS(A, B)", "PROCESS", 2),
            ("CALL INIT", "INIT", 0)
        ]
        
        for text, expected_name, expected_args in test_cases:
            with self.subTest(call_stmt=text):
                tree = self.parse(text, 'call_stmt')
                self.assertIsNotNone(tree)
                # Verify CALL keyword
                self.assertEqual(tree.children[0].getText(), 'CALL')
                # Verify subroutine name
                self.assertEqual(tree.children[1].getText(), expected_name)
                # Verify argument count
                if expected_args > 0:
                    # Has parentheses and arguments
                    self.assertGreater(len(tree.children), 2)
                    # Check for opening parenthesis
                    self.assertIn('(', tree.getText())
    
    def test_function_definition(self):
        """Test FUNCTION definitions (introduced in FORTRAN II)"""
        function_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_parser",
            "function_text.f",
        )
        
        tree = self.parse(function_text, 'function_subprogram')
        self.assertIsNotNone(tree)
        # Verify FUNCTION keyword
        self.assertIn('FUNCTION', tree.getText())
        # Verify function name
        self.assertIn('MAX', tree.getText())
        # Verify parameters
        self.assertIn('A', tree.getText())
        self.assertIn('B', tree.getText())
        # Verify RETURN statements
        self.assertEqual(tree.getText().count('RETURN'), 2)
        # Verify END statement
        self.assertIn('END', tree.getText())
    
    def test_subroutine_definition(self):
        """Test SUBROUTINE definitions (introduced in FORTRAN II)"""
        subroutine_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_parser",
            "subroutine_text.f",
        )
        
        tree = self.parse(subroutine_text, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
        # Verify SUBROUTINE keyword
        self.assertIn('SUBROUTINE', tree.getText())
        # Verify subroutine name
        self.assertIn('SWAP', tree.getText())
        # Verify parameters
        self.assertIn('X', tree.getText())
        self.assertIn('Y', tree.getText())
        # Verify assignment statements
        self.assertIn('TEMP', tree.getText())
        # Verify RETURN statement
        self.assertIn('RETURN', tree.getText())
        # Verify END statement
        self.assertIn('END', tree.getText())
    
    def test_common_statement(self):
        """Test COMMON statements (introduced in FORTRAN II)"""
        test_cases = [
            ("COMMON A, B, C", None, ["A", "B", "C"]),
            ("COMMON /BLOCK1/ X, Y, Z", "BLOCK1", ["X", "Y", "Z"]),
            ("COMMON /DATA/ ARRAY(100)", "DATA", ["ARRAY"])
        ]
        
        for text, block_name, vars in test_cases:
            with self.subTest(common_stmt=text):
                tree = self.parse(text, 'common_stmt')
                self.assertIsNotNone(tree)
                # Verify COMMON keyword
                self.assertIn('COMMON', tree.getText())
                # Verify block name if present
                if block_name:
                    self.assertIn(block_name, tree.getText())
                # Verify variables
                for var in vars:
                    self.assertIn(var, tree.getText())
    
    def test_not_a_stub(self):
        """Verify FORTRAN II grammar is a real implementation, not a stub"""
        # Read the grammar file to check for stub indicators
        import os
        grammar_path = os.path.join('grammars', 'FORTRANIIParser.g4')
        with open(grammar_path, 'r') as f:
            content = f.read()

        # Ensure no stub comments
        self.assertNotIn('grammar stub', content.lower(),
                        "Grammar contains 'stub' references")
        self.assertNotIn('phase 1', content.lower(),
                        "Grammar contains phase 1 stub references")

        # Verify key FORTRAN II features are properly implemented
        # Should have real rule implementations, not just placeholders
        self.assertIn('call_stmt', content)
        self.assertIn('function_subprogram', content)
        self.assertIn('subroutine_subprogram', content)
        self.assertIn('common_stmt', content)

        # Parse a complete FORTRAN II program to ensure it works
        program = load_fixture(
            "FORTRANII",
            "test_fortran_ii_parser",
            "subroutine_program.f",
        )
        tree = self.parse(program, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
        # Verify the parse tree contains expected elements
        text = tree.getText()
        self.assertIn('SUBROUTINE', text)
        self.assertIn('CALC', text)
        self.assertIn('COMMON', text)
        self.assertIn('DATA', text)

    def test_expression_precedence_hierarchy(self):
        """Test that FORTRAN II expression grammar has proper precedence"""
        # Verify the grammar file contains proper precedence hierarchy rules
        import os
        grammar_path = os.path.join('grammars', 'FORTRANIIParser.g4')
        with open(grammar_path, 'r') as f:
            content = f.read()

        # Verify proper precedence hierarchy rules exist
        self.assertIn('additive_expr', content,
                      "Grammar missing additive_expr rule for +/- precedence")
        self.assertIn('multiplicative_expr', content,
                      "Grammar missing multiplicative_expr rule for */÷ precedence")
        self.assertIn('unary_expr', content,
                      "Grammar missing unary_expr rule for unary +/- precedence")
        self.assertIn('power_expr', content,
                      "Grammar missing power_expr rule for ** precedence")

        # Verify expr delegates to additive_expr (not flat left-recursive)
        self.assertIn('expr\n    : additive_expr', content,
                      "expr should delegate to additive_expr for proper hierarchy")

    def test_chained_exponentiation_parses(self):
        """Test parsing of chained exponentiation expressions"""
        # Test cases for chained exponentiation (right associative)
        test_exprs = [
            "2**3",          # Simple power
            "2**3**4",       # Chained power (right assoc: 2**(3**4))
            "A**B**C",       # Variables with chained power
            "X**2**3**4",    # Triple chained
        ]

        for expr_text in test_exprs:
            with self.subTest(expr=expr_text):
                tree = self.parse(expr_text, 'expr')
                self.assertIsNotNone(tree, f"Failed to parse: {expr_text}")
                self.assertIn('**', tree.getText())

    def test_power_expr_right_associativity(self):
        """Verify ** is right associative: 2**3**4 = 2**(3**4)"""
        tree = self.parse("2**3**4", 'expr')
        self.assertIsNotNone(tree)

        # The parse tree structure should show right associativity
        # For 2**3**4, the top power_expr should have:
        #   - primary: 2
        #   - power_expr: 3**4 (which itself has primary: 3, power_expr: 4)
        #
        # Navigate down to power_expr
        additive = tree.additive_expr()
        mult = additive.multiplicative_expr()
        unary = mult.unary_expr()
        power = unary.power_expr()

        # Verify structure: power_expr -> primary POWER power_expr
        self.assertIsNotNone(power.primary())
        self.assertEqual(power.primary().getText(), '2')

        # The nested power_expr should contain 3**4
        nested_power = power.power_expr()
        self.assertIsNotNone(nested_power)
        self.assertEqual(nested_power.primary().getText(), '3')

        # The innermost should be just 4
        innermost = nested_power.power_expr()
        self.assertIsNotNone(innermost)
        self.assertEqual(innermost.primary().getText(), '4')

    def test_mixed_operator_precedence(self):
        """Test mixed operators follow correct precedence"""
        test_cases = [
            # (expression, expected_parse_text)
            ("2+3*4", "2+3*4"),       # * before +
            ("2*3+4", "2*3+4"),       # * before +
            ("2**3*4", "2**3*4"),     # ** before *
            ("-2**3", "-2**3"),       # ** before unary -
            ("2+3**4*5", "2+3**4*5"), # ** before * before +
        ]

        for expr_text, expected in test_cases:
            with self.subTest(expr=expr_text):
                tree = self.parse(expr_text, 'expr')
                self.assertIsNotNone(tree, f"Failed to parse: {expr_text}")
                self.assertEqual(tree.getText(), expected)

    def test_parenthesized_expressions(self):
        """Test parentheses override default precedence"""
        test_cases = [
            "(2+3)*4",      # Parens force + before *
            "2**(3+4)",     # Parens in exponent
            "((2**3))",     # Nested parens
            "(A+B)*(C+D)",  # Multiple paren groups
        ]

        for expr_text in test_cases:
            with self.subTest(expr=expr_text):
                tree = self.parse(expr_text, 'expr')
                self.assertIsNotNone(tree, f"Failed to parse: {expr_text}")

    def test_unary_operator_precedence(self):
        """Test unary operators have correct precedence relative to **"""
        # -2**3 should parse as -(2**3), not (-2)**3
        # This is verified by the parse tree structure
        tree = self.parse("-2**3", 'expr')
        self.assertIsNotNone(tree)

        # Navigate to unary_expr
        additive = tree.additive_expr()
        mult = additive.multiplicative_expr()
        unary = mult.unary_expr()

        # The unary_expr should have unary_op (MINUS) and another unary_expr
        self.assertIsNotNone(unary.unary_op())
        self.assertEqual(unary.unary_op().getText(), '-')

        # The nested unary_expr leads to power_expr with 2**3
        nested_unary = unary.unary_expr()
        self.assertIsNotNone(nested_unary)
        power = nested_unary.power_expr()
        self.assertIsNotNone(power)
        self.assertEqual(power.primary().getText(), '2')
        self.assertEqual(power.power_expr().primary().getText(), '3')

    def test_function_call_in_expression(self):
        """Test function calls within expressions"""
        test_cases = [
            "SIN(X)",
            "SIN(X) + COS(Y)",
            "SIN(X)**2 + COS(X)**2",
            "SQRT(A**2 + B**2)",
        ]

        for expr_text in test_cases:
            with self.subTest(expr=expr_text):
                tree = self.parse(expr_text, 'expr')
                self.assertIsNotNone(tree, f"Failed to parse: {expr_text}")

    def test_label_tokens_as_integer_literals_in_expressions(self):
        """Test LABEL tokens are valid integer literals in expression context.

        The FORTRAN II lexer defines LABEL as 1-5 digit integers starting with
        1-9 (statement labels in columns 1-5). In expression context, these
        same numeric tokens should be accepted as integer literals via the
        literal rule: INTEGER_LITERAL | LABEL.

        Per issue #210: This test explicitly documents and protects the
        LABEL-as-literal contract in the expression grammar.
        """
        # Test cases: expressions where numeric operands are lexed as LABEL
        # tokens (1-5 digit integers starting with 1-9)
        test_cases = [
            # Simple LABEL values in expressions
            ("1", "single digit"),
            ("42", "two digits"),
            ("100", "three digits"),
            ("9999", "four digits"),
            ("12345", "five digits - max LABEL length"),
            # Arithmetic with LABEL tokens
            ("1 + 2", "addition of labels"),
            ("42 * 3", "multiplication with labels"),
            ("100 - 50", "subtraction of labels"),
            ("99 / 9", "division of labels"),
            ("2 ** 3", "exponentiation of labels"),
            # Mixed expressions
            ("1 + 2 * 3", "precedence with labels"),
            ("100 + 200 + 300", "chained addition"),
            ("2 ** 3 ** 4", "chained exponentiation (right assoc)"),
            ("-42", "unary minus with label"),
            ("+100", "unary plus with label"),
            # Labels in function/array subscripts
            ("A(1)", "label as array subscript"),
            ("A(1, 2, 3)", "multiple label subscripts"),
            ("SIN(100)", "label in function argument"),
            # Complex expressions
            ("1 + A * 2", "label mixed with variable"),
            ("(100 + 200) * 3", "parenthesized labels"),
            ("2 ** 10 - 1", "power with label operands"),
        ]

        for expr_text, description in test_cases:
            with self.subTest(expr=expr_text, desc=description):
                tree = self.parse(expr_text, 'expr')
                self.assertIsNotNone(tree, f"Failed to parse: {expr_text}")
                # Verify getText matches input modulo whitespace
                expected = expr_text.replace(' ', '')
                actual = tree.getText()
                self.assertEqual(
                    actual, expected,
                    f"getText mismatch for '{expr_text}': got '{actual}'"
                )

    def test_label_in_literal_rule(self):
        """Test that LABEL tokens match through the literal parser rule.

        Verifies the grammar rule:
            literal: INTEGER_LITERAL | LABEL

        This ensures LABEL tokens are explicitly accepted as literals.
        """
        # Parse via literal rule directly
        label_values = ["1", "9", "10", "99", "100", "999", "1000", "9999",
                        "10000", "12345", "99999"]

        for value in label_values:
            with self.subTest(label=value):
                tree = self.parse(value, 'literal')
                self.assertIsNotNone(tree, f"Failed to parse literal: {value}")
                self.assertEqual(tree.getText(), value)

    def test_label_in_assignment_statement(self):
        """Test LABEL tokens work in assignment statement expressions.

        Assignment statements are the primary use of expressions in FORTRAN II.
        This ensures LABEL-as-literal works in real statement context.
        """
        test_cases = [
            "X = 1",
            "X = 100",
            "X = 1 + 2",
            "X = 2 ** 10",
            "A(1) = 42",
            "RESULT = 100 * 2 + 50",
        ]

        for stmt_text in test_cases:
            with self.subTest(stmt=stmt_text):
                tree = self.parse(stmt_text, 'assignment_stmt')
                self.assertIsNotNone(tree, f"Failed to parse: {stmt_text}")

    def test_label_in_do_statement(self):
        """Test LABEL tokens in DO statement loop bounds.

        DO statements use expressions for initial, limit, and step values.
        These are commonly integer literals (lexed as LABEL tokens).
        """
        test_cases = [
            "DO 100 I = 1, 10",
            "DO 100 I = 1, 100, 2",
            "DO 999 J = 10, 1, -1",
            "DO 50 K = 1, N",
        ]

        for stmt_text in test_cases:
            with self.subTest(stmt=stmt_text):
                tree = self.parse(stmt_text, 'do_stmt')
                self.assertIsNotNone(tree, f"Failed to parse: {stmt_text}")

    def test_leading_zero_not_lexed_as_label(self):
        """Test that leading-zero sequences are NOT lexed as LABEL tokens.

        The FORTRAN II LABEL token is defined as [1-9][0-9]{0,4} which means:
        - Must start with 1-9 (not 0)
        - 1 to 5 digits total

        Sequences with leading zeros (like 0, 01, 007) do NOT match the
        LABEL token pattern. They may match INTEGER_LITERAL from the base
        lexer, but the key contract is that LABEL specifically excludes them.

        This test verifies the lexer correctly distinguishes LABEL tokens
        from INTEGER_LITERAL tokens based on leading zeros.
        """
        # Get the LABEL token type for comparison
        label_type = FORTRANIILexer.LABEL

        # Leading-zero sequences that should NOT be lexed as LABEL
        not_label_cases = [
            "0",        # Single zero
            "01",       # Leading zero
            "007",      # Leading zeros
            "00123",    # Leading zeros
            "000",      # Multiple zeros
        ]

        for text in not_label_cases:
            with self.subTest(text=text):
                input_stream = InputStream(text)
                lexer = FORTRANIILexer(input_stream)
                token = lexer.nextToken()
                self.assertNotEqual(
                    token.type, label_type,
                    f"'{text}' was incorrectly lexed as LABEL "
                    f"(LABEL requires starting digit 1-9)"
                )

        # Verify valid LABEL values ARE lexed as LABEL
        valid_label_cases = ["1", "9", "42", "100", "12345"]
        for text in valid_label_cases:
            with self.subTest(text=text, expected="LABEL"):
                input_stream = InputStream(text)
                lexer = FORTRANIILexer(input_stream)
                token = lexer.nextToken()
                self.assertEqual(
                    token.type, label_type,
                    f"'{text}' should be lexed as LABEL token"
                )


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)
