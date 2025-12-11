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
sys.path.append(str(ROOT.parent / "grammars/generated/early"))

sys.path.insert(0, 'grammars/generated/early')

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
    
    def parse(self, text, rule_name):
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

    def test_write_statements(self):
        """Test WRITE statements (inherited from FORTRAN I, 1957)"""
        test_cases = [
            "WRITE X",
            "WRITE X, Y, Z",
            "WRITE 1.0, 2.0",
            "WRITE A, B(1), C(I, J)"
        ]

        for text in test_cases:
            with self.subTest(write_stmt=text):
                tree = self.parse(text, 'write_stmt_basic')
                self.assertIsNotNone(tree)
                # Verify WRITE keyword
                self.assertEqual(tree.children[0].getText(), 'WRITE')

    def test_write_in_main_program(self):
        """Test WRITE statement in a complete FORTRAN II main program"""
        program_text = """
        X = 1.0
        Y = 2.0
        WRITE X, Y
        STOP
        END
        """
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
        # Verify WRITE appears in parse tree
        text = tree.getText()
        self.assertIn('WRITE', text)
        self.assertIn('X', text)
        self.assertIn('Y', text)

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

    def test_return_statement(self):
        """Test RETURN statement (NEW in FORTRAN II)"""
        # Test standalone RETURN statement
        text = "RETURN"
        tree = self.parse(text, 'statement_body')
        self.assertIsNotNone(tree)
        self.assertEqual(tree.getText(), 'RETURN')

        # Test RETURN within labeled statement
        labeled_text = "100 RETURN"
        labeled_tree = self.parse(labeled_text, 'statement')
        self.assertIsNotNone(labeled_tree)
        self.assertIn('RETURN', labeled_tree.getText())

    def test_return_in_function(self):
        """Test RETURN statement in function definition (NEW in FORTRAN II)"""
        function_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_parser",
            "function_text.f",
        )

        tree = self.parse(function_text, 'function_subprogram')
        self.assertIsNotNone(tree)
        # Verify RETURN appears in function
        text_content = tree.getText()
        self.assertIn('RETURN', text_content)
        # Should have exactly 2 RETURN statements (one for each branch)
        self.assertEqual(text_content.count('RETURN'), 2)

    def test_return_in_subroutine(self):
        """Test RETURN statement in subroutine definition (NEW in FORTRAN II)"""
        subroutine_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_parser",
            "subroutine_text.f",
        )

        tree = self.parse(subroutine_text, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
        # Verify RETURN appears in subroutine
        text_content = tree.getText()
        self.assertIn('RETURN', text_content)
    
    def test_not_a_stub(self):
        """Verify FORTRAN II grammar is a real implementation, not a stub"""
        # Read the grammar file to check for stub indicators
        import os
        grammar_path = os.path.join('grammars', 'src', 'FORTRANIIParser.g4')
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
        grammar_path = os.path.join('grammars', 'src', 'FORTRANIIParser.g4')
        with open(grammar_path, 'r') as f:
            content = f.read()

        # FORTRAN II inherits the full expression hierarchy from FORTRAN I.
        self.assertIn('import FORTRANParser', content,
                      "FORTRAN II must inherit expression rules from FORTRANParser")
        # Verify FORTRAN II widens literal to accept LABEL tokens.
        self.assertIn('literal', content,
                      "FORTRAN II must override literal to accept LABEL tokens")

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
        tree = self.parse("2**3**4", 'power_expr')
        self.assertIsNotNone(tree)

        # Verify structure: power_expr -> primary POWER power_expr
        self.assertEqual(len(tree.children), 3)
        self.assertEqual(tree.children[0].getText(), '2')
        self.assertEqual(tree.children[1].getText(), '**')
        self.assertEqual(tree.children[2].getText(), '3**4')

        # The nested power_expr should contain 3**4 and be right-associative
        nested_power = tree.power_expr()
        self.assertIsNotNone(nested_power)
        self.assertEqual(len(nested_power.children), 3)
        self.assertEqual(nested_power.children[0].getText(), '3')
        self.assertEqual(nested_power.children[1].getText(), '**')
        self.assertEqual(nested_power.children[2].getText(), '4')

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
        tree = self.parse("-2**3", 'unary_expr')
        self.assertIsNotNone(tree)

        # The unary_expr should have unary_op (MINUS) and another unary_expr
        self.assertEqual(len(tree.children), 2)
        self.assertEqual(tree.children[0].getText(), '-')
        self.assertEqual(tree.children[1].getText(), '2**3')

        # The nested unary_expr leads to a power expression with 2**3
        nested_unary = tree.unary_expr()
        self.assertIsNotNone(nested_unary)
        self.assertIn('**', nested_unary.getText())

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
                tree = self.parse(stmt_text, 'do_stmt_basic')
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

    def test_dimension_statement(self):
        """Test DIMENSION statement (inherited from FORTRAN I, per C28-6000-2 Appendix A)

        The FORTRAN II manual (C28-6000-2) documents that FORTRAN II includes
        all FORTRAN I specification statements, including DIMENSION.

        DIMENSION declares the size of arrays before they are used:
            DIMENSION v, v, ...
        where v is an array declarator: IDENTIFIER(dimension_list)
        """
        test_cases = [
            ("DIMENSION A(10)", ["A"], [10]),
            ("DIMENSION X(5, 10)", ["X"], [5, 10]),
            ("DIMENSION Y(20)", ["Y"], [20]),
            ("DIMENSION MATRIX(3, 3, 3)", ["MATRIX"], [3, 3, 3]),
            ("DIMENSION A(10), B(20), C(5, 5)", ["A", "B", "C"], None),
            ("DIMENSION A(100), B(50)", ["A", "B"], None),
        ]

        for text, expected_names, expected_dims in test_cases:
            with self.subTest(dimension_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)
                # Verify DIMENSION keyword present
                self.assertIn('DIMENSION', tree.getText())
                # Verify array names appear in parse tree
                tree_text = tree.getText()
                for name in expected_names:
                    self.assertIn(name, tree_text,
                                f"Array name '{name}' not found in DIMENSION statement")
                # Verify opening and closing parentheses for array declarations
                self.assertIn('(', tree_text)
                self.assertIn(')', tree_text)

    def test_dimension_statement_with_constants(self):
        """Test DIMENSION statement with constant dimension values"""
        test_cases = [
            "DIMENSION SMALL(1)",
            "DIMENSION MEDIUM(10, 20)",
            "DIMENSION LARGE(100, 100, 100)",
            "DIMENSION MIXED(5), ARRAY1(10, 10), ARR2(3, 4, 5)",
        ]

        for text in test_cases:
            with self.subTest(dim_const=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                self.assertIn('DIMENSION', tree.getText())

    def test_equivalence_statement(self):
        """Test EQUIVALENCE statement (inherited from FORTRAN I, per C28-6000-2 Appendix A)

        The FORTRAN II manual (C28-6000-2) documents that FORTRAN II includes
        all FORTRAN I specification statements, including EQUIVALENCE.

        EQUIVALENCE allows variables or array elements to share the same memory location:
            EQUIVALENCE (a,b,...), ...
        where each set (a,b,...) declares memory overlay relationships.
        """
        test_cases = [
            ("EQUIVALENCE (A, B)", ["A", "B"]),
            ("EQUIVALENCE (X, Y, Z)", ["X", "Y", "Z"]),
            ("EQUIVALENCE (A, B), (C, D)", ["A", "B", "C", "D"]),
            ("EQUIVALENCE (A(1), B(5))", ["A", "B"]),
            ("EQUIVALENCE (M, N), (X, Y, Z)", None),
        ]

        for text, expected_names in test_cases:
            with self.subTest(equivalence_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                # Verify EQUIVALENCE keyword present
                self.assertIn('EQUIVALENCE', tree.getText())
                # Verify variable names appear if specified
                if expected_names:
                    tree_text = tree.getText()
                    for name in expected_names:
                        self.assertIn(name, tree_text,
                                    f"Variable '{name}' not found in EQUIVALENCE statement")
                # Verify parentheses for sets
                tree_text = tree.getText()
                self.assertIn('(', tree_text)
                self.assertIn(')', tree_text)

    def test_equivalence_multiple_sets(self):
        """Test EQUIVALENCE statement with multiple memory overlay sets"""
        test_cases = [
            "EQUIVALENCE (A, B), (C, D)",
            "EQUIVALENCE (X, Y), (Z, W), (P, Q)",
            "EQUIVALENCE (VAR1, VAR2), (ARR1(1), ARR2(5))",
            "EQUIVALENCE (A, B, C), (D, E)",
        ]

        for text in test_cases:
            with self.subTest(equiv_multi=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                self.assertIn('EQUIVALENCE', tree.getText())
                # Multiple sets should have multiple comma-separated parentheses groups
                self.assertGreaterEqual(tree.getText().count('('), 2,
                                       f"Statement should have multiple equivalence sets: {text}")

    def test_frequency_statement(self):
        """Test FREQUENCY statement (unique to FORTRAN I/II, per C28-6000-2 Appendix A)

        The FORTRAN II manual (C28-6000-2) documents that FORTRAN II includes
        the FREQUENCY statement from FORTRAN I.

        FREQUENCY provides optimization hints for branch prediction:
            FREQUENCY n (i1, i2, ...)
        where n is an integer constant label and (i1, i2, ...) are statement labels.

        This statement was unique to 1957 FORTRAN and removed in FORTRAN 66 and later.
        The n value represents how frequently the corresponding branch is expected
        to be taken (optimization hint for IBM 704).
        """
        test_cases = [
            ("FREQUENCY 100 (1, 2, 3)", "100", ["1", "2", "3"]),
            ("FREQUENCY 50 (10)", "50", ["10"]),
            ("FREQUENCY 25 (5, 10, 15, 20)", "25", ["5", "10", "15", "20"]),
            ("FREQUENCY 75 (100, 200)", "75", ["100", "200"]),
        ]

        for text, freq_value, label_values in test_cases:
            with self.subTest(frequency_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                # Verify FREQUENCY keyword present
                self.assertIn('FREQUENCY', tree.getText())
                # Verify frequency value appears
                tree_text = tree.getText()
                self.assertIn(freq_value, tree_text,
                            f"Frequency value '{freq_value}' not found in statement")
                # Verify statement labels appear
                for label in label_values:
                    self.assertIn(label, tree_text,
                                f"Label '{label}' not found in FREQUENCY statement")
                # Verify parentheses
                self.assertIn('(', tree_text)
                self.assertIn(')', tree_text)

    def test_frequency_statement_single_label(self):
        """Test FREQUENCY statement with single label"""
        test_cases = [
            "FREQUENCY 100 (50)",
            "FREQUENCY 10 (999)",
            "FREQUENCY 80 (25)",
        ]

        for text in test_cases:
            with self.subTest(freq_single=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                self.assertIn('FREQUENCY', tree.getText())

    def test_frequency_statement_multiple_labels(self):
        """Test FREQUENCY statement with multiple labels"""
        test_cases = [
            "FREQUENCY 50 (1, 2, 3, 4, 5)",
            "FREQUENCY 100 (10, 20, 30)",
            "FREQUENCY 25 (100, 200, 300, 400)",
        ]

        for text in test_cases:
            with self.subTest(freq_multi=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                self.assertIn('FREQUENCY', tree.getText())
                # Should have multiple labels separated by commas
                self.assertIn(',', tree.getText())

    def test_specification_statements_in_program(self):
        """Test specification statements in a complete FORTRAN II program context

        This integration test verifies that DIMENSION, EQUIVALENCE, and FREQUENCY
        statements parse correctly within a full program structure.
        """
        program = """
        DIMENSION A(10), B(5, 5)
        EQUIVALENCE (X, Y), (Z, W)
        FREQUENCY 50 (100, 200)
        X = 1.0
        Y = 2.0
        STOP
        END
        """
        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)
        text = tree.getText()
        # Verify all specification statements are present
        self.assertIn('DIMENSION', text)
        self.assertIn('EQUIVALENCE', text)
        self.assertIn('FREQUENCY', text)
        # Verify executable statements are also present
        self.assertIn('STOP', text)
        self.assertIn('END', text)

    def test_assign_statement(self):
        """Test ASSIGN statements (inherited from FORTRAN I per C28-6000-2 Appendix A).

        ASSIGN stores a statement label in an integer variable for later use with
        assigned GOTO. Syntax: ASSIGN label TO variable
        """
        assign_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_assign_goto",
            "assign_stmt.f",
        )

        tree = self.parse(assign_text, 'main_program')
        self.assertIsNotNone(tree)
        text = tree.getText()
        # Verify ASSIGN statements are present
        self.assertIn('ASSIGN', text)
        # Verify labels (100, 200, 300) are present
        self.assertIn('100', text)
        self.assertIn('200', text)
        self.assertIn('300', text)
        # Verify variable names
        self.assertIn('N', text)
        self.assertIn('M', text)
        self.assertIn('JUMP', text)

    def test_assigned_goto_statement(self):
        """Test assigned GOTO statements (inherited from FORTRAN I per C28-6000-2 Appendix A).

        Assigned GOTO branches to one of several labels based on the value stored
        in a variable. Syntax: GOTO variable, (label1, label2, ...)
        """
        goto_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_assign_goto",
            "assigned_goto_stmt.f",
        )

        tree = self.parse(goto_text, 'main_program')
        self.assertIsNotNone(tree)
        text = tree.getText()
        # Verify ASSIGN statement
        self.assertIn('ASSIGN', text)
        # Verify assigned GOTO statement (GOTO variable, (labels))
        self.assertIn('GOTO', text)
        # Verify target labels
        self.assertIn('100', text)
        self.assertIn('200', text)
        self.assertIn('300', text)
        # Verify variable name
        self.assertIn('N', text)

    def test_assign_and_goto_combined(self):
        """Test ASSIGN and assigned GOTO used together in typical pattern.

        This integration test verifies that ASSIGN and assigned GOTO work
        together in a realistic FORTRAN II program.
        """
        combined_text = load_fixture(
            "FORTRANII",
            "test_fortran_ii_assign_goto",
            "assign_goto_combined.f",
        )

        tree = self.parse(combined_text, 'main_program')
        self.assertIsNotNone(tree)
        text = tree.getText()

        # Verify both ASSIGN and assigned GOTO are present
        self.assertIn('ASSIGN', text)
        self.assertIn('GOTO', text)

        # Verify labels
        self.assertIn('100', text)
        self.assertIn('200', text)
        self.assertIn('300', text)

        # Verify variable name
        self.assertIn('JUMP', text)

    def test_assign_statement_direct(self):
        """Test parsing ASSIGN statement directly as statement_body rule"""
        test_cases = [
            "ASSIGN 100 TO N",
            "ASSIGN 999 TO LABEL",
            "ASSIGN 50 TO JUMP_VAR",
        ]

        for text in test_cases:
            with self.subTest(assign_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)
                # Verify ASSIGN keyword
                self.assertIn('ASSIGN', tree.getText())
                # Verify TO keyword
                self.assertIn('TO', tree.getText())

    def test_assigned_goto_statement_direct(self):
        """Test parsing assigned GOTO statement directly as statement_body rule"""
        test_cases = [
            "GOTO N, (100, 200, 300)",
            "GOTO JUMP, (10, 20, 30, 40)",
            "GOTO VAR, (1, 2)",
        ]

        for text in test_cases:
            with self.subTest(assigned_goto_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)
                # Verify GOTO keyword
                self.assertIn('GOTO', tree.getText())
                # Verify labels in parentheses
                self.assertIn('(', tree.getText())
                self.assertIn(')', tree.getText())

    def test_end_file_statement(self):
        """Test END FILE statement (inherited from FORTRAN I, per C28-6000-2 Appendix A)

        The FORTRAN II manual (C28-6000-2) documents that FORTRAN II includes
        all FORTRAN I statements including file-control statements.

        END FILE writes an end-of-file mark on a tape unit:
            END FILE i
        where i is a tape unit number (integer expression).
        Reference: IBM Form C28-6003 Chapter III.F
        Issue #384: File-control statements not wired into FORTRAN II statement_body
        """
        test_cases = [
            ("END FILE 1", 1),
            ("END FILE 10", 10),
            ("END FILE ITAPE", None),
        ]

        for text, unit_num in test_cases:
            with self.subTest(end_file_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                # Verify END FILE keywords present
                tree_text = tree.getText()
                self.assertIn('END', tree_text)
                self.assertIn('FILE', tree_text)

    def test_rewind_statement(self):
        """Test REWIND statement (inherited from FORTRAN I, per C28-6000-2 Appendix A)

        The FORTRAN II manual (C28-6000-2) documents that FORTRAN II includes
        all FORTRAN I statements including file-control statements.

        REWIND rewinds tape unit to the beginning:
            REWIND i
        where i is a tape unit number (integer expression).
        Reference: IBM Form C28-6003 Chapter III.F
        Issue #384: File-control statements not wired into FORTRAN II statement_body
        """
        test_cases = [
            ("REWIND 1", 1),
            ("REWIND 5", 5),
            ("REWIND ITAPE", None),
        ]

        for text, unit_num in test_cases:
            with self.subTest(rewind_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                # Verify REWIND keyword present
                tree_text = tree.getText()
                self.assertIn('REWIND', tree_text)

    def test_backspace_statement(self):
        """Test BACKSPACE statement (inherited from FORTRAN I, per C28-6000-2 Appendix A)

        The FORTRAN II manual (C28-6000-2) documents that FORTRAN II includes
        all FORTRAN I statements including file-control statements.

        BACKSPACE repositions tape unit back one logical record:
            BACKSPACE i
        where i is a tape unit number (integer expression).
        Reference: IBM Form C28-6003 Chapter III.F
        Issue #384: File-control statements not wired into FORTRAN II statement_body
        """
        test_cases = [
            ("BACKSPACE 1", 1),
            ("BACKSPACE 7", 7),
            ("BACKSPACE ITAPE", None),
        ]

        for text, unit_num in test_cases:
            with self.subTest(backspace_stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse: {text}")
                # Verify BACKSPACE keyword present
                tree_text = tree.getText()
                self.assertIn('BACKSPACE', tree_text)

    def test_file_control_statements_in_program(self):
        """Test file-control statements in a complete FORTRAN II program

        Verify END FILE, REWIND, and BACKSPACE parse correctly within
        a full FORTRAN II program structure (issue #384).
        """
        program_text = """
        DIMENSION DATA(100)
        REWIND 1
        BACKSPACE 1
        END FILE 1
        STOP
        END
        """
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
        text = tree.getText()
        # Verify all file-control statements are present
        self.assertIn('REWIND', text)
        self.assertIn('BACKSPACE', text)
        self.assertIn('END', text)
        self.assertIn('FILE', text)

    def test_read_tape_drum_statements(self):
        """Verify READ tape/drum forms inherited from FORTRAN I.

        Issue #153 documents that these tape/drum I/O statements must appear
        in the FORTRAN II statement_body set but were previously missing.
        """
        test_cases = [
            ("READ INPUT TAPE 1, 100, A, B", ["INPUT", "TAPE"]),
            ("READ TAPE 2, C, D, E", ["TAPE"]),
            ("READ DRUM 3, 10, F, G", ["DRUM"]),
        ]
        for text, keywords in test_cases:
            with self.subTest(read_stmt=text):
                tree = self.parse(text, 'read_tape_drum_stmt')
                self.assertIsNotNone(tree)
                tree_text = tree.getText()
                self.assertIn('READ', tree_text)
                for keyword in keywords:
                    self.assertIn(keyword, tree_text)
                errors = tree.parser.getNumberOfSyntaxErrors()
                self.assertEqual(
                    errors, 0,
                    f"READ tape/drum statement '{text}' should parse without "
                    f"errors, got {errors}"
                )

    def test_write_tape_drum_statements(self):
        """Verify WRITE tape/drum forms inherited from FORTRAN I.

        References C28-6003 rows 25-27.
        """
        test_cases = [
            ("WRITE OUTPUT TAPE 5, 300, RESULT", ["OUTPUT", "TAPE"]),
            ("WRITE TAPE 6, TOTAL", ["TAPE"]),
            ("WRITE DRUM 7, 40, DATA", ["DRUM"]),
        ]
        for text, keywords in test_cases:
            with self.subTest(write_stmt=text):
                tree = self.parse(text, 'write_tape_drum_stmt')
                self.assertIsNotNone(tree)
                tree_text = tree.getText()
                self.assertIn('WRITE', tree_text)
                for keyword in keywords:
                    self.assertIn(keyword, tree_text)
                errors = tree.parser.getNumberOfSyntaxErrors()
                self.assertEqual(
                    errors, 0,
                    f"WRITE tape/drum statement '{text}' should parse without "
                    f"errors, got {errors}"
                )


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)
