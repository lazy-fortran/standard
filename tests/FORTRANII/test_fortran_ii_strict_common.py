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


class TestStrictCommon(unittest.TestCase):
    """Test strict 1958 COMMON mode (blank COMMON only, per issue #156)"""

    def setUp(self):
        """Set up test fixtures"""
        if FORTRANIIParser is None:
            self.skipTest("FORTRANIIParser not available")

    def parse(self, text, rule_name):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRANIILexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANIIParser(token_stream)

        rule_method = getattr(parser, rule_name)
        return rule_method()

    def test_blank_common_parses_in_relaxed_mode(self):
        """Blank COMMON should parse in relaxed mode (common_stmt)"""
        test_cases = [
            "COMMON A, B, C",
            "COMMON X",
            "COMMON ARRAY(100)",
            "COMMON A, B, ARR(10, 10)",
        ]
        for text in test_cases:
            with self.subTest(text=text):
                tree = self.parse(text, 'common_stmt')
                self.assertIsNotNone(tree)
                self.assertIn('COMMON', tree.getText())

    def test_blank_common_parses_in_strict_mode(self):
        """Blank COMMON should parse in strict mode (common_stmt_strict)"""
        test_cases = [
            "COMMON A, B, C",
            "COMMON X",
            "COMMON ARRAY(100)",
            "COMMON A, B, ARR(10, 10)",
        ]
        for text in test_cases:
            with self.subTest(text=text):
                tree = self.parse(text, 'common_stmt_strict')
                self.assertIsNotNone(tree)
                self.assertIn('COMMON', tree.getText())

    def test_named_common_parses_in_relaxed_mode(self):
        """Named COMMON should parse in relaxed mode (common_stmt)"""
        test_cases = [
            "COMMON /BLOCK1/ A, B, C",
            "COMMON /DATA/ X",
            "COMMON /WORK/ ARRAY(100)",
        ]
        for text in test_cases:
            with self.subTest(text=text):
                tree = self.parse(text, 'common_stmt')
                self.assertIsNotNone(tree)
                self.assertIn('COMMON', tree.getText())

    def test_named_common_fails_in_strict_mode(self):
        """Named COMMON should NOT parse via common_stmt_strict

        The strict mode rule only allows blank COMMON per the 1958 spec.
        Named COMMON blocks (/name/) were introduced in FORTRAN 66.
        """
        test_cases = [
            "COMMON /BLOCK1/ A, B, C",
            "COMMON /DATA/ X",
            "COMMON /WORK/ ARRAY(100)",
        ]
        for text in test_cases:
            with self.subTest(text=text):
                input_stream = InputStream(text)
                lexer = FORTRANIILexer(input_stream)
                token_stream = CommonTokenStream(lexer)
                parser = FORTRANIIParser(token_stream)
                tree = parser.common_stmt_strict()
                errors = parser.getNumberOfSyntaxErrors()
                self.assertGreater(
                    errors, 0,
                    f"Named COMMON '{text}' should fail in strict mode"
                )

    def test_hardware_if_sense_switch_in_fortran_ii(self):
        """Test IF (SENSE SWITCH i) statements in FORTRAN II context

        Hardware IF statements from FORTRAN I (C28-6003 Appendix B) should
        parse in FORTRAN II per C28-6000-2 Appendix A statement set.
        Issue #395: Hardware IF and SENSE LIGHT statements not wired into statement_body
        """
        test_cases = [
            "IF (SENSE SWITCH 1) 100, 200",
            "IF (SENSE SWITCH 2) 150, 250",
            "IF (SENSE SWITCH 3) 300, 400",
        ]
        for text in test_cases:
            with self.subTest(if_stmt=text):
                tree = self.parse(text, 'if_stmt_sense_switch')
                self.assertIsNotNone(tree)
                self.assertIn('SENSE', tree.getText())
                self.assertIn('SWITCH', tree.getText())

    def test_hardware_if_sense_light_in_fortran_ii(self):
        """Test IF (SENSE LIGHT i) statements in FORTRAN II context

        Hardware IF statements from FORTRAN I (C28-6003 Appendix B) should
        parse in FORTRAN II per C28-6000-2 Appendix A statement set.
        Issue #395: Hardware IF and SENSE LIGHT statements not wired into statement_body
        """
        test_cases = [
            "IF (SENSE LIGHT 1) 100, 200",
            "IF (SENSE LIGHT 2) 150, 250",
            "IF (SENSE LIGHT 4) 300, 400",
        ]
        for text in test_cases:
            with self.subTest(if_stmt=text):
                tree = self.parse(text, 'if_stmt_sense_light')
                self.assertIsNotNone(tree)
                self.assertIn('SENSE', tree.getText())
                self.assertIn('LIGHT', tree.getText())

    def test_hardware_if_accumulator_overflow_in_fortran_ii(self):
        """Test IF ACCUMULATOR OVERFLOW statements in FORTRAN II context

        Hardware IF statements from FORTRAN I (C28-6003 Appendix B) should
        parse in FORTRAN II per C28-6000-2 Appendix A statement set.
        Issue #395: Hardware IF and SENSE LIGHT statements not wired into statement_body
        """
        test_cases = [
            "IF ACCUMULATOR OVERFLOW 100, 200",
            "IF ACCUMULATOR OVERFLOW 150, 250",
        ]
        for text in test_cases:
            with self.subTest(if_stmt=text):
                tree = self.parse(text, 'if_stmt_accumulator_overflow')
                self.assertIsNotNone(tree)
                self.assertIn('ACCUMULATOR', tree.getText())
                self.assertIn('OVERFLOW', tree.getText())

    def test_hardware_if_quotient_overflow_in_fortran_ii(self):
        """Test IF QUOTIENT OVERFLOW statements in FORTRAN II context

        Hardware IF statements from FORTRAN I (C28-6003 Appendix B) should
        parse in FORTRAN II per C28-6000-2 Appendix A statement set.
        Issue #395: Hardware IF and SENSE LIGHT statements not wired into statement_body
        """
        test_cases = [
            "IF QUOTIENT OVERFLOW 100, 200",
            "IF QUOTIENT OVERFLOW 150, 250",
        ]
        for text in test_cases:
            with self.subTest(if_stmt=text):
                tree = self.parse(text, 'if_stmt_quotient_overflow')
                self.assertIsNotNone(tree)
                self.assertIn('QUOTIENT', tree.getText())
                self.assertIn('OVERFLOW', tree.getText())

    def test_hardware_if_divide_check_in_fortran_ii(self):
        """Test IF DIVIDE CHECK statements in FORTRAN II context

        Hardware IF statements from FORTRAN I (C28-6003 Appendix B) should
        parse in FORTRAN II per C28-6000-2 Appendix A statement set.
        Issue #395: Hardware IF and SENSE LIGHT statements not wired into statement_body
        """
        test_cases = [
            "IF DIVIDE CHECK 100, 200",
            "IF DIVIDE CHECK 150, 250",
        ]
        for text in test_cases:
            with self.subTest(if_stmt=text):
                tree = self.parse(text, 'if_stmt_divide_check')
                self.assertIsNotNone(tree)
                self.assertIn('DIVIDE', tree.getText())
                self.assertIn('CHECK', tree.getText())

    def test_two_label_if_statements_in_fortran_ii(self):
        """Test two-label IF statements in FORTRAN II context

        Two-label IF statements from FORTRAN I (C28-6003 Appendix B rows 9-10)
        should parse in FORTRAN II per C28-6000-2 backward compatibility guarantee.
        Issue #603: Missing two-label IF statement forms from FORTRAN I inheritance
        """
        test_cases = [
            ("IF (X) 10, 20", "two-way positive (X >= 0)"),
            ("IF (Y) 100, 200", "two-way zero (Y = 0)"),
            ("IF (A+B) 5, 15", "two-way with expression"),
            ("IF (SIN(Z)) 30, 40", "two-way with function call"),
        ]
        for text, description in test_cases:
            with self.subTest(two_label_if=text, desc=description):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree, f"Failed to parse two-label IF: {text}")
                # Verify IF keyword
                self.assertIn('IF', tree.getText())
                # Verify parenthesized expression
                self.assertIn('(', tree.getText())
                self.assertIn(')', tree.getText())
                # Verify labels
                tree_text = tree.getText()
                # Simple label extraction - check for numbers
                self.assertGreaterEqual(tree_text.count(','), 1,
                                      f"Two-label IF should have comma separator: {text}")

    def test_two_label_if_in_fortran_ii_program(self):
        """Test two-label IF statement in complete FORTRAN II subroutine

        Demonstrates the example from issue #603:
        A subroutine using IF (X) n1, n2 to branch on the sign of X.
        """
        subroutine_text = """
      SUBROUTINE SIGN_TEST(X)
C     Branch on sign of X
      IF (X) 10, 20
10    CONTINUE
C     X is positive or zero
      RETURN
20    CONTINUE
C     X is negative
      RETURN
      END
        """
        tree = self.parse(subroutine_text, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
        text = tree.getText()
        # Verify subroutine structure
        self.assertIn('SUBROUTINE', text)
        self.assertIn('SIGN_TEST', text)
        self.assertIn('X', text)
        # Verify two-label IF statement
        self.assertIn('IF', text)
        # Verify labels and CONTINUE statements
        self.assertIn('10', text)
        self.assertIn('20', text)
        self.assertIn('CONTINUE', text)
        # Verify RETURN statements
        self.assertEqual(text.count('RETURN'), 2)

    def test_sense_light_statement_in_fortran_ii(self):
        """Test SENSE LIGHT statements in FORTRAN II context

        SENSE LIGHT statement from FORTRAN I (C28-6003 Appendix B row 11)
        should parse in FORTRAN II per C28-6000-2 Appendix A statement set.
        Issue #395: Hardware IF and SENSE LIGHT statements not wired into statement_body
        """
        test_cases = [
            "SENSE LIGHT 1",
            "SENSE LIGHT 2",
            "SENSE LIGHT 3",
            "SENSE LIGHT 4",
        ]
        for text in test_cases:
            with self.subTest(sense_light=text):
                tree = self.parse(text, 'sense_light_stmt')
                self.assertIsNotNone(tree)
                self.assertIn('SENSE', tree.getText())
                self.assertIn('LIGHT', tree.getText())

    def test_strict_program_entry_point_exists(self):
        """Verify strict mode entry points are available"""
        import os
        grammar_path = os.path.join('grammars', 'src', 'FORTRANIIParser.g4')
        with open(grammar_path, 'r') as f:
            content = f.read()

        strict_rules = [
            'fortran_program_strict',
            'main_program_strict',
            'subroutine_subprogram_strict',
            'function_subprogram_strict',
            'statement_list_strict',
            'statement_strict',
            'statement_body_strict',
            'common_stmt_strict',
        ]
        for rule in strict_rules:
            with self.subTest(rule=rule):
                self.assertIn(
                    rule, content,
                    f"Grammar should define strict mode rule: {rule}"
                )

    def test_blank_common_fixture_parses_strict(self):
        """Blank COMMON fixture should parse in strict mode"""
        fixture = load_fixture(
            "FORTRANII",
            "test_strict_common",
            "blank_common.f",
        )
        tree = self.parse(fixture, 'subroutine_subprogram_strict')
        self.assertIsNotNone(tree)
        self.assertIn('COMMON', tree.getText())
        errors_count = tree.parser.getNumberOfSyntaxErrors()
        self.assertEqual(errors_count, 0)

    def test_named_common_fixture_fails_strict(self):
        """Named COMMON fixture should fail in strict mode"""
        fixture = load_fixture(
            "FORTRANII",
            "test_strict_common",
            "named_common.f",
        )
        input_stream = InputStream(fixture)
        lexer = FORTRANIILexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANIIParser(token_stream)
        parser.subroutine_subprogram_strict()
        errors = parser.getNumberOfSyntaxErrors()
        self.assertGreater(
            errors, 0,
            "Named COMMON fixture should fail in strict 1958 mode"
        )

    def test_named_common_fixture_parses_relaxed(self):
        """Named COMMON fixture should parse in relaxed mode"""
        fixture = load_fixture(
            "FORTRANII",
            "test_strict_common",
            "named_common.f",
        )
        tree = self.parse(fixture, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
        errors_count = tree.parser.getNumberOfSyntaxErrors()
        self.assertEqual(errors_count, 0)

    def test_main_blank_common_parses_strict(self):
        """Main program with blank COMMON should parse in strict mode"""
        fixture = load_fixture(
            "FORTRANII",
            "test_strict_common",
            "main_blank_common.f",
        )
        tree = self.parse(fixture, 'main_program_strict')
        self.assertIsNotNone(tree)
        errors_count = tree.parser.getNumberOfSyntaxErrors()
        self.assertEqual(errors_count, 0)

    def test_format_statement_basic(self):
        """Test FORMAT statement with basic descriptors (ISO/ANSI section 13)

        Per IBM Form C28-6000-2 Appendix A, FORMAT statements are inherited
        from FORTRAN I and consist of a label followed by FORMAT keyword and
        parenthesized format specification.
        """
        test_cases = [
            ("10 FORMAT(I5, F10.2, E12.4)", ["I5", "F10.2", "E12.4"]),
            ("20 FORMAT(A20, I10)", ["A20", "I10"]),
            ("30 FORMAT(/)", []),
        ]

        for text, descriptors in test_cases:
            with self.subTest(format_stmt=text):
                tree = self.parse(text, 'statement')
                self.assertIsNotNone(tree)
                tree_text = tree.getText()
                self.assertIn('FORMAT', tree_text)
                # Verify that format contains at least the expected descriptors
                for descriptor in descriptors:
                    self.assertIn(descriptor, tree_text)

    def test_format_statement_with_hollerith(self):
        """Test FORMAT statement with Hollerith constants (legacy text format)

        Per ISO 1539:1980 Section 13.1.2, Hollerith constants in FORMAT
        specify literal text output. Format: nHtext where n is character count.
        """
        test_cases = [
            ("30 FORMAT(5HHELLO, I5)", "HHELLO"),
            ("40 FORMAT(1H1, 10X, 10HTITLE LINE)", "HTITLE"),
            ("50 FORMAT(4HTEST)", "HTEST"),
        ]

        for text, hollerith_part in test_cases:
            with self.subTest(format_hollerith=text):
                tree = self.parse(text, 'statement')
                self.assertIsNotNone(tree)
                tree_text = tree.getText()
                self.assertIn('FORMAT', tree_text)
                self.assertIn(hollerith_part, tree_text)

    def test_format_statement_in_complete_program(self):
        """Test FORMAT statement in a complete FORTRAN II program

        Verify FORMAT statements parse correctly when part of a full
        FORTRAN II program structure with variables and I/O operations.
        """
        program_text = """
        X = 1.0
        Y = 2.0
        10 FORMAT(I5, F10.2, E12.4)
        20 FORMAT(A20, I10)
        WRITE X, Y
        END
        """

        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
        tree_text = tree.getText()

        # Verify FORMAT statements are present
        self.assertIn('FORMAT', tree_text)
        self.assertIn('I5', tree_text)
        self.assertIn('F10.2', tree_text)
        self.assertIn('E12.4', tree_text)

        # Verify no syntax errors
        errors = tree.parser.getNumberOfSyntaxErrors()
        self.assertEqual(errors, 0,
                        f"FORMAT statements should parse without errors, got {errors}")



