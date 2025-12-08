#!/usr/bin/env python3
"""
Test suite for FORTRAN 66 Parser - FORTRAN 66 (1966)
Tests for FORTRAN 66 standardization features PLUS FORTRAN IV (1962) data types
Includes: BLOCK DATA, standardized structure, LOGICAL, DOUBLE PRECISION, COMPLEX
"""

import sys
import unittest
from pathlib import Path

# Add tests root (for fixture_utils) and grammars directory to path for imports
ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT))
sys.path.append(str(ROOT.parent / "grammars"))

try:
    from antlr4 import InputStream, CommonTokenStream  # type: ignore
    from FORTRAN66Lexer import FORTRAN66Lexer  # type: ignore
    from FORTRAN66Parser import FORTRAN66Parser  # type: ignore
    from fixture_utils import load_fixture
except ImportError as e:
    print(f"Import error: {e}")
    FORTRAN66Parser = None


class TestFORTRAN66Parser(unittest.TestCase):
    """Test FORTRAN 66 (1966) parser rules - First Programming Language Standard"""
    
    def setUp(self):
        """Set up test fixtures"""
        if FORTRAN66Parser is None:
            self.skipTest("FORTRAN66Parser not available")
    
    def parse(self, text, rule_name='fortran66_program'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRAN66Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRAN66Parser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_block_data_subprogram(self):
        """Test BLOCK DATA subprogram (NEW in FORTRAN 66)"""
        test_cases = [
            "BLOCK DATA\nEND",
            "BLOCK DATA INIT\nCOMMON A, B, C\nEND",
            "BLOCK DATA VALUES\nCOMMON /MYBLK/ X, Y\nEND"
        ]
        
        for text in test_cases:
            with self.subTest(block_data=text):
                tree = self.parse(text, 'block_data_subprogram')
                self.assertIsNotNone(tree)
    
    def test_block_data_structure(self):
        """Test BLOCK DATA standardized structure (FORTRAN 66)"""
        # FORTRAN 66 standardized BLOCK DATA but DATA statements came in F77
        test_cases = [
            "BLOCK DATA",
            "BLOCK DATA INIT"
        ]
        
        for text in test_cases:
            with self.subTest(block_name=text):
                # Test that BLOCK and BLOCKDATA tokens are recognized
                input_stream = InputStream(text)
                lexer = FORTRAN66Lexer(input_stream)
                tokens = []
                while True:
                    token = lexer.nextToken()
                    if token.type == -1:  # EOF
                        break
                    tokens.append(token)
                self.assertGreater(len(tokens), 0)
    
    def test_standardized_program_structure(self):
        """Test standardized program structure (FORTRAN 66 achievement)"""
        # Main program
        main_program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "main_program.f",
        )
        
        tree = self.parse(main_program, 'main_program')
        self.assertIsNotNone(tree)
        
        # Function subprogram
        function_program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "function_program.f",
        )
        
        tree = self.parse(function_program, 'function_subprogram')
        self.assertIsNotNone(tree)
        
        # Subroutine subprogram
        subroutine_program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "subroutine_program.f",
        )
        
        tree = self.parse(subroutine_program, 'subroutine_subprogram')
        self.assertIsNotNone(tree)
    
    def test_type_declarations(self):
        """Test standardized type declarations (FORTRAN 66)"""
        test_cases = [
            "INTEGER I, J, K",
            "REAL X, Y, Z",
            "LOGICAL FLAG, READY",
            "DOUBLE PRECISION PI",
            "COMPLEX Z"
        ]
        
        for text in test_cases:
            with self.subTest(type_decl=text):
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)
    
    def test_machine_independence(self):
        """Test machine-independent features (FORTRAN 66 goal)"""
        # Standard program that should work on any FORTRAN 66 system
        standard_program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "standard_program.f",
        )
        
        tree = self.parse(standard_program, 'main_program')
        self.assertIsNotNone(tree)
    
    def test_first_standard_compliance(self):
        """Test compliance with first programming language standard"""
        # Program demonstrating FORTRAN 66 standardization
        test_program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "first_standard_demo.f",
        )
        
        # Test individual components
        lines = test_program.strip().split('\n')
        main_lines = []
        current_lines = []
        
        for line in lines:
            if line.strip().startswith('SUBROUTINE'):
                if main_lines:
                    main_text = '\n'.join(main_lines)
                    tree = self.parse(main_text, 'main_program')
                    self.assertIsNotNone(tree)
                main_lines = []
                current_lines = [line]
            elif line.strip() == 'END':
                current_lines.append(line)
                if current_lines[0].strip().startswith('SUBROUTINE'):
                    sub_text = '\n'.join(current_lines)
                    tree = self.parse(sub_text, 'subroutine_subprogram')
                    self.assertIsNotNone(tree)
                else:
                    main_lines.extend(current_lines)
                current_lines = []
            else:
                if not current_lines:
                    main_lines.append(line)
                else:
                    current_lines.append(line)

    # ====================================================================
    # FORTRAN IV (1962) FEATURES - merged into FORTRAN 66
    # ====================================================================
    
    def test_logical_data_type(self):
        """Test LOGICAL data type (merged from FORTRAN IV, 1962)"""
        test_cases = [
            "LOGICAL FLAG",
            "LOGICAL BOOL1, BOOL2", 
            "LOGICAL ARRAY(10)"
        ]
        
        for text in test_cases:
            with self.subTest(logical_declaration=text):
                # Test as a type specification
                tree = self.parse(text, 'type_spec')
                self.assertIsNotNone(tree)
    
    def test_double_precision_data_type(self):
        """Test DOUBLE PRECISION data type (merged from FORTRAN IV, 1962)"""
        test_cases = [
            "DOUBLE PRECISION X",
            "DOUBLE PRECISION A, B, C",
            "DOUBLE PRECISION MATRIX(5,5)"
        ]
        
        for text in test_cases:
            with self.subTest(double_precision=text):
                # Test as a type specification
                tree = self.parse(text, 'type_spec')
                self.assertIsNotNone(tree)
    
    def test_complex_data_type(self):
        """Test COMPLEX data type (merged from FORTRAN IV, 1962)"""
        test_cases = [
            "COMPLEX Z",
            "COMPLEX C1, C2",
            "COMPLEX IMPEDANCE(100)"
        ]
        
        for text in test_cases:
            with self.subTest(complex_declaration=text):
                # Test as a type specification
                tree = self.parse(text, 'type_spec')
                self.assertIsNotNone(tree)
    
    def test_logical_literals(self):
        """Test logical literals .TRUE. and .FALSE. (merged from FORTRAN IV, 1962)"""
        test_cases = [
            ".TRUE.",
            ".FALSE."
        ]
        
        for text in test_cases:
            with self.subTest(logical_literal=text):
                tree = self.parse(text, 'logical_literal')
                self.assertIsNotNone(tree)
    
    def test_logical_operators(self):
        """Test logical operators .AND., .OR., .NOT. (merged from FORTRAN IV, 1962)"""
        test_cases = [
            ".TRUE. .AND. .FALSE.",
            ".TRUE. .OR. .FALSE.", 
            ".NOT. .TRUE.",
            "A .EQ. B .AND. C .GT. D",
            ".NOT. (X .LT. Y)"
        ]
        
        for text in test_cases:
            with self.subTest(logical_expression=text):
                tree = self.parse(text, 'logical_expr')
                self.assertIsNotNone(tree)
    
    def test_relational_operators(self):
        """Test enhanced relational operators (merged from FORTRAN IV, 1962)"""
        test_cases = [
            "A .EQ. B",
            "X .NE. Y",
            "I .LT. J",
            "P .LE. Q", 
            "R .GT. S",
            "U .GE. V"
        ]
        
        for text in test_cases:
            with self.subTest(relational=text):
                tree = self.parse(text, 'relational_expr')
                self.assertIsNotNone(tree)
    
    def test_logical_if_statement(self):
        """Test logical IF statement (merged from FORTRAN IV, 1962)"""
        test_cases = [
            "IF (.TRUE.) GOTO 100",
            "IF (A .GT. B) X = Y",
            "IF (.NOT. FLAG) STOP",
            "IF (A .EQ. B .AND. C .NE. D) PRINT X"
        ]
        
        for text in test_cases:
            with self.subTest(logical_if=text):
                tree = self.parse(text, 'logical_if_stmt')
                self.assertIsNotNone(tree)
    
    def test_data_type_revolution_features(self):
        """Test the complete data type revolution merged from FORTRAN IV"""
        # Test that all new data types work in expressions and assignments
        test_cases = [
            # Logical operations
            "READY = .TRUE.",
            "FLAG = .FALSE.",
            "RESULT = READY .AND. FLAG",

            # Double precision literals
            "PI = 3.14159265358979D0",

            # Complex literals
            "Z = (1.0, 2.0)",
            "IMPEDANCE = (3.0, 4.0)",

            # Mixed mode arithmetic
            "X = I + 3.14",
        ]

        for test_code in test_cases:
            with self.subTest(code=test_code):
                tree = self.parse(test_code, 'assignment_stmt')
                self.assertIsNotNone(tree)
                self.assertTrue(len(str(tree.getText())) > 5)

    # ====================================================================
    # FORTRAN 66 EXTERNAL AND INTRINSIC STATEMENTS (X3.9-1966 Section 7.2)
    # ====================================================================

    def test_external_statement(self):
        """Test EXTERNAL statement (X3.9-1966 Section 7.2)"""
        test_cases = [
            "EXTERNAL F",
            "EXTERNAL MYFUNC",
            "EXTERNAL F, G",
            "EXTERNAL MYFUNC, MYSUB, HELPER",
        ]

        for text in test_cases:
            with self.subTest(external_stmt=text):
                tree = self.parse(text, 'external_stmt')
                self.assertIsNotNone(tree)

    def test_intrinsic_statement(self):
        """Test INTRINSIC statement (X3.9-1966 Section 7.2)"""
        test_cases = [
            "INTRINSIC SIN",
            "INTRINSIC COS",
            "INTRINSIC SIN, COS",
            "INTRINSIC SIN, COS, SQRT, ABS",
            "INTRINSIC ATAN2, MAX, MIN",
        ]

        for text in test_cases:
            with self.subTest(intrinsic_stmt=text):
                tree = self.parse(text, 'intrinsic_stmt')
                self.assertIsNotNone(tree)

    def test_external_intrinsic_in_program(self):
        """Test EXTERNAL and INTRINSIC as statement_body alternatives"""
        test_cases = [
            "EXTERNAL F",
            "INTRINSIC SIN",
        ]

        for text in test_cases:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_external_intrinsic_fixture(self):
        """Test program with EXTERNAL and INTRINSIC declarations"""
        program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "external_intrinsic.f",
        )

        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 66 AUXILIARY I/O STATEMENTS (X3.9-1966 Section 7.1.3.3)
    # ====================================================================

    def test_rewind_statement(self):
        """Test REWIND statement (X3.9-1966 Section 7.1.3.3)"""
        test_cases = [
            "REWIND 5",
            "REWIND 1",
            "REWIND 10",
            "REWIND I",
        ]

        for text in test_cases:
            with self.subTest(rewind_stmt=text):
                tree = self.parse(text, 'rewind_stmt')
                self.assertIsNotNone(tree)

    def test_backspace_statement(self):
        """Test BACKSPACE statement (X3.9-1966 Section 7.1.3.3)"""
        test_cases = [
            "BACKSPACE 5",
            "BACKSPACE 1",
            "BACKSPACE 10",
            "BACKSPACE N",
        ]

        for text in test_cases:
            with self.subTest(backspace_stmt=text):
                tree = self.parse(text, 'backspace_stmt')
                self.assertIsNotNone(tree)

    def test_endfile_statement(self):
        """Test ENDFILE statement (X3.9-1966 Section 7.1.3.3)"""
        test_cases = [
            "ENDFILE 5",
            "ENDFILE 1",
            "ENDFILE 10",
            "ENDFILE IUNIT",
        ]

        for text in test_cases:
            with self.subTest(endfile_stmt=text):
                tree = self.parse(text, 'endfile_stmt')
                self.assertIsNotNone(tree)

    def test_auxiliary_io_in_statement_body(self):
        """Test auxiliary I/O statements as statement_body alternatives"""
        test_cases = [
            "REWIND 5",
            "BACKSPACE 5",
            "ENDFILE 5",
        ]

        for text in test_cases:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_auxiliary_io_fixture(self):
        """Test program with auxiliary I/O statements"""
        program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "auxiliary_io.f",
        )

        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 66 GO TO ASSIGNMENT AND ASSIGNED GO TO (X3.9-1966 Sections 7.1.1.3/7.1.2.1.2)
    # ====================================================================

    def test_assign_statement(self):
        """Test GO TO assignment statement (X3.9-1966 Section 7.1.1.3)"""
        test_cases = [
            "ASSIGN 100 TO N",
            "ASSIGN 200 TO I",
            "ASSIGN 10 TO LABEL",
            "ASSIGN 99999 TO K",
        ]

        for text in test_cases:
            with self.subTest(assign_stmt=text):
                tree = self.parse(text, 'assign_stmt')
                self.assertIsNotNone(tree)

    def test_assigned_goto_statement(self):
        """Test assigned GO TO statement (X3.9-1966 Section 7.1.2.1.2)"""
        test_cases = [
            "GOTO N, (100, 200)",
            "GOTO I, (10, 20, 30)",
            "GOTO LABEL, (100)",
            "GOTO K, (100, 200, 300, 400, 500)",
        ]

        for text in test_cases:
            with self.subTest(assigned_goto=text):
                tree = self.parse(text, 'assigned_goto_stmt')
                self.assertIsNotNone(tree)

    def test_assign_in_statement_body(self):
        """Test ASSIGN as statement_body alternative"""
        test_cases = [
            "ASSIGN 100 TO N",
            "ASSIGN 200 TO I",
        ]

        for text in test_cases:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_assigned_goto_in_statement_body(self):
        """Test assigned GO TO as statement_body alternative"""
        test_cases = [
            "GOTO N, (100, 200)",
            "GOTO I, (10, 20, 30)",
        ]

        for text in test_cases:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_assign_goto_fixture(self):
        """Test program with ASSIGN and assigned GO TO statements"""
        program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "assign_goto.f",
        )

        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 66 DATA STATEMENT (X3.9-1966 Section 7.2)
    # ====================================================================

    def test_data_statement_simple(self):
        """Test simple DATA statement (X3.9-1966 Section 7.2)"""
        test_cases = [
            "DATA X /1.0/",
            "DATA I /1/",
            "DATA FLAG /.TRUE./",
            "DATA X, Y /1.0, 2.0/",
            "DATA I, J, K /1, 2, 3/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                tree = self.parse(text, 'data_stmt')
                self.assertIsNotNone(tree)

    def test_data_statement_repeat_count(self):
        """Test DATA statement with repeat count (X3.9-1966 Section 7.2)"""
        test_cases = [
            "DATA A /3*0.0/",
            "DATA ARR /10*1/",
            "DATA X, Y, Z /3*0.0/",
            "DATA MAT /25*1.0/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                tree = self.parse(text, 'data_stmt')
                self.assertIsNotNone(tree)

    def test_data_statement_multiple_sets(self):
        """Test DATA statement with multiple initialization sets"""
        test_cases = [
            "DATA X /1.0/, Y /2.0/",
            "DATA I /1/, J /2/, K /3/",
            "DATA A, B /1.0, 2.0/, C /3.0/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                tree = self.parse(text, 'data_stmt')
                self.assertIsNotNone(tree)

    def test_data_statement_signed_constants(self):
        """Test DATA statement with signed constants"""
        test_cases = [
            "DATA X /-1.0/",
            "DATA I /+5/",
            "DATA A, B /+1.0, -2.0/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                tree = self.parse(text, 'data_stmt')
                self.assertIsNotNone(tree)

    def test_data_statement_array_elements(self):
        """Test DATA statement with array elements"""
        test_cases = [
            "DATA ARR(1) /1.0/",
            "DATA ARR(1), ARR(2) /1.0, 2.0/",
            "DATA MAT(1,1), MAT(2,2) /1.0, 1.0/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                tree = self.parse(text, 'data_stmt')
                self.assertIsNotNone(tree)

    def test_data_statement_implied_do(self):
        """Test DATA statement with implied DO loop (X3.9-1966 Section 7.2)"""
        test_cases = [
            "DATA (ARR(I), I=1,10) /10*0.0/",
            "DATA (A(I), I=1,5) /1.0, 2.0, 3.0, 4.0, 5.0/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                tree = self.parse(text, 'data_stmt')
                self.assertIsNotNone(tree)

    def test_data_statement_in_statement_body(self):
        """Test DATA as statement_body alternative"""
        test_cases = [
            "DATA X /1.0/",
            "DATA I, J /1, 2/",
            "DATA ARR /10*0.0/",
        ]

        for text in test_cases:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_data_statement_fixture(self):
        """Test program with DATA statements"""
        program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "data_stmt.f",
        )

        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)

    def test_data_in_block_data(self):
        """Test DATA statement in BLOCK DATA subprogram"""
        test_cases = [
            "BLOCKDATA\nCOMMON X, Y, Z\nDATA X, Y, Z /1.0, 2.0, 3.0/\nEND",
            "BLOCKDATA INIT\nCOMMON /BLK/ A, B\nDATA A, B /0.0, 0.0/\nEND",
        ]

        for text in test_cases:
            with self.subTest(block_data=text):
                tree = self.parse(text, 'block_data_subprogram')
                self.assertIsNotNone(tree)


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)
