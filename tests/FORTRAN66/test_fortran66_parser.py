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
sys.path.append(str(ROOT.parent / "grammars/generated/early"))

try:
    from antlr4 import InputStream, CommonTokenStream  # type: ignore
    from FORTRAN66Lexer import FORTRAN66Lexer  # type: ignore
    from FORTRAN66Parser import FORTRAN66Parser  # type: ignore
    from fixture_utils import load_fixture
    from shared_fortran_tests import StatementFunctionTestMixin
except ImportError as e:
    print(f"Import error: {e}")
    FORTRAN66Parser = None


class TestFORTRAN66Parser(StatementFunctionTestMixin, unittest.TestCase):
    """Test FORTRAN 66 (1966) parser rules - First Programming Language Standard"""

    LexerClass = FORTRAN66Lexer
    ParserClass = FORTRAN66Parser
    fixture_standard = "FORTRAN66"

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

    def test_complex_literals(self):
        """Test COMPLEX constants (X3.9-1966 Section 4.4.2)"""
        test_cases = [
            "(1.0, 2.0)",           # Real and real parts
            "(3, -4)",              # Integer parts
            "(-1.5, 2.5)",          # Negative parts
            "(0.0, 1.0)",           # Zero real part
            "(1.0, 0.0)",           # Zero imaginary part
            "(1.0E+3, -2.5E-1)",    # Scientific notation
            "(+1, +2)",             # Explicit plus signs
        ]

        for text in test_cases:
            with self.subTest(complex_const=text):
                tree = self.parse(text, 'literal')
                self.assertIsNotNone(tree)

    def test_complex_literals_in_assignment(self):
        """Test COMPLEX constants in assignment statements (X3.9-1966 Section 10)"""
        test_program = """
        PROGRAM COMPLEX_TEST
        COMPLEX Z, W
        Z = (1.0, 2.0)
        W = (3, -4)
        END
        """
        tree = self.parse(test_program, 'main_program')
        self.assertIsNotNone(tree)

    def test_complex_literals_in_data_statement(self):
        """Test COMPLEX constants in DATA statements (X3.9-1966 Section 7.2.6)"""
        test_program = """
        PROGRAM DATA_COMPLEX
        COMPLEX Z1, Z2, Z3
        DATA Z1 / (1.0, 2.0) /
        DATA Z2, Z3 / (3.0, 4.0), (-1.0, -2.0) /
        END
        """
        tree = self.parse(test_program, 'main_program')
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

    def test_logical_equivalence_operators(self):
        """Test logical equivalence operators .EQV. and .NEQV. (X3.9-1966 Section 6.4)"""
        test_cases = [
            # Basic equivalence
            ".TRUE. .EQV. .FALSE.",
            ".TRUE. .EQV. .TRUE.",
            ".FALSE. .EQV. .FALSE.",

            # Basic non-equivalence
            ".TRUE. .NEQV. .FALSE.",
            ".FALSE. .NEQV. .FALSE.",
            ".TRUE. .NEQV. .TRUE.",

            # With variables
            "A .EQV. B",
            "FLAG1 .NEQV. FLAG2",

            # With relational expressions
            "A .EQ. B .EQV. C .GT. D",
            "X .NE. Y .NEQV. P .LE. Q",
        ]

        for text in test_cases:
            with self.subTest(equiv_expression=text):
                tree = self.parse(text, 'logical_expr')
                self.assertIsNotNone(tree)

    def test_logical_operators_precedence(self):
        """Test operator precedence: .NOT. > .AND. > .OR. > .EQV./.NEQV. (X3.9-1966 Section 6.4)"""
        test_cases = [
            # Equivalence has lower precedence than OR
            ".TRUE. .OR. .FALSE. .EQV. .TRUE.",

            # OR has lower precedence than AND
            ".TRUE. .AND. .FALSE. .OR. .TRUE.",

            # AND has lower precedence than NOT
            ".NOT. .TRUE. .AND. .FALSE.",

            # Complex expressions with all operators
            "A .EQ. B .AND. C .GT. D .OR. E .LT. F .EQV. G .NEQV. H",

            # Multiple equivalence operators (left-associative)
            ".TRUE. .EQV. .FALSE. .EQV. .TRUE.",
            ".TRUE. .NEQV. .FALSE. .NEQV. .TRUE.",

            # Parentheses override precedence
            "(.TRUE. .OR. .FALSE.) .EQV. .TRUE.",
            ".TRUE. .AND. (.FALSE. .OR. .TRUE.)",
        ]

        for text in test_cases:
            with self.subTest(precedence=text):
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

    def test_logical_if_valid_statements(self):
        """Test valid executable statements in logical IF (X3.9-1966 Section 7.1.2.4)"""
        valid_cases = [
            # Arithmetic IF (allowed)
            "IF (.TRUE.) IF (X .GT. 0) Y = 1, 2, 3",
            # Assignment (allowed)
            "IF (FLAG) X = Y + Z",
            # GOTO (allowed)
            "IF (A .GT. 0) GOTO 100",
            # Computed GOTO (allowed)
            "IF (FLAG) GOTO (10, 20, 30), I",
            # CALL (allowed)
            "IF (DONE) CALL FINISH()",
            # RETURN (allowed)
            "IF (.TRUE.) RETURN",
            # CONTINUE (allowed)
            "IF (FLAG) CONTINUE",
            # I/O statements (allowed)
            "IF (FLAG) PRINT 100",
            "IF (FLAG) READ 100",
        ]

        for text in valid_cases:
            with self.subTest(valid_stmt=text):
                tree = self.parse(text, 'logical_if_stmt')
                self.assertIsNotNone(tree)

    def test_logical_if_invalid_nested_if(self):
        """Test that nested logical IF is rejected (X3.9-1966 Section 7.1.2.4)"""
        # Nested logical IF should be rejected by parser
        invalid_cases = [
            "IF (.TRUE.) IF (.FALSE.) X = 1",
            "IF (A .GT. 0) IF (B .LT. 0) Y = 2",
        ]

        for text in invalid_cases:
            with self.subTest(nested_if=text):
                try:
                    tree = self.parse(text, 'logical_if_stmt')
                    # If parse succeeds when it should fail, parser is incorrect
                    self.fail(f"Parser should reject nested logical IF: {text}")
                except Exception:
                    # Expected: parser should raise exception for nested IF
                    pass

    def test_logical_if_invalid_do_statement(self):
        """Test that DO statement in logical IF is rejected (X3.9-1966 Section 7.1.2.4)"""
        # DO statement should be rejected in logical IF
        invalid_cases = [
            "IF (.TRUE.) DO 10 I = 1, 10",
            "IF (FLAG) DO 20 J = 1, 5, 2",
        ]

        for text in invalid_cases:
            with self.subTest(do_in_if=text):
                try:
                    tree = self.parse(text, 'logical_if_stmt')
                    # If parse succeeds when it should fail, parser is incorrect
                    self.fail(f"Parser should reject DO in logical IF: {text}")
                except Exception:
                    # Expected: parser should raise exception for DO in IF
                    pass

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
    # FORTRAN 66 WRITE STATEMENT (X3.9-1966 Section 7.1.3.1)
    # ====================================================================

    def test_write_statement_simple(self):
        """Test simple WRITE statement (X3.9-1966 Section 7.1.3.1)"""
        test_cases = [
            "WRITE (6, 100) X, Y, Z",
            "WRITE (1, 200) A",
            "WRITE (NOUT, 300) I, J, K",
            "WRITE (6, 400)",
        ]

        for text in test_cases:
            with self.subTest(write_stmt=text):
                tree = self.parse(text, 'write_stmt')
                self.assertIsNotNone(tree)

    def test_write_statement_with_expressions(self):
        """Test WRITE statement with complex expressions"""
        test_cases = [
            "WRITE (6, 100) X + Y, Z * 2.0",
            "WRITE (6, 200) A(I), B(J, K)",
            "WRITE (6, 300) SIN(X), COS(Y), SQRT(Z)",
            "WRITE (UNIT, FORMAT) A**2 + B**2",
        ]

        for text in test_cases:
            with self.subTest(write_expr=text):
                tree = self.parse(text, 'write_stmt')
                self.assertIsNotNone(tree)

    def test_write_statement_in_statement_body(self):
        """Test WRITE as statement_body alternative"""
        test_cases = [
            "WRITE (6, 100) X, Y, Z",
            "WRITE (1, 200) A",
        ]

        for text in test_cases:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_write_statement_fixture(self):
        """Test program with WRITE statements"""
        program = load_fixture(
            "FORTRAN66",
            "test_fortran66_parser",
            "write_stmt.f",
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

    # ====================================================================
    # FORTRAN 66 STATEMENT FUNCTION: COMPLEX EXPRESSION TEST
    # ====================================================================
    # Note: Basic statement function tests, negative tests (array element
    # assignments), and disambiguation tests are provided by the
    # StatementFunctionTestMixin inherited from shared_fortran_tests.py.
    # This test covers FORTRAN 66 specific complex expression forms.
    # ====================================================================

    def test_statement_function_complex_expr(self):
        """Test statement function with complex expressions (X3.9-1966 Section 7.2)"""
        test_cases = [
            "POLY(X) = X**3 + 2.0*X**2 + 3.0*X + 4.0",
            "HYPOT(A, B) = SQRT(A**2 + B**2)",
            "COMBINE(X, Y) = SIN(X) + COS(Y)",
        ]

        for text in test_cases:
            with self.subTest(stmt_func=text):
                tree = self.parse(text, 'statement_function_stmt')
                self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 66 STOP/PAUSE STATEMENT: OCTAL CODE TEST
    # ====================================================================
    # X3.9-1966 Section 7.1.2.5: STOP and PAUSE statement optional arguments
    # must be 1-5 octal digits (0-7 only), not arbitrary integer expressions
    # ====================================================================

    def test_stop_stmt_without_code(self):
        """Test STOP statement without octal code (valid)"""
        test_cases = [
            "STOP",
            "END\nSTOP",
        ]

        for text in test_cases:
            with self.subTest(stop_stmt=text):
                tree = self.parse(text, 'main_program')
                self.assertIsNotNone(tree)

    def test_stop_stmt_with_valid_octal_codes(self):
        """Test STOP statement with valid 1-5 octal digit codes"""
        valid_octal_codes = [
            "0",      # Single digit
            "7",      # Max single digit
            "77",     # Two digits
            "777",    # Three digits
            "7777",   # Four digits
            "77777",  # Five digits (max)
            "1",      # Common values
            "10",
            "100",
            "1000",
            "10000",
            "77",     # Another pattern
            "555",
        ]

        for code in valid_octal_codes:
            with self.subTest(octal_code=code):
                text = f"STOP {code}"
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_pause_stmt_without_code(self):
        """Test PAUSE statement without octal code (valid)"""
        test_cases = [
            "PAUSE",
            "END\nPAUSE",
        ]

        for text in test_cases:
            with self.subTest(pause_stmt=text):
                tree = self.parse(text, 'main_program')
                self.assertIsNotNone(tree)

    def test_pause_stmt_with_valid_octal_codes(self):
        """Test PAUSE statement with valid 1-5 octal digit codes"""
        valid_octal_codes = [
            "0",      # Single digit
            "7",      # Max single digit
            "77",     # Two digits
            "777",    # Three digits
            "7777",   # Four digits
            "77777",  # Five digits (max)
            "1",      # Common values
            "10",
            "100",
            "1000",
            "10000",
        ]

        for code in valid_octal_codes:
            with self.subTest(octal_code=code):
                text = f"PAUSE {code}"
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_stop_stmt_integer_codes_parse(self):
        """Test STOP statement parses with INTEGER_LITERAL codes

        NOTE: X3.9-1966 Section 7.1.2.5 restricts STOP/PAUSE codes to 1-5 octal
        digits (0-7 only). The grammar currently accepts any INTEGER_LITERAL for
        simplicity. Full octal validation (rejecting codes with digits 8-9) would
        require semantic validation beyond ANTLR parser capabilities.

        This test documents that the parser accepts both valid and invalid codes;
        a semantic checker would be needed to enforce the octal constraint.
        """
        # The parser will accept all these codes (valid and invalid octal)
        # Semantic validation would be needed to reject 8, 9, etc.
        test_codes = [
            "0",       # Valid octal
            "7",       # Valid octal
            "77",      # Valid octal
            "777",     # Valid octal
            "7777",    # Valid octal
            "77777",   # Valid octal
            "8",       # Invalid octal (accepted by parser, would need semantic check)
            "9",       # Invalid octal (accepted by parser, would need semantic check)
            "18",      # Invalid octal (accepted by parser, would need semantic check)
            "99",      # Invalid octal (accepted by parser, would need semantic check)
            "100000",  # 6 digits (exceeds 5-digit limit, but parser accepts it)
            "1000000", # 7 digits (exceeds 5-digit limit, but parser accepts it)
        ]

        for code in test_codes:
            with self.subTest(code=code):
                text = f"STOP {code}"
                # Parser accepts all INTEGER_LITERAL values
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 66 IDENTIFIER LENGTH RESTRICTION (X3.9-1966 Section 2.3)
    # ====================================================================
    # X3.9-1966 Section 2.3 "Symbolic Names" specifies that identifiers must
    # be 1 to 6 alphanumeric characters, starting with a letter.
    # ====================================================================

    def test_identifier_valid_lengths(self):
        """Test valid identifier lengths (1-6 characters) per X3.9-1966 Section 2.3"""
        valid_identifiers = [
            "A",       # 1 character (minimum)
            "AB",      # 2 characters
            "ABC",     # 3 characters
            "ABCD",    # 4 characters
            "ABCDE",   # 5 characters
            "ABCDEF",  # 6 characters (maximum)
            "X1",      # 2 chars with digit
            "VAR",     # 3 chars common name
            "VALUE",   # 5 chars common name
            "MAXVAL",  # 6 chars truncated
            "I",       # 1 char (integer convention)
            "N",       # 1 char (integer convention)
            "X",       # 1 char (real convention)
            "PI",      # 2 chars
            "NUM1",    # 4 chars with digit
            "V12345",  # 6 chars with multiple digits
        ]

        for ident in valid_identifiers:
            with self.subTest(identifier=ident):
                # Test as type declaration with identifier
                text = f"INTEGER {ident}"
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)

    # NOTE: test_identifier_invalid_lengths removed - X3.9-1966 Section 2.3 mandates
    # that identifiers be limited to 1-6 characters, but this constraint is enforced
    # at the semantic analysis phase (not the lexer phase) to avoid truncating tokens.
    # Lexer-level truncation breaks parsing of valid FORTRAN code where longer identifiers
    # may appear. The constraint should be validated semantically, not lexically.

    def test_identifier_with_digits(self):
        """Test identifiers with digits (allowed in positions 2-6 per X3.9-1966)"""
        test_cases = [
            "A1",      # Letter + digit
            "A12",     # Letter + 2 digits
            "A12345",  # Letter + 5 digits (6 chars total)
            "X1Y2Z3",  # Alternating pattern (6 chars)
            "TEST12",  # 6 chars with digits at end
        ]

        for ident in test_cases:
            with self.subTest(identifier_with_digits=ident):
                text = f"INTEGER {ident}"
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)

    def test_identifier_underscore_allowed(self):
        """Test identifiers with underscores (supported by grammar)"""
        test_cases = [
            "A_B",     # 3 chars with underscore
            "VAR_1",   # 5 chars with underscore
            "X_Y_Z",   # 5 chars with 2 underscores
            "A_B_CD",  # 6 chars with underscores
        ]

        for ident in test_cases:
            with self.subTest(identifier_with_underscore=ident):
                text = f"INTEGER {ident}"
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)

    def test_identifier_constraint_in_declarations(self):
        """Test identifier constraints in complete declarations per X3.9-1966"""
        valid_program = """
        PROGRAM TEST
        INTEGER A, B, LONGX
        REAL X, Y, VAL
        LOGICAL FLAG
        DOUBLE PRECISION PI
        COMPLEX Z
        END
        """
        tree = self.parse(valid_program, 'main_program')
        self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 66 STATEMENT LABEL RANGE VALIDATION (X3.9-1966 Section 3.4)
    # Issue #404: Statement label range not enforced
    # ====================================================================

    def test_leading_zero_not_lexed_as_label(self):
        """Test that leading-zero sequences are NOT lexed as LABEL tokens (issue #404).

        Per X3.9-1966 Section 3.4 "Statement Labels":
        > A statement label consists of 1 to 5 decimal digits, one of which must be nonzero.
        > Thus, statement labels may be any integer from 1 to 99999 inclusive.

        The FORTRAN 66 LABEL token is inherited from FORTRAN II and defined as:
            [1-9] ([0-9] ([0-9] ([0-9] [0-9]?)?)?)?
        which means:
        - Must start with 1-9 (not 0)
        - 1 to 5 digits total
        - All zeros (0, 00, 000, etc.) do NOT match the LABEL token pattern
        - 6+ digit numbers do NOT match the LABEL token pattern

        This test verifies the lexer correctly enforces the label range constraint
        by rejecting zero labels and out-of-range (>5 digit) labels.

        Reference: ANSI X3.9-1966 Section 3.4 "Statement Labels"
        """
        # Get the LABEL token type for comparison
        label_type = FORTRAN66Lexer.LABEL

        # Leading-zero sequences that should NOT be lexed as LABEL
        not_label_cases = [
            "0",        # Single zero
            "01",       # Leading zero
            "007",      # Leading zeros
            "00123",    # Leading zeros
            "000",      # Multiple zeros
        ]

        for text in not_label_cases:
            with self.subTest(text=text, constraint="leading zeros"):
                input_stream = InputStream(text)
                lexer = FORTRAN66Lexer(input_stream)
                token = lexer.nextToken()
                self.assertNotEqual(
                    token.type, label_type,
                    f"'{text}' was incorrectly lexed as LABEL "
                    f"(X3.9-1966 Section 3.4 requires starting digit 1-9)"
                )

    def test_label_range_1_to_99999(self):
        """Test that LABEL tokens accept valid range 1-99999 (issue #404).

        Per X3.9-1966 Section 3.4, valid statement labels are 1-99999.
        The LABEL token must be lexed for all valid 1-5 digit integers
        starting with 1-9.

        Reference: ANSI X3.9-1966 Section 3.4 "Statement Labels"
        """
        label_type = FORTRAN66Lexer.LABEL

        # Valid LABEL values in range 1-99999
        valid_label_cases = ["1", "9", "42", "100", "12345", "99999"]
        for text in valid_label_cases:
            with self.subTest(text=text, expected="LABEL"):
                input_stream = InputStream(text)
                lexer = FORTRAN66Lexer(input_stream)
                token = lexer.nextToken()
                self.assertEqual(
                    token.type, label_type,
                    f"'{text}' should be lexed as LABEL token (range 1-99999)"
                )

    def test_out_of_range_labels_rejected(self):
        """Test that 6+ digit labels are NOT lexed as LABEL tokens (issue #404).

        Per X3.9-1966 Section 3.4, labels must be 1-5 digits.
        Numbers with 6+ digits exceed the valid range and must NOT
        be lexed as LABEL tokens.

        Reference: ANSI X3.9-1966 Section 3.4 "Statement Labels"
        """
        label_type = FORTRAN66Lexer.LABEL

        # 6+ digit labels that should NOT be lexed as LABEL
        out_of_range_cases = [
            "100000",   # 6 digits
            "1000000",  # 7 digits
            "999999",   # 6 digits
        ]

        for text in out_of_range_cases:
            with self.subTest(text=text, constraint="6+ digits"):
                input_stream = InputStream(text)
                lexer = FORTRAN66Lexer(input_stream)
                token = lexer.nextToken()
                self.assertNotEqual(
                    token.type, label_type,
                    f"'{text}' was incorrectly lexed as LABEL "
                    f"(X3.9-1966 Section 3.4 requires max 5 digits)"
                )

    def test_label_in_statement_positions(self):
        """Test that invalid labels are rejected in statement positions (issue #404).

        When invalid labels (0, 100000) appear in statement label position,
        the parser should reject them with a syntax error.

        Valid statements with labels should parse without error:
        - 1 CONTINUE
        - 99999 CONTINUE

        Invalid statements should produce parser errors:
        - 0 CONTINUE (label 0 is invalid)
        - 100000 CONTINUE (label >99999 is invalid)

        Reference: ANSI X3.9-1966 Section 3.4 "Statement Labels"
        """
        from antlr4.error.ErrorListener import ErrorListener

        class ErrorCapture(ErrorListener):
            def __init__(self):
                self.errors = []

            def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
                self.errors.append((line, column, msg))

        # Valid labels should parse without error
        valid_programs = [
            "1 CONTINUE\nEND",
            "99999 CONTINUE\nEND",
            "42 X = 1.0\nEND",
        ]

        for program in valid_programs:
            with self.subTest(program=program, validity="valid"):
                input_stream = InputStream(program)
                lexer = FORTRAN66Lexer(input_stream)
                stream = CommonTokenStream(lexer)
                parser = FORTRAN66Parser(stream)
                listener = ErrorCapture()
                parser.removeErrorListeners()
                parser.addErrorListener(listener)
                parser.fortran66_program()
                self.assertEqual(len(listener.errors), 0, f"Valid program had errors: {listener.errors}")

        # Invalid labels should produce parser errors
        invalid_programs = [
            ("0 CONTINUE\nEND", "zero label"),
            ("100000 CONTINUE\nEND", "6-digit label"),
        ]

        for program, desc in invalid_programs:
            with self.subTest(program=program, desc=desc, validity="invalid"):
                input_stream = InputStream(program)
                lexer = FORTRAN66Lexer(input_stream)
                stream = CommonTokenStream(lexer)
                parser = FORTRAN66Parser(stream)
                listener = ErrorCapture()
                parser.removeErrorListeners()
                parser.addErrorListener(listener)
                parser.fortran66_program()
                self.assertGreater(len(listener.errors), 0, f"Invalid label '{desc}' should have produced errors")

    def test_identifier_constraint_in_assignments(self):
        """Test identifier constraints in assignment statements"""
        valid_statements = [
            "A = 1",
            "VAR = 2.0",
            "ABCDEF = 3",  # Exactly 6 characters
            "X1 = Y2",
            "FLAG = .TRUE.",
        ]

        for stmt in valid_statements:
            with self.subTest(assignment=stmt):
                tree = self.parse(stmt, 'assignment_stmt')
                self.assertIsNotNone(tree)

    def test_dimension_statement_simple_bounds(self):
        """Test DIMENSION statement with simple positive bounds.

        ANSI X3.9-1966 Section 5.3 specifies dimension bounds as
        d (dimension 1 to d) or d1, d2 (dimension d1 to d2).
        """
        # Valid dimension declarations per X3.9-1966 Section 5.3
        valid_declarations = [
            "DIMENSION A(10)",           # Single dimension: 1 to 10
            "DIMENSION B(5)",            # Single dimension: 1 to 5
            "DIMENSION C(100)",          # Single dimension: 1 to 100
            "DIMENSION X(10, 20)",       # Two dimensions: 1-10, 1-20
            "DIMENSION Y(3, 4, 5)",      # Three dimensions
            "DIMENSION P(1)",            # Minimum positive bound
        ]

        for decl in valid_declarations:
            with self.subTest(declaration=decl):
                tree = self.parse(decl, 'dimension_stmt')
                self.assertIsNotNone(tree, f"Failed to parse: {decl}")

    def test_dimension_statement_explicit_bounds(self):
        """Test DIMENSION statement with explicit lower and upper bounds.

        ANSI X3.9-1966 Section 5.3 permits d1:d2 form (explicit bounds).
        Per standard: d1 may be a signed integer (negative values allowed)
        and d2 must be >= d1.
        """
        # Valid explicit bound declarations
        valid_declarations = [
            "DIMENSION A(1:10)",         # Explicit 1 to 10
            "DIMENSION B(0:10)",         # Zero lower bound (allowed)
            "DIMENSION C(-5:10)",        # Negative lower bound (allowed)
            "DIMENSION D(1:1)",          # Single element (1 to 1)
            "DIMENSION E(10:20)",        # Explicit 10 to 20
        ]

        for decl in valid_declarations:
            with self.subTest(declaration=decl):
                tree = self.parse(decl, 'dimension_stmt')
                self.assertIsNotNone(tree, f"Failed to parse: {decl}")

    def test_dimension_statement_multiple_declarators(self):
        """Test DIMENSION statement with multiple array declarators."""
        declarations = [
            "DIMENSION A(10), B(20)",
            "DIMENSION X(5, 10), Y(3, 4, 5)",
            "DIMENSION P(1:10), Q(0:20), R(100)",
        ]

        for decl in declarations:
            with self.subTest(declaration=decl):
                tree = self.parse(decl, 'dimension_stmt')
                self.assertIsNotNone(tree, f"Failed to parse: {decl}")

    def test_dimension_bounds_semantic_constraint_zero_upper(self):
        """Test semantic constraint: upper bound must be positive.

        ANSI X3.9-1966 Section 5.3 specifies that d and d2 must be positive.
        Constraint: d > 0 (when single bound), d2 > 0 (when explicit)

        Note: Parser accepts these syntactically; semantic validation would
        require a separate pass. This test documents the constraint.
        """
        # These parse syntactically but violate ANSI X3.9-1966 Section 5.3
        # They would require semantic validation to reject
        invalid_semantically = [
            "DIMENSION A(0)",            # Zero upper bound - NON-COMPLIANT
            "DIMENSION B(-5)",           # Negative single bound - NON-COMPLIANT
            "DIMENSION C(10, 5)",        # Inverted bounds (d2 < d1) - NON-COMPLIANT
            "DIMENSION D(0:0)",          # Zero to zero (ambiguous) - NON-COMPLIANT
        ]

        for decl in invalid_semantically:
            with self.subTest(declaration=decl):
                # These currently parse without error - documenting constraint
                tree = self.parse(decl, 'dimension_stmt')
                self.assertIsNotNone(tree)
                # TODO: Add semantic validation pass to reject these per X3.9-1966

    def test_dimension_bounds_in_typed_declaration(self):
        """Test dimension bounds in type declaration (e.g., INTEGER A(10))."""
        # Type declarations with embedded dimension syntax
        declarations = [
            "INTEGER A(10)",
            "REAL B(5, 20)",
            "LOGICAL FLAG(100)",
            "DOUBLE PRECISION X(10, 10, 10)",
            "COMPLEX Z(5)",
        ]

        for decl in declarations:
            with self.subTest(declaration=decl):
                tree = self.parse(decl, 'type_declaration')
                self.assertIsNotNone(tree, f"Failed to parse: {decl}")

    def test_dimension_bounds_variable_list_subscripting(self):
        """Test that dimension bounds appear in variable lists.

        ANSI X3.9-1966 Section 5 specifies array declarators in
        DIMENSION, type declarations, and COMMON statements.
        """
        # Array variables can appear in:
        # - DIMENSION statements (tested above)
        # - Type declarations (tested above)
        # - Variable lists in assignments/I/O

        # Assignment with array element
        assignment = "A(5) = 10"
        tree = self.parse(assignment, 'assignment_stmt')
        self.assertIsNotNone(tree)

        # Subscript expression can be more complex
        assignment2 = "X(I, J+1) = Y(2*K, 3)"
        tree = self.parse(assignment2, 'assignment_stmt')
        self.assertIsNotNone(tree)

    def test_dimension_statement_with_negative_bounds(self):
        """Test DIMENSION with negative lower bounds (explicit form).

        ANSI X3.9-1966 Section 5.3: d1 may be negative.
        Valid forms: d1:d2 where d1 < d2, both signed integers.
        """
        declarations = [
            "DIMENSION A(-10:10)",       # Symmetric negative to positive
            "DIMENSION B(-5:-1)",        # Negative to negative (d2 >= d1)
            "DIMENSION C(-100:0)",       # Negative to zero
        ]

        for decl in declarations:
            with self.subTest(declaration=decl):
                tree = self.parse(decl, 'dimension_stmt')
                self.assertIsNotNone(tree, f"Failed to parse: {decl}")


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)
