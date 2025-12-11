#!/usr/bin/env python3
"""
Test suite for FORTRAN 77 Parser - FORTRAN 77 (1977)
Tests for FORTRAN 77 structured programming features: CHARACTER, IF-THEN-ELSE
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
    from FORTRAN77Lexer import FORTRAN77Lexer  # type: ignore
    from FORTRAN77Parser import FORTRAN77Parser  # type: ignore
    from fixture_utils import load_fixture
    from shared_fortran_tests import StatementFunctionTestMixin
except ImportError as e:
    print(f"Import error: {e}")
    FORTRAN77Parser = None


class TestFORTRAN77Parser(StatementFunctionTestMixin, unittest.TestCase):
    """Test FORTRAN 77 (1977) parser rules - Structured Programming Revolution"""

    LexerClass = FORTRAN77Lexer
    ParserClass = FORTRAN77Parser
    fixture_standard = "FORTRAN77"

    def setUp(self):
        """Set up test fixtures"""
        if FORTRAN77Parser is None:
            self.skipTest("FORTRAN77Parser not available")

    def parse(self, text, rule_name='main_program'):
        """Helper to parse text using specified rule"""
        input_stream = InputStream(text)
        lexer = FORTRAN77Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRAN77Parser(token_stream)
        
        # Get the parse method by rule name
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def test_character_data_type(self):
        """Test CHARACTER data type (NEW in FORTRAN 77)"""
        test_cases = [
            "CHARACTER NAME",
            "CHARACTER*10 TITLE",
            "CHARACTER*(*) MESSAGE"
        ]
        
        for text in test_cases:
            with self.subTest(character_type=text):
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)
    
    def test_if_then_else_construct(self):
        """Test IF-THEN-ELSE construct (NEW in FORTRAN 77)"""
        test_cases = [
            """IF (X .GT. 0) THEN
    PRINT *, 'POSITIVE'
END IF""",
            """IF (A .EQ. B) THEN
    RESULT = 1
ELSE
    RESULT = 0
END IF""",
            """IF (X .LT. 0) THEN
    PRINT *, 'NEGATIVE'
ELSE IF (X .GT. 0) THEN
    PRINT *, 'POSITIVE'  
ELSE
    PRINT *, 'ZERO'
END IF"""
        ]
        
        for text in test_cases:
            with self.subTest(if_construct=text):
                tree = self.parse(text, 'block_if_construct')
                self.assertIsNotNone(tree)
    
    def test_character_expressions(self):
        """Test character expressions and operations (FORTRAN 77)"""
        test_cases = [
            "HELLO",
            "FIRST // SECOND"
        ]
        
        for text in test_cases:
            with self.subTest(char_expr=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)
    
    def test_enhanced_do_loops(self):
        """Test enhanced DO loops with floating point (FORTRAN 77)"""
        test_cases = [
            "DO 100 X = 1.0, 10.0, 0.5",
            "DO 200 I = 1, N"
        ]
        
        for text in test_cases:
            with self.subTest(do_stmt=text):
                tree = self.parse(text, 'do_stmt')
                self.assertIsNotNone(tree)
    
    def test_save_statement(self):
        """Test SAVE statement (NEW in FORTRAN 77)"""
        test_cases = [
            "SAVE",
            "SAVE A, B, C",
            "SAVE /COMMON_BLOCK/"
        ]
        
        for text in test_cases:
            with self.subTest(save_stmt=text):
                tree = self.parse(text, 'save_stmt')
                self.assertIsNotNone(tree)
    
    def test_intrinsic_statement(self):
        """Test INTRINSIC statement (NEW in FORTRAN 77)"""
        test_cases = [
            "INTRINSIC SIN, COS, TAN",
            "INTRINSIC SQRT"
        ]
        
        for text in test_cases:
            with self.subTest(intrinsic_stmt=text):
                tree = self.parse(text, 'intrinsic_stmt')
                self.assertIsNotNone(tree)
    
    def test_external_statement(self):
        """Test EXTERNAL statement (NEW in FORTRAN 77)"""
        test_cases = [
            "EXTERNAL FUNC1, FUNC2",
            "EXTERNAL MYSUB"
        ]

        for text in test_cases:
            with self.subTest(external_stmt=text):
                tree = self.parse(text, 'external_stmt')
                self.assertIsNotNone(tree)

    def test_entry_statement(self):
        """Test ENTRY statement (NEW in FORTRAN 77)

        Per ISO 1539:1980 Section 15.7, the ENTRY statement provides an
        alternate entry point into a FUNCTION or SUBROUTINE subprogram.

        Syntax: ENTRY entry-name [ ( [ dummy-arg-list ] ) ]

        The dummy-arg-list may include:
        - Regular dummy argument names
        - Alternate return specifiers (*) for subroutines
        """
        test_cases = [
            "ENTRY SETVAL",
            "ENTRY COMPUTE(X, Y)",
            "ENTRY RESET()",
            "ENTRY PROCESS(A, B, C)",
            "ENTRY ALTRET(*)",
            "ENTRY MIXARG(X, *, Y)",
        ]

        for text in test_cases:
            with self.subTest(entry_stmt=text):
                tree = self.parse(text, 'entry_stmt')
                self.assertIsNotNone(tree)

    def test_entry_statement_in_subroutine_fixture(self):
        """Test ENTRY statement in subroutine context (FORTRAN 77)

        Per ISO 1539:1980 Section 15.7, ENTRY statements may appear in
        FUNCTION or SUBROUTINE subprograms to define alternate entry points.
        """
        fixture_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser_extra",
            "entry_statements.f",
        )

        tree = self.parse(fixture_text, 'fortran66_program')
        self.assertIsNotNone(tree)

    def test_implicit_statement(self):
        """Test IMPLICIT statement (NEW in FORTRAN 77)

        Per ANSI X3.9-1978 Section 8.4, the IMPLICIT statement specifies
        the type and size of all variables, arrays, and functions that
        begin with specified letters.

        Syntax: IMPLICIT type (letter-spec-list) [, type (letter-spec-list)]...
        Letter-spec can be a single letter or a letter range (A-Z).

        Note: IMPLICIT NONE was NOT part of FORTRAN 77; it was added in
        Fortran 90.
        """
        test_cases = [
            "IMPLICIT INTEGER (I-N)",
            "IMPLICIT REAL (A-H, O-Z)",
            "IMPLICIT INTEGER (I-N), REAL (A-H, O-Z)",
            "IMPLICIT DOUBLE PRECISION (D)",
            "IMPLICIT COMPLEX (Z)",
            "IMPLICIT LOGICAL (L)",
            "IMPLICIT CHARACTER (C)",
            "IMPLICIT INTEGER (I, J, K)",
        ]

        for text in test_cases:
            with self.subTest(implicit_stmt=text):
                tree = self.parse(text, 'implicit_stmt')
                self.assertIsNotNone(tree)

    def test_implicit_statement_in_program(self):
        """Test IMPLICIT statement in program context (FORTRAN 77)"""
        program_text = """IMPLICIT INTEGER (I-N)
IMPLICIT REAL (A-H, O-Z)
INTEGER X
X = 1
END
"""
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)

    def test_complex_literals_fortran77(self):
        """Test COMPLEX constants (inherited from FORTRAN 66, ISO 1539:1980 Section 4.4.2)"""
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

    def test_complex_literals_in_assignment_fortran77(self):
        """Test COMPLEX constants in assignment statements (ISO 1539:1980 Section 10)"""
        test_program = """
        PROGRAM COMPLEX_TEST
        COMPLEX Z, W
        Z = (1.0, 2.0)
        W = (3, -4)
        END
        """
        tree = self.parse(test_program, 'main_program')
        self.assertIsNotNone(tree)

    def test_complex_literals_in_data_statement_fortran77(self):
        """Test COMPLEX constants in DATA statements (ISO 1539:1980 Section 9)"""
        test_program = """
        PROGRAM DATA_COMPLEX
        COMPLEX Z1, Z2, Z3
        DATA Z1 / (1.0, 2.0) /
        DATA Z2, Z3 / (3.0, 4.0), (-1.0, -2.0) /
        END
        """
        tree = self.parse(test_program, 'main_program')
        self.assertIsNotNone(tree)

    def test_parameter_statement(self):
        """Test PARAMETER statement (NEW in FORTRAN 77)

        Per ANSI X3.9-1978 Section 8.5, the PARAMETER statement defines
        named constants that may be used wherever a constant is allowed.

        Syntax: PARAMETER (named-constant = constant-expr [, ...])
        """
        test_cases = [
            "PARAMETER (PI = 3.14159)",
            "PARAMETER (MAXSIZE = 100)",
            "PARAMETER (PI = 3.14159, TWO_PI = 6.28318)",
            "PARAMETER (N = 10, M = 20, K = 30)",
            "PARAMETER (EPS = 1.0E-6)",
            "PARAMETER (TITLE = 'HELLO')",
        ]

        for text in test_cases:
            with self.subTest(parameter_stmt=text):
                tree = self.parse(text, 'parameter_stmt')
                self.assertIsNotNone(tree)

    def test_parameter_with_expressions(self):
        """Test PARAMETER with constant expressions (FORTRAN 77)

        Per ANSI X3.9-1978, constant expressions may include:
        - Literal constants
        - Previously defined named constants
        - Intrinsic functions with constant arguments
        """
        test_cases = [
            "PARAMETER (TWOPI = 2 * 3.14159)",
            "PARAMETER (SIZE = 10 * 10)",
            "PARAMETER (HALF = 1.0 / 2.0)",
            "PARAMETER (VAL = 2 ** 10)",
        ]

        for text in test_cases:
            with self.subTest(parameter_expr=text):
                tree = self.parse(text, 'parameter_stmt')
                self.assertIsNotNone(tree)

    def test_parameter_statement_in_program(self):
        """Test PARAMETER statement in program context (FORTRAN 77)"""
        program_text = """PARAMETER (PI = 3.14159)
PARAMETER (MAXSIZE = 100)
REAL RADIUS, AREA
RADIUS = 5.0
AREA = PI * RADIUS ** 2
END
"""
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
    
    def test_structured_programming_example(self):
        """Test complete structured programming example (FORTRAN 77)"""
        program_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser",
            "structured_program_example.f",
        )
        
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)
    
    def test_fortran77_revolution_features(self):
        """Test the structured programming revolution features"""
        # Complex nested IF-THEN-ELSE structure
        nested_if = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser_extra",
            "nested_if_block.f",
        )
        
        tree = self.parse(nested_if, 'block_if_construct')
        self.assertIsNotNone(tree)
    
    def test_character_string_processing(self):
        """Test character string processing capabilities (FORTRAN 77)

        Per ANSI X3.9-1978 sections 4.7, 5.3, 5.7, and 6, FORTRAN 77 supports:
        - Character constants as apostrophe-delimited strings
        - Doubled apostrophes for embedded quotes
        - Concatenation via //
        - Substring operations via (start:end)
        """
        string_operations = [
            "'HELLO'",
            "'HELLO' // ' WORLD'",
            "'IT''S A TEST'",
            "'X' // 'Y' // 'Z'",
        ]

        for text in string_operations:
            with self.subTest(string_op=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_substring_operations(self):
        """Test character substring operations (FORTRAN 77)

        Per ANSI X3.9-1978 section 5.7, a substring is a contiguous portion
        of a character datum designated by a substring name of the form:
            character-variable-name (e1:e2)
        where e1 and e2 are integer expressions for starting and ending
        positions. The starting position e1 is mandatory; the ending
        position e2 may be omitted to indicate the remainder of the string.
        """
        substring_expressions = [
            "NAME(1:5)",
            "STR(I:J)",
            "TEXT(1:LEN)",
            "WORD(START:END)",
            "LINE(1:)",
        ]

        for text in substring_expressions:
            with self.subTest(substring=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_array_element_substrings(self):
        """Test array element substring references (FORTRAN 77)

        Per ISO 1539:1980 Section 5.7, a substring is a contiguous portion
        of a character variable or array element. Array element substrings
        combine array subscripting with substring references:
            array-name (subscript-list) (e1:e2)

        Examples:
        - NAMES(I)(1:5) - substring of 1D array element
        - MATRIX(I,J)(start:end) - substring of 2D array element
        - ARRAY(1,2,3)(2:10) - substring of 3D array element
        """
        array_substring_expressions = [
            "NAMES(I)(1:5)",
            "NAMES(1)(1:5)",
            "MATRIX(I,J)(start:end)",
            "MATRIX(1,2)(1:10)",
            "ARRAY(I)(2:)",
            "DATA(1)(:",
            "WORDS(INDEX)(:",
            "GRID(X,Y,Z)(pos:)",
            "TABLE(1,2)(1:end)",
        ]

        for text in array_substring_expressions:
            with self.subTest(array_substring=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_concatenation_combinations(self):
        """Test character concatenation with mixed operand types (FORTRAN 77)

        Per ANSI X3.9-1978 section 6.2, concatenation is performed by the
        operator //. The result is a character string whose value is the
        value of the left operand followed by the value of the right operand.
        """
        concat_expressions = [
            "A // B",
            "'PREFIX' // NAME",
            "FIRST // ' ' // LAST",
            "NAME(1:5) // SUFFIX",
            "'(' // CODE // ')'",
            "A // B // C // D",
            "NAMES(I)(1:5) // SUFFIX",
            "PREFIX // ARRAY(J,K)(2:10)",
        ]

        for text in concat_expressions:
            with self.subTest(concat=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_function_references(self):
        """Test character function references in expressions (FORTRAN 77)

        Per ANSI X3.9-1978, character functions return character values
        and can be used as primaries in character expressions.
        """
        function_expressions = [
            "TRIM(NAME)",
            "CONCAT(A, B)",
            "UPPER(STR)",
            "FMT(X, Y, Z)",
        ]

        for text in function_expressions:
            with self.subTest(func_ref=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_string_literal_edge_cases(self):
        """Test STRING_LITERAL edge cases per ANSI X3.9-1978 section 4.7

        Character constants may contain any characters from the FORTRAN
        character set. An apostrophe within a character constant is
        represented by two consecutive apostrophes.
        """
        edge_cases = [
            "''",
            "' '",
            "'   '",
            "'A''B''C'",
            "'DON''T PANIC'",
            "'LINE1' // '/' // 'LINE2'",
        ]

        for text in edge_cases:
            with self.subTest(edge_case=text):
                tree = self.parse(text, 'character_expr')
                self.assertIsNotNone(tree)

    def test_character_expression_fixture(self):
        """Test CHARACTER expressions from fixture file"""
        fixture_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser",
            "character_expressions.f",
        )

        tree = self.parse(fixture_text, 'main_program')
        self.assertIsNotNone(tree)

    def test_file_io_statements_fixture(self):
        """Test FORTRAN 77 file I/O statements from fixture file

        Per ANSI X3.9-1978, FORTRAN 77 introduces OPEN, CLOSE, and INQUIRE
        file control statements, and wires REWIND, BACKSPACE, and ENDFILE
        into the grammar. This fixture exercises positional and keyword-based
        forms for all file I/O statements.
        """
        fixture_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser",
            "file_io_statements.f",
        )

        tree = self.parse(fixture_text, 'main_program')
        self.assertIsNotNone(tree)

    def test_proper_inheritance(self):
        """Test that FORTRAN77 properly inherits from FORTRAN66"""
        # Read the grammar file
        grammar_path = ROOT.parent / "grammars/src" / "FORTRAN77Parser.g4"
        with open(grammar_path, 'r') as f:
            content = f.read()
        
        # Check for proper import
        self.assertIn('import FORTRAN66Parser', content,
                     "FORTRAN77 should import FORTRAN66")
        
        # Check that type_spec rule includes comment about ANTLR4 limitation
        # Count occurrences of type definitions in type_spec rule
        import re
        type_spec_match = re.search(
            r'type_spec\s*:(.*?)(?=\n\w|\nfragment|\Z)', 
            content, re.DOTALL
        )
        if type_spec_match:
            type_spec_content = type_spec_match.group(1)
            # Due to ANTLR4 limitations, must redefine all types
            # But should have comment explaining this
            self.assertIn('ANTLR4', content[:type_spec_match.start() + 500],
                         "Should document ANTLR4 limitation")
            self.assertIn('CHARACTER', type_spec_content,
                         "type_spec should define CHARACTER")
        
        # Test that inherited types still work
        test_cases = [
            ("INTEGER I", "INTEGER"),    # Inherited from FORTRAN I
            ("REAL X", "REAL"),          # Inherited from FORTRAN I
            ("LOGICAL FLAG", "LOGICAL"), # Inherited from FORTRAN IV
            ("COMPLEX Z", "COMPLEX"),    # Inherited from FORTRAN IV
            ("CHARACTER NAME", "CHARACTER") # New in FORTRAN 77
        ]
        
        for text, expected_type in test_cases:
            with self.subTest(declaration=text):
                tree = self.parse(text, 'type_declaration')
                self.assertIsNotNone(tree)
                self.assertIn(expected_type, tree.getText())

    def test_data_repetition_factors_fixture(self):
        """Test DATA statement with repetition factors from fixture file

        ISO 1539:1980 Section 9.3 defines data constant lists with optional
        repetition factors for initializing multiple array elements with the
        same value. Examples: DATA A /10*0/, DATA B /3*1, 2*0/.
        """
        fixture_text = load_fixture(
            "FORTRAN77",
            "test_fortran77_parser",
            "data_repetition_factors.f",
        )

        tree = self.parse(fixture_text, 'main_program')
        self.assertIsNotNone(tree)

    def test_data_simple_repetition(self):
        """Test DATA statement with simple repetition factors"""
        test_cases = [
            "DATA A /10*0/",
            "DATA X /100*0.0/",
            "DATA FLAG /4*.TRUE./",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                # DATA statement is part of a declaration or main program
                program = f"""      PROGRAM TEST
      INTEGER A(10)
      REAL X(100)
      LOGICAL FLAG(4)
      {text}
      END"""
                tree = self.parse(program, 'main_program')
                self.assertIsNotNone(tree)

    def test_data_mixed_repetition(self):
        """Test DATA with mixed repetition and non-repetition factors"""
        test_cases = [
            "DATA B /3*1, 2*0/",
            "DATA Y /1.0, 2*2.0, 3*3.0/",
        ]

        for text in test_cases:
            with self.subTest(data_stmt=text):
                program = f"""      PROGRAM TEST
      INTEGER B(5)
      REAL Y(6)
      {text}
      END"""
                tree = self.parse(program, 'main_program')
                self.assertIsNotNone(tree)

    def test_data_signed_repetition(self):
        """Test DATA with repetition of signed constants"""
        program = """      PROGRAM TEST
      INTEGER B(5)
      DATA B /5*(-1)/
      END"""
        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)

    # ====================================================================
    # FORTRAN 77 ASSIGN AND ASSIGNED GO TO TESTS
    # ====================================================================
    # ISO 1539:1980 Sections 10.4 and 11.2 define ASSIGN and assigned GO TO
    # statements. These features were inherited from FORTRAN 66 (X3.9-1966
    # Sections 7.1.1.3 and 7.1.2.1.2) and remain part of FORTRAN 77.
    #
    # ASSIGN statement: Assigns a statement label to an integer variable
    # Assigned GO TO: Branches to the label stored in an integer variable
    # ====================================================================

    def test_assign_statement(self):
        """Test ASSIGN statement (ISO 1539:1980 Section 10.4)

        Per ISO 1539:1980 Section 10.4, the ASSIGN statement stores a
        statement label in an integer variable.

        Syntax: ASSIGN label TO variable
        """
        test_cases = [
            "ASSIGN 100 TO ILAB",
            "ASSIGN 200 TO TARGET",
            "ASSIGN 500 TO LABEL_VAR",
            "ASSIGN 1 TO X",
        ]

        for text in test_cases:
            with self.subTest(assign_stmt=text):
                tree = self.parse(text, 'assign_stmt')
                self.assertIsNotNone(tree)

    def test_assigned_goto_statement(self):
        """Test Assigned GO TO statement (ISO 1539:1980 Section 11.2)

        Per ISO 1539:1980 Section 11.2, the assigned GO TO statement
        branches to a label stored in an integer variable.

        Syntax: GO TO variable, (label1, label2, ...)
        """
        test_cases = [
            "GO TO ILAB, (100, 200, 300)",
            "GO TO TARGET, (10, 20)",
            "GO TO LABEL_VAR, (1, 2, 3, 4, 5)",
            "GO TO X, (50)",
        ]

        for text in test_cases:
            with self.subTest(assigned_goto=text):
                tree = self.parse(text, 'assigned_goto_stmt')
                self.assertIsNotNone(tree)

    def test_assign_and_goto_in_program(self):
        """Test ASSIGN and assigned GO TO in program context

        This test verifies that both ASSIGN and assigned GO TO statements
        parse correctly within a complete FORTRAN 77 program.
        """
        program_text = """
        PROGRAM ASSIGN_TEST
        INTEGER ILAB
        ASSIGN 100 TO ILAB
        GO TO ILAB, (100, 200)
 100    PRINT *, 'LABEL 100'
        GO TO 300
 200    PRINT *, 'LABEL 200'
 300    END
        """
        tree = self.parse(program_text, 'main_program')
        self.assertIsNotNone(tree)

    def test_assign_and_goto_in_block_if(self):
        """Test assigned GO TO within block IF construct

        Per ISO 1539:1980 Section 11.2, assigned GO TO can appear within
        an IF-THEN-ELSE block as part of executable constructs.
        """
        block_if_text = """IF (X .GT. 0) THEN
    GOTO ILAB, (100, 200)
ELSE
    GOTO ILAB, (100, 200)
END IF"""
        tree = self.parse(block_if_text, 'block_if_construct')
        self.assertIsNotNone(tree)

    def test_assign_in_block_if(self):
        """Test ASSIGN statement within block IF construct

        This verifies ISO 1539:1980 Section 10.4 (ASSIGN) is
        accepted wherever executable constructs are allowed, including block
        IF bodies that also contain assigned GO TO statements.
        """
        block_if_text = """IF (X .GT. 0) THEN
    ASSIGN 100 TO ILAB
    GO TO ILAB, (100, 200)
ELSE
    ASSIGN 200 TO ILAB
    GO TO ILAB, (100, 200)
END IF"""
        tree = self.parse(block_if_text, 'block_if_construct')
        self.assertIsNotNone(tree)

    def test_program_statement_accepts_max_identifier_length(self):
        """ISO 1539:1980 Section 2.3.3 - identifiers can be up to 31 characters"""
        max_length_name = 'A' + 'B' * 30  # 31 characters total
        program_stmt = f"PROGRAM {max_length_name}\n"
        tree = self.parse(program_stmt, 'program_stmt')
        self.assertIsNotNone(tree)

    def test_identifier_tokens_within_length_limit(self):
        """Verify lexical recognition of identifiers up to 31 characters"""
        valid_identifiers = [
            "I",                          # 1 char
            "VAR01",                      # 5 chars
            "LONGNAME",                   # 8 chars
            'A' + 'B' * 30,               # 31 chars (maximum)
        ]

        for identifier in valid_identifiers:
            with self.subTest(identifier=identifier):
                tokens = self._collect_tokens(identifier)
                identifier_tokens = [
                    token for token in tokens if token.type == FORTRAN77Lexer.IDENTIFIER
                ]
                self.assertEqual(
                    1,
                    len(identifier_tokens),
                    f"Expected one IDENTIFIER token for {identifier}",
                )
                self.assertEqual(identifier, identifier_tokens[0].text)

    def test_identifier_tokens_exceed_length_limit(self):
        """Verify identifiers longer than 31 characters are not accepted as single tokens"""
        long_identifiers = [
            'A' * 32,
            'LONG' + 'NAME' * 7,  # 32+ characters
        ]

        for identifier in long_identifiers:
            with self.subTest(identifier=identifier):
                tokens = self._collect_tokens(identifier)
                identifier_tokens = [
                    token for token in tokens if token.type == FORTRAN77Lexer.IDENTIFIER
                ]
                self.assertTrue(identifier_tokens, "Expected at least one IDENTIFIER fragment")
                self.assertFalse(
                    any(token.text == identifier for token in identifier_tokens),
                    "Lexer should not emit a single IDENTIFIER token matching the long name",
                )
                for token in identifier_tokens:
                    self.assertLessEqual(
                        len(token.text),
                        31,
                        "Identifier fragments must remain within the 31-character limit",
                    )

    def _collect_tokens(self, text: str):
        """Collect tokens from the FORTRAN 77 lexer for the given text"""
        input_stream = InputStream(text)
        lexer = FORTRAN77Lexer(input_stream)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        return tokens

    # ====================================================================
    # FORTRAN 77 STATEMENT FUNCTION TESTS
    # ====================================================================
    # Note: Statement function tests, negative tests (array element
    # assignments), and disambiguation tests are provided by the
    # StatementFunctionTestMixin inherited from shared_fortran_tests.py.
    # FORTRAN 77 (X3.9-1978 Section 8) uses identical semantics to
    # FORTRAN 66 (X3.9-1966 Section 7.2) for statement functions.
    # ====================================================================


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)
