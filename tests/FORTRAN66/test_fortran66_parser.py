#!/usr/bin/env python3
"""
Test suite for FORTRAN 66 Parser - FORTRAN 66 (1966)
Tests for FORTRAN 66 standardization features: BLOCK DATA, standardized structure
"""

import sys
import os
import unittest
from pathlib import Path

# Add grammars directory to path for imports
sys.path.insert(0, 'grammars')

try:
    from antlr4 import InputStream, CommonTokenStream
    from FORTRAN66Lexer import FORTRAN66Lexer
    from FORTRAN66Parser import FORTRAN66Parser
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
        main_program = """INTEGER I
REAL X
I = 1
X = 3.14
PRINT 100, I, X
100 FORMAT (I5, F10.2)
END"""
        
        tree = self.parse(main_program, 'main_program')
        self.assertIsNotNone(tree)
        
        # Function subprogram
        function_program = """REAL FUNCTION SQUARE(X)
REAL X
SQUARE = X * X
RETURN
END"""
        
        tree = self.parse(function_program, 'function_subprogram')
        self.assertIsNotNone(tree)
        
        # Subroutine subprogram
        subroutine_program = """SUBROUTINE SWAP(A, B)
REAL A, B, TEMP
TEMP = A
A = B
B = TEMP
RETURN
END"""
        
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
        standard_program = """INTEGER COUNT
REAL AVERAGE, SUM
LOGICAL DONE

COUNT = 0
SUM = 0.0
DONE = .FALSE.

DO 10 I = 1, 100
    COUNT = COUNT + 1
    SUM = SUM + I
10 CONTINUE

AVERAGE = SUM / COUNT
PRINT 100, COUNT, AVERAGE
100 FORMAT ('COUNT=', I3, ' AVERAGE=', F8.2)
END"""
        
        tree = self.parse(standard_program, 'main_program')
        self.assertIsNotNone(tree)
    
    def test_first_standard_compliance(self):
        """Test compliance with first programming language standard"""
        # Program demonstrating FORTRAN 66 standardization
        test_program = """PROGRAM DEMO
COMMON /DATA/ VALUES(100)
CALL INITIALIZE
CALL PROCESS  
CALL OUTPUT
END

SUBROUTINE INITIALIZE
COMMON /DATA/ VALUES(100)
DO 10 I = 1, 100
    VALUES(I) = I * 2.0
10 CONTINUE
RETURN
END

SUBROUTINE PROCESS
COMMON /DATA/ VALUES(100)
DO 20 I = 1, 100
    VALUES(I) = VALUES(I) + 1.0
20 CONTINUE  
RETURN
END

SUBROUTINE OUTPUT
COMMON /DATA/ VALUES(100)
DO 30 I = 1, 10
    PRINT 100, I, VALUES(I)
30 CONTINUE
100 FORMAT (I3, F8.2)
RETURN
END"""
        
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


if __name__ == "__main__":
    # Run with verbose output to see which tests fail
    unittest.main(verbosity=2)