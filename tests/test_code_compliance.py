#!/usr/bin/env python3
"""
Test suite for code compliance with CLAUDE.md requirements
Tests for line length, indentation, and other coding standards
"""

import os
import unittest
from pathlib import Path


class TestCodeCompliance(unittest.TestCase):
    """Test code compliance with project standards"""
    
    def test_line_length_compliance(self):
        """Test that all lines are <= 88 characters"""
        violations = []
        grammar_dir = Path('grammars')
        
        for g4_file in grammar_dir.glob('*.g4'):
            with open(g4_file, 'r') as f:
                for line_num, line in enumerate(f, 1):
                    # Remove newline for accurate count
                    line = line.rstrip('\n')
                    if len(line) > 88:
                        violations.append(
                            f"{g4_file.name}:{line_num}: "
                            f"{len(line)} chars"
                        )
        
        self.assertEqual([], violations,
                        f"Line length violations found:\n" + 
                        "\n".join(violations))
    
    def test_indentation_compliance(self):
        """Test that all files use 4 spaces, not tabs"""
        violations = []
        grammar_dir = Path('grammars')
        
        for g4_file in grammar_dir.glob('*.g4'):
            with open(g4_file, 'r') as f:
                for line_num, line in enumerate(f, 1):
                    if '\t' in line:
                        violations.append(
                            f"{g4_file.name}:{line_num}: "
                            f"contains tab character"
                        )
        
        self.assertEqual([], violations,
                        f"Tab indentation found:\n" + 
                        "\n".join(violations[:20]))  # Show first 20
    
    def test_no_commented_code(self):
        """Test that there are no commented-out code sections"""
        violations = []
        grammar_dir = Path('grammars')
        
        # Patterns that indicate commented-out code
        code_patterns = [
            '// ;',  # Commented semicolon
            '// }',  # Commented brace
            '// |',  # Commented alternative
        ]
        
        for g4_file in grammar_dir.glob('*.g4'):
            with open(g4_file, 'r') as f:
                for line_num, line in enumerate(f, 1):
                    for pattern in code_patterns:
                        if pattern in line:
                            violations.append(
                                f"{g4_file.name}:{line_num}: "
                                f"possible commented code"
                            )
        
        self.assertEqual([], violations,
                        f"Commented code found:\n" + 
                        "\n".join(violations))
    
    def test_no_stub_references(self):
        """Test that no files contain stub references"""
        violations = []
        grammar_dir = Path('grammars')
        
        stub_words = ['stub', 'placeholder', 'mock', 
                      'temporary', 'hack', 'fixme', 'xxx']
        # Note: 'dummy' is excluded as it's valid Fortran terminology
        # for dummy arguments in procedures
        
        for g4_file in grammar_dir.glob('*.g4'):
            with open(g4_file, 'r') as f:
                content = f.read().lower()
                for word in stub_words:
                    if word in content:
                        # Check if it's in actual code, not comments
                        lines = content.split('\n')
                        for line_num, line in enumerate(lines, 1):
                            if word in line and '//' not in line:
                                violations.append(
                                    f"{g4_file.name}:{line_num}: "
                                    f"contains '{word}'"
                                )
        
        self.assertEqual([], violations,
                        f"Stub references found:\n" + 
                        "\n".join(violations))


if __name__ == "__main__":
    unittest.main(verbosity=2)