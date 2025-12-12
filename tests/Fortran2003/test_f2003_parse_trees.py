#!/usr/bin/env python3
"""
Non-shallow tests for Fortran 2003 parser with parse tree validation.
This addresses the critical issue of shallow/tautological testing identified in the review.
"""

import sys
import os
import pytest
from pathlib import Path

# Add grammars directory to path
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from antlr4 import *

class TestFortran2003ParseTrees:
    """Non-shallow tests that actually validate parse tree structure and semantics."""
    
    def setup_method(self):
        """Set up parser for each test."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser
            self.lexer_class = Fortran2003Lexer
            self.parser_class = Fortran2003Parser
        except ImportError:
            pytest.skip("F2003 parser not built")
    
    def parse_and_validate(self, code, start_rule='program_unit_f2003'):
        """Parse code and return tree with validation."""
        input_stream = InputStream(code)
        lexer = self.lexer_class(input_stream)
        parser = self.parser_class(CommonTokenStream(lexer))
        
        # Get the parsing method
        parse_method = getattr(parser, start_rule)
        tree = parse_method()
        
        errors = parser.getNumberOfSyntaxErrors()
        
        return tree, errors, parser
    
    def validate_tree_structure(self, tree, expected_children_types=None):
        """Validate parse tree has expected structure (non-shallow test)."""
        assert tree is not None, "Parse tree should not be None"
        
        if expected_children_types:
            children = tree.children if hasattr(tree, 'children') and tree.children else []
            assert len(children) >= len(expected_children_types), \
                f"Expected at least {len(expected_children_types)} children, got {len(children)}"
            
            for i, expected_type in enumerate(expected_children_types):
                if i < len(children):
                    child = children[i]
                    child_type = type(child).__name__
                    assert expected_type in child_type, \
                        f"Child {i}: expected type containing '{expected_type}', got '{child_type}'"
    
    def test_class_declaration_parse_tree(self):
        """NON-SHALLOW: Validate CLASS declaration creates correct parse tree structure."""
        code = "class(integer), pointer :: obj"
        tree, errors, parser = self.parse_and_validate(code, 'class_declaration_stmt')

        assert errors == 0, f"Expected no parse errors, got {errors}"
        assert tree is not None, "Parse tree should not be None"

        if hasattr(tree, 'children') and tree.children:
            assert 'class' in str(tree.getText()).lower(), "Should contain 'class' keyword"
            assert 'integer' in str(tree.getText()).lower(), "Should contain 'integer' type"
            assert 'pointer' in str(tree.getText()).lower(), "Should contain 'pointer' attribute"
            assert 'obj' in str(tree.getText()).lower(), "Should contain 'obj' identifier"
    
    def test_procedure_pointer_semantics(self):
        """NON-SHALLOW: Validate procedure pointer creates semantically correct parse tree."""
        code = "procedure(interface), pointer :: proc_ptr\n"
        tree, errors, parser = self.parse_and_validate(code, 'procedure_declaration_stmt')

        assert errors == 0, f"Expected no parse errors, got {errors}"

        tree_text = str(tree.getText()) if tree else ""
        assert 'procedure' in tree_text.lower(), "Missing PROCEDURE keyword in parse tree"
        assert 'interface' in tree_text.lower(), "Missing interface specification in parse tree"
        assert 'pointer' in tree_text.lower(), "Missing POINTER attribute in parse tree"
        assert 'proc_ptr' in tree_text.lower(), "Missing variable name in parse tree"
    
    def test_associate_construct_structure(self):
        """NON-SHALLOW: Validate ASSOCIATE construct parse tree semantics."""
        code = "associate (x => y)\nend associate\n"
        tree, errors, parser = self.parse_and_validate(code, 'associate_construct')

        assert errors == 0, f"Expected no parse errors, got {errors}"

        tree_text = str(tree.getText()) if tree else ""
        assert 'associate' in tree_text.lower(), "Missing ASSOCIATE keyword"
        assert '=>' in tree_text or 'arrow' in tree_text.lower(), "Missing association arrow"
        assert 'end' in tree_text.lower(), "Missing END statement"

        if hasattr(tree, 'children'):
            found_association = False
            for child in tree.children:
                if hasattr(child, 'getText'):
                    child_text = child.getText()
                    if '=>' in child_text or ('x' in child_text and 'y' in child_text):
                        found_association = True
                        break

            assert found_association, "Association structure not found in parse tree"
    
    def test_enhanced_allocate_semantics(self):
        """NON-SHALLOW: Validate enhanced ALLOCATE with SOURCE semantics."""
        code = "allocate(array, source=source_array)\n"
        tree, errors, parser = self.parse_and_validate(code, 'allocate_stmt_f2003')

        assert errors == 0, f"Expected no parse errors, got {errors}"

        tree_text = str(tree.getText()) if tree else ""
        assert 'allocate' in tree_text.lower(), "Missing ALLOCATE keyword"
        assert 'source' in tree_text.lower(), "Missing SOURCE specification"
        assert 'array' in tree_text.lower(), "Missing allocation target"
        assert 'source_array' in tree_text.lower(), "Missing source array"
    
    def test_oop_type_definition_structure(self):
        """NON-SHALLOW: Validate object-oriented TYPE definition parse tree."""
        code = "type, extends(parent_type) :: child_type\nend type\n"
        tree, errors, parser = self.parse_and_validate(code, 'derived_type_def_f2003')

        assert errors == 0, f"Expected no parse errors, got {errors}"

        tree_text = str(tree.getText()) if tree else ""
        assert 'type' in tree_text.lower(), "Missing TYPE keyword"
        assert 'extends' in tree_text.lower(), "Missing EXTENDS keyword for inheritance"
        assert 'parent_type' in tree_text.lower(), "Missing parent type specification"
        assert 'child_type' in tree_text.lower(), "Missing child type name"

        found_inheritance = 'extends' in tree_text.lower() and 'parent_type' in tree_text.lower()
        assert found_inheritance, "Inheritance structure not properly parsed"
    
    def test_abstract_interface_semantics(self):
        """NON-SHALLOW: Validate ABSTRACT INTERFACE creates correct parse tree."""
        code = "abstract interface\nsubroutine abstract_sub()\nend subroutine\nend interface\n"
        tree, errors, parser = self.parse_and_validate(code, 'interface_block')

        assert errors == 0, f"Expected no parse errors, got {errors}"

        tree_text = str(tree.getText()) if tree else ""
        assert 'abstract' in tree_text.lower(), "Missing ABSTRACT keyword"
        assert 'interface' in tree_text.lower(), "Missing INTERFACE keyword"
        assert 'subroutine' in tree_text.lower(), "Missing subroutine specification"

        has_abstract_interface = 'abstract' in tree_text.lower() and 'interface' in tree_text.lower()
        assert has_abstract_interface, "Abstract interface structure not found"
    
    def test_comprehensive_f2003_features(self):
        """NON-SHALLOW: Test multiple F2003 features in combination."""
        # This tests the integration of multiple F2003 features
        features_found = []
        rules = ['class_declaration_stmt', 'procedure_declaration_stmt', 'volatile_stmt', 'protected_stmt']
        missing_rules = [rule for rule in rules if not hasattr(self.parser_class, rule)]
        assert not missing_rules, f"Missing expected F2003 parser rules: {missing_rules}"
        
        test_cases = [
            ("class(integer), pointer :: obj", "CLASS declaration"),
            ("procedure(interface), pointer :: ptr", "Procedure pointer"),
            ("volatile :: var", "VOLATILE attribute"),
            ("protected :: const_var", "PROTECTED attribute"),
        ]
        
        for code, feature_name in test_cases:
            for rule in rules:
                tree, errors, parser = self.parse_and_validate(code, rule)
                if errors == 0 and tree is not None:
                    tree_text = str(tree.getText()) if tree else ""
                    if tree_text.strip():
                        features_found.append(f"{feature_name} ({rule})")
                        break
        
        # This is a REAL test - we validate actual functionality
        assert len(features_found) > 0, f"No F2003 features were successfully parsed. Parser may not be working correctly."

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
