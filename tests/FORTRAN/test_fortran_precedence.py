#!/usr/bin/env python3
"""
FORTRAN I precedence tests
These tests validate actual parse tree structure for operator precedence
"""

import sys
import os
import unittest
from pathlib import Path

# Add grammars directory to path for imports
sys.path.insert(0, 'grammars')

from antlr4 import InputStream, CommonTokenStream
from FORTRANLexer import FORTRANLexer
from FORTRANParser import FORTRANParser


class TestFORTRANPrecedence(unittest.TestCase):
    """Test FORTRAN I operator precedence and associativity"""
    
    def parse(self, text, rule_name='expr'):
        """Helper to parse expressions"""
        input_stream = InputStream(text)
        lexer = FORTRANLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRANParser(token_stream)
        
        rule_method = getattr(parser, rule_name)
        return rule_method()
    
    def get_tree_structure(self, node):
        """Get readable tree structure for validation"""
        if not hasattr(node, 'children') or not node.children:
            return node.getText() if hasattr(node, 'getText') else str(node)
        
        children = []
        for child in node.children:
            children.append(self.get_tree_structure(child))
        
        rule_name = type(node).__name__.replace('Context', '')
        return {rule_name: children}
    
    def test_multiplication_precedence_over_addition(self):
        """Test that A + B * C parses as A + (B * C), not (A + B) * C"""
        tree = self.parse("A + B * C")
        structure = self.get_tree_structure(tree)
        
        # The correct structure should be:
        # - Top level: AdditiveExpression (+)
        # - Left operand: A  
        # - Right operand: MultiplicativeExpression (* between B and C)
        
        # Navigate: Expr -> RelationalPrimary -> AdditiveExpression
        self.assertIn('Expr', structure)
        expr = structure['Expr'][0]
        self.assertIn('RelationalPrimary', expr)
        rel_primary = expr['RelationalPrimary'][0]
        self.assertIn('AdditiveExpression', rel_primary)
        
        # This confirms correct precedence: + at top level, * nested
        additive_expr = rel_primary['AdditiveExpression']
        
        # Should find '+' operator in Additive_op
        plus_found = False
        mult_expr_found = False
        
        for item in additive_expr:
            if isinstance(item, dict):
                if 'Additive_op' in item and '+' in str(item):
                    plus_found = True
                elif 'MultiplicativeExpression' in item:
                    mult_expr_found = True
        
        self.assertTrue(plus_found, "Addition operator not found at top level")
        self.assertTrue(mult_expr_found, 
                       "Multiplication not found as sub-expression (precedence error)")
    
    def test_division_precedence_over_addition(self):
        """Test that A + B / C parses as A + (B / C)"""
        tree = self.parse("A + B / C")
        structure = self.get_tree_structure(tree)
        
        # Navigate: Expr -> RelationalPrimary -> AdditiveExpression
        expr = structure['Expr'][0]
        rel_primary = expr['RelationalPrimary'][0]
        additive_expr = rel_primary['AdditiveExpression']
        
        # Should find '+' operator and nested MultiplicativeExpression with '/'
        plus_found = False
        div_expr_found = False
        
        for item in additive_expr:
            if isinstance(item, dict):
                if 'Additive_op' in item and '+' in str(item):
                    plus_found = True
                elif 'MultiplicativeExpression' in item and '/' in str(item):
                    div_expr_found = True
        
        self.assertTrue(plus_found, "Addition not at top level")
        self.assertTrue(div_expr_found, "Division not nested (precedence error)")
    
    def test_power_precedence_over_multiplication(self):
        """Test that A * B ** C parses as A * (B ** C)"""
        tree = self.parse("A * B ** C")
        structure = self.get_tree_structure(tree)
        
        # Navigate: Expr -> RelationalPrimary -> AdditivePrimary -> MultiplicativeExpression
        expr = structure['Expr'][0]
        rel_primary = expr['RelationalPrimary'][0]
        add_primary = rel_primary['AdditivePrimary'][0]
        mult_expr = add_primary['MultiplicativeExpression']
        
        # Should find '*' operator and nested PowerExpression with '**'
        mult_found = False
        power_expr_found = False
        
        for item in mult_expr:
            if isinstance(item, dict):
                if 'Multiplicative_op' in item and '*' in str(item):
                    mult_found = True
                elif 'UnaryPrimary' in item and 'PowerExpression' in str(item) and '**' in str(item):
                    power_expr_found = True
        
        self.assertTrue(mult_found, "Multiplication not at top level")
        self.assertTrue(power_expr_found, "Power not nested (precedence error)")
    
    def test_power_right_associativity(self):
        """Test that A ** B ** C parses as A ** (B ** C) (right associative)"""
        tree = self.parse("A ** B ** C")
        structure = self.get_tree_structure(tree)
        
        # Navigate to the PowerExpression level
        # For A ** B ** C with right associativity, we should get:
        # PowerExpression: [A, '**', PowerExpression[B, '**', C]]
        
        # Navigate deep to find the PowerExpression
        def find_power_expression(node):
            if isinstance(node, dict):
                for key, value in node.items():
                    if 'PowerExpression' in key:
                        return value
                    result = find_power_expression(value)
                    if result:
                        return result
            elif isinstance(node, list):
                for item in node:
                    result = find_power_expression(item)
                    if result:
                        return result
            return None
        
        power_expr = find_power_expression(structure)
        self.assertIsNotNone(power_expr, "Should find PowerExpression")
        
        # Should have A, '**', and another PowerExpression
        has_double_star = any('**' in str(item) for item in power_expr)
        has_nested_power = any(isinstance(item, dict) and 'PowerExpression' in str(item) 
                              for item in power_expr)
        
        self.assertTrue(has_double_star, "Power operator not found")
        self.assertTrue(has_nested_power, "Right associativity not working - no nested PowerExpression")
    
    def test_subtraction_left_associativity(self):
        """Test that A - B - C parses as (A - B) - C (left associative)"""
        tree = self.parse("A - B - C")
        structure = self.get_tree_structure(tree)
        
        # Navigate: Expr -> RelationalPrimary -> AdditiveExpression
        expr = structure['Expr'][0]
        rel_primary = expr['RelationalPrimary'][0]
        
        # For left associativity, should have nested AdditiveExpression
        # AdditiveExpression: [AdditiveExpression[A, '-', B], '-', C]
        self.assertIn('AdditiveExpression', rel_primary)
        additive_expr = rel_primary['AdditiveExpression']
        
        # Should find nested AdditiveExpression (from A - B) and another '-' operator
        has_nested_additive = False
        has_minus = False
        
        for item in additive_expr:
            if isinstance(item, dict):
                if 'AdditiveExpression' in item:
                    has_nested_additive = True
                elif 'Additive_op' in item and '-' in str(item):
                    has_minus = True
        
        self.assertTrue(has_nested_additive, "Left associativity not working - no nested AdditiveExpression")
        self.assertTrue(has_minus, "Subtraction operator not found")
    
    def test_parentheses_override_precedence(self):
        """Test that parentheses can override precedence: (A + B) * C"""
        tree = self.parse("(A + B) * C")
        structure = self.get_tree_structure(tree)
        
        # With parentheses, this should parse as (A + B) * C
        # Top-level should be MultiplicativeExpression with * operator
        
        # Navigate: Expr -> RelationalPrimary -> AdditivePrimary -> MultiplicativeExpression
        expr = structure['Expr'][0]
        rel_primary = expr['RelationalPrimary'][0]
        add_primary = rel_primary['AdditivePrimary'][0]
        
        self.assertIn('MultiplicativeExpression', add_primary)
        mult_expr = add_primary['MultiplicativeExpression']
        
        # Should find '*' operator and parenthesized expression with '+'
        mult_found = False
        paren_add_found = False
        
        for item in mult_expr:
            if isinstance(item, dict):
                if 'Multiplicative_op' in item and '*' in str(item):
                    mult_found = True
                elif 'Primary' in str(item) and '+' in str(item):
                    # Parenthesized expression should contain addition
                    paren_add_found = True
        
        self.assertTrue(mult_found, "Multiplication operator not found at top level")
        self.assertTrue(paren_add_found, "Parenthesized addition not found")
    
    def test_complex_precedence_expression(self):
        """Test complex expression: A + B * C ** D - E / F"""
        tree = self.parse("A + B * C ** D - E / F")
        structure = self.get_tree_structure(tree)
        
        # This should parse with correct precedence:
        # A + (B * (C ** D)) - (E / F)
        # The top level should be subtraction: (...) - (E / F)
        # The left side should be addition: A + (B * (C ** D))
        
        # Navigate to AdditiveExpression level
        expr = structure['Expr'][0]
        rel_primary = expr['RelationalPrimary'][0]
        
        self.assertIn('AdditiveExpression', rel_primary)
        additive_expr = rel_primary['AdditiveExpression']
        
        # Should find nested structure with proper precedence
        has_mult_expr = False
        has_div_expr = False
        has_power_expr = False
        
        expr_str = str(additive_expr)
        has_mult_expr = 'MultiplicativeExpression' in expr_str
        has_div_expr = '/' in expr_str
        has_power_expr = 'PowerExpression' in expr_str and '**' in expr_str
        
        self.assertTrue(has_mult_expr, "Multiplication expression not found")
        self.assertTrue(has_div_expr, "Division not found")  
        self.assertTrue(has_power_expr, "Power expression not found")


if __name__ == '__main__':
    unittest.main()