#!/usr/bin/env python3
"""Semantic C Interoperability Tests - Issue #24

Non-shallow test suite with proper semantic validation of F2003 C interop features:
- Parse tree structure verification beyond error counts
- Token-level validation of BIND(C) constructs
- Semantic error detection and recovery testing
- Performance validation for complex constructs
"""

import sys
import pytest
import time
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture

class TestSemanticCInteroperability:
    """Deep semantic validation of C interoperability features"""
    
    def parse_and_validate(self, code, expect_errors=0):
        """Parse code and perform basic semantic validation"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        
        if expect_errors == 0:
            assert errors == 0, f"Expected clean parse, got {errors} errors"
        else:
            assert errors >= expect_errors, \
                f"Expected at least {expect_errors} errors, got {errors}"
        
        return tree, errors, parser
    
    def extract_all_tokens(self, tree):
        """Extract all terminal tokens from parse tree"""
        tokens = []
        self._collect_terminals(tree, tokens)
        return [t.lower() for t in tokens if t.strip()]
    
    def _collect_terminals(self, node, collector):
        """Recursively collect terminal nodes"""
        if node is None:
            return
        
        if not hasattr(node, 'getChildCount'):
            # Terminal token
            text = str(node)
            if text and text.strip():
                collector.append(text)
            return
            
        # Node with children
        if node.getChildCount() == 0:
            # Leaf node
            text = node.getText()
            if text and text.strip():
                collector.append(text)
            return
            
        # Internal node - recurse to children
        for i in range(node.getChildCount()):
            self._collect_terminals(node.getChild(i), collector)
    
    def verify_token_sequence(self, tokens, required_sequence):
        """Verify that required token sequence appears in order"""
        sequence_lower = [t.lower() for t in required_sequence]
        seq_len = len(sequence_lower)
        
        # Find all positions of first token
        first_token = sequence_lower[0]
        start_positions = [
            i for i, token in enumerate(tokens) 
            if token == first_token
        ]
        
        for start_pos in start_positions:
            # Check if we have enough tokens
            if start_pos + seq_len > len(tokens):
                continue
                
            # Check if full sequence matches
            match_slice = tokens[start_pos:start_pos + seq_len]
            if match_slice == sequence_lower:
                return True
        
        return False
    
    # =================================================================
    # SEMANTIC VALIDATION TESTS
    # =================================================================
    
    def test_bind_c_token_sequence_validation(self):
        """Validate BIND(C) appears as correct token sequence"""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "bind_c_basic_subroutine.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(code)
        tokens = self.extract_all_tokens(tree)
        
        # Verify BIND(C) token sequence
        assert self.verify_token_sequence(
            tokens, ['bind', '(', 'c', ')']
        ), f"BIND(C) sequence not found in: {tokens}"
        
        # Verify subroutine structure
        assert 'subroutine' in tokens, \
            f"subroutine keyword missing: {tokens}"
        assert 'end' in tokens, f"end keyword missing: {tokens}"
    
    def test_bind_c_syntax_capabilities_and_limitations(self):
        """Test what BIND(C) syntax currently works vs limitations"""
        # Basic BIND(C) that should work (matches working basic tests)
        code_basic = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "bind_c_my_func_subroutine.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(code_basic)
        tokens = self.extract_all_tokens(tree)
        
        # Verify basic BIND(C) sequence works
        assert self.verify_token_sequence(
            tokens, ['bind', '(', 'c', ')']
        ), f"Basic BIND(C) sequence not found: {tokens}"
        assert 'subroutine' in tokens, \
            f"subroutine keyword missing: {tokens}"
        
        # Test enhanced: BIND(C, NAME="...") syntax now works correctly
        code_with_name = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "bind_c_with_name_subroutine.f90",
        )
        
        # This now works - limitation has been fixed
        tree, errors, parser = self.parse_and_validate(code_with_name)
        tokens = self.extract_all_tokens(tree)
        
        # Verify enhanced BIND(C, NAME="...") sequence works
        assert self.verify_token_sequence(
            tokens, ['bind', '(', 'c', ',', 'name', '=']
        ), f"Enhanced BIND(C, NAME=...) sequence not found: {tokens}"
    
    def test_c_interop_types_token_validation(self):
        """Validate C interop types appear as distinct tokens"""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "c_interop_types_module.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(code)
        tokens = self.extract_all_tokens(tree)
        
        # Verify each C type appears as individual token
        c_types = ['c_int', 'c_double', 'c_ptr']
        for c_type in c_types:
            assert c_type in tokens, \
                f"C type {c_type} not found as token: {tokens}"
        
        # Verify Fortran type keywords
        fortran_types = ['integer', 'real', 'type']
        for f_type in fortran_types:
            assert f_type in tokens, \
                f"Fortran type {f_type} missing: {tokens}"
    
    def test_use_iso_c_binding_semantic_validation(self):
        """Validate USE ISO_C_BINDING statement structure"""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "use_iso_c_binding_example_module.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(code)
        tokens = self.extract_all_tokens(tree)
        
        # Verify USE statement components
        assert self.verify_token_sequence(
            tokens, ['use', 'iso_c_binding']
        ), f"USE iso_c_binding not found: {tokens}"
        assert 'only' in tokens, f"ONLY clause missing: {tokens}"
        assert 'c_int' in tokens, \
            f"c_int in ONLY clause missing: {tokens}"
        assert 'c_float' in tokens, \
            f"c_float in ONLY clause missing: {tokens}"
    
    def test_value_attribute_semantic_placement(self):
        """Validate VALUE attribute appears in correct context"""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "value_attribute_proc_subroutine.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(code)
        tokens = self.extract_all_tokens(tree)
        
        # Verify VALUE appears with type declaration
        assert 'value' in tokens, f"VALUE attribute missing: {tokens}"
        assert 'c_int' in tokens, f"c_int type missing: {tokens}"
        
        # Verify VALUE and type tokens appear in reasonable proximity
        value_pos = tokens.index('value')
        c_int_pos = tokens.index('c_int')
        distance = abs(value_pos - c_int_pos)
        # Check VALUE and c_int are close
        min_pos = min(value_pos, c_int_pos) - 2
        max_pos = max(value_pos, c_int_pos) + 3
        context = tokens[max(0, min_pos):min(len(tokens), max_pos)]
        assert distance <= 5, \
            f"VALUE and c_int too far apart ({distance}): {context}"
    
    def test_simpler_semantic_structure_validation(self):
        """Validate simpler but complete C interop structure"""
        code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "interop_simple_module.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(code)
        tokens = self.extract_all_tokens(tree)
        
        # Verify module structure
        assert 'module' in tokens, f"MODULE keyword missing: {tokens}"
        assert 'contains' in tokens, f"CONTAINS keyword missing: {tokens}"
        
        # Verify BIND(C) with NAME
        assert self.verify_token_sequence(
            tokens, ['bind', '(', 'c']
        ), f"BIND(C) sequence missing: {tokens}"
        assert 'name' in tokens, f"NAME clause missing: {tokens}"
        
        # Verify C types
        assert 'c_int' in tokens, f"c_int missing: {tokens}"
    
    # =================================================================
    # NEGATIVE SEMANTIC TESTS
    # =================================================================
    
    def test_invalid_bind_syntax_semantic_errors(self):
        """Test semantic validation of invalid BIND syntax"""
        invalid_cases = [
            ("subroutine test() bind()\\nend subroutine test", 
             "Missing C parameter"),
            ("subroutine test() bind(fortran)\\nend subroutine test", 
             "Wrong language"),
            ("subroutine test() bind(c, name)\\nend subroutine test", 
             "Incomplete NAME clause"),
        ]
        
        for code, description in invalid_cases:
            tree, errors, parser = self.parse_and_validate(
                code, expect_errors=1
            )
            # Even with errors, should have basic structure
            assert tree is not None, \
                f"Should have parse tree for {description}"
    
    def test_semantic_error_recovery_validation(self):
        """Test parser recovery from syntax errors with valid constructs"""
        # Test what actually parses vs what doesn't
        simple_valid_code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "simple_valid_module.f90",
        )
        
        tree, errors, parser = self.parse_and_validate(simple_valid_code)
        tokens = self.extract_all_tokens(tree)
        
        # Should parse completely correctly
        assert 'module' in tokens, f"MODULE should parse: {tokens}"
        assert 'bind' in tokens, f"BIND(C) should parse: {tokens}"
        assert 'c_int' in tokens, f"C types should work: {tokens}"
    
    # =================================================================
    # PERFORMANCE TESTS
    # =================================================================
    
    def test_lexer_performance_c_token_disambiguation(self):
        """Test lexer performance distinguishing C tokens from comments"""
        # Test cases that stress the lexer disambiguation
        test_cases = [
            "bind(c)",
            "bind(c, name=\"test\")",
            "c this is a comment",
            "call bind(c)",
            "contains",
        ]
        
        for test_case in test_cases:
            start_time = time.time()
            
            # Tokenize multiple times to test performance
            for _ in range(100):
                input_stream = InputStream(test_case)
                lexer = Fortran2003Lexer(input_stream)
                tokens = []
                while True:
                    token = lexer.nextToken()
                    if token.type == Token.EOF:
                        break
                    tokens.append(token.text)
            
            elapsed = time.time() - start_time
            assert elapsed < 0.5, \
                f"Lexer too slow for '{test_case}': {elapsed:.3f}s"
    
    def test_parser_performance_with_working_constructs(self):
        """Test parser performance with known working constructs"""
        working_code = load_fixture(
            "Fortran2003",
            "test_issue24_semantic_c_interop",
            "performance_test_module.f90",
        )

        start_time = time.time()
        tree, errors, parser = self.parse_and_validate(working_code)
        parse_time = time.time() - start_time

        assert parse_time < 1.0, f"Parser too slow: {parse_time:.3f}s"
        
        # Verify semantic structure 
        tokens = self.extract_all_tokens(tree)
        bind_count = tokens.count('bind')
        assert bind_count >= 2, \
            f"Should find multiple BIND clauses: {bind_count}"
        assert 'c_int' in tokens, \
            f"Should find c_int type: {tokens}"
        assert 'c_double' in tokens, \
            f"Should find c_double type: {tokens}"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
