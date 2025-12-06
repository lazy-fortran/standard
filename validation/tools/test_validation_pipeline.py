#!/usr/bin/env python3
"""
Test suite for the complete validation pipeline.
Tests the integration of kaby76 extraction with our validation framework.
"""

import unittest
import os
import tempfile
import shutil
from pathlib import Path
import sys
import pytest

# Add the tools directory to sys.path to import our modules
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from download_kaby76 import Kaby76Downloader
from run_extraction import ExtractionRunner


class TestValidationPipeline(unittest.TestCase):
    """Test suite for complete validation pipeline."""
    
    def setUp(self):
        """Set up test environment with temporary directory."""
        self.test_dir = tempfile.mkdtemp()
        self.downloader = Kaby76Downloader(self.test_dir)
        
    def tearDown(self):
        """Clean up test environment."""
        if os.path.exists(self.test_dir):
            shutil.rmtree(self.test_dir)
    
    @pytest.mark.skip(reason="External Trash/.NET toolchain not reliably available in CI (see issue #92)")
    def test_pipeline_setup(self):
        """Test that the complete pipeline can be set up."""
        # Download kaby76 tools
        self.downloader.download()
        
        # Create extraction runner
        runner = ExtractionRunner(self.test_dir)
        
        # Verify extraction runner can find the tools
        self.assertTrue(runner.verify_setup())
    
    def test_extraction_runner_initialization(self):
        """Test that extraction runner initializes correctly."""
        self.downloader.download()
        runner = ExtractionRunner(self.test_dir)
        
        self.assertEqual(runner.target_dir, self.test_dir)
        self.assertTrue(os.path.exists(runner.kaby76_dir))
    
    def test_can_list_available_standards(self):
        """Test that we can identify available Fortran standards."""
        self.downloader.download()
        runner = ExtractionRunner(self.test_dir)
        
        standards = runner.list_available_standards()
        self.assertIsInstance(standards, list)
        self.assertGreater(len(standards), 0)
        
        # Should contain at least some expected standards
        standard_names = [std['name'] for std in standards]
        self.assertIn('Fortran 2023', standard_names)
    
    @pytest.mark.skip(reason="External Trash/.NET toolchain not reliably available in CI (see issue #92)")
    def test_can_extract_single_standard(self):
        """Test that we can extract a single Fortran standard."""
        self.downloader.download()
        runner = ExtractionRunner(self.test_dir)
        
        # Try to extract Fortran 2023 (should be most complete)
        result = runner.extract_standard('Fortran 2023')
        
        self.assertTrue(result['success'])
        self.assertIn('grammar_file', result)
        self.assertTrue(os.path.exists(result['grammar_file']))
    
    @pytest.mark.skip(reason="External Trash/.NET toolchain not reliably available in CI (see issue #92)")
    def test_generated_grammar_is_valid_antlr(self):
        """Test that generated grammar is valid ANTLR4 syntax."""
        self.downloader.download()
        runner = ExtractionRunner(self.test_dir)
        
        result = runner.extract_standard('Fortran 2023')
        grammar_file = result['grammar_file']
        
        # Read grammar file and check basic ANTLR4 structure
        with open(grammar_file, 'r') as f:
            content = f.read()
        
        # Should contain basic ANTLR4 grammar structure
        self.assertIn('grammar', content)
        self.assertIn(';', content)  # Grammar rules end with semicolon
        
        # Should not be empty
        self.assertGreater(len(content.strip()), 100)


if __name__ == '__main__':
    # Set OMP_NUM_THREADS as specified in CLAUDE.md
    os.environ['OMP_NUM_THREADS'] = '24'
    unittest.main()
