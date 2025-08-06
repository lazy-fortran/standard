#!/usr/bin/env python3
"""
Test suite for kaby76/fortran downloader functionality.
Follows TDD principles - tests written first, then implementation.
"""

import unittest
import os
import shutil
import tempfile
from pathlib import Path
import sys

# Add the tools directory to sys.path to import our module
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from download_kaby76 import Kaby76Downloader


class TestKaby76Downloader(unittest.TestCase):
    """Test suite for Kaby76Downloader class."""
    
    def setUp(self):
        """Set up test environment with temporary directory."""
        self.test_dir = tempfile.mkdtemp()
        self.downloader = Kaby76Downloader(self.test_dir)
        
    def tearDown(self):
        """Clean up test environment."""
        if os.path.exists(self.test_dir):
            shutil.rmtree(self.test_dir)
    
    def test_downloader_initialization(self):
        """Test that downloader initializes with correct target directory."""
        self.assertEqual(self.downloader.target_dir, self.test_dir)
        self.assertTrue(os.path.exists(self.test_dir))
    
    def test_download_creates_external_directory(self):
        """Test that download creates validation/external/ directory."""
        external_dir = os.path.join(self.test_dir, 'validation', 'external')
        self.downloader.download()
        self.assertTrue(os.path.exists(external_dir))
    
    def test_download_clones_kaby76_repository(self):
        """Test that download actually clones the kaby76/fortran repo."""
        self.downloader.download()
        kaby76_dir = os.path.join(
            self.test_dir, 'validation', 'external', 'kaby76-fortran'
        )
        self.assertTrue(os.path.exists(kaby76_dir))
        
        # Check for key files that should exist in kaby76/fortran
        expected_files = [
            'extract.sh',
            'FortranLexer.g4', 
            'FortranParser.g4'
        ]
        
        for expected_file in expected_files:
            file_path = os.path.join(kaby76_dir, expected_file)
            self.assertTrue(
                os.path.exists(file_path), 
                f"Expected file {expected_file} not found"
            )
    
    def test_download_is_idempotent(self):
        """Test that running download multiple times doesn't break anything."""
        # First download
        self.downloader.download()
        kaby76_dir = os.path.join(
            self.test_dir, 'validation', 'external', 'kaby76-fortran'
        )
        
        # Get modification time of a key file
        extract_script = os.path.join(kaby76_dir, 'extract.sh')
        first_mtime = os.path.getmtime(extract_script)
        
        # Second download
        self.downloader.download()
        
        # Should still exist and be functional
        self.assertTrue(os.path.exists(extract_script))
        # Should have updated (or at least not broken)
        second_mtime = os.path.getmtime(extract_script)
        self.assertGreaterEqual(second_mtime, first_mtime)
    
    def test_get_extraction_script_path(self):
        """Test that we can get the path to the extraction script."""
        self.downloader.download()
        script_path = self.downloader.get_extraction_script_path()
        
        self.assertTrue(os.path.exists(script_path))
        self.assertTrue(script_path.endswith('extract.sh'))
        self.assertTrue(os.access(script_path, os.X_OK))
    
    def test_get_grammar_files_paths(self):
        """Test that we can get paths to the grammar files."""
        self.downloader.download()
        lexer_path, parser_path = self.downloader.get_grammar_files_paths()
        
        self.assertTrue(os.path.exists(lexer_path))
        self.assertTrue(os.path.exists(parser_path))
        self.assertTrue(lexer_path.endswith('FortranLexer.g4'))
        self.assertTrue(parser_path.endswith('FortranParser.g4'))


if __name__ == '__main__':
    # Set OMP_NUM_THREADS as specified in CLAUDE.md
    os.environ['OMP_NUM_THREADS'] = '24'
    unittest.main()