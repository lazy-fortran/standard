#!/usr/bin/env python3
"""
Automated downloader for kaby76/fortran tools.
Downloads and sets up the kaby76/fortran repository for grammar extraction.
"""

import os
import subprocess
import shutil
from pathlib import Path


class Kaby76Downloader:
    """Downloads and manages kaby76/fortran repository."""
    
    REPO_URL = "https://github.com/kaby76/fortran.git"
    
    def __init__(self, target_dir):
        """Initialize downloader with target directory."""
        self.target_dir = target_dir
        self.external_dir = os.path.join(target_dir, 'validation', 'external')
        self.kaby76_dir = os.path.join(self.external_dir, 'kaby76-fortran')
        
    def download(self):
        """Download kaby76/fortran repository."""
        # Create external directory if it doesn't exist
        os.makedirs(self.external_dir, exist_ok=True)
        
        # Remove existing directory if it exists (for clean updates)
        if os.path.exists(self.kaby76_dir):
            shutil.rmtree(self.kaby76_dir)
        
        # Clone the repository
        subprocess.run([
            'git', 'clone', self.REPO_URL, self.kaby76_dir
        ], check=True, capture_output=True)
        
        # Make extract.sh executable
        extract_script = os.path.join(self.kaby76_dir, 'extract.sh')
        if os.path.exists(extract_script):
            os.chmod(extract_script, 0o755)
    
    def get_extraction_script_path(self):
        """Get path to the extraction script."""
        return os.path.join(self.kaby76_dir, 'extract.sh')
    
    def get_grammar_files_paths(self):
        """Get paths to the grammar files."""
        lexer_path = os.path.join(self.kaby76_dir, 'FortranLexer.g4')
        parser_path = os.path.join(self.kaby76_dir, 'FortranParser.g4')
        return lexer_path, parser_path


if __name__ == '__main__':
    # Simple CLI interface for standalone usage
    import sys
    
    if len(sys.argv) != 2:
        print("Usage: python3 download_kaby76.py <target_directory>")
        sys.exit(1)
    
    target_dir = sys.argv[1]
    downloader = Kaby76Downloader(target_dir)
    
    print("Downloading kaby76/fortran repository...")
    downloader.download()
    print(f"Download complete. Repository available at: {downloader.kaby76_dir}")
    
    # Verify download
    script_path = downloader.get_extraction_script_path()
    lexer_path, parser_path = downloader.get_grammar_files_paths()
    
    print(f"Extraction script: {script_path}")
    print(f"Lexer grammar: {lexer_path}")
    print(f"Parser grammar: {parser_path}")
    
    if all(os.path.exists(f) for f in [script_path, lexer_path, parser_path]):
        print("✓ All required files are present")
    else:
        print("✗ Some required files are missing")
        sys.exit(1)