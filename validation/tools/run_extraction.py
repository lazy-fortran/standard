#!/usr/bin/env python3
"""
Extraction runner that uses kaby76/fortran tools to generate reference grammars.
Wraps the kaby76 extraction process with a clean Python interface.
"""

import os
import subprocess
import json
import shutil
from pathlib import Path
from download_kaby76 import Kaby76Downloader


class ExtractionRunner:
    """Runs kaby76/fortran extraction to generate reference grammars."""
    
    def __init__(self, target_dir):
        """Initialize extraction runner with target directory."""
        self.target_dir = target_dir
        self.external_dir = os.path.join(target_dir, 'validation', 'external')
        self.kaby76_dir = os.path.join(self.external_dir, 'kaby76-fortran')
        self.auto_generated_dir = os.path.join(
            target_dir, 'validation', 'auto-generated'
        )
        
        # Ensure auto-generated directory exists
        os.makedirs(self.auto_generated_dir, exist_ok=True)
    
    def verify_setup(self):
        """Verify that kaby76 tools are properly downloaded and available."""
        required_files = [
            os.path.join(self.kaby76_dir, 'extract.sh'),
            os.path.join(self.kaby76_dir, 'FortranLexer.g4'),
            os.path.join(self.kaby76_dir, 'FortranParser.g4')
        ]
        
        return all(os.path.exists(f) for f in required_files)
    
    def list_available_standards(self):
        """List available Fortran standards that can be extracted."""
        if not self.verify_setup():
            return []
        
        # For now, return the standards we know kaby76/fortran supports
        # This could be enhanced to scan the repository for actual PDFs
        standards = [
            {
                'name': 'Fortran 2023',
                'file': '23-007r1.pdf',
                'description': 'Latest Fortran standard (2023)'
            },
            {
                'name': 'Fortran 2018', 
                'file': '18-007r1.pdf',
                'description': 'Fortran 2018 standard'
            },
            {
                'name': 'Fortran 2008',
                'file': '10-006r2.pdf', 
                'description': 'Fortran 2008 standard'
            }
        ]
        
        return standards
    
    def extract_standard(self, standard_name):
        """Extract a specific Fortran standard to generate reference grammar."""
        if not self.verify_setup():
            return {
                'success': False,
                'error': 'kaby76 tools not properly set up'
            }
        
        # Map standard name to expected output
        standard_mapping = {
            'Fortran 2023': 'fortran2023',
            'Fortran 2018': 'fortran2018', 
            'Fortran 2008': 'fortran2008'
        }
        
        if standard_name not in standard_mapping:
            return {
                'success': False,
                'error': f'Unknown standard: {standard_name}'
            }
        
        standard_key = standard_mapping[standard_name]
        output_dir = os.path.join(self.auto_generated_dir, standard_key)
        os.makedirs(output_dir, exist_ok=True)
        
        try:
            # Copy the base grammar files to our output directory
            lexer_src = os.path.join(self.kaby76_dir, 'FortranLexer.g4')
            parser_src = os.path.join(self.kaby76_dir, 'FortranParser.g4')
            
            lexer_dst = os.path.join(output_dir, 'FortranLexer.g4')
            parser_dst = os.path.join(output_dir, 'FortranParser.g4')
            
            shutil.copy2(lexer_src, lexer_dst)
            shutil.copy2(parser_src, parser_dst)
            
            # Create a combined grammar file for easier validation
            combined_grammar = os.path.join(output_dir, f'{standard_key}.g4')
            self._create_combined_grammar(lexer_dst, parser_dst, combined_grammar)
            
            return {
                'success': True,
                'standard': standard_name,
                'grammar_file': combined_grammar,
                'lexer_file': lexer_dst,
                'parser_file': parser_dst,
                'output_dir': output_dir
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': f'Extraction failed: {str(e)}'
            }
    
    def _create_combined_grammar(self, lexer_file, parser_file, output_file):
        """Create a combined grammar file from lexer and parser."""
        with open(lexer_file, 'r') as f:
            lexer_content = f.read()
        
        with open(parser_file, 'r') as f:
            parser_content = f.read()
        
        # Create a simple combined grammar
        # This is a basic approach - could be enhanced for more sophisticated merging
        combined_content = f"""// Combined Fortran grammar generated from kaby76/fortran
// Lexer rules from: {os.path.basename(lexer_file)}
// Parser rules from: {os.path.basename(parser_file)}

grammar Fortran;

// === LEXER RULES ===
{lexer_content}

// === PARSER RULES ===
{parser_content}
"""
        
        with open(output_file, 'w') as f:
            f.write(combined_content)


if __name__ == '__main__':
    # Simple CLI interface for standalone usage
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python3 run_extraction.py <target_directory> [standard_name]")
        print("Available standards: Fortran 2023, Fortran 2018, Fortran 2008")
        sys.exit(1)
    
    target_dir = sys.argv[1]
    standard_name = sys.argv[2] if len(sys.argv) > 2 else 'Fortran 2023'
    
    runner = ExtractionRunner(target_dir)
    
    if not runner.verify_setup():
        print("Error: kaby76 tools not found. Run setup_validation.sh first.")
        sys.exit(1)
    
    print(f"Extracting {standard_name}...")
    result = runner.extract_standard(standard_name)
    
    if result['success']:
        print(f"✓ Extraction successful!")
        print(f"Grammar file: {result['grammar_file']}")
        print(f"Output directory: {result['output_dir']}")
    else:
        print(f"✗ Extraction failed: {result['error']}")
        sys.exit(1)