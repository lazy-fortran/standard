#!/usr/bin/env python3
"""
Extraction runner that uses kaby76/fortran tools to generate reference grammars.
Runs the complete PDF extraction pipeline to generate ANTLR4 grammars from ISO specs.
"""

import os
import subprocess
import json
import shutil
from pathlib import Path
from download_kaby76 import Kaby76Downloader
from setup_dependencies import DependencyManager


class ExtractionRunner:
    """Runs kaby76/fortran extraction to generate reference grammars from PDFs."""
    
    def __init__(self, target_dir):
        """Initialize extraction runner with target directory."""
        self.target_dir = os.path.abspath(target_dir)
        self.external_dir = os.path.join(self.target_dir, 'validation', 'external')
        self.kaby76_dir = os.path.join(self.external_dir, 'kaby76-fortran')
        self.auto_generated_dir = os.path.join(
            self.target_dir, 'validation', 'auto-generated'
        )
        
        # Initialize dependency manager
        self.deps_manager = DependencyManager(self.target_dir)
        
        # Ensure auto-generated directory exists
        os.makedirs(self.auto_generated_dir, exist_ok=True)
    
    def verify_setup(self):
        """Verify that kaby76 tools and dependencies are available."""
        # Check kaby76 tools
        required_files = [
            os.path.join(self.kaby76_dir, 'extract.sh'),
            os.path.join(self.kaby76_dir, 'extraction')
        ]
        
        kaby76_ok = all(os.path.exists(f) for f in required_files)
        
        # Check dependencies
        deps_ok = self.deps_manager.verify_dependencies()
        
        return kaby76_ok and all(deps_ok.values())
    
    def setup_dependencies(self):
        """Set up all required dependencies for extraction."""
        print("Setting up extraction dependencies...")
        return self.deps_manager.setup_all_dependencies()
    
    def list_available_standards(self):
        """List available Fortran standards that can be extracted from PDFs."""
        standards = []
        
        # Check which PDFs are actually available in kaby76 repo
        pdf_mapping = {
            'N692.pdf': {
                'name': 'Fortran 90',
                'description': 'Fortran 90 standard (1990)'
            },
            'N1191.pdf': {
                'name': 'Fortran 95', 
                'description': 'Fortran 95 standard (1995)'
            },
            '04-007.pdf': {
                'name': 'Fortran 2003',
                'description': 'Fortran 2003 standard'
            },
            '10-007.pdf': {
                'name': 'Fortran 2008',
                'description': 'Fortran 2008 standard'
            },
            '18-007r1.pdf': {
                'name': 'Fortran 2018',
                'description': 'Fortran 2018 standard'
            },
            '23-007r1.pdf': {
                'name': 'Fortran 2023',
                'description': 'Latest Fortran standard (2023)'
            }
        }
        
        for pdf_file, info in pdf_mapping.items():
            pdf_path = os.path.join(self.kaby76_dir, pdf_file)
            if os.path.exists(pdf_path):
                standards.append({
                    'name': info['name'],
                    'file': pdf_file,
                    'description': info['description'],
                    'path': pdf_path
                })
        
        return standards
    
    def extract_standard(self, standard_name):
        """Extract a specific Fortran standard by running kaby76 extraction on PDF."""
        if not self.verify_setup():
            # Try to set up dependencies first
            setup_result = self.setup_dependencies()
            if not setup_result.get('dotnet', {}).get('success') or not setup_result.get('tritext', {}).get('success'):
                return {
                    'success': False,
                    'error': 'Dependencies not available and setup failed'
                }
        
        # Find the PDF file for this standard
        available_standards = self.list_available_standards()
        pdf_file = None
        for std in available_standards:
            if std['name'] == standard_name:
                pdf_file = std['file']
                break
        
        if not pdf_file:
            return {
                'success': False,
                'error': f'PDF not found for standard: {standard_name}'
            }
        
        standard_key = standard_name.lower().replace(' ', '')
        output_dir = os.path.join(self.auto_generated_dir, standard_key)
        os.makedirs(output_dir, exist_ok=True)
        
        try:
            # Run the actual kaby76 extraction
            result = self._run_kaby76_extraction(pdf_file, output_dir)
            
            if result['success']:
                return {
                    'success': True,
                    'standard': standard_name,
                    'grammar_file': result.get('combined_grammar'),
                    'lexer_file': result.get('lexer_file'),
                    'parser_file': result.get('parser_file'),
                    'output_dir': output_dir,
                    'extraction_log': result.get('log', [])
                }
            else:
                return {
                    'success': False,
                    'error': result.get('error', 'Extraction failed'),
                    'extraction_log': result.get('log', [])
                }
            
        except Exception as e:
            return {
                'success': False,
                'error': f'Extraction failed: {str(e)}'
            }
    
    def _run_kaby76_extraction(self, pdf_file, output_dir):
        """Run the kaby76 extract.sh script on a specific PDF."""
        pdf_path = os.path.join(self.kaby76_dir, pdf_file)
        
        if not os.path.exists(pdf_path):
            return {'success': False, 'error': f'PDF file not found: {pdf_path}'}
        
        log = []
        
        try:
            # First, patch the extract.sh script to use our tools
            self._patch_extract_script()
            
            # Set up environment with our dependencies
            env = os.environ.copy()
            env.update(self.deps_manager.get_environment_vars())
            
            log.append(f"Running extraction on {pdf_file}")
            
            # Run the extraction script in the kaby76 directory
            result = subprocess.run(
                ['bash', './extract.sh', pdf_file],
                cwd=self.kaby76_dir,
                env=env,
                capture_output=True,
                text=True,
                timeout=300
            )
            
            log.append(f"Extract script exit code: {result.returncode}")
            if result.stdout:
                log.extend(result.stdout.split('\n'))
            if result.stderr:
                log.extend([f"STDERR: {line}" for line in result.stderr.split('\n')])
            
            if result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Extract script failed with code {result.returncode}',
                    'log': log
                }
            
            # Copy generated files to our output directory
            generated_files = self._collect_generated_files(output_dir)
            
            return {
                'success': True,
                'log': log,
                **generated_files
            }
            
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'error': 'Extraction timed out after 5 minutes',
                'log': log
            }
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'log': log
            }
    
    def _patch_extract_script(self):
        """Patch the kaby76 extract.sh script to use our local tools."""
        extract_script = os.path.join(self.kaby76_dir, 'extract.sh')
        backup_script = os.path.join(self.kaby76_dir, 'extract.sh.backup')
        
        # Only patch once
        if os.path.exists(backup_script):
            return
        
        # Read the original script
        with open(extract_script, 'r') as f:
            content = f.read()
        
        # Create backup
        shutil.copy2(extract_script, backup_script)
        
        # Apply patches to use our tools
        bin_dir = os.path.join(self.target_dir, 'validation', 'dependencies', 'bin')
        dotnet_path = os.path.join(self.target_dir, 'validation', 'dependencies', 'dotnet', 'dotnet')
        
        patches = {
            # Replace tool calls with our versions
            'tritext.exe': os.path.join(bin_dir, 'tritext'),
            'dotnet build': f'{dotnet_path} build',
            'trparse ': f'{os.path.join(bin_dir, "trparse")} ',
            'trquery ': f'{os.path.join(bin_dir, "trquery")} ',
            'trtext ': f'{os.path.join(bin_dir, "trtext")} ',
            'trsponge ': f'{os.path.join(bin_dir, "trsponge")} ',
            'dos2unix': os.path.join(self.target_dir, 'validation', 'tools', 'dos2unix'),
            
            # Fix path references for our RuleExtraction tool
            './extraction/bin/Debug/net8.0/RuleExtraction.exe': 
                f'{dotnet_path} ./extraction/bin/Debug/net8.0/RuleExtraction.dll'
        }
        
        # Apply all patches
        patched_content = content
        for old, new in patches.items():
            patched_content = patched_content.replace(old, new)
        
        # Write the patched script
        with open(extract_script, 'w') as f:
            f.write(patched_content)
    
    def _collect_generated_files(self, output_dir):
        """Collect the generated grammar files from kaby76 directory."""
        result = {}
        
        # Look for generated grammar files in kaby76 directory
        lexer_src = os.path.join(self.kaby76_dir, 'FortranLexer.g4')
        parser_src = os.path.join(self.kaby76_dir, 'FortranParser.g4')
        
        if os.path.exists(lexer_src):
            lexer_dst = os.path.join(output_dir, 'FortranLexer.g4')
            shutil.copy2(lexer_src, lexer_dst)
            result['lexer_file'] = lexer_dst
        
        if os.path.exists(parser_src):
            parser_dst = os.path.join(output_dir, 'FortranParser.g4')
            shutil.copy2(parser_src, parser_dst)
            result['parser_file'] = parser_dst
        
        # Create combined grammar if both files exist
        if 'lexer_file' in result and 'parser_file' in result:
            combined_file = os.path.join(output_dir, 'combined.g4')
            self._create_combined_grammar(result['lexer_file'], result['parser_file'], combined_file)
            result['combined_grammar'] = combined_file
        
        return result
    
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
