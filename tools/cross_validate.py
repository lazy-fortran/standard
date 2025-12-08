#!/usr/bin/env python3
"""
Cross-validation testing framework for Fortran2018 parser
Tests our parser against kaby76/fortran validation examples
"""

import os
import sys
import requests
import tempfile
import shutil
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import json

# Add grammars to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'grammars/generated/modern'))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser
from antlr4.error.ErrorListener import ErrorListener
from fragment_parser import FragmentParser


class ValidationErrorListener(ErrorListener):
    """Collect parsing errors for analysis"""
    
    def __init__(self):
        super().__init__()
        self.errors = []
        
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append({
            'line': line,
            'column': column,
            'message': msg,
            'symbol': str(offendingSymbol) if offendingSymbol else None
        })


class CrossValidator:
    """Cross-validation framework for Fortran parsers"""
    
    def __init__(self, cache_dir: str = "validation_cache"):
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(exist_ok=True)
        self.results = []
        self.fragment_parser = FragmentParser()
        
    def download_examples(self) -> List[str]:
        """Download Fortran examples from kaby76/fortran repository"""
        examples_url = "https://api.github.com/repos/kaby76/fortran/contents/examples"
        
        try:
            response = requests.get(examples_url)
            response.raise_for_status()
            files_info = response.json()
            
            downloaded_files = []
            for file_info in files_info:
                if file_info['name'].endswith(('.f90', '.f', '.for')):
                    file_path = self.cache_dir / file_info['name']
                    
                    if not file_path.exists():
                        # Download file content
                        file_response = requests.get(file_info['download_url'])
                        file_response.raise_for_status()
                        
                        with open(file_path, 'w', encoding='utf-8') as f:
                            f.write(file_response.text)
                            
                        print(f"Downloaded: {file_info['name']}")
                    
                    downloaded_files.append(str(file_path))
                    
            return downloaded_files
            
        except Exception as e:
            print(f"Error downloading examples: {e}")
            return []
    
    def parse_with_fortran2018(self, fortran_code: str) -> Tuple[bool, List[Dict]]:
        """Parse Fortran code with our Fortran2018 parser"""
        try:
            input_stream = InputStream(fortran_code)
            lexer = Fortran2018Lexer(input_stream)
            token_stream = CommonTokenStream(lexer)
            parser = Fortran2018Parser(token_stream)
            
            error_listener = ValidationErrorListener()
            parser.removeErrorListeners()
            parser.addErrorListener(error_listener)
            
            # Parse from program unit (F2018 top-level rule)
            tree = parser.program_unit_f2018()
            
            return len(error_listener.errors) == 0, error_listener.errors
            
        except Exception as e:
            return False, [{'message': f"Parser exception: {str(e)}"}]
    
    def validate_file(self, file_path: str) -> Dict:
        """Validate a single Fortran file using fragment parsing strategies"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                
            # Use enhanced fragment parser
            result = self.fragment_parser.parse_fragment(content, 
                                                       os.path.basename(file_path))
            result['path'] = file_path
            return result
            
        except Exception as e:
            return {
                'file': os.path.basename(file_path),
                'path': file_path,
                'success': False,
                'best_strategy': 'none',
                'error_count': 1,
                'errors': [{'message': f"File processing error: {str(e)}"}],
                'file_size': 0,
                'line_count': 0
            }
    
    def run_validation(self) -> Dict:
        """Run full cross-validation test suite"""
        print("Cross-Validation Framework for Fortran2018 Parser")
        print("=" * 50)
        
        # Download examples
        print("Downloading validation examples...")
        example_files = self.download_examples()
        
        if not example_files:
            return {'error': 'No validation files downloaded'}
            
        print(f"Found {len(example_files)} validation files")
        
        # Run validation
        results = []
        passed = 0
        failed = 0
        
        for file_path in example_files:
            result = self.validate_file(file_path)
            results.append(result)
            
            if result['success']:
                passed += 1
                print(f"✓ {result['file']} ({result['best_strategy']})")
            else:
                failed += 1
                print(f"✗ {result['file']} ({result['error_count']} errors, "
                      f"best: {result['best_strategy']})")
        
        # Generate summary
        summary = {
            'total_files': len(example_files),
            'passed': passed,
            'failed': failed,
            'success_rate': (passed / len(example_files)) * 100 if example_files else 0,
            'results': results
        }
        
        return summary
    
    def generate_report(self, summary: Dict, output_file: str = None):
        """Generate detailed validation report"""
        if output_file is None:
            output_file = self.cache_dir / "validation_report.json"
            
        with open(output_file, 'w') as f:
            json.dump(summary, f, indent=2)
            
        print(f"\nValidation Summary:")
        print(f"Total Files: {summary['total_files']}")
        print(f"Passed: {summary['passed']}")
        print(f"Failed: {summary['failed']}")
        print(f"Success Rate: {summary['success_rate']:.1f}%")
        print(f"Report saved to: {output_file}")
        
        # Show top error patterns
        error_patterns = {}
        for result in summary['results']:
            if not result['success']:
                for error in result['errors']:
                    msg = error['message'][:100]  # First 100 chars
                    error_patterns[msg] = error_patterns.get(msg, 0) + 1
        
        if error_patterns:
            print(f"\nTop Error Patterns:")
            for pattern, count in sorted(error_patterns.items(), 
                                       key=lambda x: x[1], reverse=True)[:5]:
                print(f"  {count}x: {pattern}")


def main():
    """Main entry point"""
    if len(sys.argv) > 1:
        cache_dir = sys.argv[1]
    else:
        cache_dir = "validation_cache"
        
    validator = CrossValidator(cache_dir)
    summary = validator.run_validation()
    
    if 'error' not in summary:
        validator.generate_report(summary)
    else:
        print(f"Validation failed: {summary['error']}")


if __name__ == "__main__":
    main()