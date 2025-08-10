#!/usr/bin/env python3
"""
Enhanced cross-validation framework that handles Fortran code fragments
Tests various parsing strategies for incomplete code samples
"""

import os
import sys
from typing import Dict, List, Tuple, Optional

# Add grammars to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'grammars'))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser
from antlr4.error.ErrorListener import ErrorListener


class FragmentErrorListener(ErrorListener):
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


class FragmentParser:
    """Enhanced parser that tries multiple strategies for code fragments"""
    
    def __init__(self):
        self.parse_strategies = [
            ('program_unit', self._parse_as_program_unit),
            ('declaration', self._parse_as_declaration),
            ('statement', self._parse_as_statement),
            ('expression', self._parse_as_expression),
            ('wrapped_program', self._parse_as_wrapped_program),
            ('subroutine_body', self._parse_as_subroutine_body),
            ('execution_part', self._parse_as_execution_part),
            ('if_construct', self._parse_as_if_construct),
            ('execution_with_end', self._parse_as_execution_with_end),
            ('minimal_program', self._parse_as_minimal_program),
        ]
    
    def _create_parser(self, code: str) -> Tuple[Fortran2018Parser, FragmentErrorListener]:
        """Create parser with error listener"""
        input_stream = InputStream(code)
        lexer = Fortran2018Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Fortran2018Parser(token_stream)
        
        error_listener = FragmentErrorListener()
        parser.removeErrorListeners()
        parser.addErrorListener(error_listener)
        
        return parser, error_listener
    
    def _parse_as_program_unit(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as complete program unit"""
        try:
            parser, error_listener = self._create_parser(code)
            tree = parser.program_unit_f2018()
            return len(error_listener.errors) == 0, error_listener.errors
        except Exception as e:
            return False, [{'message': f"Program unit parse error: {str(e)}"}]
    
    def _parse_as_declaration(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as declaration statement"""
        try:
            parser, error_listener = self._create_parser(code)
            # Try parsing as declaration construct
            if hasattr(parser, 'declaration_construct'):
                tree = parser.declaration_construct()
            elif hasattr(parser, 'specification_stmt'):
                tree = parser.specification_stmt()
            else:
                return False, [{'message': 'No declaration parse method found'}]
            return len(error_listener.errors) == 0, error_listener.errors
        except Exception as e:
            return False, [{'message': f"Declaration parse error: {str(e)}"}]
    
    def _parse_as_statement(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as executable statement"""
        try:
            parser, error_listener = self._create_parser(code)
            if hasattr(parser, 'executable_construct'):
                tree = parser.executable_construct()
            elif hasattr(parser, 'action_stmt'):
                tree = parser.action_stmt()
            else:
                return False, [{'message': 'No statement parse method found'}]
            return len(error_listener.errors) == 0, error_listener.errors
        except Exception as e:
            return False, [{'message': f"Statement parse error: {str(e)}"}]
    
    def _parse_as_expression(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as expression"""
        try:
            parser, error_listener = self._create_parser(code)
            if hasattr(parser, 'expr'):
                tree = parser.expr()
            elif hasattr(parser, 'primary'):
                tree = parser.primary()
            else:
                return False, [{'message': 'No expression parse method found'}]
            return len(error_listener.errors) == 0, error_listener.errors
        except Exception as e:
            return False, [{'message': f"Expression parse error: {str(e)}"}]
    
    def _parse_as_wrapped_program(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as wrapped in minimal program structure"""
        wrapped_code = f"""program test_fragment
{code}
end program test_fragment"""
        return self._parse_as_program_unit(wrapped_code)
    
    def _parse_as_subroutine_body(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as wrapped in subroutine"""
        wrapped_code = f"""subroutine test_fragment()
{code}
end subroutine test_fragment"""
        return self._parse_as_program_unit(wrapped_code)
    
    def _parse_as_execution_part(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as execution part of a program"""
        try:
            parser, error_listener = self._create_parser(code)
            if hasattr(parser, 'execution_part_f2018'):
                tree = parser.execution_part_f2018()
            elif hasattr(parser, 'execution_part'):
                tree = parser.execution_part()
            else:
                return False, [{'message': 'No execution part parse method found'}]
            return len(error_listener.errors) == 0, error_listener.errors
        except Exception as e:
            return False, [{'message': f"Execution part parse error: {str(e)}"}]
    
    def _parse_as_if_construct(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as IF construct directly"""
        try:
            # Clean up orphaned 'end' statements for IF constructs
            lines = code.strip().split('\n')
            clean_lines = []
            
            for line in lines:
                stripped = line.strip().lower()
                # Skip orphaned 'end' statements at the end of IF constructs
                if stripped == 'end' and clean_lines and any('endif' in cl.lower() or 'end if' in cl.lower() for cl in clean_lines):
                    continue
                clean_lines.append(line)
            
            clean_code = '\n'.join(clean_lines)
            
            parser, error_listener = self._create_parser(clean_code)
            if hasattr(parser, 'if_construct'):
                tree = parser.if_construct()
            elif hasattr(parser, 'block_if_construct'):
                tree = parser.block_if_construct()
            else:
                return False, [{'message': 'No IF construct parse method found'}]
            return len(error_listener.errors) == 0, error_listener.errors
        except Exception as e:
            return False, [{'message': f"IF construct parse error: {str(e)}"}]
    
    def _parse_as_execution_with_end(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as execution part ending with orphaned END (common in fragments)"""
        try:
            # This handles cases like IF...ENDIF...END where END terminates a program unit
            lines = code.strip().split('\n')
            
            # Check if this looks like an IF construct with trailing END
            if len(lines) >= 2:
                first_line = lines[0].strip().lower()
                last_line = lines[-1].strip().lower()
                
                if first_line.startswith('if ') and 'then' in first_line and last_line == 'end':
                    # This looks like: IF...THEN / ENDIF / END
                    # Wrap as a main program
                    execution_part = '\n'.join(lines[:-1])  # All except the END
                    wrapped_code = f"""program test_fragment
{execution_part}
end program test_fragment"""
                    return self._parse_as_program_unit(wrapped_code)
            
            return False, [{'message': 'Does not match execution_with_end pattern'}]
            
        except Exception as e:
            return False, [{'message': f"Execution with end parse error: {str(e)}"}]
    
    def _parse_as_minimal_program(self, code: str) -> Tuple[bool, List[Dict]]:
        """Parse as minimal program with implied structure"""
        # Remove any trailing 'end' that might be orphaned and fix endif
        lines = code.strip().split('\n')
        clean_lines = []
        
        for line in lines:
            stripped = line.strip().lower()
            # Skip orphaned 'end' statements
            if stripped == 'end' and clean_lines:
                continue
            clean_lines.append(line)
        
        clean_code = '\n'.join(clean_lines)
        
        wrapped_code = f"""program test_fragment
{clean_code}
end program test_fragment"""
        return self._parse_as_program_unit(wrapped_code)
    
    def parse_fragment(self, code: str, filename: str = "unknown") -> Dict:
        """Parse code fragment using multiple strategies"""
        results = []
        
        for strategy_name, strategy_func in self.parse_strategies:
            success, errors = strategy_func(code)
            results.append({
                'strategy': strategy_name,
                'success': success,
                'error_count': len(errors),
                'errors': errors
            })
            
            if success:
                break  # Stop at first successful parse
        
        # Find best result (successful or least errors)
        best_result = min(results, key=lambda r: (not r['success'], r['error_count']))
        
        return {
            'file': filename,
            'success': best_result['success'],
            'best_strategy': best_result['strategy'],
            'error_count': best_result['error_count'],
            'errors': best_result['errors'],
            'all_strategies': results,
            'file_size': len(code),
            'line_count': code.count('\n') + 1
        }


def test_fragment_parser():
    """Test the fragment parser with sample code"""
    parser = FragmentParser()
    
    test_cases = [
        ("integer(4) :: x", "declaration"),
        ("call subroutine_name()", "statement"),
        ("x + y", "expression"),
        ("program test\nend program", "complete_program"),
    ]
    
    print("Testing Fragment Parser:")
    print("=" * 40)
    
    for code, description in test_cases:
        result = parser.parse_fragment(code, description)
        status = "✓" if result['success'] else "✗"
        print(f"{status} {description}: {result['best_strategy']} "
              f"({result['error_count']} errors)")
        
        if not result['success'] and result['errors']:
            print(f"    Error: {result['errors'][0]['message'][:60]}...")


if __name__ == "__main__":
    test_fragment_parser()