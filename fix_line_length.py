#!/usr/bin/env python3
"""
Script to fix line length violations in grammar files
"""

import re
from pathlib import Path


def fix_line_length(filepath, max_length=88):
    """Fix lines that are too long in a file"""
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    fixed_lines = []
    for line in lines:
        # Don't modify if it's already short enough
        if len(line.rstrip('\n')) <= max_length:
            fixed_lines.append(line)
            continue
        
        # Handle different cases
        line_stripped = line.rstrip('\n')
        
        # Case 1: Long comment - split it
        if line_stripped.strip().startswith('//'):
            words = line_stripped.split()
            current_line = words[0]  # Start with //
            result_lines = []
            
            for word in words[1:]:
                if len(current_line + ' ' + word) <= max_length:
                    current_line += ' ' + word
                else:
                    result_lines.append(current_line + '\n')
                    # Preserve indentation
                    indent = len(line) - len(line.lstrip())
                    current_line = ' ' * indent + '//' + ' ' + word
            
            result_lines.append(current_line + '\n')
            fixed_lines.extend(result_lines)
        
        # Case 2: Rule with | alternatives - split them
        elif '|' in line_stripped and not '//' in line_stripped:
            # Find indentation
            indent = len(line) - len(line.lstrip())
            parts = line_stripped.split('|')
            
            # First line with colon if present
            first_part = parts[0].rstrip()
            fixed_lines.append(first_part + '\n')
            
            # Remaining alternatives
            for part in parts[1:]:
                part = part.strip()
                if part:
                    fixed_lines.append(' ' * (indent + 4) + '| ' + part + '\n')
        
        # Case 3: Long rule definition - split parameters
        elif '(' in line_stripped and ')' in line_stripped:
            # Try to split on commas
            if ',' in line_stripped:
                # Find the opening paren
                paren_pos = line_stripped.find('(')
                before_paren = line_stripped[:paren_pos + 1]
                after_paren = line_stripped[paren_pos + 1:]
                
                # Split on commas
                if ')' in after_paren:
                    params_end = after_paren.find(')')
                    params = after_paren[:params_end]
                    after_params = after_paren[params_end:]
                    
                    param_list = params.split(',')
                    
                    # Check if we can fit it better
                    indent = len(line) - len(line.lstrip())
                    fixed_lines.append(before_paren + '\n')
                    
                    for i, param in enumerate(param_list):
                        param = param.strip()
                        if i < len(param_list) - 1:
                            fixed_lines.append(' ' * (indent + 4) + param + ',\n')
                        else:
                            fixed_lines.append(' ' * (indent + 4) + param + after_params + '\n')
                else:
                    fixed_lines.append(line)
            else:
                fixed_lines.append(line)
        else:
            # Default: Keep as is if we can't fix it automatically
            fixed_lines.append(line)
    
    return fixed_lines


def main():
    """Fix line length in all grammar files"""
    grammar_dir = Path('grammars')
    
    files_to_fix = [
        'Fortran2003Parser.g4',
        'FORTRAN77Parser.g4', 
        'Fortran95Parser.g4',
        'Fortran90Parser.g4',
        'Fortran90Lexer.g4',
        'Fortran95Lexer.g4',
        'Fortran2023Parser.g4'
    ]
    
    for filename in files_to_fix:
        filepath = grammar_dir / filename
        if filepath.exists():
            print(f"Fixing {filename}...")
            fixed_lines = fix_line_length(filepath)
            
            # Write back
            with open(filepath, 'w') as f:
                f.writelines(fixed_lines)
            
            print(f"  Fixed {filename}")


if __name__ == "__main__":
    main()