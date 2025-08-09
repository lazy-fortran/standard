#!/usr/bin/env python3
"""
Script to replace tabs with 4 spaces in grammar files
"""

from pathlib import Path


def fix_tabs_in_file(filepath):
    """Replace all tabs with 4 spaces in a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Replace tabs with 4 spaces
    fixed_content = content.replace('\t', '    ')
    
    # Write back if changes were made
    if content != fixed_content:
        with open(filepath, 'w') as f:
            f.write(fixed_content)
        return True
    return False


def main():
    """Fix tabs in all grammar files"""
    grammar_dir = Path('grammars')
    fixed_files = []
    
    for g4_file in grammar_dir.glob('*.g4'):
        if fix_tabs_in_file(g4_file):
            fixed_files.append(g4_file.name)
            print(f"Fixed tabs in {g4_file.name}")
    
    if fixed_files:
        print(f"\nFixed {len(fixed_files)} files:")
        for f in sorted(fixed_files):
            print(f"  - {f}")
    else:
        print("No tab characters found in grammar files")


if __name__ == "__main__":
    main()