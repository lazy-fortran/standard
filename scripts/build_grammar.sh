#!/bin/bash
#
# Grammar Build Script for SharedCore FORTRAN Grammar
#
# This script generates Python parser/lexer files from ANTLR4 grammar files.
# DO NOT manually edit the generated Python files - they will be overwritten!
#
# Usage: ./build_grammar.sh [grammar_name]
# Example: ./build_grammar.sh shared_core
# Requirements: antlr4 (Java version)
#

set -e  # Exit on any error

# Get project root directory
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GRAMMAR_NAME="${1:-shared_core}"

GRAMMAR_DIR="$PROJECT_ROOT/grammars/$GRAMMAR_NAME"
BUILD_DIR="$PROJECT_ROOT/build/$GRAMMAR_NAME"

# Validate grammar directory exists
if [ ! -d "$GRAMMAR_DIR" ]; then
    echo "Error: Grammar directory not found: $GRAMMAR_DIR"
    echo "Available grammars:"
    ls -1 "$PROJECT_ROOT/grammars" 2>/dev/null || echo "  None"
    exit 1
fi

echo "Building $GRAMMAR_NAME FORTRAN Grammar..."

# Check if antlr4 is available
if ! command -v antlr4 &> /dev/null; then
    echo "Error: antlr4 command not found. Please install ANTLR4."
    echo "On Arch Linux: sudo pacman -S antlr4"
    exit 1
fi

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Handle grammar dependencies via imports
# ANTLR4 needs access to imported grammars
IMPORT_PATH=""
if [ "$GRAMMAR_NAME" != "shared_core" ]; then
    IMPORT_PATH="-lib $PROJECT_ROOT/grammars/shared_core"
    
    # For FORTRAN II and later, also include parent grammar
    case "$GRAMMAR_NAME" in
        fortran_ii)
            IMPORT_PATH="$IMPORT_PATH:$PROJECT_ROOT/grammars/fortran1957"
            ;;
        fortran_iv)
            IMPORT_PATH="$IMPORT_PATH:$PROJECT_ROOT/grammars/fortran_ii"
            ;;
        fortran66)
            IMPORT_PATH="$IMPORT_PATH:$PROJECT_ROOT/grammars/fortran_iv"
            ;;
        fortran77)
            IMPORT_PATH="$IMPORT_PATH:$PROJECT_ROOT/grammars/fortran66"
            ;;
        fortran90)
            IMPORT_PATH="$IMPORT_PATH:$PROJECT_ROOT/grammars/fortran77"
            ;;
    esac
fi

# Find and generate lexer
LEXER_FILE=$(find "$GRAMMAR_DIR" -name "*Lexer.g4" | head -1)
if [ -n "$LEXER_FILE" ]; then
    echo "Generating lexer from $(basename "$LEXER_FILE")..."
    antlr4 -Dlanguage=Python3 -o "$BUILD_DIR" $IMPORT_PATH "$LEXER_FILE"
fi

# Find and generate parser  
PARSER_FILE=$(find "$GRAMMAR_DIR" -name "*Parser.g4" | head -1)
if [ -n "$PARSER_FILE" ]; then
    echo "Generating parser from $(basename "$PARSER_FILE")..."
    antlr4 -Dlanguage=Python3 -o "$BUILD_DIR" $IMPORT_PATH "$PARSER_FILE"
fi

echo "Grammar build completed successfully!"
echo ""
echo "Generated files in $BUILD_DIR:"
ls -1 *.py *.tokens 2>/dev/null | sed 's/^/  - /'
echo ""
echo "WARNING: DO NOT manually edit these generated files!"
echo "All changes must be made to the .g4 grammar files in $GRAMMAR_DIR"