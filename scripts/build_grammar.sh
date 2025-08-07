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
GRAMMAR_NAME="${1:-SharedCore}"

GRAMMAR_DIR="$PROJECT_ROOT/grammars"
BUILD_DIR="$PROJECT_ROOT/build/$GRAMMAR_NAME"

# Validate grammar files exist
LEXER_FILE="$GRAMMAR_DIR/${GRAMMAR_NAME}Lexer.g4"
PARSER_FILE="$GRAMMAR_DIR/${GRAMMAR_NAME}Parser.g4"

if [ ! -f "$LEXER_FILE" ] && [ ! -f "$PARSER_FILE" ]; then
    echo "Error: Grammar files not found for $GRAMMAR_NAME"
    echo "Available grammars:"
    ls -1 "$PROJECT_ROOT/grammars"/*Lexer.g4 2>/dev/null | sed 's/.*\///g' | sed 's/Lexer.g4//g' || echo "  None"
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

# All grammars are in flat structure - simple import path
IMPORT_PATH="-lib $PROJECT_ROOT/grammars"

# Generate lexer if exists
LEXER_FILE="$GRAMMAR_DIR/${GRAMMAR_NAME}Lexer.g4"
if [ -f "$LEXER_FILE" ]; then
    echo "Generating lexer from ${GRAMMAR_NAME}Lexer.g4..."
    cd "$PROJECT_ROOT"  # Ensure correct working directory for imports
    antlr4 -Dlanguage=Python3 -o "$BUILD_DIR" $IMPORT_PATH "$LEXER_FILE"
fi

# Generate parser if exists  
PARSER_FILE="$GRAMMAR_DIR/${GRAMMAR_NAME}Parser.g4"
if [ -f "$PARSER_FILE" ]; then
    echo "Generating parser from ${GRAMMAR_NAME}Parser.g4..."
    cd "$PROJECT_ROOT"  # Ensure correct working directory for imports
    antlr4 -Dlanguage=Python3 -o "$BUILD_DIR" $IMPORT_PATH "$PARSER_FILE"
fi

echo "Grammar build completed successfully!"
echo ""
echo "Generated files in $BUILD_DIR:"
cd "$BUILD_DIR" && ls -1 *.py *.tokens 2>/dev/null | sed 's/^/  - /'
echo ""
echo "WARNING: DO NOT manually edit these generated files!"
echo "All changes must be made to the .g4 grammar files in $GRAMMAR_DIR"