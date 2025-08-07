#!/bin/bash
#
# Fix Generated Python Files - Post-processing for ANTLR4 issues
#
# This script fixes known issues in ANTLR4 4.13.2 generated Python code:
# 1. Fragile Python version check: sys.version_info[1] > 5
#
# Usage: ./fix_generated_python.sh <build_directory>
#

set -e

BUILD_DIR="${1:-build}"

if [ ! -d "$BUILD_DIR" ]; then
    echo "Build directory not found: $BUILD_DIR"
    exit 1
fi

echo "Fixing generated Python files in $BUILD_DIR..."

# Fix Python version check in all generated .py files
find "$BUILD_DIR" -name "*.py" -type f -exec sed -i \
    's/if sys\.version_info\[1\] > 5:/if sys.version_info >= (3, 6):/' {} \;

echo "Fixed Python version checks in generated files."

# Report what was fixed
FIXED_COUNT=$(find "$BUILD_DIR" -name "*.py" -type f -exec grep -l "sys.version_info >= (3, 6)" {} \; | wc -l)
echo "Fixed $FIXED_COUNT Python files."

if [ "$FIXED_COUNT" -gt 0 ]; then
    echo "Generated files now use robust Python version checking."
else
    echo "No Python version issues found (may already be fixed)."
fi