#!/bin/bash
# Automated setup script for the validation infrastructure.
# Downloads reference Fortran grammars and sets up the validation pipeline.

set -euo pipefail

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

echo "=== Lazy Fortran Standard - Validation Infrastructure Setup ==="
echo "Repository root: $REPO_ROOT"
echo ""

# Create necessary directories
echo "Creating directory structure..."
mkdir -p "$REPO_ROOT/validation/external"
mkdir -p "$REPO_ROOT/validation/auto-generated"
mkdir -p "$REPO_ROOT/validation/pdfs"
mkdir -p "$REPO_ROOT/validation/cache"
mkdir -p "$REPO_ROOT/validation/reports"
mkdir -p "$REPO_ROOT/handcrafted/shared-core"
mkdir -p "$REPO_ROOT/handcrafted/source-formats"
mkdir -p "$REPO_ROOT/handcrafted/standards"
echo "✓ Directory structure created"

# Check for required dependencies
echo ""
echo "Checking dependencies..."

# Check for git
if ! command -v git &> /dev/null; then
    echo "✗ git is required but not installed"
    exit 1
fi
echo "✓ git found"

# Check for python3
if ! command -v python3 &> /dev/null; then
    echo "✗ python3 is required but not installed"
    exit 1
fi
echo "✓ python3 found"

# Check for Java (needed for ANTLR4)
if ! command -v java &> /dev/null; then
    echo "⚠ java not found - will be needed for ANTLR4 grammar validation"
else
    echo "✓ java found"
fi

# Download reference Fortran grammars
echo ""
echo "Downloading reference Fortran grammars from kaby76/fortran..."
cd "$REPO_ROOT"
python3 validation/tools/download_reference_grammars.py "$REPO_ROOT"

# Verify the download
REFERENCE_DIR="$REPO_ROOT/validation/auto-generated/fortran-reference"
if [[ -f "$REFERENCE_DIR/FortranLexer.g4" && -f "$REFERENCE_DIR/FortranParser.g4" ]]; then
    echo "✓ Reference grammars downloaded successfully"
else
    echo "✗ Reference grammar download failed"
    exit 1
fi

# Make scripts executable
echo ""
echo "Setting up permissions..."
chmod +x "$REPO_ROOT/scripts"/*.sh
echo "✓ Permissions set"

echo ""
echo "=== Setup Complete! ==="
echo ""
echo "Next steps:"
echo "1. Run validation tests: ./scripts/validate_all.sh"
echo "2. Compare grammars: ./scripts/compare_grammars.sh"
echo ""
echo "Key directories:"
echo "  - validation/auto-generated/fortran-reference/ - Reference grammars (NEVER COMMITTED)"
echo "  - validation/reports/ - Validation reports (COMMITTED)"
echo "  - handcrafted/ - Our modular grammars (COMMITTED)"
echo ""