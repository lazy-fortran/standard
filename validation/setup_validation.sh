#!/bin/bash
#
# Setup validation environment by downloading kaby76/fortran repository on-demand
# This keeps the repo clean while providing access to reference grammars and examples
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "Setting up validation environment..."
echo "Project root: $PROJECT_ROOT"

# Download kaby76/fortran repository
echo "Downloading kaby76/fortran repository..."
python3 "$SCRIPT_DIR/tools/download_kaby76.py" "$PROJECT_ROOT"

# Verify setup
KABY76_DIR="$SCRIPT_DIR/external/kaby76-fortran"
if [ -d "$KABY76_DIR" ]; then
    echo "✓ kaby76/fortran repository downloaded successfully"
    echo "  Location: $KABY76_DIR"
    echo "  Grammar files:"
    echo "    - $(ls -la "$KABY76_DIR"/*.g4 2>/dev/null | wc -l) .g4 files found"
    echo "    - $(ls -la "$KABY76_DIR/examples"/*.f90 2>/dev/null | wc -l) example files found"
    echo ""
    echo "Validation environment ready!"
    echo "Files are git-ignored and downloaded on-demand."
else
    echo "✗ Failed to setup validation environment"
    exit 1
fi