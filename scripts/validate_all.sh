#!/bin/bash
"""
Main validation script that runs the complete validation pipeline.
Downloads tools if needed, generates reference grammars, and validates them.
"""

set -euo pipefail

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

echo "=== LFortran Standard - Complete Validation Pipeline ==="
echo "Repository root: $REPO_ROOT"
echo ""

# Check if kaby76 tools are available, download if not
KABY76_DIR="$REPO_ROOT/validation/external/kaby76-fortran"
if [[ ! -d "$KABY76_DIR" || ! -f "$KABY76_DIR/extract.sh" ]]; then
    echo "kaby76/fortran tools not found. Running setup..."
    "$SCRIPT_DIR/setup_validation.sh"
    echo ""
fi

# Run all validation tests
echo "Running validation test suite..."
cd "$REPO_ROOT"

echo ""
echo "1. Testing kaby76 downloader..."
OMP_NUM_THREADS=24 python3 validation/tools/test_download_kaby76.py

echo ""
echo "2. Testing validation pipeline..."
OMP_NUM_THREADS=24 python3 validation/tools/test_validation_pipeline.py

echo ""
echo "3. Generating reference grammars..."
OMP_NUM_THREADS=24 python3 validation/tools/run_extraction.py "$REPO_ROOT" "Fortran 2023"
OMP_NUM_THREADS=24 python3 validation/tools/run_extraction.py "$REPO_ROOT" "Fortran 2018"

echo ""
echo "4. Validating generated grammars exist..."
EXPECTED_GRAMMARS=(
    "validation/auto-generated/fortran2023/fortran2023.g4"
    "validation/auto-generated/fortran2018/fortran2018.g4"
)

ALL_FOUND=true
for grammar in "${EXPECTED_GRAMMARS[@]}"; do
    if [[ -f "$REPO_ROOT/$grammar" ]]; then
        echo "✓ Found: $grammar"
    else
        echo "✗ Missing: $grammar"
        ALL_FOUND=false
    fi
done

if [[ "$ALL_FOUND" == "true" ]]; then
    echo ""
    echo "=== Validation Pipeline Complete! ==="
    echo "✓ All tools downloaded and functional"
    echo "✓ All tests passing"
    echo "✓ Reference grammars generated successfully"
    echo ""
    echo "Generated files:"
    find "$REPO_ROOT/validation/auto-generated" -name "*.g4" -type f | sort
else
    echo ""
    echo "=== Validation Pipeline Failed! ==="
    echo "Some reference grammars could not be generated."
    exit 1
fi