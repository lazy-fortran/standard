#!/bin/bash

for dir in Fortran2003 Fortran2008 Fortran2018 Fortran2023 LazyFortran2025; do
    if [ -f "/var/tmp/ert/code/standard/tree-sitter/$dir/package.json" ]; then
        echo "Adding test script to $dir"
        cd "/var/tmp/ert/code/standard/tree-sitter/$dir"
        
        # Check if test script already exists
        if ! grep -q '"test"' package.json; then
            # Add test script after "license" line
            sed -i '/"license":/a\  "scripts": {\n    "test": "tree-sitter test"\n  },' package.json
        fi
    fi
done