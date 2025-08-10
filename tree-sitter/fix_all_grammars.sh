#!/bin/bash

# Function to fix a grammar and run its tests
fix_grammar() {
    local dir=$1
    local name=$(basename "$dir")
    
    echo "=== Fixing $name ==="
    cd "$dir"
    
    # Generate the parser
    tree-sitter generate
    
    # Run tests and check results
    total=$(npm test 2>/dev/null | grep -E "✓|✗" | wc -l)
    passed=$(npm test 2>/dev/null | grep -c "✓" || echo 0)
    
    if [ $total -gt 0 ]; then
        pct=$((passed * 100 / total))
        echo "$name: $passed/$total ($pct%)"
        
        # If not 100%, try to auto-fix common issues
        if [ $pct -lt 100 ]; then
            echo "Attempting to fix $name grammar..."
            
            # Common fixes:
            # 1. Ensure program rule exists
            if ! grep -q "^[[:space:]]*program:" grammar.js; then
                echo "Adding program rule..."
                # This would need custom logic per grammar
            fi
            
            # 2. Regenerate and test again
            tree-sitter generate
            npm test
        fi
    fi
    
    cd ..
}

# Fix each grammar
for dir in /var/tmp/ert/code/standard/tree-sitter/*/; do
    if [ -f "$dir/package.json" ] && [ -f "$dir/grammar.js" ]; then
        fix_grammar "$dir"
    fi
done

echo "=== Final Summary ==="
./check_all_tests.sh