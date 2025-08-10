#!/bin/bash

for dir in /var/tmp/ert/code/standard/tree-sitter/*/; do 
  if [ -f "$dir/package.json" ]; then
    name=$(basename "$dir")
    cd "$dir" 2>/dev/null
    total=$(npm test 2>/dev/null | grep -E "✓|✗" | wc -l)
    passed=$(npm test 2>/dev/null | grep -c "✓" || echo 0)
    if [ $total -gt 0 ]; then
      pct=$((passed * 100 / total))
      echo "$name: $passed/$total ($pct%)"
    fi
  fi
done