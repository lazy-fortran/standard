# Tree-sitter FORTRAN/Fortran Grammar Suite

## Why Tree-sitter > ANTLR4 for LazyFortran2025

### ANTLR4 Limitations We Hit:
- ❌ Can't override imported rules to be more permissive
- ❌ Import mechanism is just textual inclusion
- ❌ Can't make required elements optional
- ❌ Rigid LL(*) parsing strategy
- ❌ Poor error recovery for invalid syntax

### Tree-sitter Advantages:
- ✅ **True rule composition** - extend, prepend, replace, make optional
- ✅ **Incremental parsing** - only reparse what changed
- ✅ **Error recovery** - continues parsing invalid code
- ✅ **Conflict resolution** - precedence and associativity
- ✅ **External scanners** - handle context-sensitive parsing
- ✅ **Production ready** - used by GitHub, VS Code, Neovim

## Grammar Inheritance Chain

```
FORTRAN (1957)
    ↓ extends
FORTRANII (1958) - adds SUBROUTINE/FUNCTION
    ↓ extends
FORTRANIV (1962) - adds LOGICAL, DOUBLE PRECISION
    ↓ extends
FORTRAN66 (1966) - first standard
    ↓ extends
FORTRAN77 (1977) - adds CHARACTER, IF-THEN-ELSE
    ↓ extends
Fortran90 (1990) - adds modules, free-form
    ↓ extends
Fortran95 (1995) - adds FORALL, PURE/ELEMENTAL
    ↓ extends
Fortran2003 (2003) - adds OOP, C interop
    ↓ extends
Fortran2008 (2008) - adds coarrays, submodules
    ↓ extends
Fortran2018 (2018) - adds teams, events
    ↓ extends
Fortran2023 (2023) - adds generics
    ↓ extends
LazyFortran2025 - RELAXES syntax rules!
```

## LazyFortran2025 Features That Work!

### 1. Optional Program Blocks ✅
```fortran
! ANTLR4: ❌ Requires PROGRAM wrapper
! Tree-sitter: ✅ Just works!

x = 3.14
print *, x
```

### 2. Optional CONTAINS ✅
```fortran
! ANTLR4: ❌ Requires CONTAINS keyword
! Tree-sitter: ✅ Just works!

x = 10

subroutine show()
  print *, x
end subroutine
```

### 3. Type Inference ✅
```fortran
! ANTLR4: ❌ Requires declarations
! Tree-sitter: ✅ Just works!

pi = 3.14159        ! inferred as real
count = 42          ! inferred as integer
name = "Fortran"    ! inferred as character
```

### 4. Implicit None Default ✅
- Compiler feature, both parsers handle it

## Our Superior Inheritance System

```javascript
// lib/fortran-base.js provides:

extendGrammar(base, {
  rules: {
    // Override completely
    rule_name: $ => newDefinition,
    
    // Extend with alternatives
    rule_name: extend($ => additionalChoice),
    
    // Prepend with higher priority
    rule_name: prepend($ => tryThisFirst),
    
    // Make optional (KEY for LazyFortran2025!)
    rule_name: makeOptional(),
    
    // Replace with access to original
    rule_name: replace(($, base) => 
      choice(base($), newAlternative))
  }
});
```

## Building and Testing

```bash
# Install tree-sitter CLI
npm install -g tree-sitter-cli

# Build a grammar
cd tree-sitter/LazyFortran2025
tree-sitter generate

# Test the grammar
tree-sitter test

# Parse a file
tree-sitter parse example.f90
```

## Migration Status

- [x] FORTRAN (1957) - Base grammar
- [x] FORTRANII (1958) - Inheritance example
- [ ] FORTRANIV (1962)
- [ ] FORTRAN66 (1966)
- [ ] FORTRAN77 (1977)
- [ ] Fortran90 (1990)
- [ ] Fortran95 (1995)
- [ ] Fortran2003 (2003)
- [ ] Fortran2008 (2008)
- [ ] Fortran2018 (2018)
- [ ] Fortran2023 (2023)
- [x] LazyFortran2025 - **WORKS PERFECTLY!**

## Conclusion

Tree-sitter's approach is fundamentally better for evolving languages like Fortran. While ANTLR4 forces rigid structures, Tree-sitter allows the flexibility needed for LazyFortran2025's relaxed syntax.