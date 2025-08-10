# ANTLR4 LazyFortran2025 Implementation Workarounds

## The Problem

ANTLR4's import mechanism has a fundamental limitation: **you cannot disable or truly override imported rules**. When `LazyFortran2025Parser` imports from `Fortran2023Parser`, which imports from `Fortran2018Parser`, etc., all the way back to `FORTRANParser`, we inherit ALL the strict requirements including:

- Required `PROGRAM`/`MODULE` blocks
- Required `CONTAINS` keyword
- Required type declarations
- Strict statement ordering

This creates a conflict with LazyFortran2025's goals of optional syntax.

## Proposed Workarounds

### Option 1: **Dual Entry Points** (Recommended)
Create alternative entry rules that bypass the traditional structure:

```antlr
parser grammar LazyFortran2025Parser;

import Fortran2023Parser;

// WORKAROUND: Alternative entry point for lazy syntax
lazy_program
    : lazy_statement_list EOF
    ;

// Direct statements without program wrapper
lazy_statement_list
    : (lazy_statement | traditional_program_unit)*
    ;

lazy_statement
    : use_stmt
    | assignment_stmt_lazy      // Enhanced assignment
    | call_stmt
    | print_stmt
    | if_construct
    | do_construct
    | lazy_procedure_def        // Procedures without CONTAINS
    ;

// Assignment with type inference
assignment_stmt_lazy
    : IDENTIFIER '=' expr       // No prior declaration needed
    ;

// Procedure without CONTAINS
lazy_procedure_def
    : function_stmt specification_part? execution_part? end_function_stmt
    | subroutine_stmt specification_part? execution_part? end_subroutine_stmt
    ;

// Traditional program units still available for backward compatibility
traditional_program_unit
    : program_unit_f2023
    ;
```

**Usage:**
```java
// Parser can choose entry point based on file extension or content
if (isLazyFortran) {
    parser.lazy_program();  // Use relaxed entry point
} else {
    parser.program_unit_core();  // Use traditional entry point
}
```

### Option 2: **Preprocessing Transformation**
Transform lazy syntax to standard syntax before parsing:

```python
class LazyToStandardTransformer:
    def transform(self, lazy_code):
        """
        Wrap bare code in PROGRAM block if needed
        """
        lines = lazy_code.split('\n')
        
        # Check if code lacks program/module wrapper
        if not self.has_program_wrapper(lines):
            # Wrap in implicit program
            lines.insert(0, "PROGRAM LAZY_MAIN")
            lines.append("END PROGRAM LAZY_MAIN")
        
        # Insert CONTAINS before procedures if needed
        lines = self.insert_contains_if_needed(lines)
        
        # Add type declarations for undeclared variables
        lines = self.add_type_declarations(lines)
        
        return '\n'.join(lines)
```

**Advantages:**
- Works with existing ANTLR4 grammar unchanged
- Can be implemented as a separate preprocessing step
- Maintains full compatibility

### Option 3: **Semantic Actions with Empty Productions**
Make strict rules "optional" through empty alternatives:

```antlr
// Modified Fortran2023Parser rules
program_unit_f2023
    : main_program_f2023
    | module_f2023
    | submodule_f2008
    | external_subprogram_f2023
    | lazy_unit                    // NEW: Accept lazy syntax
    ;

lazy_unit
    : /* empty - semantic action handles this */
      { 
        // In semantic action, treat following statements
        // as if wrapped in implicit PROGRAM block
        implicitProgramMode = true;
      }
    ;

// Modified to accept undeclared variables
primary_f2023
    : IDENTIFIER
      {
        // Check if declared, if not, add to inferred variables
        if (!isDeclared($IDENTIFIER.text)) {
            inferType($IDENTIFIER.text, context);
        }
      }
    | INTEGER_LITERAL
    | REAL_LITERAL
    // ... rest of primary rules
    ;
```

### Option 4: **Grammar Flattening and Selective Import**
Instead of importing the entire chain, create a flattened grammar that selectively includes rules:

```antlr
parser grammar LazyFortran2025FlatParser;

// Don't import! Instead, copy and modify needed rules

// Top-level rule - completely redefined
program
    : (statement | procedure_def)* EOF
    ;

// Direct statement without program wrapper
statement
    : assignment_stmt
    | control_stmt
    | io_stmt
    | declaration_stmt
    ;

// Include F2023 features directly
conditional_expr
    : expr '?' expr ':' expr
    ;

// ... selectively copy other needed rules
```

**Pros:** Full control over syntax
**Cons:** Loses inheritance benefits, maintenance burden

### Option 5: **Multi-Pass Parsing**
Parse in multiple stages with different grammars:

```java
class LazyFortranParser {
    public AST parse(String code) {
        // Pass 1: Try parsing as lazy syntax with relaxed grammar
        try {
            LazyGrammarParser lazy = new LazyGrammarParser(code);
            AST ast = lazy.parseRelaxed();
            
            // Pass 2: Validate and transform to standard AST
            StandardTransformer transformer = new StandardTransformer();
            return transformer.normalize(ast);
        } catch (ParseException e) {
            // Fall back to standard parser
            Fortran2023Parser standard = new Fortran2023Parser(code);
            return standard.parse();
        }
    }
}
```

## Recommended Approach

**For ANTLR4, use a combination of Option 1 and Option 2:**

1. **Create dual entry points** in the grammar for maximum flexibility
2. **Implement a preprocessor** for complex transformations
3. **Use semantic actions** for type inference

This provides:
- ✅ Backward compatibility with standard Fortran
- ✅ Support for lazy syntax
- ✅ Clean separation of concerns
- ✅ Maintainable codebase

## Implementation Example

```antlr
// LazyFortran2025Parser.g4
parser grammar LazyFortran2025Parser;

options {
    tokenVocab = LazyFortran2025Lexer;
}

import Fortran2023Parser;

// ============================================================================
// DUAL ENTRY POINTS - Choose based on context
// ============================================================================

// Entry point 1: Traditional Fortran (inherited)
traditional_fortran
    : program_unit_f2023 EOF
    ;

// Entry point 2: Lazy Fortran (new)
lazy_fortran
    : lazy_program EOF
    ;

// Lazy program without required wrappers
lazy_program
    : (lazy_element)*
    ;

lazy_element
    : use_stmt                          // Use statements at top level
    | implicit_stmt                     // Implicit statements (though NONE is default)
    | declaration_or_statement          // Mixed declarations and statements
    | procedure_definition_lazy         // Procedures without CONTAINS
    ;

// Declarations and statements can be intermixed
declaration_or_statement
    : declaration_stmt
    | executable_stmt
    | assignment_lazy                   // Assignment with type inference
    ;

// Assignment that creates variables
assignment_lazy
    : IDENTIFIER '=' expr
      {
        // Semantic action: infer type if not declared
        if (!symbolTable.contains($IDENTIFIER.text)) {
            Type inferredType = inferTypeFromExpression($expr.type);
            symbolTable.add($IDENTIFIER.text, inferredType);
        }
      }
    ;

// Procedures without CONTAINS requirement
procedure_definition_lazy
    : function_subprogram
    | subroutine_subprogram
    ;

// ============================================================================
// HELPER RULES
// ============================================================================

// Accept any identifier as potentially undeclared variable
undeclared_variable
    : IDENTIFIER
    ;

// Primary can be undeclared variable
primary_lazy
    : undeclared_variable
    | literal
    | '(' expr ')'
    ;
```

## Testing Strategy

Create test files with both syntaxes:

**traditional.f90:**
```fortran
PROGRAM TEST
    IMPLICIT NONE
    INTEGER :: x
    x = 42
    PRINT *, x
END PROGRAM TEST
```

**lazy.f2025:**
```fortran
x = 42
print *, x
```

Both should parse successfully using different entry points.

## Conclusion

While ANTLR4's import mechanism doesn't allow disabling inherited rules, these workarounds provide practical solutions for implementing LazyFortran2025's relaxed syntax. The dual entry point approach with preprocessing offers the best balance of compatibility, maintainability, and functionality.