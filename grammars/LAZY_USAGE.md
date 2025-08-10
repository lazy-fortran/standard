# Using LazyFortran2025 with ANTLR4

## The Dual Entry Point Solution

The LazyFortran2025Parser.g4 implements a workaround for ANTLR4's inability to disable inherited rules by providing two separate entry points:

### 1. Traditional Entry Point (`traditional_entry`)
- Used for: `.f90`, `.f95`, `.f03`, `.f08`, `.f18`, `.f23` files
- Requires: PROGRAM or MODULE blocks
- Follows: All standard Fortran rules

### 2. Lazy Entry Point (`lazy_entry`)
- Used for: `.lf` files (LazyFortran)
- Optional: PROGRAM/MODULE blocks
- Optional: CONTAINS keyword
- Supports: Type inference
- Allows: Direct statements at top level

## Parser Usage

### Java Implementation
```java
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class FortranParser {
    public ParseTree parse(String filename, String content) {
        // Create lexer and parser
        LazyFortran2025Lexer lexer = new LazyFortran2025Lexer(
            CharStreams.fromString(content)
        );
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        LazyFortran2025Parser parser = new LazyFortran2025Parser(tokens);
        
        // Choose entry point based on file extension
        if (filename.endsWith(".lf")) {
            // Use lazy entry point for .lf files
            return parser.lazy_entry();
        } else {
            // Use traditional entry point for standard Fortran
            return parser.traditional_entry();
        }
    }
}
```

### Python Implementation
```python
from antlr4 import *
from LazyFortran2025Lexer import LazyFortran2025Lexer
from LazyFortran2025Parser import LazyFortran2025Parser

def parse_fortran(filename, content):
    # Create lexer and parser
    input_stream = InputStream(content)
    lexer = LazyFortran2025Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = LazyFortran2025Parser(token_stream)
    
    # Choose entry point based on file extension
    if filename.endswith('.lf'):
        # Use lazy entry point for .lf files
        return parser.lazy_entry()
    else:
        # Use traditional entry point for standard Fortran
        return parser.traditional_entry()
```

## Example Comparison

### LazyFortran (.lf file)
```fortran
! Direct code - no PROGRAM needed
x = 42
print *, x

! Direct function - no CONTAINS needed
function double(n)
    real :: n, double
    double = n * 2.0
end function

result = double(x)
print *, result
```

### Traditional Fortran (.f90 file)
```fortran
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: x, result
    
    x = 42
    PRINT *, x
    
    result = double(x)
    PRINT *, result
    
CONTAINS
    
    FUNCTION double(n)
        REAL :: n, double
        double = n * 2.0
    END FUNCTION double
    
END PROGRAM MAIN
```

## Building the Parser

### Generate ANTLR4 Files
```bash
# Generate the lexer and parser
antlr4 -Dlanguage=Python3 LazyFortran2025Lexer.g4
antlr4 -Dlanguage=Python3 LazyFortran2025Parser.g4

# Or for Java
antlr4 LazyFortran2025Lexer.g4
antlr4 LazyFortran2025Parser.g4
javac -cp "antlr-4.*.jar:." *.java
```

## Testing

### Test Files Provided
- `test_lazy.lf` - Demonstrates all LazyFortran2025 features
- `test_traditional.f90` - Same functionality in traditional Fortran

### Running Tests
```bash
# Parse lazy file
python parse_test.py test_lazy.lf

# Parse traditional file  
python parse_test.py test_traditional.f90
```

## Benefits of This Approach

1. **Clean Separation**: Different entry points for different syntax styles
2. **No Grammar Conflicts**: Bypasses ANTLR4's import limitations
3. **Full Compatibility**: Traditional Fortran files work unchanged
4. **Progressive Adoption**: Mix .f90 and .lf files in same project
5. **Clear Intent**: File extension indicates syntax style

## Limitations

- The parser must be invoked with the correct entry point
- IDE support needs to recognize .lf extension
- Build systems need updating to handle .lf files

## Future Enhancements

- Automatic entry point detection based on content analysis
- Preprocessing step to convert .lf to .f90 for legacy tools
- Language server protocol implementation for IDE support