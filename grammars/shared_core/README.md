# SharedCore FORTRAN Grammar

This directory contains the core FORTRAN grammar implementation using ANTLR4.

## Grammar Source Files

- `SharedCoreLexer.g4` - Lexical analyzer grammar defining universal FORTRAN tokens
- `SharedCoreParser.g4` - Parser grammar defining syntax rules with correct operator precedence

**These are the only files you should edit.**

## Building the Grammar

From the project root directory:

```bash
./scripts/build_grammar.sh shared_core
```

This generates Python files in `build/shared_core/`:
- `SharedCoreLexer.py` - Generated lexer implementation
- `SharedCoreParser.py` - Generated parser implementation  
- `SharedCoreParserListener.py` - Generated listener
- `*.tokens`, `*.interp` - ANTLR4 metadata files

## Testing

Run tests from project root:

```bash
# First build the grammar
./scripts/build_grammar.sh shared_core

# Then run tests
python -m pytest tests/shared_core/ -v
```

## Grammar Features

Universal FORTRAN constructs supported across all standards (1957-2023):

- **Arithmetic expressions** with correct operator precedence
- **Relational operators** (.EQ., .NE., .LT., .LE., .GT., .GE.)
- **Control statements** (IF, GOTO, DO, CONTINUE, STOP, END)
- **I/O statements** (READ, WRITE)
- **Subroutine calls** (CALL statements)
- **Comments** (! style)
- **Identifiers** with underscores
- **Literals** (integers, reals with scientific notation)