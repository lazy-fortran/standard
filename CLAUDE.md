# Fortran Grammar Repository

ANTLR4 grammars for Fortran standards from 1957 to 2023, plus LFortran extensions.

## Standard References

| Standard | Reference Document |
|----------|-------------------|
| FORTRAN 1957 | IBM Form C28-6003 |
| FORTRAN II | IBM Form C28-6000-2 |
| FORTRAN 66 | ANSI X3.9-1966 |
| FORTRAN 77 | ANSI X3.9-1978 |
| Fortran 90 | ISO/IEC 1539:1991 |
| Fortran 95 | ISO/IEC 1539-1:1997 |
| Fortran 2003 | ISO/IEC 1539-1:2004 |
| Fortran 2008 | ISO/IEC 1539-1:2010 |
| Fortran 2018 | ISO/IEC 1539-1:2018 |
| Fortran 2023 | ISO/IEC 1539-1:2023 |
| LFortran | J3/24-107r1 (Generics) |

PDFs available in `validation/pdfs/`.

## Grammar Structure

```
grammars/
  FORTRANLexer.g4, FORTRANParser.g4           # 1957
  FORTRANIILexer.g4, FORTRANIIParser.g4       # 1958
  FORTRAN66Lexer.g4, FORTRAN66Parser.g4       # 1966
  FORTRAN77Lexer.g4, FORTRAN77Parser.g4       # 1977
  Fortran90Lexer.g4, Fortran90Parser.g4       # 1990
  Fortran95Lexer.g4, Fortran95Parser.g4       # 1995
  Fortran2003Lexer.g4, Fortran2003Parser.g4   # 2003
  Fortran2008Lexer.g4, Fortran2008Parser.g4   # 2008
  Fortran2018Lexer.g4, Fortran2018Parser.g4   # 2018
  Fortran2023Lexer.g4, Fortran2023Parser.g4   # 2023
  LFortranLexer.g4, LFortranParser.g4         # LFortran Standard
  LFortranInferLexer.g4, LFortranInferParser.g4  # LFortran Infer
```

Grammars inherit from predecessors. Each standard only defines NEW features.

## Development

### Build and Test

```bash
make all      # Build all grammars
make test     # Run all tests
make clean    # Clean generated files
```

### Adding Features

1. Add tokens to the appropriate `*Lexer.g4`
2. Add parser rules to the appropriate `*Parser.g4`
3. Add test fixtures in `tests/fixtures/<Standard>/`
4. Run `make test` to verify

### Historical Accuracy

Features belong in the FIRST standard that supports them:
- PURE/ELEMENTAL: Fortran 95 (not 90)
- CLASS/EXTENDS: Fortran 2003 (not 95)
- Coarrays: Fortran 2008 (not 2003)
- Conditional expressions: Fortran 2023 (not 2018)

### Scope

These grammars are **syntactic only**. Out of scope:
- Type checking
- Interface characteristics matching
- Semantic validation

Use GitHub issues for tracking gaps. Reference ISO R-numbers when relevant (e.g., R916 type-param-inquiry).

## Documentation

| Document | Purpose |
|----------|---------|
| [README.md](README.md) | Project overview and quick start |
| [lfortran-standard.md](docs/lfortran-standard.md) | LFortran Standard specification |
| [lfortran-infer.md](docs/lfortran-infer.md) | LFortran Infer mode specification |
| [design-rationale.md](docs/design-rationale.md) | Design decisions explained |
| [implementation-notes.md](docs/implementation-notes.md) | Status and known limitations |
