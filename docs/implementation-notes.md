# Implementation Notes

Status and known limitations for the Fortran grammar collection.

## Grammar Status

All grammars are **complete and tested**. Each implements the full syntax of its standard.

| Standard | Status | Tests | Notes |
|----------|--------|-------|-------|
| FORTRAN 1957 | Complete | Yes | Strict fixed-form validator in `tools/strict_fixed_form.py` |
| FORTRAN II | Complete | Yes | SUBROUTINE, FUNCTION, COMMON, Hollerith |
| FORTRAN 66 | Complete | Yes | First ANSI standard |
| FORTRAN 77 | Complete | Yes | CHARACTER, IF-THEN-ELSE, last fixed-form-only |
| Fortran 90 | Complete | Extensive | Free-form, modules, derived types, arrays |
| Fortran 95 | Complete | Yes | FORALL, PURE/ELEMENTAL |
| Fortran 2003 | Complete | Extensive | OOP, C interop, PDTs, IEEE |
| Fortran 2008 | Complete | Yes | Coarrays, submodules, DO CONCURRENT |
| Fortran 2018 | Complete | Yes | Teams, events, atomics |
| Fortran 2023 | Complete | Yes | Conditional expressions, TYPEOF/CLASSOF |
| LFortran | Complete | Yes | J3 Generics (TEMPLATE, REQUIREMENT, INSTANTIATE) |
| LFortran Infer | Complete | Yes | Type inference (`:=`, `--infer`), global scope, auto realloc |

## Semantic Validation

Grammar-level parsing is complete. Semantic validators exist in `tools/` for:

- **Fixed-form column layout** (`strict_fixed_form.py`) - FORTRAN 1957/II/90
- **Hollerith length validation** - 1957/II
- **Numeric constant ranges** - 1957
- **FORMAT descriptor validation** - 1957
- **Statement ordering** - FORTRAN 66/77, Fortran 90
- **Expression precedence** - Fortran 90+
- **CYCLE/EXIT context** - Fortran 90+
- **ALLOCATE constraints** - Fortran 2003/2008
- **BLOCK construct semantics** - Fortran 2008

## Design Decisions

### Fixed-Form Model

Grammars use **layout-lenient parsing**:
- Tokens parsed by order, not column position
- Strict validation available via `tools/strict_fixed_form.py`
- Supports both legacy `.f` and modern `.f90` layouts

### Expression Grammar

Operator precedence fully encoded for Fortran 90+ (issue #678 resolved).

### Scope Boundary

Grammars are **syntactic**. Type checking and interface matching are out of scope.

## References

| Standard | Document |
|----------|----------|
| FORTRAN 1957 | IBM C28-6003 |
| FORTRAN II | IBM C28-6000-2 |
| FORTRAN 66 | ANSI X3.9-1966 |
| FORTRAN 77 | ANSI X3.9-1978 |
| Fortran 90 | ISO/IEC 1539:1991 |
| Fortran 95 | ISO/IEC 1539-1:1997 |
| Fortran 2003 | ISO/IEC 1539-1:2004 |
| Fortran 2008 | ISO/IEC 1539-1:2010 |
| Fortran 2018 | ISO/IEC 1539-1:2018 |
| Fortran 2023 | ISO/IEC 1539-1:2023 |
| LFortran | J3/24-107r1 |

PDFs in `validation/pdfs/`.
