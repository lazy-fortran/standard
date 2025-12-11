# Project-Specific Instructions: Fortran Grammar Repository

## ABSOLUTE REQUIREMENTS - ZERO TOLERANCE

### Feature Completeness and Historical Accuracy

This repository implements ANTLR4 grammars for ALL Fortran standards from 1957 to 2023.
The following requirements are MANDATORY and NON-NEGOTIABLE:

1. **FEATURE COMPLETE**: Every grammar MUST implement 100% of the syntax rules defined
   in its corresponding ISO/ANSI standard. Partial implementations are BUGS.

2. **HISTORICALLY ACCURATE**: Features MUST appear in the FIRST standard that supports
   them, NOT earlier, NOT later. PURE/ELEMENTAL belong in F95, NOT F90.

3. **NO SHORTCUTS ALLOWED**: Every syntax rule from the standard MUST have a
   corresponding grammar rule. Vague "intentionally minimal" language is FORBIDDEN.

4. **EVERY GAP TRACKED**: Every missing feature, failing fixture, or known limitation
   MUST have a GitHub issue. If you discover a gap without an issue, CREATE ONE.

5. **EVERY xfail DOCUMENTED**: Every xfail/xpass fixture MUST reference the tracking
   issue in its reason string. No orphan failures allowed.

### Issue Filing Requirements

When you discover a grammar gap that is NOT covered by an existing issue:

1. **IMMEDIATELY** create a new GitHub issue with:
   - Clear title identifying the standard and missing feature
   - ISO/ANSI section reference (e.g., "ISO/IEC 1539:1991 Section 9")
   - ISO R-number syntax rules affected (e.g., "R904 open-stmt")
   - Impact description (what programs fail to parse)
   - Acceptance criteria checklist

2. **UPDATE** the relevant audit document (`docs/fortran_*_audit.md`) with the issue reference

3. **UPDATE** `tests/xpass_fixtures.py` if the gap affects test fixtures

### Standard References

| Standard | Reference Document | Local Path |
|----------|-------------------|------------|
| FORTRAN 1957 | IBM Form C28-6003 | validation/pdfs/ |
| FORTRAN II | IBM Form C28-6000-2 | validation/pdfs/ |
| FORTRAN 66 | ANSI X3.9-1966 | validation/pdfs/ |
| FORTRAN 77 | ANSI X3.9-1978 | validation/pdfs/ |
| Fortran 90 | WG5 N692 / ISO/IEC 1539:1991 | validation/pdfs/ |
| Fortran 95 | ISO/IEC 1539-1:1997 | validation/pdfs/ |
| Fortran 2003 | J3/03-007 / ISO/IEC 1539-1:2004 | validation/pdfs/ |
| Fortran 2008 | J3/08-007 / ISO/IEC 1539-1:2010 | validation/pdfs/ |
| Fortran 2018 | J3/15-007 / ISO/IEC 1539-1:2018 | validation/pdfs/ |
| Fortran 2023 | J3/22-007 / ISO/IEC 1539-1:2023 | validation/pdfs/ |

### Grammar File Structure

```
grammars/
  FORTRANLexer.g4, FORTRANParser.g4           # FORTRAN 1957
  FORTRANIILexer.g4, FORTRANIIParser.g4       # FORTRAN II
  FORTRAN66Lexer.g4, FORTRAN66Parser.g4       # FORTRAN 66
  FORTRAN77Lexer.g4, FORTRAN77Parser.g4       # FORTRAN 77
  Fortran90Lexer.g4, Fortran90Parser.g4       # Fortran 90
  Fortran95Lexer.g4, Fortran95Parser.g4       # Fortran 95
  Fortran2003Lexer.g4, Fortran2003Parser.g4   # Fortran 2003
  Fortran2008Lexer.g4, Fortran2008Parser.g4   # Fortran 2008
  Fortran2018Lexer.g4, Fortran2018Parser.g4   # Fortran 2018
  Fortran2023Lexer.g4, Fortran2023Parser.g4   # Fortran 2023
```

### Audit Documents

Each standard has an audit document in `docs/fortran_*_audit.md` that MUST contain:

1. Implementation coverage percentage
2. List of implemented features with ISO section references
3. List of GAPS with ISO R-numbers and tracking issue
4. xfail fixture count and issue reference
5. Future work priorities

### Current Open Issues (Grammar Gaps)

| Issue | Standard | Description |
|-------|----------|-------------|
| #399 | FORTRAN 66 | DO loop terminal statement restrictions not enforced (X3.9-1966 Section 7.1.2.8) |
| #415 | Fortran 95 | Interface-definition characteristics matching not enforced (ISO/IEC 1539-1:1997 Section 12.2) |
| #427 | FORTRAN 1957 | IF statement forms from Appendix B rows 9-10 not implemented (C28-6003) |

### Validation Workflow

Before ANY commit that modifies grammar files:

1. Run `make test` - ALL tests MUST pass
2. Run `make lint` - ALL linting MUST pass
3. Verify no new xfail fixtures without issue references
4. Update audit documents if implementation status changed

### Test Coverage Requirements

Every grammar feature MUST be covered by tests:

1. **EVERY grammar rule** MUST have at least one test fixture exercising it
2. **EVERY lexer token** MUST be tested in context
3. **Test fixtures MUST be realistic** - use actual Fortran code patterns from standards
4. **NO untested grammar rules** - if a rule exists, it MUST have test coverage
5. **Tests MUST verify correct parsing** - check parse tree structure, not just "no errors"

### Forbidden Practices

- "Intentionally minimal" or "left for future work" without issue tracking
- Features in wrong standard (e.g., F2003 features in F90 grammar)
- xfail fixtures without issue reference in reason string
- Grammar gaps without GitHub issues
- Closing issues before fixtures pass
- Claiming "complete" without 100% ISO rule coverage
- Grammar rules without test coverage
- Untested tokens or parser rules
