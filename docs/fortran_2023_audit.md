# Fortran 2023 – Grammar Audit (status: in progress)

This document summarizes **Fortran 2023** grammar support based on:

- `grammars/Fortran2023Lexer.g4`, `grammars/Fortran2023Parser.g4`
- Tests in `tests/Fortran2023/test_fortran_2023_comprehensive.py`
- Fixture expectations in `tests/test_fixture_parsing.py`

## 1. Lexer features implemented

`TestFortran2023Lexer` confirms that the lexer recognizes:

- Enhanced enumeration:
  - `ENUMERATOR`.
- Conditional expression operator:
  - `?` token (`QUESTION`).
- IEEE arithmetic enhancements:
  - `IEEE_MAX`, `IEEE_MIN`, `IEEE_MAX_MAG`, `IEEE_MIN_MAG`.
- Additional intrinsic-related constants:
  - `LOGICAL_KINDS`, `CHARACTER_KINDS`.
- Backward compatibility with:
  - F2018 coarray and team-related tokens (e.g. `CO_SUM`,
    `SELECT_RANK`, `FORM_TEAM`, `TEAM_TYPE`).
  - F2003 OOP tokens (CLASS, EXTENDS, PROCEDURE, etc.).
  - F90 module system tokens.

The lexer tests assert correct token types for these keywords and
operators.

## 2. Parser and fixtures

`TestFortran2023Parser` exercises:

- Basic F2023 program parsing using fixtures such as
  `basic_program.f90`.
- F2023 features:
  - Enhanced ENUM usage (`enum_program.f90`).
  - Conditional expressions (`conditional_expression.f90`).
  - IEEE function usage (`ieee_program.f90`).
  - BOZ constants in array constructors (`boz_array_constructor.f90`).
  - F2018 compatibility program (`f2018_compat_program.f90`).

The tests currently treat successful parse tree construction as the
primary criterion and are intentionally tolerant while coverage
improves.

## 3. Known gaps and XPASS fixtures

In `tests/test_fixture_parsing.py` many F2023 fixtures are marked
XPASS, with messages that the F2023 parser is “intentionally minimal”
and that these examples still produce syntax errors. This includes:

- `Fortran2023/test_fortran_2023_comprehensive/basic_program.f90`
- `boz_array_constructor.f90`
- `conditional_expression.f90`
- `f2018_compat_program.f90`
- `enum_program.f90`
- `ieee_program.f90`
- `mixed_era_program.f90`
- Additional NAMELIST enhancement fixtures under
  `test_fortran_2023_comprehensive_extra/`.

These XPASS entries provide the concrete list of F2023 examples that
are not yet fully accepted by the grammar.

## 4. Summary

The Fortran 2023 grammar:

- Adds tokens for F2023 enumerations, conditional expressions and IEEE
  enhancements.
- Maintains compatibility with F2018 and earlier tokens.
- Has an intentionally minimal parser that is still being extended to
  handle all F2023 fixtures without syntax errors.

Future work should:

- Use the XPASS fixtures as a roadmap for expanding F2023 coverage.
- Keep this audit aligned with the evolving comprehensive F2023 test
  suite.

