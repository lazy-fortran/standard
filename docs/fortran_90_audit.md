# Fortran 90 (1990) – Grammar Audit (status: in progress)

This audit summarizes the **Fortran 90** grammar based on:

- `grammars/Fortran90Lexer.g4`, `grammars/Fortran90Parser.g4`
- `docs/fixed_form_support.md`
- Tests under `tests/Fortran90/`
- Fixture expectations in `tests/test_fixture_parsing.py`

It focuses on what is implemented and where the test suite still
documents gaps.

## 1. Major F90 features

The comprehensive tests in `tests/Fortran90/test_fortran_90_comprehensive.py`
confirm that the lexer and parser support:

- Module system (`MODULE`, `USE`, `ONLY`, PUBLIC/PRIVATE).
- Interface blocks (INTERFACE, END INTERFACE, GENERIC, OPERATOR,
  ASSIGNMENT).
- Derived types and components.
- Dynamic arrays (ALLOCATABLE, POINTER, TARGET, ALLOCATE/DEALLOCATE).
- Enhanced control (`SELECT CASE`, `WHERE`, EXIT/CYCLE).
- Array operations and constructors.
- Modern I/O features (NAMELIST, non-advancing I/O, etc.).
- Modern operators (`::`, `=>`, `%`, relational operators, string
  concatenation, BOZ constants, logical literals).

These are extensively exercised at the lexer level and through targeted
parser tests.

## 2. Unified fixed/free-form architecture

Per `docs/fixed_form_support.md`:

- Fortran 90 uses a **unified** lexer/parser for both fixed-form and
  free-form.
- Fixed-form is modeled as a **layout‑lenient subset**:
  - `FIXED_FORM_COMMENT` treats `C`/`c`/`*` at line start as a comment
    without column tracking.
  - No enforcement of strict label columns, continuation column or
    sequence numbers.
- Free-form continuation and comments are fully supported.

Known lexer limitation:

- Some tokens (e.g. bare `CASE`, `CYCLE`, `COUNT`) can clash with the
  fixed-form comment rule in isolation, as documented in the F90
  tests.

## 3. Fixture coverage and XPASS gaps

`tests/test_fixture_parsing.py` marks many Fortran 90 fixtures as
XPASS, with explanations that they still produce syntax errors or
exercise rough edges in the unified grammar. These include:

- `tests/fixtures/Fortran90/test_fortran_90_comprehensive/`:
  - `basic_program.f90`
  - `mathematics_module.f90`
  - `derived_types_module.f90`
  - `dynamic_arrays.f90`
  - `select_case_program.f90`
  - `array_constructor_program.f90`
  - `enhanced_procedures_program.f90`
  - `test_fortran_90_dual_format/free_form_module.f90`
- `tests/fixtures/Fortran90/test_comprehensive_parsing/`:
  - `mathematics_module.f90`
  - `dynamic_arrays_program.f90`
  - `types_module.f90`
  - `fixed_form_program.f`
  - `free_form_features_program.f90`
  - `advanced_features_module.f90`
  - `fortran95_features_program.f90`
  - `mixed_format_program.f90`

The XPASS reasons describe:

- Remaining limitations in:
  - Module/program-unit handling.
  - Derived type constructs in larger modules.
  - Dynamic arrays and array constructors.
  - Enhanced procedure usage.
  - Unified fixed/free-form integration.

These fixtures together form the concrete list of F90 cases that the
current grammar still fails to parse.

## 4. Summary

The Fortran 90 grammar:

- Implements the major F90 innovations and passes extensive targeted
  tests.
- Provides a unified fixed/free-form lexer with a documented,
  layout‑lenient fixed-form subset.
- Still fails on a set of realistic comprehensive fixtures, which are
  currently XPASS and documented as known limitations.

Future work should:

- Resolve the XPASS fixtures by improving module handling, array
  constructs and unified fixed/free-form behavior.
- Expand this audit with a more explicit feature matrix once those
  gaps are addressed.

