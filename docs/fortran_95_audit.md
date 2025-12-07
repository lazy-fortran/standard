# Fortran 95 (1995) – Grammar Audit (status: in progress)

This document summarizes the **Fortran 95** grammar based on:

- `grammars/Fortran95Lexer.g4`, `grammars/Fortran95Parser.g4`
- Tests in `tests/Fortran95/test_fortran_95_features.py`
- Fixture usage via `fixture_utils.py`

It focuses on F95‑specific extensions layered on the F90 core.

## 1. F95 language extensions implemented

From the tests:

- FORALL constructs:
  - `forall_stmt` and `forall_construct` are parsed and tested using
    fixtures such as `forall_stmt.f90` and `forall_construct.f90`.
- Enhanced WHERE:
  - `where_construct_f95` is parsed and tested with
    `where_construct.f90`.
- PURE and ELEMENTAL procedures:
  - `pure_function_stmt` and `elemental_subroutine_stmt` are exercised
    by `pure_function_stmt.f90` and `elemental_subroutine_stmt.f90`.
- F95-style array constructors:
  - `array_constructor_f95` is parsed in
    `array_constructor.f90`.
- F95 intrinsics / intrinsic tokens:
  - `CEILING_INTRINSIC`, `FLOOR_INTRINSIC`, `MODULO_INTRINSIC`,
    `BIT_SIZE_INTRINSIC`, `BTEST_INTRINSIC`, `IAND_INTRINSIC`,
    `IBITS_INTRINSIC`, `TRANSFER_INTRINSIC`, `CPU_TIME_INTRINSIC`,
    `SYSTEM_CLOCK_INTRINSIC` are recognized via small fixture files.

## 2. Tests and coverage model

`tests/Fortran95/test_fortran_95_features.py` explicitly describes
itself as:

- A **minimal** test suite validating a subset of F95-specific
  extensions.

It does not:

- Parse full Fortran 95 programs or modules that combine F90 and F95
  features extensively.
- Integrate any Fortran 95 fixtures into the generic fixture parsing
  harness (`tests/test_fixture_parsing.py`).

## 3. Known limitations

Given the above:

- The grammar is exercised only on small F95‑specific snippets, not on
  full programs.
- There is no audit yet mapping the full F95 feature set (as per the
  standard) to grammar rules and tests.
- Some F95‑style constructs are indirectly tested via Fortran 90
  comprehensive fixtures (e.g. `fortran95_features_program.f90`) that
  are currently XPASS in the F90 harness, indicating remaining gaps in
  integration.

## 4. Summary

The Fortran 95 grammar:

- Implements and tests key F95 extensions such as FORALL, enhanced
  WHERE, PURE/ELEMENTAL procedures and specific intrinsics.
- Has only minimal, feature‑focused tests and no fixture coverage for
  full F95 programs.

Future work should:

- Add a richer fixture suite under `tests/fixtures/Fortran95/` covering
  realistic F95 programs and modules.
- Integrate those fixtures into `tests/test_fixture_parsing.py`.
- Expand this audit with a complete F95 feature matrix once the above
  tests exist.

