# Fortran 2008 – Grammar Audit (status: in progress)

This document summarizes **Fortran 2008** grammar support based on:

- `grammars/Fortran2008Lexer.g4`, `grammars/Fortran2008Parser.g4`
- Tests in `tests/Fortran2008/`
- References in the closed issues for coarrays, DO CONCURRENT,
  submodules and intrinsics

## 1. Core F2008 features implemented

From the tests:

- Coarrays and image control:
  - Declarations and simple usage in fixtures such as
    `coarray_declaration.f90` and `coarray_sync_all.f90`.
  - Image intrinsics (`image_intrinsics.f90`).
- Submodules:
  - Basic submodule syntax tested in
    `test_basic_f2008_features_extra/submodule_basic_syntax.f90`.
- DO CONCURRENT:
  - Basic `DO CONCURRENT` syntax tested in `do_concurrent.f90`.
- Intrinsic enhancements:
  - Enhanced intrinsic functions and named arguments in
    `enhanced_intrinsics.f90` and
    `enhanced_intrinsics_named_args.f90`.
- New integer kinds and ERROR STOP:
  - `integer_kinds.f90`, `error_stop.f90`, `error_stop_no_code.f90`.
- CONTIGUOUS attribute:
  - `contiguous_attribute.f90`, `contiguous_statement.f90`.

Closed issues (e.g. #83–#87) indicate these areas went through
iterative TDD and are considered implemented for representative usage.

## 2. Known limitations

While many features are implemented and tested, the tests and historical
issues note that:

- Coarray support is focused on representative patterns rather than all
  edge cases.
- Fixed-form semantics for F2008 follow the same unified, layout‑lenient
  model as F90/F95/F2003 and do not enforce strict 80‑column rules.
- More exotic combinations of coarrays, DO CONCURRENT locality specs
  and advanced intrinsics may not yet be covered by tests.

## 3. Summary

The F2008 grammar:

- Implements and tests the key F2008 features needed by modern code
  (coarrays, submodules, DO CONCURRENT, intrinsic and attribute
  enhancements).
- Relies on the unified fixed/free-form architecture and a subset
  fixed-form model.
- Has historically tracked gaps via issues like #83–#87; any new gaps
  should be recorded here and in new issues.

