# Fortran 2018 Implementation – Current Status

This document describes the status of the Fortran 2018 grammar in this repository. It is about the **implementation here**, not a claim of full conformance to the ISO/IEC 1539‑1:2018 standard.

## Overview

- The Fortran 2018 lexer and parser are defined in `grammars/Fortran2018Lexer.g4` and `grammars/Fortran2018Parser.g4`.
- They are built on top of the Fortran 2008 grammars via ANTLR’s grammar import mechanism.
- The top‑level `Makefile` target `Fortran2018` generates the corresponding Python modules in `grammars/` and they can be imported by the tests.

From a language‑design point of view, Fortran 2018 extends Fortran 2008 with features such as:

- additional coarray collective subroutines (e.g. `CO_SUM`, `CO_MIN`, `CO_MAX`, `CO_BROADCAST`),
- `SELECT RANK` and assumed‑rank dummy arguments,
- teams and events for coarray parallelism (`FORM TEAM`, `TEAM_TYPE`, `EVENT_TYPE`, etc.),
- C descriptor support (`C_F_POINTER`, `C_SIZEOF` and related facilities),
- various intrinsic and error‑handling enhancements.

The lexer in this repository defines tokens for these F2018 concepts, and the parser contains rules that use those tokens.

## What is Working

Based on the current test suite in `tests/Fortran2018`:

- **Build and imports**
  - `make Fortran2018` (or `make all`) successfully generates `Fortran2018Lexer.py` and `Fortran2018Parser.py`.
  - The tests are able to import and instantiate the lexer and parser.

- **Basic structure**
  - Simple modules, programs and subroutines can be parsed with a low syntax‑error count (see `test_basic_f2018_features.py`).
  - The grammar correctly inherits Fortran 2008 features such as basic coarray declarations and `SYNC ALL`.

- **Selected F2018 features**
  - Tokens for key F2018 constructs (collective operations, `SELECT_RANK`, team/event support, C descriptor helpers, etc.) are present in the lexer and wired into parser rules.
  - `tests/Fortran2018/test_basic_f2018_features.py` includes “REAL” tests that:
    - compare F2018 parsing against the F2008 parser on the same coarray code,
    - exercise some F2018‑style calls such as `co_sum`, `select rank`, `event post`, `form team`,
    - check error‑recovery behaviour on invalid code,
    - perform a simple coverage sanity check on basic program structures.
  - Additional tests in `tests/Fortran2018` validate IF / ENDIF variants, direct parsing of `ENDIF`, basic tokenization behaviour and simple parse‑tree inspection for IF constructs.

All tests in `tests/Fortran2018` currently pass.

## Known Limitations

- **Partial language coverage**
  - The Fortran 2018 standard is large; this grammar only covers a subset of its facilities and the tests exercise only some of those.
  - Complex combinations of teams, events and coarray collectives, as well as many less‑commonly used intrinsics and constructs, are not yet covered by tests and may not parse correctly.

- **Error handling and recovery**
  - Error recovery has only been lightly exercised (see the “error recovery and robustness” test); behaviour on heavily malformed or highly parallel code has not been tuned.

- **No formal conformance measurement**
  - The implementation has not been systematically compared against the full text of the Fortran 2018 standard or against a comprehensive external test suite.
  - Numerical “percentage coverage” claims would be misleading; instead, the emphasis is on describing what is known to work via the existing tests.

## Recommended Usage

- Treat the Fortran 2018 grammar as an **experimental extension** of the tested Fortran 2003/2008 grammars.
- It is suitable for:
  - experimenting with F2018‑style code and coarray features,
  - research and tooling prototypes that only rely on the subset exercised by the tests,
  - iteratively extending support for additional F2018 features.
- It is **not yet appropriate** to rely on this grammar as a fully conforming, production‑quality parser for arbitrary Fortran 2018 code bases.

For details on the language itself, refer to the official Fortran 2018 standard and the J3 committee documents.

