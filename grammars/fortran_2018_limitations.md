# Fortran 2018 grammar limitations

This document describes the subset of Fortran 2018 that is currently
modeled by `Fortran2018Lexer.g4` / `Fortran2018Parser.g4` and exercised
by the tests under `tests/Fortran2018`. It is intentionally factual and
aims to reflect the behavior of the shipped grammars, not the full ISO
standard.

## Teams, events, and collectives

- **Collective subroutines (CO_SUM, CO_MIN, CO_MAX, CO_BROADCAST, CO_REDUCE)**  
  Simple scalar and array cases with optional `STAT=`, `ERRMSG=` and
  `SOURCE_IMAGE=` keywords parse without syntax errors when written in
  the style of `tests/fixtures/Fortran2018/test_issue61_teams_and_collectives/teams_collectives_module.f90`.
  More exotic argument lists (user-defined reduction operators, derived
  types with type-bound procedures, and mixed keyword/positional forms)
  are not currently covered by tests and may fail.

- **TEAM and EVENT constructs**  
  The grammar includes rules for `FORM TEAM`, `CHANGE TEAM`,
  `EVENT POST`, `EVENT WAIT` and `EVENT QUERY`. In the generated
  Python parser that ships with this repository, only the internal
  underscored spellings (`form_team`, `change_team`, `event_post`,
  `event_wait`, `event_query`, `end_team`) are accepted reliably.
  The status test
  `TestF2018TeamsAndCollectivesStatus.test_team_and_event_constructs_supported_subset`
  in `tests/Fortran2018/test_issue61_teams_and_collectives.py`
  explicitly allows either the standard spellings or these internal
  spellings and asserts that at least one variant parses with zero
  syntax errors.

- **TEAM/EVENT declarations and advanced patterns**  
  The dedicated declaration forms
  `type(team_type) :: ...` and `type(event_type) :: ...`,
  coarray association lists in `CHANGE TEAM`, and full sync-stat
  combinations are present in the grammar but are not exercised by the
  current tests. Their behavior should be considered experimental.

- **Reserved identifiers**  
  Several F2018 keywords are tokenised as dedicated tokens in the
  lexer (`TEAM`, `COUNT`, `RESULT_IMAGE`, etc.). Using these words as
  ordinary identifiers in declarations (for example
  `integer :: count`) can cause syntax errors because they are no
  longer recognised as `IDENTIFIER`. The tests use names such as
  `team_handle`, `count_value` and `result_image_var` instead.

## SELECT RANK construct

- The lexer exposes tokens `SELECT_RANK`, `RANK_KEYWORD`, `RANK_STAR`
  and `RANK_DEFAULT`, and these are imported into later standards
  (Fortran 2023 and LazyFortran2025) so that the feature is available
  along the full inheritance chain.

- The grammar file `Fortran2018Parser.g4` contains a provisional
  `select_rank_construct` rule intended to support:
  - `select rank (selector)`
  - `rank (n)` and `rank (*)` cases
  - `rank default` for the default case

  However, the generated `Fortran2018Parser.py` in this repository has
  not yet been rebuilt from that updated rule, so standard-conforming
  `SELECT RANK` constructs still produce syntax errors in the current
  parser.

- The test
  `TestF2018TeamsAndCollectivesStatus.test_select_rank_construct_minimal_example`
  in `tests/Fortran2018/test_issue61_teams_and_collectives.py`
  is marked `xfail` and serves as a status test. Once the grammars are
  regenerated and tuned under the planned implementation work
  (see issue #88), this test is expected to be tightened to require
  zero syntax errors.

## General notes

- The Fortran 2018 grammar is designed as a practical superset of the
  Fortran 2008 grammar used elsewhere in this repository. It is not a
  complete implementation of ISO/IEC 1539-1:2018.

- When in doubt, treat the unit tests under `tests/Fortran2018` as the
  source of truth for what is currently accepted. This document
  highlights the most important limitations but does not enumerate
  every corner case.

