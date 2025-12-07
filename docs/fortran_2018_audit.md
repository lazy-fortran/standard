# Fortran 2018 (ISO/IEC 1539‑1:2018) – Grammar Audit (status: in progress)

This audit describes what the **Fortran 2018** grammar in this repository
implements and how it relates to the Fortran 2018 language as defined in:

- ISO/IEC 1539‑1:2018 (Fortran 2018).
- J3/15‑007 “Fortran 2018” draft/final text, stored as
  `validation/pdfs/Fortran2018_J3_15-007.pdf`.

The OCR’d `.txt` file is currently empty, so this audit uses the known
F2018 feature set plus the local PDF and compares that to:

- `grammars/Fortran2018Lexer.g4`, `grammars/Fortran2018Parser.g4`
- Inherited grammars:
  - `grammars/Fortran2008Lexer.g4`, `grammars/Fortran2008Parser.g4`
- Tests under `tests/Fortran2018/`:
  - `test_basic_f2018_features.py`
  - `test_issue61_teams_and_collectives.py`
  - The small IF/ENDIF tests (`test_simple_if.py`, `test_if_construct.py`,
    etc.) which exercise basic control‑flow inheritance.

The tests explicitly call themselves “REAL” and “status” tests: they
document what the grammar actually accepts, rather than claiming full
F2018 conformance. This audit follows the same philosophy.

---

## 1. Program structure and F2018 entry points

Specification:

- F2018 retains the F2008 program‑unit structure and extends the
  execution model with teams, events and new coarray collectives, plus
  SELECT RANK and locality specifiers for DO CONCURRENT.

Grammar implementation:

- Entry rule for F2018 programs:
  - `program_unit_f2018`:
    - Accepts leading/trailing `NEWLINE*`.
    - Chooses among:
      - `main_program_f2018`
      - `module_f2018`
      - `submodule_f2008` (submodules are unchanged relative to F2008)
      - `external_subprogram_f2018`.
- Main program:
  - `main_program_f2018`:
    - `program_stmt specification_part_f2018? execution_part_f2018? internal_subprogram_part? end_program_stmt`.
- Modules and external subprograms:
  - `module_f2018`:
    - `module_stmt specification_part_f2018? module_subprogram_part_f2018? end_module_stmt`.
  - `module_subprogram_part_f2018` and `module_subprogram_f2018` ensure
    that module-contained procedures use the F2018 subprogram rules.
  - External procedures:
    - `external_subprogram_f2018` uses `function_subprogram_f2018` and
      `subroutine_subprogram_f2018`.
- Global overrides:
  - At the end of the grammar, F2018 overrides:
    - `specification_part : specification_part_f2018`.
    - `execution_part : execution_part_f2018`.
    - `program_unit : program_unit_f2018`.

Tests:

- `test_basic_f2018_features.py`:
  - Confirms that the F2018 lexer/parser can be instantiated and that
    basic modules, programs and subroutines parse with zero errors.
  - Compares F2018 vs F2008 parsing on the same code to ensure F2018
    does not regress F2008’s behavior.
  - Uses `complex_program.f90` to sanity‑check larger program structure.
- The small IF/ENDIF tests under `tests/Fortran2018/` (`test_simple_if.py`,
  `test_if_construct*.py`, etc.) exercise the inherited control‑flow
  rules via the F2018 entry points.

Gaps:

- Block data remains inherited from F2003/F2008; F2018 does not add new
  block data syntax.
- Program‑unit structure is intentionally conservative: it does not
  introduce new top‑level unit types beyond those in F2008.

---

## 2. Enhanced specification part: intrinsic modules, events, teams

Specification:

- F2018 extends the specification part with:
  - Additional intrinsic modules (e.g. ISO_FORTRAN_ENV).
  - EVENT_TYPE and TEAM_TYPE declarations.
  - Assumed‑rank dummy arguments via DIMENSION(..).

Grammar implementation:

- `specification_part_f2018`:
  - Wraps `use_stmt`, `import_stmt`, `implicit_stmt`, and the new
    `declaration_construct_f2018`.
- Extended USE:
  - `use_stmt` overrides the F2008 rule:
    - Supports `USE module`, `USE module, ONLY: only_list`.
    - Supports `USE, INTRINSIC :: intrinsic_module_name [ , ONLY: ...]`.
  - `intrinsic_module_name`:
    - Either `ieee_module_name` or a generic `IDENTIFIER`.
    - This allows ISO_FORTRAN_ENV and other intrinsic modules even
      though they are not enumerated by name.
- `declaration_construct_f2018`:
  - Includes:
    - F2003 types, CLASS declarations and procedure declarations.
    - Interface blocks.
    - VOLATILE and PROTECTED.
    - F2008 CONTIGUOUS.
    - New F2018 declarations:
      - `event_declaration_stmt` (EVENT_TYPE).
      - `team_declaration_stmt` (TEAM_TYPE).
      - `type_declaration_stmt_f2018` (assumed‑rank, F2008 enhanced
        types, inherited type declarations).

Tests:

- `test_basic_f2018_features.py`:
  - Fixtures such as `basic_module.f90`, `comparison_module.f90` and
    `assumed_rank_dummy.f90` exercise USE with intrinsic modules and
    assumed‑rank declarations in realistic contexts.
- `test_issue61_teams_and_collectives.py`:
  - Team/events fixtures (e.g. `team_skeleton_module.f90`,
    `team_event_semantics.f90`) test EVENT_TYPE/TEAM_TYPE declarations
    via the derived‑type‑style declarations:
    - `TYPE(EVENT_TYPE) :: e`.
    - `TYPE(TEAM_TYPE) :: t`.

Gaps:

- The grammar treats intrinsic module names beyond the IEEE modules as
  generic identifiers; that is acceptable for syntax but does not
  differentiate between ISO_FORTRAN_ENV and arbitrary modules.
- No syntactic enforcement of restrictions on where EVENT_TYPE or
  TEAM_TYPE declarations may appear; those are semantic.

---

## 3. SELECT RANK construct

Specification:

- SELECT RANK selects at runtime based on the rank of an assumed‑rank
  argument:
  - `SELECT RANK (selector)` followed by:
    - `RANK (n)` cases.
    - `RANK (*)` for the assumed‑rank case.
    - `RANK DEFAULT` for the default branch.

Grammar implementation:

- Lexer:
  - Tokens `SELECT_RANK`, `RANK_STAR`, `RANK_DEFAULT`, and `RANK_KEYWORD`
    exist, but the parser uses the general `RANK_KEYWORD` token and the
    existing `DEFAULT` token rather than the “underscored” variants.
- Parser:
  - `select_rank_construct`:
    - `select_rank_stmt rank_construct* end_select_rank_stmt`.
  - `select_rank_stmt`:
    - `(IDENTIFIER COLON)? SELECT RANK_KEYWORD (expr_f90) NEWLINE`.
  - `rank_construct`:
    - `rank_case_stmt execution_part_f2018?`.
  - `rank_case_stmt`:
    - `RANK_KEYWORD (rank_value) NEWLINE` or `RANK_KEYWORD DEFAULT NEWLINE`.
  - `rank_value`:
    - Scalar integer expression (`expr_f90`) or `MULTIPLY` for `RANK(*)`.
  - `end_select_rank_stmt`:
    - `END_SELECT (IDENTIFIER)? NEWLINE`.

Tests:

- `test_issue61_teams_and_collectives.py::test_select_rank_construct_minimal_example`:
  - Uses `select_rank_minimal.f90` to exercise SELECT RANK with basic
    RANK cases and a default branch via `program_unit_f2018`.

Gaps:

- The grammar doesn’t use the underscored `SELECT_RANK`, `RANK_STAR`,
  or `RANK_DEFAULT` tokens; instead it relies on the keyword `RANK` and
  `DEFAULT`. This is consistent with the actual Fortran syntax (e.g.
  `SELECT RANK (a)`, `RANK (n)`, `RANK DEFAULT`), but the extra tokens
  are not used and are effectively redundant.
- Semantic requirements (e.g., selector must be assumed‑rank, constant
  rank selectors) are not encoded; these must be handled by semantic
  analysis.

---

## 4. Collective coarray operations

Specification:

- F2018 introduces collective subroutines:
  - `CO_SUM`, `CO_MIN`, `CO_MAX`, `CO_REDUCE`, `CO_BROADCAST`.
  - With STAT, ERRMSG and RESULT_IMAGE arguments, including TEAM
    arguments in some cases (TEAM semantics are modeled separately).

Grammar implementation:

- Lexer:
  - `CO_SUM`, `CO_MIN`, `CO_MAX`, `CO_REDUCE`, `CO_BROADCAST`,
    `RESULT_IMAGE`.
- Parser:
  - `collective_subroutine_call`:
    - Chooses among `co_sum_stmt`, `co_min_stmt`, `co_max_stmt`,
      `co_reduce_stmt`, `co_broadcast_stmt`.
  - `co_sum_stmt` / `co_min_stmt` / `co_max_stmt`:
    - `CALL CO_SUM/CO_MIN/CO_MAX (collective_arg_list)`.
  - `co_reduce_stmt`:
    - `CALL CO_REDUCE (variable_f2008, operation_spec [, collective_stat_list])`.
  - `co_broadcast_stmt`:
    - `CALL CO_BROADCAST (variable_f2008, source_image_spec [, collective_stat_list])`.
  - `collective_arg_list`:
    - `variable_f2008 [, collective_stat_list]`.
  - `collective_stat`:
    - `STAT = variable_f90`, `ERRMSG = variable_f90`,
      `RESULT_IMAGE = variable_f90`.
  - `operation_spec` and `source_image_spec`:
    - Generic identifier / expression forms.

Tests:

- `test_issue61_teams_and_collectives.py::test_collective_subroutines_parse_without_errors`:
  - `teams_collectives_module.f90` exercises all five collective calls
    with representative STAT/ERRMSG/RESULT_IMAGE usage.

Gaps:

- The grammar does not enforce:
  - That coarray arguments have the correct rank or codimensions.
  - That RESULT_IMAGE is present only where allowed.
  - That TEAM arguments (when used) meet the standard’s constraints.
  - Those are semantic constraints and belong to later passes (see
    F2008 coarray semantics issue #188 and related F2018 follow‑ups).

---

## 5. Teams, team constructs, and team declarations

Specification:

- Fortran 2018 adds:
  - `FORM TEAM`, `CHANGE TEAM`, `END TEAM`.
  - TEAM_TYPE and GET_TEAM.
  - Team values and team numbers for segmentation of coarray images.

Grammar implementation:

- Lexer:
  - `FORM_TEAM`, `CHANGE_TEAM`, `END_TEAM`, `TEAM_TYPE`, `GET_TEAM`,
    `TEAM`, `NEW_INDEX`.
- Parser:
  - `team_construct`:
    - `change_team_construct | form_team_stmt`.
  - `change_team_construct`:
    - `change_team_stmt execution_part_f2018? end_team_stmt`.
  - `change_team_stmt`:
    - Optional construct name +
      `CHANGE_TEAM (team_value [, coarray_association_list] [, sync_stat_list])`.
  - `end_team_stmt`:
    - `END_TEAM (sync_stat_list)? (construct_name)?`.
  - `form_team_stmt`:
    - `FORM_TEAM (team_number_expr, team_variable [, form_team_stat_list])`.
  - `coarray_association_list`:
    - `coarray_association`, where `IDENTIFIER [cosubscript_list] -> variable_f2008`.
  - `form_team_stat_list`:
    - `NEW_INDEX = expr_f90`, `STAT = variable_f90`, `ERRMSG = variable_f90`.
  - Team declarations:
    - `team_declaration_stmt`:
      - `TYPE(TEAM_TYPE) :: entity_decl_list`.

Tests:

- `test_issue61_teams_and_collectives.py`:
  - `team_skeleton_module.f90` and `team_event_semantics.f90` cover:
    - FORM TEAM and CHANGE TEAM in a module context.
    - END TEAM with and without sync stats.
    - Use of TEAM_TYPE declarations and coarray associations.
  - The test explicitly accepts both:
    - Standard spellings (`form team`, `change team`, `event post`).
    - Legacy underscored variants (`form_team`, `change_team`, etc.)
      by transforming the source and picking the variant with the fewest
      syntax errors.

Gaps:

- Lexer vs parser spelling:
  - The lexer defines `FORM_TEAM`, `CHANGE_TEAM`, etc. as compound
    tokens with embedded whitespace (e.g. `FORM WS+ TEAM`), while the
    tests still support legacy underscored spellings as a migration aid.
  - As a result, both `FORM TEAM` and `form_team` may be accepted in
    practice depending on casing; the audit notes this as a historical
    artifact rather than a claim about strict standard spelling.
- Semantic constraints:
  - The grammar does not enforce all rules about team numbers,
    team values, and interactions with coarrays or collectives.

---

## 6. Events, EVENT_TYPE, and event constructs

Specification:

- F2018 introduces events for fine‑grained synchronization:
  - EVENT_TYPE objects and subroutines `EVENT POST`, `EVENT WAIT`,
    `EVENT QUERY`.

Grammar implementation:

- Lexer:
  - `EVENT_TYPE`, `EVENT_POST`, `EVENT_WAIT`, `EVENT_QUERY`,
    `COUNT`, `UNTIL_COUNT`.
- Parser:
  - `event_construct`:
    - `event_post_stmt | event_wait_stmt | event_query_stmt`.
  - `event_post_stmt`:
    - `EVENT_POST (event_variable [, event_stat_list])`.
  - `event_wait_stmt`:
    - `EVENT_WAIT (event_variable [, event_wait_spec_list])`.
  - `event_query_stmt`:
    - `EVENT_QUERY (event_variable, COUNT = variable_f90 [, event_stat_list])`.
  - `event_stat_list`:
    - `STAT = variable_f90` or `ERRMSG = variable_f90`.
  - `event_wait_spec_list`:
    - `UNTIL_COUNT = expr_f90` and/or `event_stat` entries.
  - Event declaration:
    - `event_declaration_stmt`:
      - `TYPE(EVENT_TYPE) :: entity_decl_list`.

Tests:

- `test_issue61_teams_and_collectives.py`:
  - `team_event_semantics.f90` includes EVENT POST/WAIT/QUERY in a
    program that also uses teams and collectives.

Gaps:

- Semantic rules (e.g. relationship between UNTIL_COUNT and COUNT,
  allowed contexts for events) are not modeled.
- The same “legacy underscored spelling” compatibility as for teams
  applies in tests (via string replacements), but the grammar aims for
  standard spellings `event post`, `event wait`, `event query`.

---

## 7. DO CONCURRENT with locality specifiers

Specification:

- F2018 enhances DO CONCURRENT with locality specifiers:
  - Local/LOCAL_INIT/SHARED variables and DEFAULT(NONE).
  - The DO CONCURRENT header is extended to include a concurrency
    header and locality list.

Grammar implementation:

- `do_construct_f2018`:
  - Extends `do_construct` with `do_concurrent_construct_f2018`.
- `do_concurrent_construct_f2018`:
  - `do_concurrent_stmt_f2018 execution_part_f2018? end_do_stmt`.
- `do_concurrent_stmt_f2018`:
  - Optional construct name + `DO CONCURRENT concurrent_header_f2018 concurrent_locality_list?`.
- `concurrent_header_f2018`:
  - Reuses `forall_triplet_spec_list` and an optional `scalar_mask_expr`.
- `concurrent_locality_list`:
  - One or more `concurrent_locality` items.
- `concurrent_locality`:
  - `LOCAL(...)`, `LOCAL_INIT(...)`, `SHARED(...)`, or `DEFAULT(NONE)`.

Tests:

- `test_basic_f2018_features.py::test_do_concurrent_with_locality_and_mask`:
  - `do_concurrent_locality.f90` exercises DO CONCURRENT with:
    - A full concurrent header.
    - Both a mask and locality list.

Gaps:

- Semantic constraints (e.g. which variables can be LOCAL/SHARED, and
  whether the locality list is consistent with the loop body) are not
  encoded.
- The header syntax reuses FORALL triplet rules; any subtle syntactic
  differences between the F2018 DO CONCURRENT header and FORALL
  control list are not distinguished.

---

## 8. STOP / ERROR STOP, assumed rank, and new intrinsic functions

Specification:

- F2018 extends:
  - STOP and ERROR STOP with QUIET= keyword.
  - Assumed‑rank dummy arguments via DIMENSION(..).
  - Intrinsic functions for images, collectives, and numerical
    enhancements (IMAGE_STATUS/FAILED_IMAGES/STOPPED_IMAGES, COSHAPE,
    TEAM_NUMBER, OUT_OF_RANGE, REDUCE with DIM/MASK, RANDOM_INIT with
    REPEATABLE/IMAGE_DISTINCT).

Grammar implementation:

- Enhanced STOP / ERROR STOP:
  - `stop_stmt_f2018`, `error_stop_stmt_f2018`:
    - Both can take `stop_code` and optional `, QUIET = logical_expr`.
- Assumed rank:
  - `assumed_rank_declaration`:
    - `type_spec, DIMENSION(..) :: entity_list`.
  - Included in `type_declaration_stmt_f2018`.
- Intrinsic functions:
  - Lexer:
    - `IMAGE_STATUS`, `FAILED_IMAGES`, `STOPPED_IMAGES`,
      `COSHAPE`, `TEAM_NUMBER`, `OUT_OF_RANGE`, `REDUCE`,
      `RANDOM_INIT`, `REPEATABLE`, `IMAGE_DISTINCT`, `TEAM`, `DIM`,
      `MASK`, `NONE`.
  - Parser:
    - `intrinsic_function_call_f2018`:
      - Extends `intrinsic_function_call_f2008` with:
        - `image_status_function_call` (IMAGE_STATUS, FAILED_IMAGES, STOPPED_IMAGES).
        - `collective_function_call` (COSHAPE, TEAM_NUMBER).
        - `random_init_call` (CALL RANDOM_INIT with REPEATABLE/MAGE_DISTINCT).
        - `enhanced_math_function_call` (OUT_OF_RANGE, REDUCE with DIM/MASK).

Tests:

- `test_basic_f2018_features.py`:
  - `assumed_rank_dummy.f90` for assumed‑rank.
  - `do_concurrent_locality.f90` (DO CONCURRENT with mask/locality).
  - The same fixtures that exercise F2008 intrinsics are parsed via
    F2018 and compared.
- `test_issue61_teams_and_collectives.py`:
  - `team_event_semantics.f90` uses IMAGE_STATUS functions and TEAM
    intrinsics in a team/collective context.

Gaps:

- As with earlier standards:
  - The grammar does not restrict intrinsic arguments to the exact sets
    allowed by the standard (rank, type, keyword combinations).
  - It does not enforce that STOP/ERROR STOP codes satisfy all
    constraints; it only accepts the syntactic forms.

---

## 9. Inheritance from F2008 and control‑flow tests

Specification:

- F2018 does not substantially change the core control‑flow constructs
  (IF, SELECT CASE, DO/DO WHILE) or basic program structure beyond
  what F2008 already supports, so most of this behavior is inherited.

Grammar implementation:

- By importing `Fortran2008Parser` and overriding `program_unit`,
  `specification_part`, and `execution_part`, the F2018 grammar
  inherits all F2008 syntax:
  - Submodules, coarrays, DO CONCURRENT, CONTIGUOUS, ERROR STOP.
  - Enhanced integer/real kinds and intrinsics.

Tests:

- The battery of IF/ENDIF tests in `tests/Fortran2018/` exist mainly to
  ensure:
  - The F2018 parser can handle a variety of IF syntax patterns
    (IF/ELSE IF/ELSE/ENDIF, IF blocks with bodies), and
  - That these constructs behave the same as in F2008 for ordinary code.

Gaps:

- None at the syntactic level beyond what already exists in F2008; any
  further semantic rules (e.g. for DO CONCURRENT, coarrays) are already
  called out in the F2008 audit and the F2018 semantic issues below.

---

## 10. Summary and issue mapping

The Fortran 2018 layer in this repository:

- **Implements and tests, for practical use:**
  - F2018‑specific program entry (`program_unit_f2018`) and integration
    with F2008 features.
  - SELECT RANK, including RANK cases and RANK DEFAULT.
  - Collective coarray subroutines (CO_SUM, CO_MIN, CO_MAX, CO_REDUCE,
    CO_BROADCAST) with STAT/ERRMSG/RESULT_IMAGE.
  - Team constructs (FORM TEAM, CHANGE TEAM, END TEAM), TEAM_TYPE
    declarations, and coarray association syntax.
  - Event constructs (EVENT POST/WAIT/QUERY) and EVENT_TYPE
    declarations.
  - DO CONCURRENT with locality specifiers and mask.
  - Enhanced STOP and ERROR STOP with QUIET.
  - Assumed‑rank dummy arguments via DIMENSION(..).
  - Additional intrinsic procedures from F2018 (image status, teams,
    collectives, RANDOM_INIT, OUT_OF_RANGE, REDUCE extensions).
  - Inheritance of all F2008 syntax (submodules, coarrays, CONTIGUOUS,
    etc.) with F2018 entry points.
- **Intentionally leaves to semantic tooling:**
  - Coarray semantics (ranks, codimensions, image control).
  - DO CONCURRENT semantics and locality rules.
  - Team/event semantics, including TEAM formation and event counts.
  - Intrinsic procedure argument validity and usage contexts.
  - Submodule linkage (inherited from F2008 concerns).

Existing umbrella issues relevant to this audit:

- #140 – **Standard audits** (this document is the Fortran 2018 slice).
- #177 – **Fortran 2018: annotate grammar with J3/15‑007 sections**:
  - Should use this audit as the spec→grammar cross‑walk and ensure
    every F2018 gap identified here has its own issue.

Additional issues (existing or to be opened) should cover:

- Semantic checks for F2018 coarray collectives, teams and events on
  top of the F2008 coarray semantics.
- DO CONCURRENT with locality (semantic checks).
- Intrinsic usage semantics for new F2018 intrinsics.

When those issues and their follow‑ups are addressed, and the grammar
annotations required by #177 are in place, Fortran 2018 will have a
spec‑aware grammar audit matching the depth of the Fortran 90/95/2003/2008
audits already in this repository.

