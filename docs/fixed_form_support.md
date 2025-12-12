# Fixed-form Fortran Support and Historical Subset

This document describes how this repository models **fixed-form Fortran**
across the historical standards (FORTRAN 1957, FORTRAN II, FORTRAN 66,
FORTRAN 77) and the modern unified grammars (Fortran 90/95/2003+).

The goal is to make the **supported subset explicit** and to point out
fixed-form behaviours that remain **intentionally out of scope** and are
tracked as missing work (umbrella Issue #91).

## 1. Conceptual Model vs. Strict Column Rules

Classic fixed-form Fortran uses an 80-column card layout:

- Columns **1–5**: statement labels (1–99999)
- Column **6**: continuation mark
- Columns **7–72**: statement text
- Columns **73–80**: sequence numbers (usually ignored by the compiler)
- Column **1**: `C` / `c` / `*` comment indicator in many dialects

In this repository:

- Grammars treat fixed-form as a **layout‑lenient model**:
  - Labels and statements are parsed based on tokens and rule order,
    **not** on physical column numbers.
  - Sequence numbers (73–80) are **not** modeled.
  - Column‑6 continuation is **not** implemented as a separate pre‑lexing
    phase.
- Strict 80‑column enforcement and full historical line‑layout semantics
  are **not yet implemented** and remain **out of scope** for now.

Issue **#91** is the umbrella tracker for any future work toward
full, column‑accurate fixed-form semantics.

## 2. Historical Dialects (FORTRAN, FORTRAN II, FORTRAN 66, FORTRAN 77)

The historical grammars provide a **representative, educational subset**
of each standard rather than a complete, bit‑perfect reconstruction of
every vendor implementation.

### 2.1 FORTRAN (1957)

- Implemented as a **historical stub**:
  - Arithmetic expressions, DO loops, arithmetic IF, computed GOTO.
  - FORMAT, DIMENSION, EQUIVALENCE, FREQUENCY, PAUSE, simple I/O.
- Fixed-form handling:
  - Uses a **statement‑oriented** lexer (`FORTRANLexer.g4`) with explicit
    `NEWLINE` tokens.
  - **Does not** enforce columns 1–5 / 6 / 7–72 / 73–80 in the grammar.
  - Comments use modern `!` syntax in the grammar; classic `C` column‑1
    comments are handled by the strict fixed-form preprocessor.

#### Strict Fixed-Form Mode for FORTRAN 1957 (tools/strict_fixed_form.py)

FORTRAN 1957 now provides an optional **strict fixed-form preprocessor** that
validates and converts IBM 704 card-image source files according to the
C28-6003 (Oct 1958) specification:

- **Card layout validation** per C28-6003 Chapter I.B:
  - Columns 1–5: Statement labels (numeric 1–32767 or blank)
  - Column 6: Continuation mark (non-blank = continuation)
  - Columns 7–72: Statement text
  - Columns 73–80: Sequence/identification field (ignored)
  - Column 1: `C` marks a comment card (historical; `*` generates warning)

- **Usage**:
  ```python
  from tools.strict_fixed_form import (
      validate_strict_fixed_form_1957,
      convert_to_lenient_1957
  )

  # Validate strict 1957 card layout
  result = validate_strict_fixed_form_1957(source)
  if not result.valid:
      for error in result.errors:
          print(f"Line {error.line_number}: {error.message}")

  # Convert to layout-lenient form for FORTRANParser
  lenient, result = convert_to_lenient_1957(source, strip_comments=True)
  ```

- **Test fixtures** in `tests/fixtures/FORTRAN/test_strict_fixed_form/`:
  - `valid_arithmetic.f`, `valid_do_loop.f`, `valid_if_goto.f`: Authentic
    80-column card layouts with sequence numbers
  - `valid_continuation.f`: Multi-card statement continuations
  - `label_range.f`: Valid labels within 1957 range (1–32767)
  - `invalid_label_alpha.f`, `invalid_continuation_labeled.f`: Invalid
    layouts that strict mode correctly rejects
  - `invalid_label_out_of_range.f`: Label exceeding 1957 limit
  - `star_comment_warning.f`: Non-historical `*` comment generates warning

- **Key differences from FORTRAN II strict mode**:
  - Labels limited to 1–32767 (vs. 1–99999 for FORTRAN II)
  - Only `C` in column 1 is historical for 1957 (`*` generates warning)

The strict mode preprocessor enables historical audits requiring exact
card-image conformance while keeping the lenient grammar parsing mode
available for modern tooling.

### 2.2 FORTRAN II (1958)

- Extends FORTRAN with:
  - `SUBROUTINE`, `FUNCTION`, `CALL`, `RETURN`, `COMMON`.
  - Explicit `LABEL` tokens (1–5 digits, no leading zero) in
    `FORTRANIILexer.g4`.
- Fixed-form handling:
  - `FORTRANIIParser.g4` models **optional labels followed by a statement**
    and an optional `NEWLINE`, reflecting the punch‑card structure.
  - Layout is still **logical**, not column‑sensitive; any indentation
    that keeps tokens in the right order is accepted.

#### Strict Fixed-Form Mode (tools/strict_fixed_form.py)

FORTRAN II now provides an optional **strict fixed-form preprocessor** that
validates and converts IBM 704 card-image source files according to the
C28-6000-2 (1958) specification:

- **Card layout validation** per C28-6000-2 Part I, Chapter 2:
  - Columns 1–5: Statement labels (numeric 1–99999 or blank)
  - Column 6: Continuation mark (non-blank = continuation)
  - Columns 7–72: Statement text
  - Columns 73–80: Sequence/identification field (ignored)
  - Column 1: `C` or `*` marks a comment card

- **Usage**:
  ```python
  from tools.strict_fixed_form import validate_strict_fixed_form, convert_to_lenient

  # Validate strict card layout
  result = validate_strict_fixed_form(source)
  if not result.valid:
      for error in result.errors:
          print(f"Line {error.line_number}: {error.message}")

  # Convert to layout-lenient form for parsing
  lenient, result = convert_to_lenient(source, strip_comments=True)
  ```

- **Test fixtures** in `tests/fixtures/FORTRANII/test_strict_fixed_form/`:
  - `valid_subroutine.f`, `valid_function.f`, `valid_main.f`: Authentic
    80-column card layouts with sequence numbers
  - `valid_continuation.f`: Multi-card statement continuations
  - `invalid_label_alpha.f`, `invalid_continuation_labeled.f`: Invalid
    layouts that strict mode correctly rejects

The strict mode preprocessor enables historical audits requiring exact
card-image conformance while keeping the lenient grammar parsing mode
available for modern tooling.

### 2.3 FORTRAN 66 (1966) and FORTRAN 77 (1977)

- FORTRAN 66 (`FORTRAN66Lexer.g4`) and FORTRAN 77 (`FORTRAN77Lexer.g4`)
  extend the earlier dialects with:
  - FORTRAN IV logical operators and data types (LOGICAL, DOUBLE PRECISION,
    COMPLEX).
  - Standardized labels, BLOCK/BLOCK DATA, EXTERNAL/INTRINSIC (F66).
  - CHARACTER, IF–THEN–ELSE/ENDIF, PARAMETER, SAVE, PROGRAM,
    expanded I/O (F77).
- Fixed-form handling:
  - Inherits the **layout‑lenient** model from earlier grammars.
  - Tests under `tests/FORTRAN77` and the shared fixture suites exercise
    labeled statements and typical fixed‑form layouts, but they do **not**
    require exact column positions.
  - Classic column‑1 comment forms (`C`/`*`) and sequence‑number handling
    are treated as historical context, not as part of the core subset.

## 3. Fortran 90/95 Unified Fixed/Free Form

Fortran 90 and later use a **unified grammar** that accepts both
fixed-form and free-form syntax in a single lexer/parser per standard.

- `Fortran90Lexer.g4` introduces:
  - `FREE_FORM_COMMENT` (`! ...`) and simplified `FIXED_FORM_COMMENT`
    that treats `C`, `c` or `*` at the start of a line as a comment
    without tracking explicit column numbers.
  - `CONTINUATION` based on the free-form `&` continuation convention.
- `Fortran90Parser.g4` and `Fortran95Parser.g4`:
  - Accept both traditional `.f` and modern `.f90` layouts.
  - Model labels and statements at the token/rule level, not via
    positional column constraints.

### 3.1 Fortran 90 Fixed-form Supported Subset

The Fortran 90 grammar supports a **documented fixed-form subset** that
uses F90 constructs rather than legacy F77 constructs. This reflects the
revolutionary nature of F90, which introduced free-form source as the
primary format while maintaining backward compatibility.

**Supported in F90 fixed-form:**
- Free-form `!` comments (anywhere on the line)
- Column-1 `C`/`c`/`*` comments (following a newline, not at file start)
- `DO ... END DO` loop constructs
- `IF ... THEN ... END IF` constructs
- `SELECT CASE ... END SELECT` constructs
- `WHERE ... END WHERE` constructs
- `PRINT *, list` list-directed output
- All F90 type declarations, modules, interfaces, etc.

**Not supported in F90 grammar (legacy F77 constructs):**
- Column-1 `C`/`c`/`*` comments at the very start of a file (no preceding
  newline) - these require a leading blank line
- Labeled `DO label var=start,end` loops with terminal `label CONTINUE`
  (use `DO ... END DO` instead)
- `WRITE(*,*)` shorthand for list-directed I/O (use `PRINT *,` or
  `WRITE(UNIT=..., FMT=...)` with explicit keyword specifiers)
- Column-6 continuation marks (use `&` free-form continuation)
- Statement labels in columns 1-5 (labels work but column enforcement is
  not strict)

The fixture `tests/fixtures/Fortran90/test_comprehensive_parsing/fixed_form_program.f`
demonstrates valid F90 fixed-form source using the supported constructs.
This approach aligns with F90 being a transitional standard that strongly
encouraged adoption of free-form source and modern control structures.

### 3.2 Fortran 90 Strict Fixed-Form Mode (Issue #673)

Fortran 90 now provides an optional **strict fixed-form validator** that enforces
card layout semantics per ISO/IEC 1539:1991 Section 3.3, complementing the
layout-lenient grammar parsing mode.

**Usage:**

```python
from tools.strict_fixed_form import validate_strict_fixed_form_90, convert_to_lenient_90

# Validate strict 80-column Fortran 90 source
result = validate_strict_fixed_form_90(source)
if not result.valid:
    for error in result.errors:
        print(f"Line {error.line_number}, Col {error.column}: {error.message}")

# Convert to layout-lenient form for Fortran90Parser
lenient, result = convert_to_lenient_90(source, strip_comments=False)
```

**Card Layout Validation** per ISO/IEC 1539:1991 Section 3.3:
- Columns 1–5: Statement labels (numeric 1–99999 or blank)
- Column 6: Continuation mark (non-blank = continuation)
- Columns 7–72: Statement text
- Columns 73–80: Sequence/identification field (ignored)
- Column 1: `C` or `*` marks a comment card

**Key Constraints Enforced:**
- Labels must be numeric and in range 1–99999
- Continuation cards (column 6 non-blank) must not have labels
- Continuation cards must follow a statement card
- Comment cards are recognized by column 1 markers (`C` or `*`)
- Sequence numbers (cols 73–80) are stripped during conversion

**Test Fixtures** in `tests/fixtures/Fortran90/test_strict_fixed_form/`:
- `valid_program.f90`: Basic program with proper column layout
- `valid_with_labels.f90`: Labeled statements in columns 1–5
- `valid_with_continuation.f90`: Multi-card statements with column 6 marks
- `valid_star_comment.f90`: Star comments in column 1
- `valid_module.f90`: Module syntax in strict fixed form
- `invalid_alpha_label.f90`: Non-numeric label (rejected)
- `invalid_continuation_with_label.f90`: Continuation with label (rejected)
- `invalid_label_out_of_range.f90`: Invalid label format (rejected)

**Test Suite** in `tests/Fortran90/test_strict_fixed_form.py`:
- 24 test cases covering card parsing, validation, and lenient conversion
- Comprehensive fixture testing for both valid and invalid cases

The strict mode preprocessor enables historical audits requiring exact
card-image conformance while keeping the lenient grammar parsing mode
available for modern Fortran 90 tooling. Issue #673 implements this feature
to achieve full ISO/IEC 1539:1991 Section 3.3 compliance for Fortran 90
fixed-form source validation.

## 4. Fortran 2003 Fixed-form (Unified Grammar)

Fortran 2003 inherits the unified fixed/free architecture:

- `Fortran2003Lexer.g4` imports `Fortran95Lexer.g4` and adds:
  - F2003 object‑oriented, C interoperability, IEEE arithmetic tokens.
  - Fixed-form comment tokens tuned for the representative test cases.
- `docs/fortran_2003_audit.md` documents the current status:
  - Representative fixed-form F2003 code paths are implemented and
    tested (`tests/Fortran2003/test_fortran_2003_comprehensive.py`,
    `tests/Fortran2003/test_issue72_fixed_form_f2003.py`).
  - **Strict historical column semantics for fixed-form source are
    explicitly listed as “Intentionally out of scope or not yet
    implemented” and remain under umbrella Issue #91.**

In practice this means:

- The F2003 grammar supports fixed-form **layouts** that look like
  traditional `.f` code (labels, uppercase keywords, C‑style comments),
  but it does **not** attempt to enforce every 80‑column rule from the
  ISO text.
- Tests assert that modern F2003 features (OOP, CLASS(*), BIND(C),
  PDTs) work correctly when written in typical fixed-form style, while
  more exotic column‑sensitive behaviours are intentionally left out.

## 5. Out-of-scope Fixed-form Features (Tracked by Issue #91)

The following behaviours are **not currently implemented** and are
considered out of scope for the subset implemented in this repository:

- Full 80‑column line semantics per historical standards, including:
  - Enforcing statement labels strictly in columns 1–5.
  - Treating any non‑blank in column 6 as a continuation mark.
  - Distinguishing statement text (7–72) from sequence numbers (73–80).
- Vendor‑specific or dialect‑specific fixed-form extensions not covered
  by the tests in `tests/` (for example, non‑standard comment markers).
- Card‑image reconstruction, listing‑file reproduction and other
  presentation‑level behaviours that depend on physical column widths.

Future work on these features should:

- Reference **Issue #91** in the issue body and tests.
- Include **spec‑grounded test cases** (preferably real historical
  programs) that demonstrate both:
  - The intended behaviour under strict column rules.
  - The current behaviour of this grammar (to avoid regressions).

This document, together with the existing audit in
`docs/fortran_2003_audit.md` and the fixture expectations in
`tests/test_fixture_parsing.py`, forms the explicit specification of the
fixed-form subset implemented today.

