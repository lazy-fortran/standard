# LF‑0001: Lazy Fortran 2025 Feature Overview (WORK IN PROGRESS)

> **WORK IN PROGRESS** – This document sketches the user‑facing design of
> Lazy Fortran 2025. Details and syntax may still change. For discussion,
> see issues #51–#57 and related design notes.

This document is intentionally short and skimmable. It groups planned
features into:

- **Syntax & Semantics** – What the language guarantees in `.lf` files.
- **Tooling** – REPL, formatter, linter.
- **Packages & World** – How code, modules and packages are found and built.

Each feature section has:

- A **summary** and **user story**.
- 1–2 tiny **examples**.
- A **status marker**:  
  - `DECIDED` – Core model is stable.  
  - `PROVISIONAL` – Likely direction, details may change.  
  - `OPEN` – Concept accepted, syntax/semantics under active design.

---

## 1. Syntax & Semantics

### 1.1 LF‑SYN‑01 – Zero‑Boilerplate `.lf` Files (Issue #52) – *PROVISIONAL*

**Summary.**  
`.lf` files are “scripts by default”: you can write top‑level statements
and procedures without `PROGRAM`, `MODULE` or `CONTAINS`. The toolchain
wraps them in an implicit program/module as needed.

**User story.**  
As a user, I want to paste a small numerical experiment into `example.lf`
and just run it, without thinking about program units.

**Example.**

```fortran
! example.lf

print *, "Hello, Lazy Fortran!"

function square(x)
  square = x * x
end function

print *, square(3)   ! 9
```

From the user’s perspective this is the whole program. The compiler
creates an implicit main program around the top‑level statements and
makes `square` visible in that program.

---

### 1.2 LF‑SYN‑02 – Type Inference & Implicit Rules (Issue #53) – *DECIDED (CORE MODEL)*

**Summary.**  
Lazy Fortran infers types from usage in `.lf` files (assignments,
literals, expressions and calls) while keeping standard Fortran numeric
promotion rules. Explicit declarations remain required at module
boundaries and under `implicit none`.

**User story.**  
As a user, I want `x = 3` and `y = x + 2.5` to “just work” with sensible
types, but I still want clear, explicit types in exported interfaces.

**Examples.**

```fortran
! default .lf mode (no implicit statement)
x = 3         ! inferred: integer(4)
y = 2.5       ! inferred: real(8)
z = x + y     ! inferred: real(8), using standard promotion
```

```fortran
! library.lf
implicit none

module procedure norm2(x)
  real(8), intent(in) :: x(:)
  ! body can use inferred temporaries:
  !   s = sum(x * x)
  ! s inferred as real(8)
end procedure norm2
```

Key decisions (see issue #53 for details):

- Kinds represent **bytes per numeric component** (e.g. `real(8)`).
- In plain `.lf` without `implicit`, undeclared names are allowed and
  use inference only; no legacy letter‑based implicit typing.
- With `implicit none`, undeclared names are errors, but inference still
  operates on declared entities.
- With any other `implicit` statement, classic Fortran implicit typing
  takes precedence and lazy inference for undeclared names is disabled.

---

### 1.3 LF‑SYN‑03 – World‑Wide Automatic Specializations (Issue #51) – *PROVISIONAL*

**Summary.**  
Starting from an entry point, the Lazy Fortran toolchain treats the
reachable graph of `.lf` files and packages as one **world** and
generates specializations only for the concrete combinations of argument
types that actually occur.

**User story.**  
As a user, I want generic routines like `dot` or `axpy` to specialize
across my entire codebase and dependencies automatically, without
hand‑written interface blocks or manual instantiation.

**Example.**

```fortran
! math.lf
function dot(x, y)
  dot = sum(x * y)
end function

! main.lf
x = [1.0, 2.0, 3.0]    ! real(8) vector
y = [4.0, 5.0, 6.0]
print *, dot(x, y)
```

The compiler sees `dot` used with `real(8)` arrays and generates the
needed specialization(s). If another part of the world calls `dot`
with `integer(4)` arrays, a matching specialization is generated.

Open design points (tracked in #51):

- How aggressively to specialize (per call‑site vs. per type pattern).
- Caching and invalidation in the world‑wide build graph.

---

### 1.4 LF‑SYN‑04 – Optional Trait‑Like Contracts (Issue #57) – *OPEN*

**Summary.**  
Traits provide optional, lightweight contracts on procedures and types,
aligned in spirit with Traits‑for‑Fortran. They are primarily for
documentation and improved error messages, not a mandatory feature.

**User story.**  
As a user, I want to express “this routine works for any type that
supports addition and scalar multiplication” in a compact way, so that
errors in generic code point to trait violations rather than cryptic
template instantiation failures.

**Example (illustrative only, syntax OPEN).**

```fortran
! PROVISIONAL syntax sketch
trait AdditiveMonoid(T)
  requires:
    T + T  -> T
    zero() -> T
end trait

@AdditiveMonoid(T)
function sum_all(x)
  sum_all = zero()
  do i = 1, size(x)
    sum_all = sum_all + x(i)
  end do
end function
```

Decisions still OPEN:

- Exact trait syntax and annotation mechanism.
- Whether traits remain purely “static hints” or participate in overload
  resolution.

---

## 2. Tooling

### 2.1 LF‑TOOL‑01 – Interactive REPL and Notebooks (Issue #54) – *PROVISIONAL*

**Summary.**  
Lazy Fortran should support an interactive REPL and notebook experience
(e.g. via LFortran) with the **same syntax and inference** as `.lf`
files.

**User story.**  
As a user, I want to try small snippets interactively, keep state
between cells, and later move that code into `.lf` files with minimal
edits.

**Examples.**

```fortran
! REPL session (conceptual)
>>> x = 3
>>> y = 2.5
>>> x + y
  5.5
```

```fortran
! Notebook-style cell
! Cell 1
x = [1.0, 2.0, 3.0]

! Cell 2
v = sum(x)   ! v inferred real(8), same as in a file
```

Design goals:

- REPL cells behave like incremental `.lf` chunks in a shared module.
- Errors and types reported in the same way as in file‑based compilation.

---

### 2.2 LF‑TOOL‑02 – Canonical Formatter and Linter (Issue #56) – *PROVISIONAL*

**Summary.**  
There is a single, canonical Lazy Fortran formatter and linter built on
the LazyFortran2025 grammar, defining a predictable style and a common
set of static checks.

**User story.**  
As a user, I want `lfmt` and `lflint` to be the “obvious” formatter and
linter, so that all tools and editors agree on how Lazy Fortran should
look and what warnings are issued.

**Examples.**

```bash
# Format a file in-place
lfmt main.lf

# Lint a package
lflint ./src
```

```fortran
! before
if(x>0)print*,"ok"

! after lfmt
if (x > 0) then
  print *, "ok"
end if
```

Planned behaviors:

- Formatter is **idempotent** and grammar‑aware.
- Linter focuses on:
  - suspicious implicit typing or shadowing,
  - style issues (layout, naming conventions),
  - opportunities to use newer Lazy Fortran constructs.

---

## 3. Packages & World

### 3.1 LF‑PKG‑01 – Go/Julia‑Style Package Management (Issue #55) – *PROVISIONAL*

**Summary.**  
Lazy Fortran uses a system‑wide cache and a simple module/package
resolution scheme: writing `use foo` is enough for tooling to find,
fetch, build and cache the corresponding package.

**User story.**  
As a user, I want dependencies to “just work”: if a package is
discoverable (e.g. by name and version), the tooling should handle
downloading and building without pip‑style dependency hell.

**Example.**

```fortran
! main.lf
use lf_linear_algebra  ! resolves from registry or local cache

x = [1.0, 2.0, 3.0]
y = [4.0, 5.0, 6.0]
print *, dot(x, y)
```

From the user’s perspective:

- `use lf_linear_algebra` triggers:
  - registry lookup and download (if not cached),
  - build and specialization of that package,
  - caching of build artifacts in a system‑wide store.

Open design points (tracked in #55):

- Exact layout and naming for packages (e.g. `lf-foo` vs. `foo`).
- Version selection policy (lockfiles vs. minimal version constraints).

---

## 4. Status Overview

| ID           | Feature                                   | Status       | Issue |
|--------------|-------------------------------------------|-------------:|:-----:|
| LF‑SYN‑01    | Zero‑boilerplate `.lf` files              | PROVISIONAL  |  #52  |
| LF‑SYN‑02    | Type inference & implicit rules           | DECIDED (core)| #53  |
| LF‑SYN‑03    | World‑wide automatic specializations      | PROVISIONAL  |  #51  |
| LF‑SYN‑04    | Trait‑like contracts                      | OPEN         |  #57  |
| LF‑TOOL‑01   | Interactive REPL & notebooks              | PROVISIONAL  |  #54  |
| LF‑TOOL‑02   | Canonical formatter and linter            | PROVISIONAL  |  #56  |
| LF‑PKG‑01    | Package management & system‑wide cache    | PROVISIONAL  |  #55  |

This table is the primary “at a glance” view for busy readers; details
and changes should be coordinated with the corresponding GitHub issues.

