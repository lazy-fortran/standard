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

**What is allowed at the top level.**

In a `.lf` file, anything that would normally live inside a program or
module body can appear directly at the top level:

- Executable statements such as assignments, procedure calls, control
  constructs, and `print` statements.
- Specification constructs such as `use`, `import`, `implicit`, and
  the full Fortran 2018 `declaration_construct` family (type
  declarations, `parameter`, `interface`, `procedure`, etc.).
- Internal procedures – `function` and `subroutine` definitions – that
  do not require a preceding `contains` keyword.

These constructs can be interleaved: you can declare names, execute
statements, and then introduce helper procedures without changing the
file structure.

**When an implicit program or module is assumed.**

Lazy Fortran distinguishes between “script” and “library” `.lf` files
based on the presence of executable statements at the top level:

- If there is at least one top‑level executable statement (not inside a
  procedure), the file behaves as an implicit main program. The toolchain
  wraps the file in a synthetic `program` whose name is derived from the
  filename, and all top‑level declarations and procedures belong to that
  program.
- If there are no top‑level executable statements but there are
  declarations and/or procedures, the file behaves as an implicit
  module. The toolchain wraps the file in a synthetic `module` with a
  name derived from the filename, and top‑level procedures are treated as
  module procedures.

This classification is purely a front‑end convenience: the generated
symbol tables and linkage follow normal Fortran rules. The exact naming
scheme for the synthetic program/module remains PROVISIONAL and is
tracked alongside Issue #52.

**Mixing lazy and traditional Fortran units.**

Lazy Fortran is designed for gradual adoption within existing Fortran
projects:

- Traditional `.f90+` source files continue to compile using the strict
  Fortran 2023 entry point; they may `use` modules that originate from
  implicit‑module `.lf` files as long as the build system compiles those
  `.lf` files first.
- Script‑style `.lf` files (those with top‑level executable statements)
  are intended to be entry points, not reusable modules. They can call
  into both traditional modules and implicit‑module `.lf` libraries via
  ordinary `use` statements.
- Build tools are expected to decide, per `.lf` file, whether to treat
  it as a program or module according to the rules above, and to produce
  normal Fortran objects that can be linked by existing toolchains.

From a user point of view, this means:

- You can start by adding a single `.lf` script next to existing
  `.f90` code and calling into your existing modules.
- Over time, you can migrate helper scripts into `.lf` library files
  (no top‑level executable statements) that behave like conventional
  modules while keeping the zero‑boilerplate authoring experience.

---

### 1.2 LF‑SYN‑02 – Type Inference & Implicit Rules (Issue #53) – *DECIDED (CORE MODEL)*

**Summary.**  
Lazy Fortran infers types from usage in `.lf` files (assignments,
literals, expressions and calls) while keeping standard Fortran numeric
promotion rules. Explicit declarations remain required at module
boundaries and under `implicit none`. Inference is always **local and
predictable**: it never changes the meaning of a program that would be
valid Fortran 2018 without Lazy Fortran.

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

**Numeric kinds and legacy specifiers.**

- `real(4)` / `real(8)` mean 4‑byte and 8‑byte reals respectively.
- `integer(4)` / `integer(8)` follow the same convention.
- `complex(k)` has two `real(k)` components.
- Legacy specifiers map onto this model in a predictable way:
  - `double precision => real(8)`
  - `double complex => complex(8)`

**Expression typing and promotions.**

- Type promotion in expressions follows **standard Fortran rules**:
  - integer division stays integer,
  - complex dominates real,
  - real dominates integer at the highest participating kind.
- Lazy inference never introduces new, non‑standard promotions; it only
  infers types that are consistent with the ISO Fortran rules.

**Where inference is encouraged vs. explicit types.**

- In implicit‑program `.lf` scripts, inference is encouraged for local
  temporaries and scratch variables so that short examples like
  `x = 3` and `y = x + 2.5` “just work”.
- At module boundaries (public procedures, exported types, and
  module‑wide state), explicit type declarations remain the norm to keep
  APIs stable and self‑documenting.
- When the compiler cannot infer a type unambiguously from usage, it
  must issue a clear error that points at the relevant assignment or
  declaration site rather than guessing.

---

### 1.3 LF‑SYN‑03 – World‑Wide Automatic Specializations (Issue #51) – *DECIDED (WORLD MODEL)*

**Summary.**  
Starting from an entry point, the Lazy Fortran toolchain treats the
reachable graph of `.lf` files and packages as one **world** and
generates specializations only for the concrete combinations of
argument types that actually occur.

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

**World definition.**

- A world is defined by an entry program or package plus the transitive
  `use`‑reachable graph of `.lf` libraries and traditional Fortran
  modules. It is **entry‑point local**, not “every module on the
  machine”.
- For a given world, the set of reachable source files and versions is
  fixed for the duration of compilation; rerunning compilation with the
  same inputs produces the same set of specializations.

**Specialization model.**

- Within a world, each generic Lazy Fortran procedure body (such as
  `dot` above) is conceptually parameterized by the types, kinds and
  ranks of its dummy arguments.
- When a call like `dot(x, y)` is encountered, the compiler looks at
  the argument type pattern in that world:
  - If there is already a specialization for that pattern, it is reused.
  - Otherwise a new specialization for that pattern is generated.
- Specializations are shared per type pattern across the world, not per
  call site: all `dot(real(8), real(8))` calls in the same world use
  the same specialization.

**Interaction with user-written procedures and ISO Fortran.**

- Generic resolution at each call site remains **STANDARD-COMPLIANT**
  with ISO/IEC 1539-1:2018, using the rules for generic interfaces in
  Subclause 15.4.3.4 (and 15.4.3.4.5 for unambiguous resolution).
- Within that model, explicit, user-written specific procedures always
  win over generated specializations for the same generic identifier:
  they are considered before any Lazy Fortran generated variants.
- Among all applicable candidates (explicit and generated), the most
  specific candidate is chosen; if no unique most specific candidate
  exists, the call is rejected with a compile-time ambiguity error.
- Generated specializations are visible only as additional specific
  procedures under an existing generic; they must not change behavior
  that would be valid Fortran 2018 in the absence of Lazy Fortran.

**Errors and implicit typing.**

- If no candidate (explicit or generated) can handle a call, the
  compiler reports a compile-time error rather than silently choosing a
  “best guess”.
- Lazy Fortran does not reintroduce letter-based implicit typing for
  world‑wide specializations: type inference rules from LF‑SYN‑02 apply
  unchanged, and all generated entities must satisfy the type and kind
  rules of ISO/IEC 1539-1:2018.

**Caching and reproducibility.**

- Specializations are cached per world in the same system‑wide store
  described in LF‑PKG‑01.
- Cache hits are an implementation detail only: the set of visible
  specializations for a given world is determined entirely by the
  source graph and ISO‑compliant generic resolution rules above, not by
  previous runs.

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

#### Session model and mapping to files

- Each REPL session behaves as a single implicit `.lf` file whose
  contents grow over time; conceptually, the toolchain maintains an
  in‑memory `session.lf` module that is updated as you execute cells.
- At any time, exporting the session to a real `session.lf` file and
  compiling it with the Lazy Fortran toolchain must produce the same
  observable behavior (up to I/O ordering) as the interactive run.
- Notebook cells correspond to contiguous chunks in this implicit file,
  ordered by execution order, not by their visual position in the UI.

#### State, scope, and persistence

- Top‑level variables, procedures, and types defined in a cell remain
  available to later cells in the same session, just as if they had been
  defined earlier in a `.lf` file.
- Re‑executing a cell re‑runs its executable statements and updates
  definitions in the implicit module; subsequent calls and uses see the
  most recent definition.
- Names follow the same scoping rules as in a single module or program:
  there is one global scope per session, with blocks and internal
  procedures introducing nested scopes exactly as in files.

#### Types, implicit rules, and inference

- The REPL uses the same default typing rules as `.lf` files:
  in the absence of an `implicit` statement, undeclared names are
  allowed and participate in type inference.
- An `implicit none` statement in an early cell affects the whole
  session and forbids new undeclared names in later cells, matching the
  behavior of a file where `implicit none` appears near the top.
- When a session is exported to a file, the resulting `session.lf` must
  type‑check under the Lazy Fortran rules without adding extra
  declarations.

#### Redefinitions and errors

- Redeclaring a variable or procedure name in a later cell replaces the
  previous definition for future cells while leaving the behavior of
  already‑run cells unchanged (similar to re‑executing cells in a
  notebook).
- The environment should surface clear diagnostics when a redefinition
  would violate standard Fortran rules at file granularity (for example,
  incompatible interfaces for the same procedure name), and suggest
  moving conflicting definitions into separate modules.
- Runtime errors raised while executing a cell do not roll back
  successful prior definitions in that cell; partially executed cells
  follow the same semantics as partially executed code blocks in a
  program.

#### Integration with traditional Fortran

- `use` statements in the REPL behave exactly as in a `.lf` file:
  sessions can import both traditional modules and implicit‑module
  `.lf` libraries compiled on disk.
- Sessions that contain top‑level executable statements are treated as
  implicit programs when exported; sessions that only define procedures
  and types are exported as implicit modules, using the same rules as
  `.lf` files.
- Tooling should make it easy to promote a stable REPL session into a
  checked‑in `.lf` file, encouraging users to migrate exploratory code
  into version‑controlled modules once it matures.

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
| LF‑SYN‑03    | World‑wide automatic specializations      | DECIDED (world)| #51  |
| LF‑SYN‑04    | Trait‑like contracts                      | OPEN         |  #57  |
| LF‑TOOL‑01   | Interactive REPL & notebooks              | PROVISIONAL  |  #54  |
| LF‑TOOL‑02   | Canonical formatter and linter            | PROVISIONAL  |  #56  |
| LF‑PKG‑01    | Package management & system‑wide cache    | PROVISIONAL  |  #55  |

This table is the primary “at a glance” view for busy readers; details
and changes should be coordinated with the corresponding GitHub issues.
