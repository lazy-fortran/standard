# LFortran Synthesis

**Status:** Draft normative specification for the Lazy Fortran ecosystem
**Proposal:** [#756](https://github.com/lazy-fortran/standard/issues/756)
**Derived from:** [LFortran Standard](lfortran-standard.md) (F2028 base)
**Frontend flag:** `--synthesis` (FortFront #2976)

## Overview

LFortran Synthesis is a minimal, Fortranic language extension for scientific
synthesis: mathematical definitions become the source of truth; derivable
formulas and numerical kernels are generated; correctness obligations are
discharged statically where possible; tests remain for unresolved and external
behavior.

This is **not** a general CAS or proof engine. Synthesis preserves the best
properties of Fortran: explicit declarations, modules, procedures, arrays,
purity, predictable evaluation, and lowering to standard Fortran.

### Scope Boundary

This specification is normative for the language surface, its static and
dynamic semantics, the symbolic/machine-number distinction, the obligation and
evidence model, and the lowering/source-map contract. Theorem proving,
simplification, and code generation are performed by the separate `fortsym`
engine; this document specifies *what* must hold, not *how* those engines are
implemented.

Repository status source of truth:
- [README: Implementation Status](../README.md#implementation-status)
- [Implementation Notes: Grammar Status](implementation-notes.md#grammar-status)

### Feature Staging

Synthesis is staged so each phase can land independently:

| Stage | Name | Contents |
|-------|------|----------|
| **1** | Symbolic synthesis | `symbolic`, `derive`, `generate`, `assume` |
| **2** | Contracts | `requires`, `ensures`, `invariant`, `prove`, `specification`/`implementation` |
| **3** | Floating-point guarantees | dedicated floating-point analysis (later, separate proposal) |

Stages 1 and 2 are the normative scope of this document. Stage 3 is tracked
separately and reserved.

---

## Compiler Modes

| Mode | Flag | Synthesis constructs | Standard-Fortran lowering |
|------|------|----------------------|---------------------------|
| **Strict** | `--std=lf` | rejected (error) | not applicable |
| **Synthesis** | `--synthesis` | accepted | emitted to standard Fortran |
| **Preprocessed** | `--synthesis --emit-fortran` | accepted | output only; input not compiled |

`--synthesis` implies the LFortran Standard defaults (implicit none, 8-byte
real, intent(in) default). The preprocessed output must compile under any
ISO Fortran 2023 compiler.

---

## Normative Grammar

Synthesis is an overlay on the LFortran grammar. The following productions
extend the LFortran parser. Keywords are reserved in Synthesis mode.

### Lexical additions

```
SYMBOLIC      : 'symbolic'
ASSUME        : 'assume'
DERIVE        : 'derive'
PROVE         : 'prove'
REQUIRES      : 'requires'
ENSURES       : 'ensures'
INVARIANT     : 'invariant'
SPECIFICATION : 'specification'
IMPLEMENTATION: 'implementation'
GENERATE      : 'generate'
SYNTHESIS_KW  : 'synthesis'
MODEL_KW      : 'model'
PROVED_KW     : 'PROVED'
DISPROVED_KW  : 'DISPROVED'
UNKNOWN_KW    : 'UNKNOWN'
```

`END MODEL`, `END SYNTHESIS MODEL`, `END SPECIFICATION`, and
`END IMPLEMENTATION` are recognized as keyword phrases
(`END_MODEL_STMT : END SYNTHESIS_KW? MODEL_KW`).

Keywords follow the ordinary free-form lexical rules: they are case
insensitive, and comments and continuation lines are allowed wherever the
productions show `NEWLINE`. An optional trailing name, as on
`end synthesis model name`, must match the opening name by ordinary Fortran
name comparison. Names keep their source spelling for diagnostics and use
case-folded identity for lookup.

### Program units

A Synthesis `model` is a new program unit, analogous to a `module`. The
optional `synthesis` prefix is the spelling used throughout this document
(`synthesis model name` ... `end synthesis model`), and the shorter
`model name` ... `end model` is equivalent:

```
program_unit
    : ... existing program units ...
    | synthesis_model
    ;

synthesis_model
    : SYNTHESIS_KW? MODEL_KW identifier NEWLINE*
      model_part*
      END_MODEL_STMT identifier? NEWLINE*
    ;
```

### Model body statements

```
model_part
    : symbolic_decl
    | assumption_stmt
    | derivation_stmt
    | proof_stmt
    | generation_stmt
    | executable_construct        // ordinary Fortran inside the model body
    ;

symbolic_decl
    : SYMBOLIC type_spec '::' symbol_attr_list NEWLINE*
    ;

symbol_attr_list
    : symbol_entity (',' symbol_entity)*
    ;

symbol_entity
    : identifier ('(' array_spec ')')?     // arrays keep shape and extent
    ;

assumption_stmt
    : ASSUME expr NEWLINE*
    ;

derivation_stmt
    : DERIVE identifier '=' expr NEWLINE*
    ;

proof_stmt
    : PROVE expr NEWLINE*
    ;

generation_stmt
    : GENERATE identifier FROM '[' identifier_list ']' NEWLINE*
    ;
```

### Procedure contracts

Contracts attach to ordinary Fortran procedures inside a `contains` section of
any module/program unit in Synthesis mode:

```
contract_stmt
    : requires_clause
    | ensures_clause
    | invariant_clause
    | proof_stmt
    ;

requires_clause : REQUIRES expr NEWLINE* ;
ensures_clause  : ENSURES  expr NEWLINE* ;
invariant_clause: INVARIANT expr NEWLINE* ;
```

A `contract_stmt` is accepted wherever the base grammar accepts a
`declaration_construct` or an `executable_construct` inside a procedure body,
including the body of a `do` construct (for `invariant`). `procedure_spec` is
the ordinary procedure header (prefixes, `function`/`subroutine`, name,
dummy arguments, `result`) of the base grammar.

### Specification / implementation pairs

A contracted procedure may be split into a `specification` interface and an
`implementation`:

```
specification_construct
    : SPECIFICATION procedure_spec NEWLINE* (contract_stmt)* END_SPECIFICATION_STMT identifier?
    ;

implementation_construct
    : IMPLEMENTATION procedure_spec NEWLINE* (contract_stmt)*
      (declaration_construct | executable_construct)*
      END_IMPLEMENTATION_STMT identifier?
    ;
```

A `specification` declares only the interface and its contracts. An
`implementation` may satisfy it directly or via `derive`/`generate`.

---

## Symbols and Declarations

### `symbolic` declarations

`symbolic` declares a name whose value is an exact symbolic expression rather
than an IEEE machine number:

```fortran
symbolic real :: q, p, m
symbolic real(dp) :: q, p, m
```

Rules:

- A `symbolic` declaration introduces a name in the current model scope.
- The declared type gives the *static type* used after lowering (the type of
  the generated Fortran variable). The *symbolic* qualifier controls the
  exact-real semantics during derivation.
- `symbolic` names are read-only during derivation except through `derive`
  assignments.
- A `symbolic` name may not be declared with `allocatable` or `pointer`
  attributes. It may be an array; array shape and extent semantics are
  preserved.

### Symbolic vs machine-number semantics

Synthesis maintains two disjoint numeric semantics:

| Aspect | Symbolic (exact) | Machine number (IEEE) |
|--------|------------------|-----------------------|
| Values | exact real/rational expressions | binary floating point |
| `+`, `*`, `/` | exact algebra | rounded per IEEE-754 |
| `==` | symbolic equality after normalization | exact bit comparison |
| Associativity | held (exact) | not guaranteed (rounding) |
| Division by zero | excluded by assumptions | IEEE inf/nan possible |
| Proof target | yes | no (analysis is stage 3) |

Within a `symbolic` expression, operators denote exact operations. A symbolic
expression is never silently reinterpreted as an IEEE value; every
`symbolic -> machine` boundary is an explicit `generate` step or a
`specification`/`implementation` hand-off. This is the single source of
symbolic/machine distinction and prevents a proof over exact reals from being
silently applied to a rounded computation.

Decimal literals in a symbolic expression denote exact rationals including
their exponent, independently of the default real kind, so
`0.1 + 0.2 == 0.3` holds in a symbolic proof. That statement says nothing
about the corresponding IEEE comparison.

### Exact expressions and predicates

An exact expression is built from symbolic names, integer and decimal real
literals, parentheses, unary signs, `+`, `-`, `*`, `/`, and integer-literal
powers. A predicate is a scalar comparison (`==`, `/=`, `<`, `<=`, `>`,
`>=`) or a combination of them with `.not.`, `.and.`, and `.or.`. Ordinary
Fortran lexical rules and operator precedence apply to both. In stage 1 a
`derive` expression may call only `diff`; other procedure calls, kind
suffixes in exact expressions, and complex values are errors.

### Name binding and scope

- Every `symbolic` name is declared once in a model.
- A name without a definition is an input. A defined or derived name has
  exactly one definition and is not an input.
- Definitions and derivations refer only to declared inputs and to names
  defined earlier in the model; self-reference and forward reference are
  errors. Input classification uses the complete model, so a name defined
  later is not an input.
- The target of `derive` must be a `symbolic` name of the model.
- `generate` kernel names are unique within the model.

---

## Assumptions and Domains

### Scoped assumptions

```
assume(m > 0)
assume(size(x) > 0)
assume(norm2(x) > 0)
```

- An `assume` binds an assumption to the current model/contract scope and all
  nested scopes.
- Assumptions are checked for well-formedness (must be a boolean expression
  over in-scope names and intrinsic predicates).
- Assumptions are used by the proof engine as hypotheses.
- An assumption does **not** emit a runtime check by default. Use
  `requires` in a contract for runtime-checked preconditions.
- Inner scopes may add assumptions; they may not remove an outer assumption.
- The assumption set of a scope carries its own consistency obligation. A
  `DISPROVED` set rejects the model, an `UNKNOWN` set is recorded and leaves
  every proof over it explicitly conditional.
- No assumption and no discharged obligation establishes that the assumptions
  hold for a caller's arguments. A definition cannot change an assumption or
  discharge its own prerequisite.

### Domains

A `symbolic` name carries an optional domain, declared with the symbolic
declaration or established by `assume`. Typical domains: real, nonzero real,
positive real, closed interval `[a,b]`, integer. Domain information is part of
the assumption context and is propagated to generated proof obligations.

Division requires a nonzero denominator and a negative power requires a
nonzero base. Each such requirement is a domain obligation under the active
assumptions, and an expression becomes usable for `generate` only once its
domain obligations are discharged. Differentiation is mathematical
differentiation on that domain; it does not differentiate a sequence of
floating-point operations.

---

## Derivation

### `derive`

```
derive qdot = diff(h, p)
derive pdot = -diff(h, q)
```

- `derive <name> = <expr>` defines `<name>` as the symbolic result of
  simplifying `<expr>`.
- `<expr>` is an exact symbolic expression. `diff(f, x)` denotes the symbolic
  partial derivative of `f` with respect to `x`.
- Derivation is deterministic: identical input expressions produce identical
  symbolic results and identical generated code (see [Regeneration and
  provenance](#regeneration-and-provenance-hashes)).
- A derived name may be used in later `derive`, `prove`, and `generate`
  statements.

### Supported differentiation rules

The initial symbolic engine supports the standard elementary rules: linearity,
product rule, quotient rule, chain rule, and derivatives of polynomials,
rational functions, and the intrinsic elementary functions (`exp`, `log`,
`sin`, `cos`, `tan`, `sqrt`, `abs` on positive domains, and powers). The
normative set is defined by `fortsym`; any rule added there is a normative
extension and must be listed in the provenance hash.

---

## Proof Obligations

### `prove`

```
prove diff(qdot, q) + diff(pdot, p) == 0
```

A `prove` statement introduces a proof obligation. The obligation is a
boolean symbolic proposition.

### Three-valued proof status

Every obligation resolves to exactly one of three statuses:

| Status | Meaning | Action |
|--------|---------|--------|
| `PROVED` | the proposition is entailed by the current assumptions and proved by an accepted evidence source | obligation discharged |
| `DISPROVED` | a counterexample or refutation was found under the assumptions | compile error by default |
| `UNKNOWN` | neither proved nor disproved within budget | policy applies (see below) |

Status is per-obligation and stored with the evidence. A backend that does not
support the obligation, a missing executable, a timeout, or a certificate that
was not checked all produce `UNKNOWN` together with a reason. `DISPROVED`
requires a checked counterexample or a checked negation.

### Evidence provenance

Every discharged or refuted obligation records evidence:

- **Evidence kind:** symbolic normalization, Why3/SMT, Lean artifact, interval
  proof, numerical probing, or axiom/assumption.
- **Engine and version:** e.g. `fortsym 0.3.1`, `z3 4.12`, `lean 4`.
- **Inputs:** the proposition, the active assumption set, and the seed/probe
  bounds.
- **Result hash:** a cryptographic hash over the proposition, assumptions, and
  engine version.
- **Trace/artifact reference:** a pointer to the stored certificate when the
  engine produces one.

The evidence is emitted alongside the lowered Fortran (see
[Lowering and source maps](#lowering-and-source-map-contract)). A numerical
probe or a passing test is recorded as evidence while the status of the
obligation remains `UNKNOWN`, so it cannot satisfy a policy that requires a
proof.

### Policy for unresolved obligations

For each `UNKNOWN` obligation the user selects a policy:

| Policy | Behavior |
|--------|----------|
| `error` | compilation fails; the obligation is reported |
| `assert` | a runtime assertion is emitted at the lowered location |
| `test` | a property/unit test is generated and registered for the test harness |

The default policy is `error`. Policy selection is a per-model or per-file
directive:

```
synthesis model ...
    assume(m > 0)
    prove diff(qdot, q) + diff(pdot, p) == 0   ! policy: test
```

A `DISPROVED` obligation always fails verification, and an `UNKNOWN` required
obligation fails before code generation. A non-required `UNKNOWN` obligation
proceeds only where the policy explicitly permits its obligation class, and
then the selected `assert` or `test` action is additional required work; it
does not turn the obligation into a proof. An unresolved domain obligation
cannot be bypassed by a runtime guard, because the expression is not generated
until its domain obligations are discharged.

---

## Generation

### `generate`

```
generate rhs from [qdot, pdot]
```

- `generate <name> from [<name>, ...]` emits a generated numerical kernel.
- The generated kernel is standard Fortran; its interface and types are
  derived from the symbolic declarations and the `generate` statement.
- The kernel body is the lowered form of the listed symbolic expressions.
- A `generate` name becomes a callable procedure in the enclosing scope after
  lowering.
- The generated kernel carries a provenance hash linking it to the source
  `derive`/`generate` statements and the symbolic engine version.

Example:

```fortran
synthesis model harmonic
    symbolic real :: q, p, m, k
    assume(m > 0)

    derive h = p**2/(2*m) + k*q**2/2
    derive qdot = diff(h, p)
    derive pdot = -diff(h, q)

    prove diff(qdot, q) + diff(pdot, p) == 0
    generate rhs from [qdot, pdot]
end synthesis model
```

`generate` lowers to a module function, `pure` unless it carries mandatory
runtime guards, since a pure function may not execute `error stop`. `prove`
and `generate` are compile-time-only; they do not survive into the emitted
standard Fortran except as the corresponding kernel and (per policy) runtime
assertions or tests.

### Generated kernel interface

Each model emits one module named `<model>_synthesis`, and each `generate`
statement emits a function with the requested name in the first numerical
profile:

- The dummy arguments are exactly the inputs referenced by the listed
  expressions, by their transitive definitions, by their domain obligations,
  and by all active assumptions, in declaration order. Each is scalar
  `real(real64), intent(in)`.
- The result is `real(real64)` with explicit extent equal to the number of
  listed expressions.
- The module imports `real64` from `iso_fortran_env` and uses `implicit
  none`. It requires an IEEE binary64 target (`radix == 2`, `digits == 53`,
  and IEEE datatype support); an unsupported target is diagnosed.
- A collision with an existing module or procedure name is diagnosed before
  any artifact is published.

```fortran
module oscillator_synthesis
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    private
    public :: rhs
contains
    function rhs(q, p, m) result(values)
        real(real64), intent(in) :: q, p, m
        real(real64) :: values(2)
        if (.not. ieee_is_finite(q)) error stop 'oscillator.input.q'
        if (.not. ieee_is_finite(p)) error stop 'oscillator.input.p'
        if (.not. ieee_is_finite(m)) error stop 'oscillator.input.m'
        if (.not. (m > 0.0_real64)) error stop 'oscillator.assume.1'
        values(1) = p / m
        values(2) = -q
    end function rhs
end module oscillator_synthesis
```

### Guards and numerical fidelity

- The kernel checks at entry that every input is finite and that each active
  assumption holds. A violation emits a diagnostic naming the originating
  assumption or domain obligation and then executes `error stop 1`; the
  handwritten twin uses character `error stop` codes for the same required
  termination and diagnostic behavior.
- Each domain guard runs immediately before its guarded operation and tests
  the rounded value actually used. An exact proof that `m*m` is nonzero does
  not remove the check on `m*m`, because multiplication can underflow when
  `m` is a positive finite machine number, and the checked temporary is the
  one used in the operation.
- Expressions preserve source evaluation order, expanded through referenced
  definitions, and every numeric literal is written with an explicit `real64`
  kind. Derivatives are emitted in a deterministic backend-defined form whose
  backend identity and options are part of the provenance.
- An exact identity never licenses reassociation or contraction of the
  emitted floating-point expression. The generated source establishes the
  chosen numerical implementation, and floating-point error, overflow and
  underflow guarantees require further obligations (stage 3).
- No symbolic value or proof object appears in the runtime interface of the
  emitted module.

---

## Contracts

Contracts are ordinary-looking Fortran attached to procedures.

### `requires` / `ensures`

```fortran
pure function normalize(x) result(y)
    real(dp), intent(in) :: x(:)
    real(dp) :: y(size(x))

    requires size(x) > 0
    requires norm2(x) > 0
    ensures all(shape(y) == shape(x))

    y = x / norm2(x)
end function
```

- `requires <expr>` is a precondition. It is a runtime-checked assertion in the
  lowered output and a hypothesis for proof.
- `ensures <expr>` is a postcondition. It is a runtime-checked assertion at
  each return point and the goal for proof.
- Expressions in contracts are scalar boolean expressions; they may not have
  side effects. An expression that yields an array of logical values, such as
  `shape(y) == shape(x)`, must be reduced with `all(...)` to form a scalar
  contract.

### `invariant`

```
do i = 1, n
    invariant s == sum(a(1:i))
    s = s + a(i)
end do
```

- `invariant <expr>` inside a loop is an inductive invariant: it is checked at
  loop entry (implied by the precondition or previous iteration) and
  preserved by each iteration.
- Invariants are hypotheses for proving loop properties and are emitted as
  runtime assertions under the `assert` policy.

### `specification` / `implementation`

A `specification` declares a contract without a body; an `implementation`
provides the body and must establish the specification's contracts:

```fortran
specification pure function norm2n(x) result(y)
    real(dp), intent(in) :: x(:)
    real(dp) :: y
    requires size(x) > 0
    ensures y > 0
end specification norm2n

implementation pure function norm2n(x) result(y)
    real(dp), intent(in) :: x(:)
    real(dp) :: y
    y = sqrt(sum(x*x))
end implementation norm2n
```

Rules:

- A `specification` declares only the interface and contracts.
- An `implementation` with the same signature must satisfy the
  specification's `requires`/`ensures`.
- Satisfying an implementation may itself be a proof obligation, an `assert`,
  or a `test`, per policy.
- Multiple implementations of one specification are not allowed in the initial
  design (stage 2). A single implementation is normative.

---

## Interaction with Fortran Features

| Feature | Behavior |
|---------|----------|
| **Purity** | `derive`/`prove`/`generate` are pure; `symbolic` names are read-only. Contract expressions must be pure. A `specification` of a pure function yields a pure implementation, unless the kernel carries a mandatory runtime guard, which makes it impure. |
| **Elemental** | Elemental procedures may carry contracts; contract expressions are elementally applied to each element. |
| **Generics** | A Synthesis model may be parameterized by the same deferred type mechanism as LFortran templates; symbolic operations require a numeric deferred type. |
| **Arrays** | `symbolic` arrays are exact elementwise; `diff` acts elementwise on arrays. `shape`/`size` are available in contracts and proofs. |
| **Derived types** | A `symbolic` declaration may use a derived type with arithmetic operators defined; the symbolic engine treats the operators by their mathematical meaning. |
| **Modules** | Models, generated kernels, and contracted procedures live in modules and follow normal module scoping and `use` rules. |

---

## Lowering and Source-Map Contract

### Lowering target

Synthesis source is preprocessed/lowered to:

1. **Standard Fortran** — the executable/procedure kernel body with contracts
   lowered to runtime assertions (per policy) and symbolic constructs erased.
2. **Generated modules** — the emitted `generate` kernels and, if used, the
   `specification`/`implementation` bindings.
3. **Proof obligations** — the list of obligations with their status and
   evidence.
4. **Certificates** — stored evidence artifacts (SMT/Lean/interval proofs).
5. **Source maps** — a mapping from generated Fortran ranges and obligation
   ranges back to original Synthesis source positions.

### Determinism and provenance hashes

- Lowering is deterministic: identical source and engine versions yield
  byte-identical output and identical hashes.
- Each generated artifact carries a provenance hash over the originating
  source span, the symbolic engine version, the assumption context, and the
  evidence kind/version.
- Regeneration with a changed engine version is permitted only if the resulting
  behavior is equivalent; the hash must change to record the regeneration.
- Regeneration from identical semantic inputs, frontend and backend versions and
  options, numeric target and policy produces identical source and manifest
  bytes, and timestamps and absolute installation paths stay out of semantic
  provenance.
- A stale certificate or an unsupported schema version is an error and never a
  cache hit.

### Source maps

Source locations use workspace-relative UTF-8 paths and half-open byte spans
into the exact source whose SHA256 is recorded. Every generated executable
statement maps back to its source expression, assumption or domain obligation,
and a synthetic declaration maps to the `generate` statement that produced it.
Each diagnostic carries the original span, the obligation identifier, the
status, and the backend failure reason when one applies.

### Obligation hashes and invalidation

Manifests and source maps serialize as UTF-8 JSON with sorted keys, compact
separators and one trailing newline, while arrays keep their semantic order,
and a hash is the SHA256 of those exact bytes. A hash reference never includes
the hash of the artifact that contains it. The semantic hash of an obligation
covers its typed expression, its assumptions, its dependency hashes, the
contract revision, and the checker semantics. For a generated-kernel
equivalence claim it additionally covers the generated-source and interface
hashes, the numerical target, and the generation tool and options. Source
locations belong to the containing manifest and do not enter that hash, so
moving an obligation reuses its checked proof while its source mapping is
regenerated. Consumers also check unique obligation identifiers, acyclic
existing dependencies, byte spans inside their hashed files, and source-map
coverage; a JSON Schema alone establishes none of those relationships.

Changed assumptions invalidate every dependent obligation and every generated
procedure. Changing unrelated implementation code does not invalidate a
mathematical claim whose semantic inputs are unchanged, but it does invalidate
that implementation's equivalence certificate.

### Artifact bundles and schemas

An artifact bundle is a content-addressed object directory: every bare hash of
a policy, assumption set, option set, interface or semantic claim resolves to a
file named by that SHA256 whose exact bytes are checked, and those payloads use
the canonical JSON rules above. A checker executable hash identifies the
installed accepted checker and is measured from its executable bytes; it does
not authorize downloading or running a bundled program. A certificate explicitly
binds the claim's semantic hash, assumption hash, dependency hashes and checker
semantics, so a successful checker run without that binding does not establish
the claim.

The draft formats are [manifest-v1.schema.json](synthesis/manifest-v1.schema.json)
and [source-map-v1.schema.json](synthesis/source-map-v1.schema.json), under the
contract revision `synthesis-draft-1`. A consumer validates the schema version,
every referenced hash, checker compatibility and claim dependencies before it
trusts a status.

### Compile-time-only constructs

The following constructs are compile-time-only and do not survive into the
emitted standard Fortran:

- `symbolic` qualifiers (the declared variable type survives, the exact
  semantics do not)
- `assume` statements
- `derive` statements
- `prove` statements (except per-policy `assert`/`test` artifacts)
- `generate` statements (the generated kernel body survives; the statement
  itself does not)
- `requires`/`ensures`/`invariant` (survive only as runtime assertions under
  the `assert` policy)

---

## Compatibility Story During Incubation

During incubation, Synthesis constructs may also be expressed as
directive/comment syntax so that ordinary Fortran compilers ignore them:

```fortran
!$synthesis symbolic real :: q, p, m
!$synthesis assume(m > 0)
!$synthesis derive qdot = diff(h, p)
```

Directive forms are accepted by the Synthesis preprocessor and are
semantically identical to the keyword forms. They allow progressive adoption
in codebases whose toolchains do not yet support the keyword syntax. The
keyword form is normative; the directive form is an incubation-only alias that
must be accepted or rejected uniformly (a file must not mix forms for the same
construct). A directive comment does not itself enforce an obligation: a
compiler that ignores the comment enforces nothing, so enforcement comes from
the Synthesis frontend that reads the file.

---

## Non-goals

- General delayed evaluation (every value remains an eager, typed value).
- Making every Fortran value a symbolic expression.
- A universal CAS or proof engine (Synthesis delegates to `fortsym`, Why3/SMT,
  Lean, interval, or probing).
- Proving arbitrary I/O, MPI, operating-system, or hardware behavior.
- Replacing all testing; `test` policy obligations and ordinary tests remain.

---

## Verification Architecture

The language does not implement its own theorem prover. Initial backends:

- **`fortsym`**: symbolic IR, derivation, simplification, code generation,
  readback, candidate equivalence.
- **Why3/SMT**: routine contracts, loop invariants, bounds, ranges, standard
  verification conditions.
- **Lean**: difficult mathematical theorems and independently checked proof
  artifacts.
- **Numerical probing / interval**: bounded counterexample search and interval
  proofs for `UNKNOWN` obligations.
- **Dedicated floating-point analysis**: later (stage 3), separate proposal.

Backends are pluggable; evidence records which backend discharged each
obligation. Acceptance of a stage names the accepted proof checkers and a
bounded canonicalization algorithm for exact algebra, together with the exact
expression subset, the numerical profile, the mandatory entry guards, and the
evidence and source-map formats.

### Independent oracles

The handwritten [oscillator twin](synthesis/oscillator-twin.f90) fixes the
generated interface and the representative numerical behavior of the harmonic
model with `k = 1`, whose kernel `rhs(q, p, m)` returns `[p/m, -q]`. It is
ordinary Fortran and an independent oracle for a future generator; it is not
evidence that a Synthesis frontend or a proof backend already exists. The draft
artifact checks run with the current installed `fo`:

```sh
uv run --with jsonschema python docs/synthesis/verify_contract.py
```

The checker validates the manifest and source-map schemas with an independent
JSON Schema implementation and builds the twin through `fo` in an isolated
`/var/tmp` fixture, checking three numerical points and the refusal of zero,
negative, NaN and infinite input. It does not parse Synthesis and does not
prove the mathematical claims.

### Conformance fixtures

Positive, invalid, boundary and cross-feature fixtures cover duplicate symbolic
declarations, forward definitions, self-reference, mismatched trailing `end`
names, unsupported kinds, missing and stale certificates, inconsistent
assumptions, zero denominators, nonfinite inputs, and module or procedure name
collisions.

---

## Examples

### Algebra

```fortran
synthesis model quad
    symbolic real :: a, b, c, x
    assume(a /= 0)
    derive disc = b**2 - 4*a*c
    prove disc == b*b - 4*a*c
    generate eval from [disc]
end synthesis model
```

### Differentiation

```fortran
synthesis model harmonic
    symbolic real :: q, p, m, k
    assume(m > 0)
    derive h = p**2/(2*m) + k*q**2/2
    derive qdot = diff(h, p)
    derive pdot = -diff(h, q)
    prove diff(qdot, q) + diff(pdot, p) == 0
    generate rhs from [qdot, pdot]
end synthesis model
```

### Contracts

```fortran
pure function normalize(x) result(y)
    real(dp), intent(in) :: x(:)
    real(dp) :: y(size(x))
    requires size(x) > 0
    requires norm2(x) > 0
    ensures all(shape(y) == shape(x))
    y = x / norm2(x)
end function
```

### Loops

```fortran
subroutine sum_prefix(a, s)
    real(dp), intent(in) :: a(:)
    real(dp), intent(out) :: s
    s = 0.0_dp
    do i = 1, size(a)
        invariant s == sum(a(1:i-1))
        s = s + a(i)
    end do
    ensures s == sum(a)
end subroutine
```

### Generated kernels

```fortran
synthesis model field
    symbolic real :: x, y
    derive fx = x + y
    derive fy = x - y
    generate gradient from [fx, fy]
end synthesis model
```

---

## Related Standards and Issues

- **[LFortran Standard](lfortran-standard.md)** — base dialect
- **[LFortran Infer](lfortran-infer.md)** — inference mode
- **FortFront #2976** — syntax, semantic IR, backend interfaces, standard-Fortran lowering
- **fortsym #48** — symbolic derivation, translation validation, generated kernels, Lean bridge
- **fo #120** — build/proof/generation pipeline, caching, policy, diagnostics, MCP
- **ffc #632** — optional native compiler path after lowering semantics stabilize
- **fortnum #62**, **fortfem #62** — consumer pilots
- **differentiable-fortran #1** — symbolic, AD, hybrid, implicit derivatives

Each owner's first implementation slice needs an oracle that does not depend on
that owner's own frontend:

| Owner | Independent acceptance oracle |
|-------|-------------------------------|
| FortFront #2976 | valid syntax and invalid neighbors; the oscillator interface and twin |
| fortsym #48 | exact rational identities and invalid denominator neighbors |
| ffc #632 | lowered and native interface, numerical agreement, refusal of stale proofs |
| fo #120 | assumption invalidation, reuse of an unchanged claim, `UNKNOWN` for a missing tool |

## References

- [Issue #756](https://github.com/lazy-fortran/standard/issues/756)
- [LFortran Compiler](https://lfortran.org)
- ISO/IEC 1539-1:2023 base standard
