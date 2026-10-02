# Fortran Synthesis proposal

Status: **draft for acceptance**, contract revision `synthesis-draft-1`.
This document proposes the first contract for
[standard #756](https://github.com/lazy-fortran/standard/issues/756).
It does not authorize grammar changes, expand supported language coverage,
or change ordinary Fortran semantics. Acceptance must identify the reviewed
revision and resolve the decisions listed below before implementation starts.

The reader is a frontend, compiler, or build-driver implementer. Mathematical
definitions often produce several numerical kernels; recording their meaning
once allows generated kernels to share a checkable derivation and source map.
The proposed first stage covers exact scalar algebra and differentiation.
Array contracts, loop verification, and floating-point guarantees follow in
separate stages with their own acceptance gates.

## Proposed syntax

Synthesis is enabled explicitly by the calling application. Ordinary Fortran
mode does not interpret these constructs. Keywords are case insensitive and
use Fortran free-form comments and continuation. The following EBNF adds a
standalone synthesis unit; it does not alter Fortran program units:

```text
synthesis-unit = "synthesis", name, newline,
                 declaration, { declaration | statement },
                 "end", "synthesis", [ name ], newline ;
declaration    = "symbolic", "real", "::", name, { ",", name }, newline ;
statement      = assumption | definition | derivation | proof | generation ;
assumption     = "assume", "(", predicate, ")", newline ;
definition     = name, "=", exact-expression, newline ;
derivation     = "derive", name, "=", "diff", "(", exact-expression,
                 ",", name, ")", newline ;
proof          = "prove", predicate, newline ;
generation     = "generate", name, "from", "[", exact-expression,
                 { ",", exact-expression }, "]", newline ;
```

An exact expression uses scalar names, integer and decimal real literals,
parentheses, unary signs, addition, subtraction, multiplication, division,
and integer-literal powers. A predicate is a scalar comparison (`==`, `/=`,
`<`, `<=`, `>`, `>=`) or its combination with `.not.`, `.and.`, and `.or.`.
Fortran lexical rules and operator precedence apply. Procedure calls other
than `diff` in a derivation, kind suffixes, complex values, arrays, mutation,
and executable I/O are errors in this first stage. Later stages may add them.

Every symbolic name is declared once. A name without a definition is an input;
each defined or derived name has one definition and cannot also be an input.
Definitions refer only to declared input names or already-defined names.
Input classification uses the complete unit: a name defined later is not an
input. Self-reference and forward definitions are errors. A derivation
variable must be an input. Assumptions precede definitions, derivations,
proofs, and generations and refer only to inputs. Generated procedure names
are unique within the unit. An end name, when present, matches its opening
name using Fortran name comparison. Names retain their source spelling for
diagnostics and use case-folded identity for lookup.

```fortran
synthesis oscillator
    symbolic real :: q, p, m, h, qdot, pdot
    assume(m > 0)
    h = p**2 / (2*m) + q**2 / 2
    derive qdot = diff(h, p)
    derive pdot = diff(h, q)
    prove qdot*m == p
    generate rhs from [qdot, -pdot]
end synthesis oscillator
```

The generated vector is `[p/m, -q]`. A negative derivative is expressed in
the generation expression so that the initial `derive` rule remains small.
No compiler may accept the richer illustrative syntax from the issue as
equivalent without a separately accepted grammar revision.

## Exact semantics and domains

Symbolic real values denote mathematical real numbers. Decimal literals
denote exact rational numbers, including their exponent, independently of
the machine default real kind. For example, `0.1 + 0.2 == 0.3` is true in a
symbolic proof. This statement does not prove the corresponding IEEE
floating-point equality.

First-stage assumptions are restricted to comparisons of one input with a
signed integer literal of absolute value at most `2**53`, combined with the
logical operators above. They are evaluated without arithmetic on the input.
Thus their ordinary IEEE binary64 entry guards also check their exact-real
meaning. A constant-arithmetic assumption such as
`assume(0.1 + 0.2 == 0.3)` is rejected by this stage, even though the same
predicate is allowed in `prove`. Richer assumptions require a separately
accepted exact guard evaluator.

Division requires a nonzero denominator; negative powers require a nonzero
base. Each such requirement becomes a domain obligation under the unit's
assumptions. A definition is usable for generation only after all its domain
obligations are proved. Derivation is mathematical differentiation on that
domain; it does not differentiate a sequence of floating-point operations.
Generated arithmetic additionally tests each rounded denominator for zero
before division or a negative power. For example, an exact proof that `m*m`
is nonzero does not remove this check: multiplication can underflow when `m`
is a positive finite machine number. The checked temporary is the one used
in the operation, avoiding a changed recomputation.

Assumptions are local to the synthesis unit and inherited by its obligations.
A disproved consistency obligation rejects the unit. Unknown consistency
remains recorded; any proof is explicitly conditional on the assumptions.
No conditional claim establishes that the assumptions hold for caller inputs.
Definitions cannot change assumptions or discharge their own prerequisites.

Exact identities must not justify reassociation or contraction of ordinary
Fortran floating-point expressions. Generated source establishes the chosen
numerical implementation. Floating-point error, overflow, and underflow
guarantees require additional obligations outside this first stage.

## Generated Fortran interface

Each synthesis unit emits one module named `<unit>_synthesis`. Each generation
emits a function with the requested name. Its dummy arguments are exactly the
input names referenced by its expressions, their transitive definitions and
domain obligations, and all unit assumptions, in declaration order; each is scalar
`real(real64), intent(in)`. Its result is `real(real64)` with explicit extent
equal to the generation expression count. The module imports `real64` from
`iso_fortran_env` and uses `implicit none`. Existing Fortran module/procedure
name collisions are diagnosed before any artifact is published.
This first numerical profile requires IEEE binary64 (`radix == 2`,
`digits == 53`, and IEEE datatype support); unsupported targets are diagnosed.

Generated entry points check that all inputs are finite and satisfy the
assumptions before generated arithmetic. Each domain guard runs immediately
before its guarded operation, using the checked temporary. Violations
execute `error stop 1` with the originating assumption or domain obligation
identifier in a preceding diagnostic. The twin uses character `error stop`
codes for the same required nonzero termination and diagnostic behavior.
These functions are not declared `pure` in this first stage.
Contracts must not promise purity while requiring runtime failure checks.

Expressions preserve source evaluation order, expanded through referenced
definitions. Derivatives are emitted in a deterministic backend-defined form
whose backend identity and options are part of provenance. Each numeric
literal is explicitly `real64`; conversion to that kind follows ordinary
Fortran rules. Domain checks prevent division by zero but do not promise that
the finite result avoids machine overflow. No symbolic values or proof
objects are present in the emitted module's runtime interface.

The executable [oscillator twin](synthesis/oscillator-twin.f90) fixes the
interface and representative numerical behavior. It is handwritten ordinary
Fortran and is an independent oracle for a future generator. It is not
evidence that a Synthesis frontend or proof backend already exists.

Run the draft artifact checks with the current installed `fo`:

```sh
uv run --with jsonschema python docs/synthesis/verify_contract.py
```

The checker uses an independent JSON Schema implementation and builds the
handwritten twin through `fo` in an isolated `/var/tmp` fixture. It checks three
numerical points and zero, negative, NaN, and infinite input refusals. It
does not parse Synthesis or prove its mathematical claims.

## Obligations and evidence

Every obligation reports `PROVED`, `DISPROVED`, or `UNKNOWN`. A `PROVED`
status requires an accepted checker to verify the complete claim under its
recorded assumptions. A backend's unsupported result, missing executable,
timeout, or unverified certificate produces `UNKNOWN`, with a reason.
`DISPROVED` requires a checked counterexample or checked negation. Numerical
probes and passing tests are recorded as evidence while the proof status
remains `UNKNOWN`; they cannot satisfy a required-proof policy.

The [manifest schema](synthesis/manifest-v1.schema.json) distinguishes checker
identity, evidence kind, claim status, and content hashes. A consumer validates
the schema version, every referenced hash, checker compatibility, and claim
dependencies before trusting a status. A changed assumption invalidates all
dependent claims and generated procedures. Changing unrelated implementation
code does not invalidate a mathematical claim whose semantic inputs remain
identical, but does invalidate that implementation's equivalence certificate.

A disproved claim always fails verification. Unknown required claims fail
before code generation. A nonrequired unknown claim may proceed only when
policy explicitly permits its claim class. Runtime assertions or property
tests are additional required actions under such policy; they do not convert
the original claim into a proof. Unknown exact domain obligations cannot be
bypassed by runtime fallback in the first stage.

## Source maps and regeneration

Source locations use workspace-relative UTF-8 paths and half-open byte spans.
Offsets count bytes in the exact source whose SHA256 is recorded. Every
generated executable statement has a mapping to its source expression,
assumption, or domain obligation. Synthetic declarations map to the generation
statement. Each diagnostic includes the original source span, claim identifier,
status, and backend failure reason when applicable.

Manifests and source maps serialize as UTF-8 JSON with sorted keys, compact
separators, and one trailing newline; arrays retain semantic order. A hash
uses SHA256 of those exact bytes. Hash references exclude the containing
artifact's own hash. Regeneration from identical semantic inputs, frontend
and backend versions/options, numeric target, and policy produces identical
source and manifest bytes. Timestamps and absolute installation paths are
excluded from semantic provenance. A stale certificate or unsupported schema
version is an error, never a cache hit.

A claim's semantic hash covers its typed expression, assumptions, dependency
hashes, contract revision, and checker semantics. For a
`generated-kernel-equivalence` claim it additionally covers generated-source
and interface hashes, numerical target, and the generation tool/options.
Source locations belong to
the containing manifest and do not enter that hash: moving a claim can reuse
its checked proof while regenerating its source mapping. Consumers additionally
check unique claim identifiers, acyclic existing dependencies, byte spans
within their hashed files, and source-map coverage. JSON Schema alone cannot
establish those relationships or verify a certificate.

Artifact bundles provide a content-addressed object directory: every bare
hash of a policy, assumptions, options, interface, or semantic claim resolves
to a file named by that SHA256 whose exact bytes are checked. Those payloads
use the canonical JSON rules above. Checker executable hashes identify the
installed accepted checker and are measured from its executable bytes; they
do not authorize downloading or executing an untrusted bundled program.
Certificates explicitly bind the claim's semantic hash, assumption hash,
dependency hashes, and checker semantics; a successful checker run without
that binding cannot establish the manifest's claim.

## Later stages

Stage 2 proposes `requires`, `ensures`, and loop `invariant` statements using
typed logical Fortran expressions. It must specify entry/exit snapshots,
allocation and alias effects, shape equality, purity, loop induction, and
runtime assertion behavior before those constructs are enabled. In
particular, `shape(y) == shape(x)` produces an array of logical values in
ordinary Fortran; a scalar contract requires `all(shape(y) == shape(x))`.

Stage 3 proposes separate mathematical `specification` and machine
`implementation` blocks and floating-point analysis. Their grammar, evidence
classes, and refinement relation require a new accepted contract. Why3 and
Lean adapters are implementation providers, not new language semantics.
Comment/directive incubation is deferred: ordinary compilers may ignore such
comments, so it cannot silently enforce required proof obligations.

## Acceptance decisions and implementation chain

Acceptance must approve the scalar grammar, fixed real64 interface, mandatory
entry guards, and evidence/schema contract, or replace them in a reviewed
revision. It must name accepted proof checkers and a bounded canonicalization
algorithm for exact algebra. The three later-stage surfaces remain proposals.

| Owner | First implementation slice | Independent acceptance oracle |
|-------|----------------------------|-------------------------------|
| [FortFront #2976](https://github.com/lazy-fortran/fortfront/issues/2976) | Explicit-mode parser, typed exact IR, domains, source spans | Valid syntax and invalid neighbors; oscillator interface/twin |
| fortsym #48 | Derivative generation and checked algebraic readback | Exact rational identities and invalid denominator neighbors |
| [ffc #632](https://github.com/lazy-fortran/ffc/issues/632) | Compile emitted ordinary Fortran, then consume the same IR | Lowered/native interface and numerical agreement; stale-proof refusal |
| [fo #120](https://github.com/lazy-fortran/fo/issues/120) | Policy, checker execution, content-addressed actions | Assumption invalidation; unchanged-claim reuse; missing-tool UNKNOWN |

Implementation begins with the accepted provider contract, migrates consumers,
and then enables the input mode. Positive, invalid, boundary, and cross-feature
fixtures must cover duplicate names, forward definitions, mismatched end names,
unsupported arrays/kinds, missing/stale certificates, inconsistent assumptions,
zero denominators, nonfinite inputs, and module/procedure collisions.
