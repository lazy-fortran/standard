# Module Interface Signatures (Stable, Content-Addressed)

**Status:** Draft proposal
**Base:** Fortran 2028 working draft + LFortran extensions
**Scope:** Compiler/build-tool-level specification for stable module interface signatures
**Issue:** [#753](https://github.com/lazy-fortran/standard/issues/753)
**Related:** #734 (roadmap), #747 (effects), #736 (containers/templates), #738 (sum types), #740 (runtime cache tracking)

---

## Purpose

Large Fortran projects rebuild downstream modules on almost any edit because
build tools conservatively depend on the whole `.mod` file. A module
implementation change that does not alter the exported interface should not
force downstream recompilation.

This document specifies a **stable module interface signature**: a compact,
deterministic, content-addressed hash derived from only the public interface of
a module. Build tools compare signatures before triggering downstream rebuilds;
the compiler emits the signature alongside its `.mod` payload so the two are
never out of step.

The model is independent of any particular build system and complements
content-addressed `fo` caches (see #740): `fo` keys artifacts by content, the
signature keys *semantic* public API stability.

---

## Definitions

- **Module interface signature (LF signature).** A stable hash over the
  normalized, public-only interface of a module.
- **`.mod` payload.** The compiler-serialized module file that downstream
  compilation actually consumes (interface bodies, derived-type definitions,
  constants, generic templates, trait information).
- **Public interface.** The set of entities a module exports that callers may
  legitimately depend on: `public` procedures (including generics and
  templates), public derived types and their public components/bindings,
  public constants/parameters, public generic instantiations, public trait
  implementations, and any public contracts that affect callers.
- **Private implementation.** Module-body `contains` internals, private
  components, private procedures, local state, and any entity not reachable
  through the public interface.

The LF signature is a *fingerprint of the public interface*, not of the source
text and not of the full `.mod` payload. Two modules with different private
implementations but identical public interfaces produce identical LF
signatures.

---

## What Belongs to the Public Interface Signature

The signature covers exactly the externally observable calling contract.
For each public entity the following are hashed:

| Entity | Signature inputs |
|--------|------------------|
| Public procedure (function/subroutine) | Name, argument names, argument types, rank/shape (deferred where `assumed-shape`), intents, optionality, result type/rank/shape, `pure`/`elemental`/`impure`, **effects** (see below), trailing-argument defaults that affect caller ABI |
| Public generic (`interface`/`generic`) | Generic name and the ordered set of specific procedures it resolves to, including any ambiguity-significant attributes |
| Public derived type | Name, type parameters (kinds/lengths), public components (name, type, rank/shape, allocatable/pointer, deferred), public type-bound bindings (names and their signatures), parent type (for `extends`), **public layout/ABI policy** (see below) |
| Public constant/parameter | Name, type, kind, and the constant's value where it can affect callers (e.g. array bounds, kind parameters) |
| Public generic/template instantiation | Template name, the actual type/constant arguments, and the resulting public procedures |
| Public trait implementation | The implemented trait, the implementing type, and the resolved method set, where trait conformance is externally observable |
| Public contracts | Effects/shape/rank/unit contracts that constrain caller obligations (see #745, #747) |
| Module identity | Module name, LF signature **schema version**, target/runtime fingerprint, and a compatibility tag |

The following are **explicitly excluded** and therefore never invalidate a
downstream rebuild:

- Private procedures, private components, and module-body internals.
- Statement reordering, whitespace, comments, and non-interface source text.
- Implementation-only changes inside `contains` that preserve the public API.
- Compiler-internal temporaries, inlining decisions, and optimization choices.
- Private constants or private derived types reachable only through the
  implementation.

### Effects, shape/rank/unit contracts, traits, layout

Consistent with #747, effects are part of a callable's interface when they
constrain callers (e.g. `pure`, `impure`, `io`, `allocating`). A change that
adds or removes a public effect changes the signature and forces a rebuild.

Shape/rank/unit contracts (#745) become part of the signature only when they
are part of the *public* calling contract. A private internal reshaping of
arrays does not change the signature.

Trait implementations must have a stable public representation: the trait
name, the implementing type, and the resolved method set are hashed
canonically (see [Stable serialization](#stable-serialization)).

Derived/variant public layout follows an explicit ABI/interface policy. When a
public derived type's storage layout is part of the caller ABI (passed by
reference, common blocks, `bind(C)`, variant/sum-type tag layout per #738),
the layout policy version and the layout-relevant fields are part of the
signature. If a public type is always passed by descriptor and its layout is
implementation-defined, only the descriptor-visible contract is hashed.

---

## Stable Hashing and Serialization

### Canonicalization

Hashing operates on a **canonical serialized form** of the public interface,
not on source text or compiler internals. Canonicalization rules:

1. **Stable ordering.** Public entities are ordered canonically: module name,
   then public constants, derived types, procedures, generics, templates,
   trait implementations, contracts — each category sorted by a stable key
   (name, then overload set). Order of declaration in source must not matter.
2. **Normalized names.** Entity names are case-folded to a canonical case
   (module names and public names are case-insensitive in Fortran). Renames
   (`use, only`) are not hashed; the canonical entity name is used.
3. **Canonical types.** Kinds and types are spelled canonically (e.g.
   `integer(kind=8)` normalized to a canonical kind spelling).
4. **No source fingerprints.** Comments, indentation, statement order, and
   private bodies contribute nothing.
5. **Traits/templates canonicalization.** A trait implementation is hashed by
   (trait-name, type-name, sorted method set); a template instantiation by
   (template-name, sorted actual-argument list).

### Hash and schema versioning

- The signature is a fixed-width hash (e.g. 256-bit SHA-256 truncated to the
  configured width) of the canonical form, prefixed by a **schema version**.
- **Schema version** bumps are mandatory whenever the canonicalization rules
  or the set of signature inputs change (e.g. adding effects as a signature
  input in a later release). Signatures with different schema versions are
  never compared directly.
- A **compatibility tag** distinguishes semantically *compatible* signatures:
  a downstream artifact rebuilt against signature `S1` is valid against a new
  signature `S2` only if `S2` is declared compatible with `S1` under the
  schema's compatibility rules (see below).

### Determinism

Signature computation must be **deterministic**: the same module source and
the same compiler version must yield byte-identical LF signatures on every
run, regardless of process scheduling, parallel build order, filesystem
state, or machine. Determinism requirements:

- No dependence on timestamps, environment variables, locale, or absolute
  paths.
- No dependence on hash-map iteration order anywhere in canonicalization;
  all unordered collections are sorted before hashing.
- No mutable hidden global state consulted during signature computation.
- Numeric formatting and kind resolution are canonical and
  implementation-independent within a compiler version.

---

## Relationship Between `.mod` Payload and LF Signature

The LF signature is **stored inside and emitted alongside** the `.mod` payload
so the two are atomic and never diverge:

- A `.mod` file's header contains its module name, schema version, and LF
  signature.
- The signature is computed from the same normalized public interface used to
  serialize the `.mod` interface section, so a `.mod`'s signature always
  reflects that `.mod`'s public interface by construction.
- A downstream compilation reads the `.mod` and records the *producer's* LF
  signature. On a later incremental build it compares the *new producer's*
  signature with the recorded one: if equal (or declared compatible), the
  downstream object and cached downstream artifacts are reused; otherwise the
  downstream module is recompiled.
- `fo`-style content-addressable caches (#740) may key a downstream build unit
  on the set of upstream LF signatures plus the downstream unit's own source,
  giving reuse exactly when public interfaces are unchanged.

```
producer source  ──►  normalize public interface ──► canonical form ──► LF signature
                        │                                                │
                        ▼                                                ▼
                      .mod payload                              stored in .mod header
                                                                            │
downstream build ◄──── compare recorded vs new LF signature ───────────────┘
```

### Invalidation rules

- A downstream unit is **rebuilt** when any upstream LF signature changes in a
  way that is not declared compatible.
- A downstream unit is **not rebuilt** when upstream private implementation
  changes leave the LF signature byte-identical.
- A downstream unit is **rebuilt** when a producer's schema version changes,
  because old and new canonicalizations are not directly comparable.
- A downstream unit is **rebuilt** when the target/runtime fingerprint changes
  (see [Target and runtime dependence](#target-and-runtime-dependence)).

---

## Compatibility and Versioning

Two signatures `S1` and `S2` (same schema version) are compared under explicit
compatibility rules:

- **Byte-identical** signatures are always compatible.
- **Effects change:** adding or removing a public effect that constrains
  callers (#747) is an *incompatible* change; downstream units must rebuild.
- **Shape/rank/unit contract change** (#745): a public contract change is
  incompatible; a private contract change does not touch the signature.
- **Added public entity:** adding a *new* public procedure/component/constant
  is generally compatible (downstream code that did not use it still links),
  but the signature changes so downstream units rebuild to stay correct for
  any new use. Build tools may opt into "additive-only" fast paths where the
  schema records a compatibility flag.
- **Removed or renamed public entity:** always incompatible.
- **Public derived-type layout change:** incompatible under the ABI/interface
  policy (see above).
- **Target/runtime change:** incompatible when the target fingerprint differs.

The schema version and compatibility tag must be specified before `ffc` changes
its published `.fmod` contract (see ROADMAP #753 note).

---

## Target and Runtime Dependence

The LF signature is **not** a purely source-level fingerprint when the target
or runtime affects the public calling contract:

- A **target fingerprint** (ISA, ABI, default kind sizes, alignment, `bind(C)`
  conventions, default real/integer size under `--std=lf`) is hashed into the
  signature because a downstream unit built against a different target's `.mod`
  may be ABI-incompatible even with identical source.
- A **runtime fingerprint** is included only when the runtime affects the
  public contract (e.g. an allocator ABI, coarray runtime, or variant layout).
- Purely implementation-level target tuning (instruction selection, inlining,
  code layout) must **not** affect the signature.

This keeps signature equality meaningful across rebuilds on the same machine
while preventing cross-target false positives.

---

## Examples

### 1. Implementation change avoids downstream rebuild

`tx_parser` exposes a stable public API. The private implementation changes.

```fortran
module tx_parser
    private
    public :: parse_document, parse_inline

    ! private helper, not part of the public interface
    interface
        module subroutine tokenize(s, toks)
            character(*), intent(in)  :: s
            character(:), allocatable :: toks(:)
        end subroutine tokenize
    end interface
contains
    ! public API
    function parse_document(s) result(tree)
        character(*), intent(in) :: s
        type(xml_tree)           :: tree
        ! ... calls tokenize ...
    end function parse_document

    function parse_inline(s) result(frag)
        character(*), intent(in) :: s
        type(xml_frag)           :: frag
    end function parse_inline
end module tx_parser
```

An edit that changes only the body of `tokenize` (or the internals of
`parse_document`) leaves every public signature input unchanged:

```fortran
module tx_parser
    private
    public :: parse_document, parse_inline
    ! interface block identical to before
    ...
contains
    function parse_document(s) result(tree)          ! unchanged interface
        ...
        ! body rewritten: new internal algorithm
    end function parse_document
    ...
end module tx_parser
```

**Result:** LF signature is byte-identical; downstream modules that `use
tx_parser` are **not rebuilt**.

### 2. Public effect change requires rebuild

A procedure's `pure` attribute is part of its calling contract.

```fortran
! before
function compute(x) result(y)
    real, intent(in) :: x(:)
    real             :: y
    y = sum(x)
end function compute

! after — becomes impure, callers' evaluation-order assumptions may change
function compute(x) result(y)
    real, intent(in) :: x(:)
    real             :: y
    y = sum(x)
    call log_usage()        ! side effect → interface effect changes
end function compute
```

**Result:** the public effect of `compute` changed, so the LF signature
changes and downstream units **must rebuild**.

### 3. Public shape/rank contract change requires rebuild

```fortran
! before: assumed-shape rank-1
subroutine apply(v, out)
    real, intent(in)  :: v(:)
    real, intent(out) :: out(:)
end subroutine apply

! after: rank-2 (shape/rank contract change, #745)
subroutine apply(v, out)
    real, intent(in)  :: v(:,:)
    real, intent(out) :: out(:,:)
end subroutine apply
```

**Result:** the public rank contract changed → LF signature changes →
downstream rebuild. A private internal reshape that keeps the public
`v(:)` contract would not change the signature.

### 4. Added public entity (additive change)

```fortran
! before
module m
    private
    public :: a
contains
    subroutine a()
    end subroutine a
end module m

! after — new public entity added
module m
    private
    public :: a, b
contains
    subroutine a()
    end subroutine a
    subroutine b()      ! new, additive
    end subroutine b
end module m
```

**Result:** the signature changes (a new entity is public), so downstream units
rebuild; under an additive-only fast path the change may be recorded as
compatible so existing downstream objects need not relink.

### 5. Private-to-public and visibility

```fortran
module m
    private
    public :: exposed
contains
    subroutine exposed()
    end subroutine exposed
    subroutine hidden()        ! private — not in signature
    end subroutine hidden
end module m
```

Making `hidden` private still (unchanged) contributes nothing. Promoting a
private procedure to `public` adds an entity to the signature and invalidates
downstream units (new public surface).

---

## Thread Safety and Parallel Builds

Requirements for parallel, incremental builds:

1. **Independent signature computation.** Each producer module computes its
   LF signature with no shared mutable state; parallel builds may compute
   signatures for different modules concurrently without locks or hidden
   global mutation. Pure functions over (source, compiler version, target,
   runtime, schema) only.
2. **Deterministic results.** Identical inputs ⇒ identical signatures across
   any scheduling order (see [Determinism](#determinism)).
3. **Content-addressed signature caches.** Signature caches are keyed by the
   canonical input hash (content-address), never by path or timestamp. Cache
   entries are immutable once written.
4. **Concurrent read safety.** Multiple build agents may read a cached
   `.mod`/signature concurrently; reads are lock-free and observe a complete,
   atomically-published record (signature and `.mod` payload written together
   and published under a content-addressed key).
5. **No hidden global state.** The compiler must not consult mutable globals
   (random seeds, counters, allocator state) during canonicalization or
   hashing.
6. **Write-once, compare-and-swap.** Producers publish under a
   content-addressed key with atomic replace; a stale read never observes a
   partially-written `.mod` or signature.

A conforming build tool may compute signatures for all changed modules in
parallel, then diff the new signatures against the previously recorded ones and
schedule downstream rebuilds from the diff.

---

## Non-Goals for This Document

- Semantic/type validation of module contents (out of scope; see CLAUDE.md).
- The full `.mod` serialization byte format (that is `ffc`'s `.fmod` contract;
  this document fixes the signature identity, versioning, and compatibility
  rules it must satisfy).
- The content-addressed `fo` cache design itself (#740); this document defines
  the signature `fo` should key on.
- A specific build-system integration.

---

## Definition of Done Checklist

- [x] Specify what belongs to a module's public interface signature.
- [x] Define stable hashing/serialization requirements (canonicalization,
      schema versioning, determinism).
- [x] Define how `.mod` payloads and LF signature hashes relate (atomic,
      stored in `.mod` header, comparison-driven invalidation).
- [x] Include examples where implementation changes avoid downstream rebuilds.
- [x] Include examples where public effects/shape/contracts change and require
      rebuild.
- [x] Define thread-safety and determinism requirements for parallel builds.

---

## References

- ROADMAP #753: stable module interface signatures (compiler-critical).
- Issue #734: roadmap.
- Issue #747: effects.
- Issue #736: containers/templates.
- Issue #738: sum types / variants.
- Issue #740: runtime cache tracking / `fo`.
- Issue #745: shape/rank types and array contracts.
- [Traits Proposal](traits-proposal.md): trait conformance representation.
