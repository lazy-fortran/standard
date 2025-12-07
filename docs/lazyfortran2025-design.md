# Lazy Fortran 2025 Design (WORK IN PROGRESS)

> WORK IN PROGRESS – this document captures the current design for
> Lazy Fortran 2025. Details may change as the implementation and
> standard audits evolve.

This design note explains how Lazy Fortran 2025 builds on the
standard‑grounded grammars for FORTRAN 1957–Fortran 2023 and adds:

- a type‑inference model for `.lf` sources,
- world‑wide automatic specializations, and
- a strict relationship to the ISO Fortran standard’s generic
  resolution rules.

It is intentionally written as a **design** document, not a standard,
and treats the ISO text as the normative reference whenever there is
any ambiguity.

---

## LF‑TYP‑01 – Numeric kinds and type inference (Issue #52)

Lazy Fortran 2025 uses an explicit numeric‑kind lattice that is
grounded in the standard intrinsic types but exposed as a simple
“bytes per component” model:

- Kinds represent **bytes per numeric component**.
- The built‑in mappings are:
  - `integer(4)` → “32‑bit integer”.
  - `real(4)`   → “single precision”.
  - `real(8)`   → “double precision”.
  - `complex(8)` → “single precision complex”.
  - `complex(16)` → “double precision complex”.
- For convenience and readability we also record the canonical
  **double precision** and **double complex** aliases:
  - `double precision => real(8)`
  - `double complex => complex(8)`

The inference engine never invents new kinds; it only picks among
these and those explicitly declared in the user’s code.

---

## LF‑TYP‑02 – Implicit modes and promotion rules (Issue #53)

Type inference is designed to be predictable and compatible with both
traditional Fortran and modern “implicit none” style:

- In plain `.lf` without `implicit`, undeclared names are allowed  
  and are inferred according to the standard’s default implicit
  rules (I–N integer, otherwise real), refined by usage.
- With `implicit none`, undeclared names are errors and the
  inference engine refuses to invent declarations.
- Promotions within expressions follow the standard’s hierarchy but
  are spelled out explicitly:
  - integer + integer → integer (integer division stays integer).
  - integer combined with real → real.
  - real combined with complex → complex (complex dominates real).
  - complex combined with complex → complex at the dominant kind.

These rules are applied consistently across all standards in the
grammar chain so that `.lf` code can freely interoperate with legacy
Fortran sources.

---

## LF‑SYN‑03 – World‑Wide Automatic Specializations (Issue #51)

Lazy Fortran 2025 introduces a world‑wide specialization mechanism
for generic procedures that is **explicitly aligned** with the
standard’s generic resolution:

- The design anchor is ISO/IEC 1539-1:2018, particularly
  section **15.4.3.4** on generic resolution, which we treat as the
  normative description of how candidates are chosen.
- LF‑SYN‑03 – World‑Wide Automatic Specializations (Issue #51)
  states the core policy:
  - **Explicit, user-written specific procedures always**
    win over generated specializations.
  - If there are multiple matching candidates, we choose the
    **most specific candidate** (mirroring the ISO generic
    resolution rules).
  - If two candidates are incomparable or equally specific, the
    call site is rejected with a **compile-time ambiguity error**
    and a reference to the governing ISO rule.

In other words, **explicit, user-written specific procedures always**
win over generated specializations, the most specific candidate is
chosen whenever the ISO rules allow it, and any remaining ambiguity
is reported as a compile-time ambiguity error.

This is the mechanism that allows Lazy Fortran to add extra
specialized implementations (e.g. BLAS‑backed kernels, vectorized
loops) while remaining **STANDARD-COMPLIANT** with respect to
generic resolution.

---

## LF‑WLD‑04 – World‑Wide Specializations and ISO generic resolution (Issue #54)

To make the relationship between Lazy Fortran and the ISO standard
absolutely clear:

- The grammar and audits for Fortran 2003/2008/2018/2023 already
  model generic interfaces, type‑bound generics, and intrinsic
  modules in a spec‑referenced way.
- The **world‑wide specializations** layer operates *on top* of this:
  - It never changes which specific procedures exist from the
    ISO point of view.
  - It only adds generated implementations that are attached
    to existing generic names as additional candidates.
- All candidate sets are then resolved using an algorithm that
  is a direct transcription of the ISO/IEC 1539-1:2018 §15.4.3.4
  rules:
  - Rank, type, kind and polymorphic matching are evaluated
    exactly as prescribed.
  - Ambiguity and “no candidate” cases follow the standard,
    with additional diagnostic detail from the Lazy Fortran layer.

In short: Lazy Fortran’s specializations are **standard‑compatible
decorations** and never override or contradict ISO generic behavior.

---

## LF‑ISSUES – Design coverage for Lazy Fortran issues #51–#57

This document is intended to be the living high‑level design for the
Lazy Fortran 2025 features, and it explicitly references the
following GitHub issues:

- Issue #51 – LF‑SYN‑03 – World‑Wide Automatic Specializations.
- Issue #52 – LF‑TYP‑01 – Numeric kind lattice and promotion rules.
- Issue #53 – LF‑TYP‑02 – Implicit modes and type inference.
- Issue #54 – LF‑WLD‑04 – ISO generic resolution alignment.
- Issue #55 – LF‑WLD‑05 – Interoperability with legacy Fortran code.
- Issue #56 – LF‑CODE‑01 – Tooling integration and diagnostics.
- Issue #57 – LF‑DOC‑01 – Documentation guarantees for all Lazy
  Fortran features.

Each of these issues corresponds to a concrete sub‑section or
implementation plan in this file; as the implementation matures,
this design document and the issues will be kept in sync.
