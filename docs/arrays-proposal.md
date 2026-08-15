# Arrays Proposal: Shape/Rank Types, Checked Broadcasting, and Array Contracts

**Status:** Draft proposal
**Base:** Fortran 2028 working draft + LFortran extensions
**Scope:** Design-level specification (normative syntax, semantics, lowering, and checks)
**Issue:** [#745](https://github.com/lazy-fortran/standard/issues/745)

---

## Purpose

This document lifts rank, shape, axis names, and broadcasting rules into the
LFortran type system so that shape/rank mistakes are caught early, intent is
documented in the type signature, and the optimizer can rely on static shape
metadata without hiding semantics in libraries.

Scientific bugs frequently come from:

- wrong rank (scalar vs. vector vs. matrix confusion)
- wrong extent along an axis
- accidental broadcasting
- transposed dimensions
- mixing particle, coordinate, species, or time axes

NumPy/Python catch many of these at runtime; C++/Rust often hide them in
libraries. Fortran/LFortran make them part of the language contract.

---

## 1. Shape and Rank Annotation Syntax

### 1.1 The `shape` attribute

A new declaration attribute, `shape(...)`, documents the rank and the extent
along every axis of an array entity. It composes with the existing LFortran
attribute list:

```fortran
subroutine advance(x, v, dt)
    real(dp), shape(n, 3), intent(inout) :: x
    real(dp), shape(n, 3), intent(in)    :: v
    real(dp),             intent(in)    :: dt
end subroutine
```

The number of entries in the shape list is the **rank**. Each entry is the
expected extent along that axis.

### 1.2 Extent forms

A shape entry is one of:

| Form | Meaning |
|------|---------|
| integer literal or named constant | fixed compile-time extent |
| named type/rank parameter (from templates/generics) | parameterized extent |
| integer expression | dynamic extent (checked at runtime) |
| `*` | assumed-shape / rank-polymorphic extent |

`shape(*)` (a single `*`) means **assumed rank** — the entity accepts any rank
at the call site (rank-polymorphic, analogous to Fortran 2018 `rank(*)`).

Examples:

```fortran
real(dp), shape(3)             :: vector          ! rank 1, extent 3
real(dp), shape(n, 3)          :: pos             ! rank 2, extents (n, 3)
real(dp), shape(particle, xyz) :: pos             ! rank 2, named axes
real(dp), shape(particle)      :: mass            ! rank 1, named axis
real(dp), shape(*)             :: any_rank        ! rank-polymorphic
real(dp), shape(n, *)          :: first_fixed     ! rank 2, n fixed, 2nd dynamic
```

### 1.3 Named axes / index spaces

Named axes give extents a name that is checked for consistency across a
procedure boundary. Axis names are scoped to the declaration; two entities
whose extents must match declare the same axis name:

```fortran
real(dp), shape(particle, xyz) :: x
real(dp), shape(particle)      :: mass
```

An axis name is an identifier. Two entities with the same axis name in the
same procedure must have the same extent along that axis. Axis names do **not**
carry a value; they are a *contract*: the actual extent is supplied by the
call site or by the allocation.

### 1.4 Interaction with `dimension` and `allocatable`

`shape(...)` is complementary to the standard `dimension(...)` / `allocatable`
/ `pointer` attributes. It documents the **contract**, not the storage
allocation:

- `allocatable, shape(n)` — allocated with shape `(n)`; allocation shape must
  match the contract.
- `dimension(n)` — storage shape, kept as-is; `shape(n)` may be repeated for
  clarity but must agree.
- `shape(*)` on an explicit-shape dummy declares an assumed-rank contract.

The grammar accepts `shape(...)` as an attribute in any declaration; semantic
enforcement (see below) is a compiler concern.

---

## 2. Lowering

### 2.1 Desugared twin (standard Fortran)

`shape(...)` is a *contract* annotation. It lowers to standard Fortran by
adding runtime checks and keeping the ordinary storage attributes. The
desugared form is valid Fortran 2023 and is the reference model for
verification.

For a fixed-shape dummy:

```fortran
! LFortran source
subroutine advance(x, v, dt)
    real(dp), shape(n, 3), intent(inout) :: x
    real(dp), shape(n, 3), intent(in)    :: v
    real(dp),             intent(in)    :: dt
end subroutine

! Desugared twin (standard Fortran 2023)
subroutine advance(x, v, dt)
    use, intrinsic :: iso_fortran_env, only: error_unit
    real(dp), intent(inout) :: x(:, :)
    real(dp), intent(in)    :: v(:, :)
    real(dp), intent(in)    :: dt
    ! shape contract checks (debug / release-safe)
    if (size(x, 1) /= n) call shape_error("x", 1, n, size(x, 1))
    if (size(x, 2) /= 3) call shape_error("x", 2, 3, size(x, 2))
    if (size(v, 1) /= n) call shape_error("v", 1, n, size(v, 1))
    if (size(v, 2) /= 3) call shape_error("v", 2, 3, size(v, 2))
end subroutine
```

The check can be hoisted out of the procedure body; because it is inserted
once at procedure entry (or at allocation), it does not run per loop iteration.

### 2.2 No descriptor allocation in hot loops

The `shape(...)` annotation does **not** require a descriptor or heap
allocation. Static shapes are compile-time metadata. Dynamic shapes reuse the
existing array descriptor already present for assumed-shape dummies. No new
runtime object is introduced; checks read existing `SIZE`/`UBOUND` information.

### 2.3 Compiler query model

The accepted representation maps to FortFront typed queries and ffc's single
canonical descriptor/expression model:

- `shape_of(entity)` -> rank + per-axis extent (static constant, parameter, or
  runtime expression)
- `rank_of(entity)` -> integer rank
- `axis_names(entity)` -> list of axis name identifiers
- `shape_satisfied?(entity_shape, contract_shape)` -> boolean/error

These queries are the interface consumed by FortFront and `ffc`; this document
specifies their semantics, not their ABI.

---

## 3. Static vs. Dynamic Checking

### 3.1 Compile-time (static) checks

When extents are constants or type parameters, shape conformance is checked at
compile time:

- A `shape(3)` entity assigned a `shape(4)` constant array is a compile error.
- A `shape(n, 3)` entity passed to a dummy declared `shape(n, 3)` is verified
  when `n` is a known type parameter.
- Rank mismatches are always compile-time errors when both ranks are statically
  known: `shape(3)` vs `shape(2, 2)`.

### 3.2 Runtime checks

When extents are dynamic (procedure arguments, allocatable variables,
expressions), checks run in **Debug** and **ReleaseSafe** modes:

- A dynamic extent mismatch raises a runtime error naming the entity, the axis,
  the expected extent, and the actual extent.
- Allocation of an `allocatable, shape(n)` entity with a non-matching shape is
  a runtime error (consistent with the existing "Array Reallocation on LHS"
  rule).

### 3.3 ReleaseFast mode

In **ReleaseFast** (`--fast`) mode, runtime checks are elided. Where the
compiler can prove a contract is satisfied, it converts the (elided) check into
an optimizer assumption enabling bounds-check elimination, inlining,
vectorization, and unrolling. Elision is only permitted for shape *checks*;
element bounds checks follow the existing build-mode rules.

### 3.4 Hoisting

Runtime shape checks are hoisted out of loops. A shape check is loop-invariant
when its operands are not assigned inside the loop; the compiler hoists it to
the loop preheader so it executes once, not per iteration.

---

## 4. Broadcasting Semantics

### 4.1 No implicit broadcasting

LFortran does **not** implicitly broadcast. Assigning or combining arrays of
mismatched shape is always an error (compile-time when static, runtime when
dynamic). This is the key divergence from NumPy, chosen to catch accidental
scalar/vector/matrix confusion.

Example (both rejected):

```fortran
real(dp), shape(2, 2) :: a, b
real(dp), shape(2)    :: v
a = b + v          ! ERROR: shapes (2,2) vs (2) do not conform
a = v              ! ERROR: shapes (2,2) vs (2) do not conform
```

### 4.2 Explicit `broadcast` operation

Broadcasting is explicit via the `broadcast(expr, over=axis...)` operation:

```fortran
x = x + broadcast(dt * v, over=particle)
```

Semantics:

- `broadcast(src, over=axis-name-or-index, [align=axis-name-or-index])`
- The source is replicated along the named (or indexed) axis to match the
  destination shape.
- The `over=` axis must exist in the destination shape; the source must conform
  to the destination after replication.
- Broadcasting a scalar is always a no-op and is permitted without `over=`.
- Broadcasting a rank-1 source over a rank-2 destination extends the source
  along the second axis.

Lowering (desugared twin): an explicit broadcast lowers to a fused loop. No
temporary array is materialized when the operation is fused with the consumer
(see [Performance](#7-performance-requirements)).

```fortran
! x(particle, xyz) = x(particle, xyz) + broadcast(dt * v(xyz), over=xyz)
do p = 1, nparticle
    do c = 1, 3
        x(p, c) = x(p, c) + dt * v(c)
    end do
end do
```

### 4.3 Policy-controlled implicit broadcasting

An implementation may expose a policy (e.g., `--broadcast=implicit`) that
permits the NumPy-style subset of broadcasting. This is **off by default** and
is a compiler flag, not a language default. The language itself specifies only
explicit broadcasting; implicit broadcasting is never inferred from context.

### 4.4 Fused lowering

Explicit broadcasting lowers to fused loops where legal:

- `dest = dest + broadcast(f(src), over=axis)` fuses the broadcast, the
  elementwise operation, and the store.
- No mandatory temporaries are required; the compiler may materialize one only
  when the source expression has side effects or aliasing prevents fusion.

---

## 5. Examples

### 5.1 Vectors and matrices

```fortran
module particles
    implicit none
    integer, parameter :: dp = kind(1.0d0)
contains
    subroutine advance(x, v, dt)
        real(dp), shape(n, 3), intent(inout) :: x
        real(dp), shape(n, 3), intent(in)    :: v
        real(dp),             intent(in)    :: dt
        x = x + broadcast(dt * v, over=particle)
    end subroutine
end module particles
```

### 5.2 Rank-polymorphic generics

Shape contracts compose with templates/generics (`{T}` and F2028 templates).
A generic can constrain rank via `shape(*)` and constrain extents via a
template parameter:

```fortran
subroutine norm2{T}(x) result(s)
    type(T), shape(*), intent(in) :: x
    type(T) :: s
    s = sum(x * x)
end subroutine norm2
```

The generic body may use any rank; the call site provides the concrete rank
and extents. If the generic additionally declares `shape(n)` with `n` a
template parameter, rank and extent are fixed at instantiation.

### 5.3 Named axes

```fortran
subroutine force(positions, masses, forces)
    real(dp), shape(particle, xyz), intent(in)    :: positions
    real(dp), shape(particle),      intent(in)    :: masses
    real(dp), shape(particle, xyz), intent(inout) :: forces
    ! particle axis of positions, masses, forces must agree
    ! xyz axis of positions and forces must agree
    forces = broadcast(masses, over=particle) * positions
end subroutine force
```

The named `particle` axis enforces that `positions`, `masses`, and `forces`
share one particle count; the `xyz` axis enforces that `positions` and `forces`
share one coordinate count. Passing an array with a different particle count
along that axis is a checked error.

### 5.4 Array sections and views

Shape contracts apply to array sections and views without copying:

```fortran
real(dp), allocatable, shape(n, 3) :: all_pos
call advance(all_pos(1:n, :), v, dt)   ! section still satisfies shape(n, 3)
```

The section's shape is computed from its bounds; the contract check runs once
at the call boundary.

---

## 6. Thread-Safety Requirements

### 6.1 Immutable shape metadata

Shape metadata is **immutable** for arrays/views passed to parallel code
(OpenMP, `do concurrent`) unless the entity is explicitly reallocated. A shape
contract cannot change for a given entity between entering and leaving a
parallel region; reallocation is an explicit, synchronization-visible event.

### 6.2 No hidden global mutable state

Shape checks must not rely on hidden global mutable state. All information
needed to evaluate a shape contract is derivable from:

- the entity's declared shape (immutable metadata),
- the entity's current bounds (read from the array descriptor),
- explicit arguments.

### 6.3 Read-only descriptors in parallel regions

Shape descriptors used in OpenMP parallel regions must be read-only or
thread-local. Because shape metadata is immutable and checks read the existing
descriptor, no writes occur inside a parallel region, so no data race on shape
metadata is possible.

### 6.4 Diagnostics safety

Diagnostics generated by a shape mismatch must not allocate from a
non-threadsafe allocator or mutate shared state. In a parallel region, a shape
mismatch is reported deterministically to the controlling image/thread.

---

## 7. Performance Requirements

- **No descriptor allocation in hot loops**: shape annotations do not force
  allocation; static shapes are compile-time metadata, dynamic shapes reuse the
  existing descriptor.
- **Static-shape optimization**: static shapes enable inlining, vectorization,
  unrolling, and bounds-check elimination.
- **Check hoisting**: dynamic checks are hoisted out of loops (Section 3.4).
- **Fused broadcast**: explicit broadcasting lowers to fused loops where legal;
  no mandatory temporaries (Section 4.4).
- **Elision as assumption**: in ReleaseFast, elided checks become optimizer
  assumptions (Section 3.3).

---

## 8. Diagnostics

Shape/rank mismatches produce diagnostics that name the mismatched entity,
axis, expected extent, and actual extent.

### 8.1 Rank mismatch (compile-time)

```text
error: rank mismatch for 'b' in assignment
  --> particles.f90:5:7
   |
 5 | a = b + v
   |     ^
   = shape of 'b' is rank 2 (2, 2)
   = shape of 'v' is rank 1 (2)
   = arrays do not conform without explicit broadcast
```

### 8.2 Extent mismatch (compile-time)

```text
error: shape mismatch for 'x' in call to 'advance'
  --> caller.f90:12:18
   |
12 | call advance(x, v, dt)
   |                  ^
   = dummy 'v' requires shape (n, 3) along axis 2 = 3
   = actual has extent 4 along axis 2
```

### 8.3 Extent mismatch (runtime, debug/release-safe)

```text
Runtime error: shape contract violated
  entity: 'x' (axis 1)
  expected extent: 100 (from shape(n))
  actual extent:   99
  at particles.f90:14
```

### 8.4 Named-axis mismatch

```text
error: axis 'particle' mismatch in 'force'
  --> force.f90:4:32
   |
 4 | real(dp), shape(particle, xyz), intent(inout) :: forces
   |                                ^
   = 'positions' particle extent = 1000
   = 'forces'   particle extent = 1001
```

### 8.5 Unintended-broadcast hint

When shapes do not conform and no explicit `broadcast` is present, the
diagnostic suggests `broadcast(..., over=...)`:

```text
   = arrays do not conform; if broadcasting was intended, write
     a = b + broadcast(v, over=axis)
```

---

## 9. Interaction with Other Features

- **Templates/generics**: generic routines can constrain rank/shape via
  `shape(*)`, `shape(n)` with template parameters, and named axes (Section 5.2).
- **`do concurrent` and OpenMP policy**: shape metadata is immutable and
  thread-safe (Section 6); checks are hoisted out of the parallel loop.
- **Units/dimensions and index spaces**: named axes compose with units and
  index spaces; an axis is an index space whose extent must match.
- **Array sections and views**: contracts apply without copying (Section 5.4).
- **Ownership/frozen views** (#739): shape metadata is immutable for frozen
  views, matching the immutability rule in Section 6.1.

---

## 10. Verification

Per the roadmap's proposal-to-implementation rule, array proposals need a
small reference evaluator or a standard-Fortran desugared twin plus invalid
neighbors. This proposal ships both:

- **Desugared twin**: the standard-Fortran lowering in [Section 2.1](#21-desugared-twin-standard-fortran)
  and the fused-loop lowering in [Section 4.4](#44-fused-lowering).
- **Reference evaluator**: `tools/array_contracts_checker.py` implements the
  shape/rank contract, named-axis, and broadcasting semantics. It is run by
  `tests/test_array_contracts_checker.py` and exits 0 when all checks pass.
- **Invalid neighbors**: `tests/LFortran/fixtures/arrays_invalid_implicit_broadcast.f90`
  (grammar-valid, semantically rejected) and the negative cases in the
  evaluator (rank mismatch, named-axis mismatch, no implicit broadcast, bad
  broadcast axis, dynamic extent mismatch).

---

## 11. Definition of Done Checklist



- [x] Specify shape/rank annotation syntax and lowering (Sections 1-2)
- [x] Define static vs dynamic checking rules (Section 3)
- [x] Define broadcast semantics and implicit-broadcast policy (Section 4)
- [x] Examples for vectors, matrices, rank-polymorphic generics, named axes (Section 5)
- [x] Thread-safety requirements (Section 6)
- [x] Performance requirements (Section 7)
- [x] Diagnostics examples for shape/rank mismatch (Section 8)

---

## References

- [LFortran Standard](lfortran-standard.md)
- [LFortran Design](lfortran-design.md)
- [Traits Proposal](traits-proposal.md)
- [ROADMAP](../ROADMAP.md) - compiler-critical proposal mapping (#745)
- [Fortran 2018 R1148-R1151 SELECT RANK](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
- ISO/IEC 1539-1:2023 (Fortran 2023)
- J3/26-007 (Fortran 2028 Working Draft)
