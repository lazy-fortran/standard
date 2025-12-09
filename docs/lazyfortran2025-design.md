# Lazy Fortran 2025 Design

> **Status:** Work in progress. Details may change as implementation evolves.

Lazy Fortran 2025 builds on the standard-grounded grammars for FORTRAN 1957 through Fortran 2023, adding:

- Type inference for `.lf` sources
- World-wide automatic specializations
- Strict alignment with ISO Fortran generic resolution rules

This is a **design document**, not a standard. The ISO text is the normative reference for any ambiguity.

---

## LF-TYP: Automatic Type Inference (#52, #53)

Lazy Fortran 2025 uses **automatic type inference** - a modern approach that eliminates the legacy I-N naming convention while remaining predictable.

**First assignment wins:**
- The type of an undeclared variable is determined by its **first assignment**
- `x = 4` makes `x` an integer
- `y = 3.14` makes `y` a real
- `z = (1.0, 2.0)` makes `z` a complex
- `obj = create_particle()` makes `obj` whatever type `create_particle` returns

**Subsequent assignments follow standard coercion:**
- After type is established, later assignments use normal Fortran coercion rules
- `x = 5.3` after `x = 4` is valid (real truncated to integer)
- `y = 7` after `y = 3.14` is valid (integer promoted to real)

**Interaction with `implicit none`:**
- With `implicit none`: undeclared names are errors; inference is disabled
- This preserves compatibility with strict coding styles

**Expression promotion rules:**

| Expression | Result |
|------------|--------|
| integer + integer | integer |
| integer + real | real |
| real + complex | complex |
| complex + complex | complex (dominant kind) |

---

## LF-SYN: World-Wide Automatic Specializations (#51)

Lazy Fortran 2025 introduces world-wide specialization for generic procedures, aligned with ISO/IEC 1539-1:2018 Section 15.4.3.4.

**Resolution policy:**
1. **User-written specifics win** - explicit procedures always take precedence over generated specializations
2. **Most specific candidate wins** - follows ISO generic resolution rules
3. **Ambiguity is an error** - incomparable candidates trigger compile-time errors with ISO rule references

This enables Lazy Fortran to add optimized implementations (BLAS-backed kernels, vectorized loops) while remaining standard-compliant.

---

## LF-WLD: ISO Generic Resolution Alignment (#54)

The relationship between Lazy Fortran and the ISO standard:

**Foundation:**
- Grammars for Fortran 2003/2008/2018/2023 model generic interfaces, type-bound generics, and intrinsic modules per spec

**World-wide specializations layer:**
- Operates *on top* of ISO semantics
- Never changes which specific procedures exist from ISO perspective
- Only adds generated implementations as additional candidates to existing generic names

**Resolution algorithm (per ISO/IEC 1539-1:2018 Section 15.4.3.4):**
- Rank, type, kind, and polymorphic matching evaluated exactly as prescribed
- Ambiguity and "no candidate" cases follow standard, with enhanced diagnostics

Lazy Fortran specializations are **standard-compatible decorations** that never override ISO generic behavior.

---

## Open Questions

Design decisions that need further discussion before finalizing.

### OQ-1: Default numeric kinds

| Option | Description |
|--------|-------------|
| A: ISO defaults | `integer(4)`, `real(4)`, `complex(8)` - maximum compatibility |
| B: Double precision | `integer(8)`, `real(8)`, `complex(16)` - modern scientific computing practice |

**Tradeoff:** ISO compatibility vs. precision/safety for numerical work.

### OQ-2: Inference from `intent(out)` arguments

Fortran idiomatically uses `intent(out)` arguments instead of function return values.

```fortran
call init_particle(p)  ! Should this declare p automatically?
```

| Option | Description |
|--------|-------------|
| A: No | Only assignment triggers inference |
| B: Yes | Inspect callee signature to infer `intent(out)` argument types |

**Tradeoff:** Simplicity vs. idiomatic Fortran support. Option B requires cross-procedure analysis.

---

## Issue Tracking

| Issue | ID | Description |
|-------|-----|-------------|
| #51 | LF-SYN | World-wide automatic specializations |
| #52, #53 | LF-TYP | Automatic type inference |
| #54 | LF-WLD | ISO generic resolution alignment |
| #55 | LF-WLD | Interoperability with legacy Fortran code |
| #56 | LF-CODE | Tooling integration and diagnostics |
| #57 | LF-DOC | Documentation guarantees |
