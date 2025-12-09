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

| Option | Pros | Cons |
|--------|------|------|
| A: ISO defaults (`real(4)`, `integer(4)`) | Compatible with existing code, predictable, smaller memory | Precision loss, integer overflow at ~2B |
| B: Double precision (`real(8)`, `integer(8)`) | Safer for scientific computing, no precision surprises | Breaks ISO expectations, 2x memory |

**Chris' preference:** Option A - standard compliance is paramount.

### OQ-2: Inference from `intent(out)` arguments

Fortran idiomatically uses `intent(out)` arguments instead of function return values.

```fortran
call init_particle(p)  ! Should this declare p automatically?
```

| Option | Pros | Cons |
|--------|------|------|
| A: No (assignment only) | Simple local analysis, predictable | Doesn't support idiomatic Fortran patterns |
| B: Yes (inspect callee) | Supports `intent(out)` patterns | Requires cross-procedure analysis, interface files |

**Chris' preference:** No clear preference - both options have merit.

### OQ-3: Declaration placement

Should declarations be allowed anywhere in the code, or only at block beginning?

```fortran
x = 1
y = 2
integer :: z  ! Allowed mid-block?
z = x + y
```

| Option | Pros | Cons |
|--------|------|------|
| A: Block beginning only | Clean structure, all declarations visible at once | Verbose, variables far from first use |
| B: Anywhere | Declare near use, familiar to C/Rust devs | Scattered declarations, redundant with inference |

**Chris' preference:** Option A - type inference handles convenience; if you want structure, declare at top.

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
