# Traits Proposal for LFortran

**Status:** Draft proposal  
**Base:** Fortran 2023 + LFortran generics extensions  
**Scope:** Design-level specification (syntax and intended behavior)

---

## Purpose

This document formalizes the traits proposal referenced by issue #725.
It specifies how traits compose with two existing LFortran generic systems:

- **J3 generics:** `template` / `requirement` / `requires` / `instantiate`
- **Procedure-level generics:** `{T}` syntax on function/subroutine definitions

Traits are complementary to both systems. They provide nominal conformance
(`implements`), trait composition, and runtime polymorphism via `class(ITrait)`.

---

## Composition Model

| System | Primary role | Dispatch model |
|--------|--------------|----------------|
| J3 generics (`template`) | Module-level reusable generic definitions | Static, explicit instantiation |
| Procedure-level generics (`{T}`) | Procedure-local generic constraints | Static, call-site type binding |
| Traits (`abstract interface` + `implements`) | Capability contracts and nominal conformance | Static (`type(T)`) and dynamic (`class(ITrait)`) |

All three systems are valid in one program unit.

---

## 1. Traits as Abstract Interfaces

Traits are declared using `abstract interface` blocks. A trait may include:

- **Type sets** (allowed implementing types)
- **Required generic or concrete methods**

### Example

```fortran
abstract interface :: INumeric
   integer | real(real64)
end interface INumeric

abstract interface :: ISum
   function sum{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
   end function sum
end interface ISum
```

### Rules

- The trait name after `end interface` should match the opening name when
  provided.
- Type-set clauses constrain valid `T` bindings in trait-constrained generics.
- Method signatures in a trait are requirements, not implementations.

---

## 2. `implements` Statement (Nominal Conformance)

Conformance is explicit and nominal.

### 2.1 Type Attribute Form

A concrete type may declare conformance in its attribute list:

```fortran
type, sealed, implements(ISum) :: SimpleSum
contains
   procedure, nopass :: sum
end type SimpleSum
```

### 2.2 Standalone Conformance Form

Retroactive conformance is allowed, including intrinsic types:

```fortran
implements INumeric :: integer
end implements integer
```

### Rules

- Conformance is opt-in; matching method shape alone is not sufficient.
- A type/trait pair shall not be declared more than once in one scoping unit.
- If the trait requires methods, the implementation must provide matching
  bindings (checked semantically, not by syntax alone).

---

## 3. `sealed` Attribute

`sealed` marks a concrete type as non-extensible.

```fortran
type, sealed, implements(ISum) :: SimpleSum
```

### Rules

- A sealed type cannot appear as a parent in `extends(...)`.
- `sealed` does not prevent trait conformance; it prevents inheritance.
- Violations are compile-time errors.

---

## 4. `initial` Constructors

`initial` declares named constructors as type-bound procedures.

```fortran
type, sealed, implements(ISum) :: PairwiseSum
contains
   initial :: init
   procedure, pass :: sum
end type PairwiseSum
```

### Rules

- `initial` targets must name procedures in the same type-bound scope.
- Multiple constructor names are allowed via a list.
- Constructor resolution follows normal generic binding rules.

---

## 5. Procedure-Level Generics (`{T}` Syntax)

Procedure-level generics allow constraints directly on procedures:

```fortran
function sum{INumeric :: T}(x) result(s)
   type(T), intent(in) :: x(:)
   type(T)             :: s
end function sum
```

### Rules

- `{Trait :: T}` binds `T` to types conforming to `Trait`.
- Trait-constrained type parameters can be used in dummy/result declarations.
- This feature is orthogonal to `template` and may be used independently.

---

## 6. `itself` Keyword

`itself` names the eventual implementing concrete type inside a trait method
signature.

```fortran
abstract interface :: INumeric
   function operator(+)(lhs, rhs) result(res)
      type(itself), intent(in) :: lhs, rhs
      type(itself)             :: res
   end function operator(+)
end interface INumeric
```

### Rules

- `itself` is valid only within trait declarations.
- During conformance checking, `itself` is substituted with the implementing
  concrete type.
- `itself` enables self-referential operator and method contracts.

---

## 7. Trait Composition

A type may conform to a composed trait expression:

```fortran
implements (IReducible + IPrintable) :: MyType
```

### Rules

- `+` composes requirements by set union.
- Composition is associative for requirement collection.
- Conflicting required signatures are compile-time errors.

---

## 8. `class(ITrait)` Runtime Polymorphism

Traits can be used as runtime polymorphic references:

```fortran
class(ISum), allocatable :: drv
s = self%drv%sum(x)
```

### Rules

- `class(ITrait)` may hold any concrete value that nominally implements
  `ITrait`.
- Procedure calls through a trait class reference dispatch dynamically.
- Dynamic dispatch complements static trait-constrained generics.

---

## Syntax Summary (Proposed)

```text
trait_decl            := ABSTRACT INTERFACE [::] trait_name trait_body END INTERFACE [trait_name]
trait_body            := ( type_set_stmt | method_sig_stmt )*
type_set_stmt         := type_spec ('|' type_spec)+

implements_attr       := IMPLEMENTS '(' trait_expr ')'
implements_stmt       := IMPLEMENTS trait_expr '::' type_spec END IMPLEMENTS [type_name]
trait_expr            := trait_name | '(' trait_name ('+' trait_name)+ ')'

initial_stmt          := INITIAL '::' binding_name_list
sealed_attr           := SEALED
itself_type_spec      := TYPE '(' ITSELF ')'
proc_generic_header   := FUNCTION name '{' trait_name '::' type_name '}'
```

This summary is intentionally grammar-adjacent and not yet a normative ANTLR
production.

---

## Compatibility and Migration

- Existing `abstract interface` syntax remains valid.
- Existing J3 `requirement`/`template` code remains valid.
- Traits can be introduced incrementally, starting with explicit
  `implements` declarations and optional runtime dispatch via `class(ITrait)`.

---

## Non-Goals for This Document

- Full semantic conformance algorithm for method matching
- Code generation strategy for trait dictionaries/vtables
- Performance guarantees for dynamic dispatch

These belong in compiler implementation notes and follow-up design issues.

---

## References

- Issue #725: Add traits proposal specification to LFortran standard docs
- Issue #716: J3/Simpler/Traits distinction clarification
- lfortran/lfortran#1838: Procedure-level generics `{T}` syntax
- lfortran/lfortran#929: Generics tracking
- krystophny/lfortran#27: DI/OCP gap analysis motivating trait support
