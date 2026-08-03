# Lazy Fortran standard roadmap

The standard repository specifies language, runtime, ownership, layout, and
reproducibility contracts. It is not an implementation backlog: accepted
proposals are mapped to atomic issues in FortFront, ffc, LIRIC, fo, or other
implementing repositories.

## Current handoff (2026-08-03)

- `main` is `b432327571835e0fa10e51ad2211dabfcdcc86b7`.
- Active proposals relevant to the compiler architecture include
  [#745](https://github.com/lazy-fortran/standard/issues/745) for shape/rank
  and checked broadcasting, [#753](https://github.com/lazy-fortran/standard/issues/753)
  for stable module signatures, and [#756](https://github.com/lazy-fortran/standard/issues/756)
  for Fortran Synthesis contracts.
- These proposals remain specification inputs until accepted. Do not silently
  change ffc's descriptor, module-artifact, or synthesis ABI to anticipate an
  unaccepted proposal.

## Implementer links

- Array and broadcasting decisions feed ffc [#337](https://github.com/lazy-fortran/ffc/issues/337),
  [#338](https://github.com/lazy-fortran/ffc/issues/338), and [#339](https://github.com/lazy-fortran/ffc/issues/339).
- Stable module signatures feed ffc [#297](https://github.com/lazy-fortran/ffc/issues/297),
  [#414](https://github.com/lazy-fortran/ffc/issues/414), and [#415](https://github.com/lazy-fortran/ffc/issues/415).
- Synthesis contracts feed FortFront and fo only after the accepted syntax and
  proof obligations are split into independently verifiable implementation
  issues.

## Delivery gate

Every accepted contract needs normative text, positive and negative examples,
an implementation issue with an owner, and an independent behavioral oracle.
