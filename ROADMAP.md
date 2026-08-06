# Lazy Fortran standard roadmap

Snapshot: 2026-08-06. This repository specifies language, runtime, ownership,
layout, and reproducibility contracts. It is not an implementation backlog.
An accepted proposal is mapped to small issues in the repositories that
produce and consume its public contract.

The audited baseline is `e4183d2`. All proposals #734 through #756 remain open. They do
not expand ffc's current completion denominator merely because they exist.

## Compiler-critical proposals

- [#745](https://github.com/lazy-fortran/standard/issues/745): shape/rank types,
  checked broadcasting, and array contracts. Its accepted representation will
  map to FortFront typed queries and ffc's one canonical descriptor/expression
  model.
- [#753](https://github.com/lazy-fortran/standard/issues/753): stable module
  interface signatures. It must specify identity, schema versioning,
  compatibility, target/runtime dependence, and invalidation before ffc
  changes its published `.fmod` contract.
- [#756](https://github.com/lazy-fortran/standard/issues/756): Fortran
  Synthesis, contracts, proof obligations, and verified generation. The
  implementation chain is FortFront #2976, ffc #632, then fo #120 only after
  normative syntax and semantics are accepted.

Related design inputs are exact strings #735, ownership/lifetimes #739,
runtime extraction #740, reproducibility #748, layout #749, compile-time
staging #752, and unsafe interop #754. They inform architecture discussions
but cannot silently change standard-Fortran behavior or an existing ABI.

## Proposal-to-implementation rule

Every accepted contract includes:

- normative syntax and semantics, including evaluation order, side effects,
  errors, ownership/lifetime, and interaction with standard Fortran.
- positive, negative, boundary, and cross-feature examples.
- a desugaring or independent reference model where possible.
- compatibility and versioning rules for serialized/runtime contracts.
- named producer and consumer repositories.
- atomic implementation issues with independent behavioral oracles.

Implementation order for a breaking cross-repository contract is additive
provider API, migrated consumers, default switch, then prompt deletion of the
old path. Temporary dual-mode comparison belongs in tests, not two permanent
production semantics.

## Verification

Examples must be executable or mechanically checkable. A syntax example alone
does not validate semantics. Array, ownership, staging, and reproducibility
proposals need a small reference evaluator or standard-Fortran desugared twin,
plus invalid neighbors. Module/runtime proposals need separate
producer-consumer compile/link/run examples and incompatible-version rejection.

When a proposal changes, update its implementer links. Do not copy live corpus
counts or issue status here. The
[ffc roadmap](https://github.com/lazy-fortran/ffc/blob/main/ROADMAP.md) owns the
current compiler convergence plan.
