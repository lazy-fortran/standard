# Language specification goals

Specify independently reviewable language behavior for accepted Lazy Fortran
features while preserving the meaning of existing standard Fortran. Apply
[goals and architectural freedom](https://github.com/lazy-fortran/fo/blob/main/doc/GOAL_DRIVEN_DEVELOPMENT.md).
Open proposal issues define desired outcomes and questions; candidate syntax,
representations and compiler architecture remain open until accepted.

## Required proposal outcomes

- Precise user-visible meaning, evaluation/side effects, errors and interaction
  with ordinary Fortran, with independently checkable positive/negative examples.
- Ownership/lifetime, interoperability, numerical and concurrency guarantees
  that are explicit and implementable for the claimed scope.
- Compatibility/versioning for accepted public contracts and named implementers
  when the proposal is ready to become an implementation obligation.
- A reference/desugaring/model where useful. A syntax sketch or compiler-produced
  output alone does not validate the promised semantics.

## Goal families

| Family | Issues |
| --- | --- |
| Useful compatible language/runtime capabilities | #734, #740, #741 |
| Strings, containers, traits and alternative values | #735–#738 |
| Lifetime, safe sharing, effects and explicit unsafe behavior | #739, #743, #747, #754 |
| Derivation, patterns and predictable staging | #742, #744, #752 |
| Arrays, units, layout and tensor notation | #745, #746, #749, #755 |
| Reproducible numerical behavior and differentiation | #748, #751 |
| Contracts and efficient separate-compilation identities | #750, #753 |
| Accepted scientific Synthesis semantics | #756 |

The open proposal set does not expand FFC's standard-Fortran completion scope
merely by existing. Accepted contracts become scoped implementation goals in
FortFront/FFC and their actual providers. Synthesis links #756, FortFront #2976
and FFC #632; it does not reintroduce a proof system into Fo.

## Delivery

Revise proposals when examples reveal missing meaning; choose architecture only
when implementation evidence requires it. Preserve authoritative standard and
already accepted syntax/semantics. Track provider-consumer compatibility without
prescribing migration steps or runtime layout in advance.

[FFC PLAN](https://github.com/lazy-fortran/ffc/blob/main/PLAN.md) owns compiler
delivery. [Fo #205](https://github.com/lazy-fortran/fo/issues/205) also covers
unnecessary proposal/planning/documentation volume: keep active descriptions
short and avoid repeating competing architecture narratives.

Historical proposal connections remain at
[the pre-revision roadmap](https://github.com/lazy-fortran/standard/blob/42aab99eec42a1fe67b5e5c87af72295dea04e60/ROADMAP.md).
