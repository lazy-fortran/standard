#!/usr/bin/env python3
"""
Reference evaluator for the LFortran array-contracts proposal (issue #745).

This is the "desugared twin / small reference evaluator" required by the
roadmap verification rule for array proposals. It implements the *semantics*
specified in docs/arrays-proposal.md:

  - shape/rank contracts (`shape(...)` attribute): static and dynamic checks
  - named axes: same-name axes must share extent
  - broadcasting: no implicit broadcast; explicit `broadcast(src, over=axis)`
  - diagnostics: name entity, axis, expected extent, actual extent

It is a standalone model (Python), not tied to the ANTLR grammar, so it can run
in any environment. Each checkable example is paired with an invalid neighbor.

Usage:
    python3 tools/array_contracts_checker.py

Exits 0 when all checks pass, 1 otherwise. It is also importable so tests can
reuse the model.
"""

from __future__ import annotations

import sys
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Sequence, Tuple, Union


class ShapeError(RuntimeError):
    """Raised when a shape/rank/broadcast contract is violated."""


# A shape extent is either a concrete integer, a symbolic name bound later, or
# '*' meaning "any rank / any extent" (assumed-shape / rank-polymorphic).
Extent = Union[int, str]


@dataclass
class ShapeContract:
    """A parsed `shape(...)` annotation."""
    axes: List[Extent]

    @property
    def rank(self) -> Optional[int]:
        # '*' as the only entry means assumed rank -> unknown rank.
        if self.axes == ["*"]:
            return None
        if any(a == "*" for a in self.axes):
            # rank is known (count of entries) but at least one extent is open.
            return len(self.axes)
        return len(self.axes)

    def __str__(self) -> str:  # pragma: no cover - debugging aid
        return "shape(" + ", ".join(str(a) for a in self.axes) + ")"


@dataclass
class Entity:
    """A declared array entity with a shape contract."""
    name: str
    contract: ShapeContract
    # Actual extents, bound at allocation/assignment time. None until bound.
    extent: Optional[Tuple[int, ...]] = None

    def bind(self, extent: Tuple[int, ...]) -> None:
        self.extent = extent


class ShapeEnvironment:
    """Tracks declared shape contracts and named-axis bindings in one scope.

    Shape metadata is immutable for the lifetime of the scope unless an entity
    is explicitly re-bound (reallocation), which is always an explicit event.
    """

    def __init__(self) -> None:
        self.entities: Dict[str, Entity] = {}
        self.axis_bindings: Dict[str, int] = {}

    # -- declaration -------------------------------------------------------
    def declare(self, name: str, axes: Sequence[Extent]) -> Entity:
        entity = Entity(name, ShapeContract(list(axes)))
        self.entities[name] = entity
        return entity

    # -- static checking ----------------------------------------------------
    def check_static_assign(self, dest: Entity, src_shape: ShapeContract,
                            src_name: str) -> None:
        """Compile-time conformance check between two static shapes."""
        dr, sr = dest.contract.rank, src_shape.rank
        if dr is not None and sr is not None and dr != sr:
            raise ShapeError(
                f"rank mismatch for '{src_name}' in assignment to '{dest.name}': "
                f"shape of '{src_name}' is rank {sr} ({src_shape}), "
                f"shape of '{dest.name}' is rank {dr} ({dest.contract})"
            )
        # Extent checks where both sides are constants.
        for i, (d, s) in enumerate(zip(dest.contract.axes, src_shape.axes)):
            if isinstance(d, int) and isinstance(s, int) and d != s:
                raise ShapeError(
                    f"shape mismatch: '{src_name}' axis {i + 1} has extent {s}, "
                    f"'{dest.name}' requires extent {d}"
                )

    # -- runtime checking ---------------------------------------------------
    def bind_and_check(self, name: str, axes: Sequence[Extent],
                       extent: Tuple[int, ...]) -> None:
        """Bind an entity to an actual extent and run the shape contract."""
        entity = self.declare(name, axes)
        entity.bind(extent)
        contract = entity.contract
        if contract.rank is not None and len(extent) != contract.rank:
            raise ShapeError(
                f"rank mismatch for '{name}': expected rank {contract.rank} "
                f"({contract}), got rank {len(extent)}"
            )
        for i, ax in enumerate(contract.axes):
            if isinstance(ax, int):
                if extent[i] != ax:
                    raise ShapeError(
                        f"shape contract violated for '{name}' (axis {i + 1}): "
                        f"expected extent {ax}, actual extent {extent[i]}"
                    )
            elif isinstance(ax, str) and ax != "*":
                # named axis: enforce same-name axis agreement
                if ax in self.axis_bindings:
                    if self.axis_bindings[ax] != extent[i]:
                        raise ShapeError(
                            f"axis '{ax}' mismatch for '{name}': "
                            f"expected extent {self.axis_bindings[ax]} (from "
                            f"previous binding), actual extent {extent[i]}"
                        )
                else:
                    self.axis_bindings[ax] = extent[i]

    # -- broadcasting --------------------------------------------------------
    def check_broadcast(self, dest: Entity, src_extent: Tuple[int, ...],
                        src_name: str, over: Optional[int]) -> None:
        """Check that src conforms to dest after an explicit broadcast.

        ``over`` is the 1-based axis of ``dest`` along which a rank-1 source is
        replicated. Without a scalar/rank-1 broadcast, shapes must already
        conform.
        """
        if dest.extent is None:
            raise ShapeError(f"destination '{dest.name}' has no bound extent")
        de = dest.extent
        # Scalar (empty extent) broadcasts trivially.
        if not src_extent:
            return
        if len(src_extent) == 1:
            if over is None:
                # rank-1 source must already match dest if no axis is named
                if len(de) != 1 or de[0] != src_extent[0]:
                    raise ShapeError(
                        f"no implicit broadcast: '{src_name}' shape "
                        f"{src_extent} does not conform to '{dest.name}' shape "
                        f"{de}; add broadcast(..., over=axis)"
                    )
                return
            axis = over - 1
            if axis < 0 or axis >= len(de):
                raise ShapeError(
                    f"broadcast axis {over} out of range for '{dest.name}' "
                    f"rank {len(de)}"
                )
            # The rank-1 source must match the dest extent along the 'over'
            # axis; it is then replicated over every other axis.
            if de[axis] != src_extent[0]:
                raise ShapeError(
                    f"broadcast: '{src_name}' extent {src_extent[0]} along "
                    f"'over' axis {over} does not match '{dest.name}' extent "
                    f"{de[axis]}"
                )
            return
        # multi-rank source: must conform without broadcasting
        if src_extent != de:
            raise ShapeError(
                f"no implicit broadcast: '{src_name}' shape {src_extent} does "
                f"not conform to '{dest.name}' shape {de}"
            )


# ---------------------------------------------------------------------------
# Examples from docs/arrays-proposal.md, each with an invalid neighbor.
# ---------------------------------------------------------------------------

def example_fixed_shape_ok() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("x", ["n", 3], (100, 3))
    env.bind_and_check("v", ["n", 3], (100, 3))
    assert env.entities["x"].contract.rank == 2


def example_fixed_shape_rank_mismatch() -> None:
    env = ShapeEnvironment()
    env.declare("a", [2, 2])
    env.declare("v", [2])
    try:
        env.check_static_assign(env.entities["a"], env.entities["v"].contract, "v")
        raise AssertionError("expected rank mismatch")
    except ShapeError as e:
        assert "rank mismatch" in str(e)


def example_named_axes_ok() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("positions", ["particle", "xyz"], (1000, 3))
    env.bind_and_check("masses", ["particle"], (1000,))
    env.bind_and_check("forces", ["particle", "xyz"], (1000, 3))
    assert env.axis_bindings["particle"] == 1000
    assert env.axis_bindings["xyz"] == 3


def example_named_axes_mismatch() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("positions", ["particle", "xyz"], (1000, 3))
    try:
        env.bind_and_check("forces", ["particle", "xyz"], (1001, 3))
        raise AssertionError("expected axis mismatch")
    except ShapeError as e:
        assert "axis 'particle' mismatch" in str(e)


def example_no_implicit_broadcast() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("a", [2, 2], (2, 2))
    env.bind_and_check("v", [2], (2,))
    try:
        env.check_broadcast(env.entities["a"], (2,), "v", over=None)
        raise AssertionError("expected no-implicit-broadcast error")
    except ShapeError as e:
        assert "no implicit broadcast" in str(e)


def example_explicit_broadcast_ok() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("x", ["particle", "xyz"], (1000, 3))
    # rank-1 source replicated over axis 2 (xyz)
    env.check_broadcast(env.entities["x"], (3,), "v", over=2)
    # rank-1 source replicated over axis 1 (particle)
    env.bind_and_check("masses", ["particle"], (1000,))
    env.check_broadcast(env.entities["x"], (1000,), "masses", over=1)


def example_explicit_broadcast_bad_axis() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("x", ["particle", "xyz"], (1000, 3))
    try:
        env.check_broadcast(env.entities["x"], (3,), "v", over=3)
        raise AssertionError("expected out-of-range axis error")
    except ShapeError as e:
        assert "out of range" in str(e)


def example_dynamic_extent_runtime_check() -> None:
    env = ShapeEnvironment()
    env.bind_and_check("x", ["n", 3], (100, 3))   # ok
    try:
        env.bind_and_check("x2", ["n", 3], (100, 4))  # dynamic extent 4 != 3
        raise AssertionError("expected runtime extent error")
    except ShapeError as e:
        assert "expected extent 3, actual extent 4" in str(e)


def example_rank_polymorphic() -> None:
    # shape(*) accepts any rank
    env = ShapeEnvironment()
    env.bind_and_check("any_rank", ["*"], (2, 2, 2))
    env.bind_and_check("any_rank2", ["*"], (5,))
    assert env.entities["any_rank"].contract.rank is None


CHECKS = [
    ("fixed-shape contract ok", example_fixed_shape_ok),
    ("rank mismatch rejected", example_fixed_shape_rank_mismatch),
    ("named axes agree", example_named_axes_ok),
    ("named axis mismatch rejected", example_named_axes_mismatch),
    ("no implicit broadcast", example_no_implicit_broadcast),
    ("explicit broadcast ok", example_explicit_broadcast_ok),
    ("explicit broadcast bad axis", example_explicit_broadcast_bad_axis),
    ("dynamic extent runtime check", example_dynamic_extent_runtime_check),
    ("rank-polymorphic shape(*)", example_rank_polymorphic),
]


def main() -> int:
    failed = 0
    for name, fn in CHECKS:
        try:
            fn()
            print(f"PASS  {name}")
        except AssertionError as e:
            failed += 1
            print(f"FAIL  {name}: {e}")
        except ShapeError as e:  # unexpected
            failed += 1
            print(f"FAIL  {name}: unexpected ShapeError: {e}")
    print(f"\n{len(CHECKS) - failed}/{len(CHECKS)} checks passed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
