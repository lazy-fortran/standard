#!/usr/bin/env python3
"""
Test suite for the array-contracts reference evaluator (issue #745).

Validates the semantics specified in docs/arrays-proposal.md: shape/rank
contracts, named axes, no implicit broadcasting, explicit `broadcast(...)`,
static vs dynamic checking, and rank-polymorphic `shape(*)`.
"""

import sys
import unittest
from pathlib import Path

# Add tools directory to path
TOOLS_DIR = Path(__file__).resolve().parent.parent / "tools"
sys.path.insert(0, str(TOOLS_DIR))

from array_contracts_checker import (
    ShapeEnvironment,
    ShapeError,
    ShapeContract,
)


class TestShapeContractBasics(unittest.TestCase):
    """Basic rank and shape contract semantics."""

    def test_rank_of_shape(self):
        self.assertEqual(ShapeContract([3]).rank, 1)
        self.assertEqual(ShapeContract([2, 2]).rank, 2)
        self.assertEqual(ShapeContract(["particle", "xyz"]).rank, 2)

    def test_rank_polymorphic_shape_star(self):
        # shape(*) -> assumed rank
        self.assertIsNone(ShapeContract(["*"]).rank)

    def test_fixed_shape_ok(self):
        env = ShapeEnvironment()
        env.bind_and_check("x", ["n", 3], (100, 3))
        env.bind_and_check("v", ["n", 3], (100, 3))
        self.assertEqual(env.entities["x"].contract.rank, 2)

    def test_rank_mismatch_rejected(self):
        env = ShapeEnvironment()
        env.declare("a", [2, 2])
        env.declare("v", [2])
        with self.assertRaises(ShapeError) as ctx:
            env.check_static_assign(env.entities["a"], env.entities["v"].contract, "v")
        self.assertIn("rank mismatch", str(ctx.exception))

    def test_static_extent_mismatch_rejected(self):
        env = ShapeEnvironment()
        env.declare("a", [2, 2])
        env.declare("b", [3, 2])
        with self.assertRaises(ShapeError) as ctx:
            env.check_static_assign(env.entities["a"], env.entities["b"].contract, "b")
        self.assertIn("axis 1 has extent 3", str(ctx.exception))


class TestNamedAxes(unittest.TestCase):
    """Named axes / index-space contracts."""

    def test_named_axes_agree(self):
        env = ShapeEnvironment()
        env.bind_and_check("positions", ["particle", "xyz"], (1000, 3))
        env.bind_and_check("masses", ["particle"], (1000,))
        env.bind_and_check("forces", ["particle", "xyz"], (1000, 3))
        self.assertEqual(env.axis_bindings["particle"], 1000)
        self.assertEqual(env.axis_bindings["xyz"], 3)

    def test_named_axis_mismatch_rejected(self):
        env = ShapeEnvironment()
        env.bind_and_check("positions", ["particle", "xyz"], (1000, 3))
        with self.assertRaises(ShapeError) as ctx:
            env.bind_and_check("forces", ["particle", "xyz"], (1001, 3))
        self.assertIn("axis 'particle' mismatch", str(ctx.exception))


class TestBroadcasting(unittest.TestCase):
    """Broadcast semantics: explicit only, no implicit."""

    def setUp(self):
        self.env = ShapeEnvironment()
        self.env.bind_and_check("x", ["particle", "xyz"], (1000, 3))

    def test_no_implicit_broadcast(self):
        with self.assertRaises(ShapeError) as ctx:
            self.env.check_broadcast(self.env.entities["x"], (2,), "v", over=None)
        self.assertIn("no implicit broadcast", str(ctx.exception))

    def test_explicit_broadcast_over_axis(self):
        # rank-1 source replicated over the xyz axis
        self.env.check_broadcast(self.env.entities["x"], (3,), "v", over=2)
        # rank-1 source replicated over the particle axis
        self.env.check_broadcast(self.env.entities["x"], (1000,), "mass", over=1)

    def test_broadcast_scalar_noop(self):
        self.env.check_broadcast(self.env.entities["x"], (), "dt", over=None)

    def test_broadcast_bad_axis_rejected(self):
        with self.assertRaises(ShapeError) as ctx:
            self.env.check_broadcast(self.env.entities["x"], (3,), "v", over=3)
        self.assertIn("out of range", str(ctx.exception))

    def test_broadcast_extent_mismatch_rejected(self):
        with self.assertRaises(ShapeError) as ctx:
            self.env.check_broadcast(self.env.entities["x"], (4,), "v", over=2)
        self.assertIn("does not match", str(ctx.exception))


class TestDynamicRuntimeChecks(unittest.TestCase):
    """Dynamic extents checked at runtime."""

    def test_dynamic_extent_ok(self):
        env = ShapeEnvironment()
        env.bind_and_check("x", ["n", 3], (100, 3))

    def test_dynamic_extent_mismatch_rejected(self):
        env = ShapeEnvironment()
        with self.assertRaises(ShapeError) as ctx:
            env.bind_and_check("x", ["n", 3], (100, 4))
        self.assertIn("expected extent 3, actual extent 4", str(ctx.exception))


if __name__ == "__main__":
    unittest.main()
