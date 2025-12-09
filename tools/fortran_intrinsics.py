#!/usr/bin/env python3
"""Fortran Intrinsic Procedure Names

Provides centralized sets of Fortran intrinsic procedure names per
ISO/IEC 1539-1:2010 (Fortran 2008) Section 13 (Intrinsic Procedures).

This module eliminates duplication of intrinsic name lists across validators
and provides a single source of truth for intrinsic detection in DO CONCURRENT
and other semantic analysis contexts.

Reference: ISO/IEC 1539-1:2010 (Fortran 2008 International Standard), Chapter 13
"""

from typing import FrozenSet

FORTRAN_TYPE_KEYWORDS: FrozenSet[str] = frozenset({
    "real", "integer", "logical", "character", "complex", "double", "precision",
})

FORTRAN_2008_NUMERIC_INTRINSICS: FrozenSet[str] = frozenset({
    "abs", "aimag", "aint", "anint", "ceiling", "cmplx", "conjg", "dble",
    "dim", "dprod", "floor", "int", "max", "min", "mod", "modulo", "nint",
    "real", "sign",
})

FORTRAN_2008_MATH_INTRINSICS: FrozenSet[str] = frozenset({
    "acos", "asin", "atan", "atan2", "cos", "cosh", "exp", "log", "log10",
    "sin", "sinh", "sqrt", "tan", "tanh",
    "acosh", "asinh", "atanh", "bessel_j0", "bessel_j1", "bessel_jn",
    "bessel_y0", "bessel_y1", "bessel_yn", "erf", "erfc", "erfc_scaled",
    "gamma", "hypot", "log_gamma", "norm2",
})

FORTRAN_2008_CHARACTER_INTRINSICS: FrozenSet[str] = frozenset({
    "achar", "adjustl", "adjustr", "char", "iachar", "ichar", "index",
    "len", "len_trim", "lge", "lgt", "lle", "llt", "repeat", "scan",
    "trim", "verify", "new_line",
})

FORTRAN_2008_KIND_INTRINSICS: FrozenSet[str] = frozenset({
    "kind", "selected_char_kind", "selected_int_kind", "selected_real_kind",
})

FORTRAN_2008_MISC_TYPE_INQUIRY: FrozenSet[str] = frozenset({
    "bit_size", "digits", "epsilon", "huge", "maxexponent", "minexponent",
    "precision", "radix", "range", "tiny", "storage_size",
})

FORTRAN_2008_ARRAY_INTRINSICS: FrozenSet[str] = frozenset({
    "all", "any", "count", "cshift", "dot_product", "eoshift", "lbound",
    "matmul", "maxloc", "maxval", "merge", "minloc", "minval", "pack",
    "product", "reshape", "shape", "size", "spread", "sum", "transpose",
    "ubound", "unpack", "findloc", "is_contiguous",
})

FORTRAN_2008_POINTER_INTRINSICS: FrozenSet[str] = frozenset({
    "allocated", "associated", "extends_type_of", "same_type_as",
    "c_associated", "c_funloc", "c_loc", "c_sizeof", "c_f_pointer",
    "c_f_procpointer", "move_alloc",
})

FORTRAN_2008_BIT_INTRINSICS: FrozenSet[str] = frozenset({
    "bge", "bgt", "ble", "blt", "dshiftl", "dshiftr", "iall", "iand",
    "iany", "ibclr", "ibits", "ibset", "ieor", "ior", "ishft", "ishftc",
    "leadz", "maskl", "maskr", "merge_bits", "mvbits", "not", "popcnt",
    "poppar", "shifta", "shiftl", "shiftr", "trailz",
})

FORTRAN_2008_INQUIRY_INTRINSICS: FrozenSet[str] = frozenset({
    "command_argument_count", "present", "is_iostat_end", "is_iostat_eor",
})

FORTRAN_2008_COARRAY_INTRINSICS: FrozenSet[str] = frozenset({
    "lcobound", "ucobound", "image_index", "num_images", "this_image",
})

FORTRAN_2008_TRANSFER_INTRINSICS: FrozenSet[str] = frozenset({
    "transfer",
})

FORTRAN_2008_SYSTEM_INTRINSICS: FrozenSet[str] = frozenset({
    "cpu_time", "date_and_time", "execute_command_line", "get_command",
    "get_command_argument", "get_environment_variable", "random_number",
    "random_seed", "system_clock",
})

FORTRAN_2008_NULL_INTRINSIC: FrozenSet[str] = frozenset({
    "null",
})

FORTRAN_2008_ALL_INTRINSICS: FrozenSet[str] = (
    FORTRAN_2008_NUMERIC_INTRINSICS |
    FORTRAN_2008_MATH_INTRINSICS |
    FORTRAN_2008_CHARACTER_INTRINSICS |
    FORTRAN_2008_KIND_INTRINSICS |
    FORTRAN_2008_MISC_TYPE_INQUIRY |
    FORTRAN_2008_ARRAY_INTRINSICS |
    FORTRAN_2008_POINTER_INTRINSICS |
    FORTRAN_2008_BIT_INTRINSICS |
    FORTRAN_2008_INQUIRY_INTRINSICS |
    FORTRAN_2008_COARRAY_INTRINSICS |
    FORTRAN_2008_TRANSFER_INTRINSICS |
    FORTRAN_2008_SYSTEM_INTRINSICS |
    FORTRAN_2008_NULL_INTRINSIC
)

FORTRAN_KEYWORDS_AND_INTRINSICS: FrozenSet[str] = (
    FORTRAN_TYPE_KEYWORDS | FORTRAN_2008_ALL_INTRINSICS
)
