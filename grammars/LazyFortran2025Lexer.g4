lexer grammar LazyFortran2025Lexer;

import Fortran2023Lexer;

// LazyFortran2025 extends Fortran2023 with syntactic relaxations
// Most features are semantic/compiler-level, so lexer remains largely unchanged
// Type inference and implicit none default are handled at semantic phase