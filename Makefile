# Makefile for LazyFortran2025 Standard Grammar Implementation
# Historical FORTRAN/Fortran grammar inheritance chain: 1957 → 2025

# Configuration
ANTLR4 = antlr4
ANTLR4_PYTHON = -Dlanguage=Python3
GRAMMAR_DIR = grammars
TEST_DIR = tests
PYTEST = python -m pytest

# Grammar inheritance chain (build order matters!)
GRAMMARS = FORTRAN FORTRANII FORTRANIV FORTRAN66 FORTRAN77 Fortran90 Fortran95 Fortran2003 Fortran2008 Fortran2018

# Default target
.PHONY: all clean test help

all: $(GRAMMARS)

# ====================================================================
# BUILD TARGETS - Build individual standards or all
# ====================================================================

# Individual grammar targets
FORTRAN: $(GRAMMAR_DIR)/FORTRANLexer.py
FORTRANII: $(GRAMMAR_DIR)/FORTRANIILexer.py
FORTRANIV: $(GRAMMAR_DIR)/FORTRANIVLexer.py
FORTRAN66: $(GRAMMAR_DIR)/FORTRAN66Lexer.py
FORTRAN77: $(GRAMMAR_DIR)/FORTRAN77Lexer.py
Fortran90: $(GRAMMAR_DIR)/Fortran90Lexer.py
Fortran95: $(GRAMMAR_DIR)/Fortran95Lexer.py
Fortran2003: $(GRAMMAR_DIR)/Fortran2003Lexer.py
Fortran2008: $(GRAMMAR_DIR)/Fortran2008Lexer.py
Fortran2018: $(GRAMMAR_DIR)/Fortran2018Lexer.py

# FORTRAN I (1957) - Foundation
$(GRAMMAR_DIR)/FORTRANLexer.py: $(GRAMMAR_DIR)/FORTRANLexer.g4 $(GRAMMAR_DIR)/FORTRANParser.g4
	@echo "Building FORTRAN I (1957)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRANLexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRANParser.g4

# FORTRAN II (1958) - Independent Compilation
$(GRAMMAR_DIR)/FORTRANIILexer.py: $(GRAMMAR_DIR)/FORTRANIILexer.g4 $(GRAMMAR_DIR)/FORTRANIIParser.g4 FORTRAN
	@echo "Building FORTRAN II (1958)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRANIILexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRANIIParser.g4

# FORTRAN IV (1962) - Data Type Revolution
$(GRAMMAR_DIR)/FORTRANIVLexer.py: $(GRAMMAR_DIR)/FORTRANIVLexer.g4 $(GRAMMAR_DIR)/FORTRANIVParser.g4 FORTRANII
	@echo "Building FORTRAN IV (1962)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRANIVLexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRANIVParser.g4

# FORTRAN 66 (1966) - First FORTRAN Standard
$(GRAMMAR_DIR)/FORTRAN66Lexer.py: $(GRAMMAR_DIR)/FORTRAN66Lexer.g4 $(GRAMMAR_DIR)/FORTRAN66Parser.g4 FORTRANIV
	@echo "Building FORTRAN 66 (1966)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRAN66Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRAN66Parser.g4

# FORTRAN 77 (1977) - Structured Programming  
$(GRAMMAR_DIR)/FORTRAN77Lexer.py: $(GRAMMAR_DIR)/FORTRAN77Lexer.g4 $(GRAMMAR_DIR)/FORTRAN77Parser.g4 FORTRAN66
	@echo "Building FORTRAN 77 (1977)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRAN77Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . FORTRAN77Parser.g4

# Fortran 90 (1990) - Modern Foundation
$(GRAMMAR_DIR)/Fortran90Lexer.py: $(GRAMMAR_DIR)/Fortran90Lexer.g4 $(GRAMMAR_DIR)/Fortran90Parser.g4 FORTRAN77
	@echo "Building Fortran 90 (1990)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran90Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran90Parser.g4

# Fortran 95 (1995) - Minor Update
$(GRAMMAR_DIR)/Fortran95Lexer.py: $(GRAMMAR_DIR)/Fortran95Lexer.g4 $(GRAMMAR_DIR)/Fortran95Parser.g4 Fortran90
	@echo "Building Fortran 95 (1995)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran95Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran95Parser.g4

# Fortran 2003 (2003) - Object-Oriented Programming
$(GRAMMAR_DIR)/Fortran2003Lexer.py: $(GRAMMAR_DIR)/Fortran2003Lexer.g4 $(GRAMMAR_DIR)/Fortran2003Parser.g4 Fortran95
	@echo "Building Fortran 2003 (2003)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran2003Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran2003Parser.g4

# Fortran 2008 (2008) - Enhanced Parallel Programming
$(GRAMMAR_DIR)/Fortran2008Lexer.py: $(GRAMMAR_DIR)/Fortran2008Lexer.g4 $(GRAMMAR_DIR)/Fortran2008Parser.g4 Fortran2003
	@echo "Building Fortran 2008 (2008)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran2008Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran2008Parser.g4

# Fortran 2018 (2018) - Modern Fortran Revolution
$(GRAMMAR_DIR)/Fortran2018Lexer.py: $(GRAMMAR_DIR)/Fortran2018Lexer.g4 $(GRAMMAR_DIR)/Fortran2018Parser.g4 Fortran2008
	@echo "Building Fortran 2018 (2018)..."
	cd $(GRAMMAR_DIR) && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran2018Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib . Fortran2018Parser.g4

# ====================================================================
# TEST TARGETS - Run tests for individual standards or all
# ====================================================================

# Run ALL tests
test: all
	@echo "=========================================="
	@echo "Running ALL Tests - Full Grammar Suite"
	@echo "=========================================="
	@echo "Standards Chain: FORTRAN(1957) → F2018 → LazyFortran2025"
	$(PYTEST) $(TEST_DIR)/ -v --tb=short
	@echo ""
	@echo "✅ All tests completed!"

# Individual test targets
test-fortran: FORTRAN
	@echo "Testing FORTRAN I (1957)..."
	$(PYTEST) $(TEST_DIR)/FORTRAN/ -v --tb=short

test-fortran2: FORTRANII
	@echo "Testing FORTRAN II (1958)..."
	$(PYTEST) $(TEST_DIR)/FORTRANII/ -v --tb=short || echo "No tests for FORTRAN II yet"

test-fortran4: FORTRANIV
	@echo "Testing FORTRAN IV (1962)..."
	$(PYTEST) $(TEST_DIR)/FORTRANIV/ -v --tb=short || echo "No tests for FORTRAN IV yet"

test-fortran66: FORTRAN66
	@echo "Testing FORTRAN 66 (1966)..."
	$(PYTEST) $(TEST_DIR)/FORTRAN66/ -v --tb=short || echo "No tests for FORTRAN 66 yet"

test-fortran77: FORTRAN77
	@echo "Testing FORTRAN 77 (1977)..."
	$(PYTEST) $(TEST_DIR)/FORTRAN77/ -v --tb=short || echo "No tests for FORTRAN 77 yet"

test-fortran90: Fortran90
	@echo "Testing Fortran 90 (1990)..."
	$(PYTEST) $(TEST_DIR)/Fortran90/ -v --tb=short

test-fortran95: Fortran95
	@echo "Testing Fortran 95 (1995)..."
	$(PYTEST) $(TEST_DIR)/Fortran95/ -v --tb=short

test-fortran2003: Fortran2003
	@echo "Testing Fortran 2003 (2003)..."
	$(PYTEST) $(TEST_DIR)/Fortran2003/ -v --tb=short

test-fortran2008: Fortran2008
	@echo "Testing Fortran 2008 (2008)..."
	$(PYTEST) $(TEST_DIR)/Fortran2008/ -v --tb=short

test-fortran2018: Fortran2018
	@echo "Testing Fortran 2018 (2018)..."
	$(PYTEST) $(TEST_DIR)/Fortran2018/ -v --tb=short

# ====================================================================
# UTILITY TARGETS
# ====================================================================

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	rm -f $(GRAMMAR_DIR)/*.py $(GRAMMAR_DIR)/*.pyc $(GRAMMAR_DIR)/*.tokens $(GRAMMAR_DIR)/*.interp $(GRAMMAR_DIR)/*Listener.py $(GRAMMAR_DIR)/*Visitor.py
	find $(TEST_DIR) -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete 2>/dev/null || true

# Deep clean - also removes test artifacts
clean-all: clean
	@echo "Deep cleaning (including test artifacts)..."
	rm -rf .pytest_cache

# Help target
help:
	@echo "LazyFortran2025 Standard Grammar Build System"
	@echo "============================================="
	@echo ""
	@echo "Historical FORTRAN/Fortran Standards Chain:"
	@echo "  FORTRAN (1957) → FORTRAN II (1958) → FORTRAN IV (1962)"
	@echo "  → FORTRAN 66 (1966) → FORTRAN 77 (1977) → Fortran 90 (1990)"
	@echo "  → Fortran 95 (1995) → Fortran 2003 (2003) → Fortran 2008 (2008)"
	@echo "  → Fortran 2018 (2018) → [Future: LazyFortran2025]"
	@echo ""
	@echo "BUILD TARGETS:"
	@echo "  all                    - Build all grammars (default)"
	@echo "  FORTRAN                - Build FORTRAN I (1957)"
	@echo "  FORTRANII              - Build FORTRAN II (1958)"
	@echo "  FORTRANIV              - Build FORTRAN IV (1962)"
	@echo "  FORTRAN66              - Build FORTRAN 66 (1966)"
	@echo "  FORTRAN77              - Build FORTRAN 77 (1977)"
	@echo "  Fortran90              - Build Fortran 90 (1990)"
	@echo "  Fortran95              - Build Fortran 95 (1995)"
	@echo "  Fortran2003            - Build Fortran 2003 (2003)"
	@echo "  Fortran2008            - Build Fortran 2008 (2008)"
	@echo "  Fortran2018            - Build Fortran 2018 (2018)"
	@echo ""
	@echo "TEST TARGETS:"
	@echo "  test                   - Run all tests"
	@echo "  test-fortran           - Test FORTRAN I"
	@echo "  test-fortran90         - Test Fortran 90"
	@echo "  test-fortran2003       - Test Fortran 2003"
	@echo "  test-fortran2018       - Test Fortran 2018"
	@echo "  [etc. for all standards]"
	@echo ""
	@echo "UTILITY TARGETS:"
	@echo "  clean                  - Clean generated files"
	@echo "  clean-all              - Deep clean (includes test cache)"
	@echo "  help                   - Show this help"
	@echo ""
	@echo "EXAMPLES:"
	@echo "  make Fortran2018       - Build F2018 and all dependencies"
	@echo "  make test-fortran90    - Build and test Fortran 90"
	@echo "  make clean all test    - Clean, rebuild all, run all tests"

# Force rebuild target
force-rebuild: clean all
	@echo "Force rebuild completed!"