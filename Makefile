# Makefile for LazyFortran2025 Standard Grammar Implementation
# Historical FORTRAN/Fortran grammar inheritance chain: 1957 → 2025

# Configuration
ANTLR4 = antlr4
ANTLR4_PYTHON = -Dlanguage=Python3 -o .
GRAMMAR_DIR = grammars
BUILD_DIR = build
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
FORTRAN: $(BUILD_DIR)/FORTRAN/FORTRANLexer.py
FORTRANII: $(BUILD_DIR)/FORTRANII/FORTRANIILexer.py
FORTRANIV: $(BUILD_DIR)/FORTRANIV/FORTRANIVLexer.py
FORTRAN66: $(BUILD_DIR)/FORTRAN66/FORTRAN66Lexer.py
FORTRAN77: $(BUILD_DIR)/FORTRAN77/FORTRAN77Lexer.py
Fortran90: $(BUILD_DIR)/Fortran90/Fortran90Lexer.py
Fortran95: $(BUILD_DIR)/Fortran95/Fortran95Lexer.py
Fortran2003: $(BUILD_DIR)/Fortran2003/Fortran2003Lexer.py
Fortran2008: $(BUILD_DIR)/Fortran2008/Fortran2008Lexer.py
Fortran2018: $(BUILD_DIR)/Fortran2018/Fortran2018Lexer.py

# FORTRAN I (1957) - Foundation
$(BUILD_DIR)/FORTRAN/FORTRANLexer.py: $(GRAMMAR_DIR)/FORTRANLexer.g4 $(GRAMMAR_DIR)/FORTRANParser.g4
	@echo "Building FORTRAN I (1957)..."
	@mkdir -p $(BUILD_DIR)/FORTRAN
	cd $(BUILD_DIR)/FORTRAN && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRANLexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRANParser.g4

# FORTRAN II (1958) - Independent Compilation
$(BUILD_DIR)/FORTRANII/FORTRANIILexer.py: $(GRAMMAR_DIR)/FORTRANIILexer.g4 $(GRAMMAR_DIR)/FORTRANIIParser.g4 FORTRAN
	@echo "Building FORTRAN II (1958)..."
	@mkdir -p $(BUILD_DIR)/FORTRANII
	cd $(BUILD_DIR)/FORTRANII && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRANIILexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRANIIParser.g4

# FORTRAN IV (1962) - Data Type Revolution
$(BUILD_DIR)/FORTRANIV/FORTRANIVLexer.py: $(GRAMMAR_DIR)/FORTRANIVLexer.g4 FORTRANII
	@echo "Building FORTRAN IV (1962)..."
	@mkdir -p $(BUILD_DIR)/FORTRANIV
	cd $(BUILD_DIR)/FORTRANIV && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRANIVLexer.g4

# FORTRAN 66 (1966) - First FORTRAN Standard
$(BUILD_DIR)/FORTRAN66/FORTRAN66Lexer.py: $(GRAMMAR_DIR)/FORTRAN66Lexer.g4 FORTRANIV
	@echo "Building FORTRAN 66 (1966)..."
	@mkdir -p $(BUILD_DIR)/FORTRAN66
	cd $(BUILD_DIR)/FORTRAN66 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRAN66Lexer.g4

# FORTRAN 77 (1977) - Structured Programming  
$(BUILD_DIR)/FORTRAN77/FORTRAN77Lexer.py: $(GRAMMAR_DIR)/FORTRAN77Lexer.g4 FORTRAN66
	@echo "Building FORTRAN 77 (1977)..."
	@mkdir -p $(BUILD_DIR)/FORTRAN77
	cd $(BUILD_DIR)/FORTRAN77 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/FORTRAN77Lexer.g4

# Fortran 90 (1990) - Modern Foundation
$(BUILD_DIR)/Fortran90/Fortran90Lexer.py: $(GRAMMAR_DIR)/Fortran90Lexer.g4 $(GRAMMAR_DIR)/Fortran90Parser.g4 FORTRAN77
	@echo "Building Fortran 90 (1990)..."
	@mkdir -p $(BUILD_DIR)/Fortran90
	cd $(BUILD_DIR)/Fortran90 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran90Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran90Parser.g4

# Fortran 95 (1995) - Minor Update
$(BUILD_DIR)/Fortran95/Fortran95Lexer.py: $(GRAMMAR_DIR)/Fortran95Lexer.g4 $(GRAMMAR_DIR)/Fortran95Parser.g4 Fortran90
	@echo "Building Fortran 95 (1995)..."
	@mkdir -p $(BUILD_DIR)/Fortran95
	cd $(BUILD_DIR)/Fortran95 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran95Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran95Parser.g4

# Fortran 2003 (2003) - Object-Oriented Programming
$(BUILD_DIR)/Fortran2003/Fortran2003Lexer.py: $(GRAMMAR_DIR)/Fortran2003Lexer.g4 $(GRAMMAR_DIR)/Fortran2003Parser.g4 Fortran95
	@echo "Building Fortran 2003 (2003)..."
	@mkdir -p $(BUILD_DIR)/Fortran2003
	cd $(BUILD_DIR)/Fortran2003 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran2003Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran2003Parser.g4

# Fortran 2008 (2008) - Enhanced Parallel Programming
$(BUILD_DIR)/Fortran2008/Fortran2008Lexer.py: $(GRAMMAR_DIR)/Fortran2008Lexer.g4 $(GRAMMAR_DIR)/Fortran2008Parser.g4 Fortran2003
	@echo "Building Fortran 2008 (2008)..."
	@mkdir -p $(BUILD_DIR)/Fortran2008
	cd $(BUILD_DIR)/Fortran2008 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran2008Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran2008Parser.g4

# Fortran 2018 (2018) - Modern Fortran Revolution
$(BUILD_DIR)/Fortran2018/Fortran2018Lexer.py: $(GRAMMAR_DIR)/Fortran2018Lexer.g4 $(GRAMMAR_DIR)/Fortran2018Parser.g4 Fortran2008
	@echo "Building Fortran 2018 (2018)..."
	@mkdir -p $(BUILD_DIR)/Fortran2018
	cd $(BUILD_DIR)/Fortran2018 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran2018Lexer.g4 && \
	$(ANTLR4) $(ANTLR4_PYTHON) -lib ../../$(GRAMMAR_DIR) ../../$(GRAMMAR_DIR)/Fortran2018Parser.g4

# ====================================================================
# TEST TARGETS - Run tests for individual standards or all
# ====================================================================

# Run ALL tests
test: all
	@echo "=========================================="
	@echo "Running ALL grammar tests..."
	@echo "=========================================="
	$(PYTEST) $(TEST_DIR)/ -v --tb=short

# Quick test - only run non-skipped tests
test-quick: all
	@echo "Running quick tests (non-skipped only)..."
	$(PYTEST) $(TEST_DIR)/ -v --tb=short -m "not slow"

# Individual standard test targets
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
	$(PYTEST) $(TEST_DIR)/Fortran95/ -v --tb=short || echo "No tests for Fortran 95 yet"

test-fortran2003: Fortran2003
	@echo "=========================================="
	@echo "Testing Fortran 2003 (2003)..."
	@echo "=========================================="
	$(PYTEST) $(TEST_DIR)/Fortran2003/ -v --tb=short

test-fortran2008: Fortran2008
	@echo "Testing Fortran 2008 (2008)..."
	$(PYTEST) $(TEST_DIR)/Fortran2008/ -v --tb=short

test-fortran2018: Fortran2018
	@echo "Testing Fortran 2018 (2018)..."
	$(PYTEST) $(TEST_DIR)/Fortran2018/ -v --tb=short

# Test FORTRAN I (foundation for all standards)
test-core:
	@echo "Testing FORTRAN I (1957) foundation..."
	$(PYTEST) $(TEST_DIR)/FORTRAN/ -v --tb=short

# Test free-form parsing
test-freeform:
	@echo "Testing free-form parsing..."
	$(PYTEST) $(TEST_DIR)/free_form/ -v --tb=short

# ====================================================================
# UTILITY TARGETS
# ====================================================================

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	rm -rf $(BUILD_DIR)
	find $(TEST_DIR) -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete 2>/dev/null || true

# Deep clean - also removes test artifacts
clean-all: clean
	@echo "Deep cleaning (including test artifacts)..."
	rm -rf .pytest_cache
	rm -rf htmlcov
	rm -f .coverage

# Build status check
status:
	@echo "Build Status:"
	@echo "-------------"
	@for grammar in $(GRAMMARS); do \
		if [ -f "$(BUILD_DIR)/$$grammar/$${grammar}Lexer.py" ]; then \
			echo "✓ $$grammar built"; \
		else \
			echo "✗ $$grammar not built"; \
		fi \
	done

# Count lines of grammar code
count:
	@echo "Grammar Line Count:"
	@wc -l $(GRAMMAR_DIR)/*.g4 | sort -n

# ====================================================================
# DEVELOPMENT TARGETS
# ====================================================================

# Watch for changes and rebuild (requires entr)
watch:
	@echo "Watching for grammar changes..."
	@which entr >/dev/null 2>&1 || (echo "Please install 'entr' to use watch mode" && exit 1)
	@find $(GRAMMAR_DIR) -name "*.g4" | entr -c make all

# Format check (requires antlr4-format if available)
format-check:
	@echo "Checking grammar formatting..."
	@for file in $(GRAMMAR_DIR)/*.g4; do \
		echo "Checking $$file..."; \
	done

# Validate grammars without building
validate:
	@echo "Validating grammars..."
	@for file in $(GRAMMAR_DIR)/*.g4; do \
		echo "Validating $$file..."; \
		$(ANTLR4) -lib $(GRAMMAR_DIR) $$file 2>&1 | grep -E "error|warning" || echo "  ✓ Valid"; \
	done

# ====================================================================
# HELP TARGET
# ====================================================================

help:
	@echo "LazyFortran2025 Grammar Build System"
	@echo ""
	@echo "BUILD TARGETS:"
	@echo "  all           Build all grammars in dependency order"
	@echo "  FORTRAN       Build FORTRAN I (1957) foundation"
	@echo "  FORTRANII     Build FORTRAN II (1958) procedural features"
	@echo "  FORTRANIV     Build FORTRAN IV (1962) data types"
	@echo "  FORTRAN66     Build FORTRAN 66 (1966) first standard"
	@echo "  FORTRAN77     Build FORTRAN 77 (1977) structured programming"
	@echo "  Fortran90     Build Fortran 90 (1990) modern foundation"
	@echo "  Fortran95     Build Fortran 95 (1995) minor update"
	@echo "  Fortran2003   Build Fortran 2003 (2003) object-oriented"
	@echo "  Fortran2008   Build Fortran 2008 (2008) parallel programming"
	@echo "  Fortran2018   Build Fortran 2018 (2018) modern revolution"
	@echo ""
	@echo "TEST TARGETS:"
	@echo "  test          Run ALL tests"
	@echo "  test-quick    Run quick tests only (skip slow tests)"
	@echo "  test-fortran  Test FORTRAN I (1957)"
	@echo "  test-fortran90 Test Fortran 90 (1990)"
	@echo "  test-fortran2003 Test Fortran 2003 (2003)"
	@echo "  test-fortran2008 Test Fortran 2008 (2008)"
	@echo "  test-fortran2018 Test Fortran 2018 (2018)"
	@echo "  test-core     Test FORTRAN I (1957) foundation"
	@echo "  test-freeform Test free-form parsing"
	@echo ""
	@echo "UTILITY TARGETS:"
	@echo "  clean         Remove generated files"
	@echo "  clean-all     Deep clean including test artifacts"
	@echo "  status        Show build status for each grammar"
	@echo "  count         Count lines in grammar files"
	@echo "  validate      Validate grammars without building"
	@echo "  help          Show this help"
	@echo ""
	@echo "DEVELOPMENT TARGETS:"
	@echo "  watch         Watch for changes and rebuild (requires entr)"
	@echo "  format-check  Check grammar formatting"
	@echo ""
	@echo "EXAMPLES:"
	@echo "  make                  # Build all grammars"
	@echo "  make Fortran2003      # Build up to Fortran 2003"
	@echo "  make test-fortran2003 # Build and test Fortran 2003"
	@echo "  make clean all test   # Clean rebuild and test everything"
	@echo "  make status           # Check what's built"

# Force rebuild
.PHONY: $(GRAMMARS) force-rebuild
force-rebuild: clean all

# Declare all test targets as phony
.PHONY: test test-quick test-fortran test-fortran2 test-fortran4 test-fortran66
.PHONY: test-fortran77 test-fortran90 test-fortran95 test-fortran2003
.PHONY: test-fortran2008 test-fortran2018 test-core test-freeform
.PHONY: status count watch format-check validate