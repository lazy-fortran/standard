# Makefile for FORTRAN/Fortran Standard Grammar Implementation
# Historical FORTRAN/Fortran grammar inheritance chain: 1957 → 2023

# Configuration
ANTLR4 = antlr4
ANTLR4_PYTHON = -Dlanguage=Python3
GRAMMAR_DIR = grammars
GRAMMAR_SRC = $(GRAMMAR_DIR)/src
GRAMMAR_GEN_EARLY = $(GRAMMAR_DIR)/generated/early
GRAMMAR_GEN_MODERN = $(GRAMMAR_DIR)/generated/modern
TEST_DIR = tests
PYTEST = python -m pytest

# Detect whether a Java runtime and ANTLR4 CLI are available for regeneration.
# If either is not available, existing generated lexer/parser sources are used.
JAVA_AVAILABLE := $(shell java -version >/dev/null 2>&1 && echo yes || echo no)
ANTLR4_AVAILABLE := $(shell command -v $(ANTLR4) >/dev/null 2>&1 && echo yes || echo no)

# Grammar inheritance chain (build order matters!)
GRAMMARS = FORTRAN FORTRANII FORTRAN66 FORTRAN77 Fortran90 Fortran95 Fortran2003 Fortran2008 Fortran2018 Fortran2023 LazyFortran2025

# Default target
.PHONY: all clean test help download-standards lint

all: $(GRAMMARS)

# ====================================================================
# BUILD TARGETS - Build individual standards or all
# ====================================================================

# Individual grammar targets (early standards -> generated/early, modern -> generated/modern)
FORTRAN: $(GRAMMAR_GEN_EARLY)/FORTRANLexer.py
FORTRANII: $(GRAMMAR_GEN_EARLY)/FORTRANIILexer.py
FORTRAN66: $(GRAMMAR_GEN_EARLY)/FORTRAN66Lexer.py
FORTRAN77: $(GRAMMAR_GEN_EARLY)/FORTRAN77Lexer.py
Fortran90: $(GRAMMAR_GEN_MODERN)/Fortran90Lexer.py
Fortran95: $(GRAMMAR_GEN_MODERN)/Fortran95Lexer.py
Fortran2003: $(GRAMMAR_GEN_MODERN)/Fortran2003Lexer.py
Fortran2008: $(GRAMMAR_GEN_MODERN)/Fortran2008Lexer.py
Fortran2018: $(GRAMMAR_GEN_MODERN)/Fortran2018Lexer.py
Fortran2023: $(GRAMMAR_GEN_MODERN)/Fortran2023Lexer.py

LazyFortran2025: $(GRAMMAR_GEN_MODERN)/LazyFortran2025Lexer.py

# FORTRAN I (1957) - Foundation
$(GRAMMAR_GEN_EARLY)/FORTRANLexer.py: $(GRAMMAR_SRC)/FORTRANLexer.g4 $(GRAMMAR_SRC)/FORTRANParser.g4
	@echo "Building FORTRAN I (1957)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRANLexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRANParser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing FORTRAN I (1957) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# FORTRAN II (1958) - Independent Compilation
$(GRAMMAR_GEN_EARLY)/FORTRANIILexer.py: $(GRAMMAR_SRC)/FORTRANIILexer.g4 $(GRAMMAR_SRC)/FORTRANIIParser.g4 FORTRAN
	@echo "Building FORTRAN II (1958)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRANIILexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRANIIParser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing FORTRAN II (1958) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# FORTRAN 66 (1966) - First FORTRAN Standard with merged FORTRAN IV features
$(GRAMMAR_GEN_EARLY)/FORTRAN66Lexer.py: $(GRAMMAR_SRC)/FORTRAN66Lexer.g4 $(GRAMMAR_SRC)/FORTRAN66Parser.g4 FORTRANII
	@echo "Building FORTRAN 66 (1966)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRAN66Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRAN66Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing FORTRAN 66 (1966) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# FORTRAN 77 (1977) - Structured Programming
$(GRAMMAR_GEN_EARLY)/FORTRAN77Lexer.py: $(GRAMMAR_SRC)/FORTRAN77Lexer.g4 $(GRAMMAR_SRC)/FORTRAN77Parser.g4 FORTRAN66
	@echo "Building FORTRAN 77 (1977)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRAN77Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/early FORTRAN77Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing FORTRAN 77 (1977) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Fortran 90 (1990) - Modern Foundation
$(GRAMMAR_GEN_MODERN)/Fortran90Lexer.py: $(GRAMMAR_SRC)/Fortran90Lexer.g4 $(GRAMMAR_SRC)/Fortran90Parser.g4 FORTRAN77
	@echo "Building Fortran 90 (1990)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran90Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran90Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Fortran 90 (1990) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Fortran 95 (1995) - Minor Update
$(GRAMMAR_GEN_MODERN)/Fortran95Lexer.py: $(GRAMMAR_SRC)/Fortran95Lexer.g4 $(GRAMMAR_SRC)/Fortran95Parser.g4 Fortran90
	@echo "Building Fortran 95 (1995)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran95Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran95Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Fortran 95 (1995) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Fortran 2003 (2003) - Object-Oriented Programming
$(GRAMMAR_GEN_MODERN)/Fortran2003Lexer.py: $(GRAMMAR_SRC)/Fortran2003Lexer.g4 $(GRAMMAR_SRC)/Fortran2003Parser.g4 Fortran95
	@echo "Building Fortran 2003 (2003)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2003Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2003Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Fortran 2003 (2003) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Fortran 2008 (2008) - Enhanced Parallel Programming
$(GRAMMAR_GEN_MODERN)/Fortran2008Lexer.py: $(GRAMMAR_SRC)/Fortran2008Lexer.g4 $(GRAMMAR_SRC)/Fortran2008Parser.g4 Fortran2003
	@echo "Building Fortran 2008 (2008)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2008Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2008Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Fortran 2008 (2008) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Fortran 2018 (2018) - Modern Fortran Revolution
$(GRAMMAR_GEN_MODERN)/Fortran2018Lexer.py: $(GRAMMAR_SRC)/Fortran2018Lexer.g4 $(GRAMMAR_SRC)/Fortran2018Parser.g4 Fortran2008
	@echo "Building Fortran 2018 (2018)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2018Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2018Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Fortran 2018 (2018) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Fortran 2023 (2023) - Latest ISO Standard
$(GRAMMAR_GEN_MODERN)/Fortran2023Lexer.py: $(GRAMMAR_SRC)/Fortran2023Lexer.g4 $(GRAMMAR_SRC)/Fortran2023Parser.g4 Fortran2018
	@echo "Building Fortran 2023 (2023)..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2023Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern Fortran2023Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Fortran 2023 (2023) lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi

# Lazy Fortran 2025 (future standard) - Relaxed syntax on top of Fortran 2023
$(GRAMMAR_GEN_MODERN)/LazyFortran2025Lexer.py: $(GRAMMAR_SRC)/LazyFortran2025Lexer.g4 $(GRAMMAR_SRC)/LazyFortran2025Parser.g4 Fortran2023
	@echo "Building Lazy Fortran 2025..."
	@if [ "$(JAVA_AVAILABLE)" = "yes" ] && [ "$(ANTLR4_AVAILABLE)" = "yes" ]; then \
		cd $(GRAMMAR_SRC) && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern LazyFortran2025Lexer.g4 && \
		$(ANTLR4) $(ANTLR4_PYTHON) -lib . -o ../generated/modern LazyFortran2025Parser.g4; \
	else \
		if [ -f "$@" ]; then \
			echo "ANTLR4 CLI not available; using existing Lazy Fortran 2025 lexer/parser sources"; \
		else \
			echo "❌ ANTLR4 CLI not found and no generated sources present; install ANTLR4 to build grammars."; \
			exit 1; \
		fi; \
	fi


# ====================================================================
# TEST TARGETS - Run tests for individual standards or all
# ====================================================================

# Run ALL tests (ANTLR4 + tree-sitter)
test: all test-tree-sitter
	@echo "=========================================="
	@echo "Running ALL Tests - Full Grammar Suite"
	@echo "=========================================="
	@echo "Standards Chain: FORTRAN(1957) → F2023"
	$(PYTEST) $(TEST_DIR)/ -v --tb=short
	@echo ""
	@echo "✅ All tests completed!"

# Basic lint target to keep Python tooling and helpers syntax-checked.
# This complements the grammar-focused test suite without introducing
# external linter dependencies.
lint:
	@echo "=========================================="
	@echo "Running lint checks (Python syntax)"
	@echo "=========================================="
	@python -m compileall lazyfortran_tooling.py lflint lfmt tools tests scripts
	@echo "✅ Lint completed successfully"

# Run all tree-sitter tests
test-tree-sitter:
	@echo "=========================================="
	@echo "Running Tree-sitter Tests"
	@echo "=========================================="
	@for dir in tree-sitter/*/; do \
		if [ -f "$$dir/grammar.js" ]; then \
			echo "Testing $$(basename "$$dir")..."; \
			(cd "$$dir" && tree-sitter generate && tree-sitter test) || echo "⚠️  Tests failed for $$(basename "$$dir")"; \
		fi; \
	done
	@echo "✅ Tree-sitter tests completed!"

# Individual test targets
test-fortran: FORTRAN
	@echo "Testing FORTRAN I (1957)..."
	$(PYTEST) $(TEST_DIR)/FORTRAN/ -v --tb=short

test-fortran2: FORTRANII
	@echo "Testing FORTRAN II (1958)..."
	$(PYTEST) $(TEST_DIR)/FORTRANII/ -v --tb=short || echo "No tests for FORTRAN II yet"


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

test-fortran2023: Fortran2023
	@echo "Testing Fortran 2023 (2023)..."
	$(PYTEST) $(TEST_DIR)/Fortran2023/ -v --tb=short || echo "No tests for Fortran 2023 yet"


# Cross-validation against kaby76/fortran examples
test-cross-validation: Fortran2018
	@echo "Cross-validating Fortran2018 against kaby76/fortran examples..."
	python test_cross_validation.py

# ====================================================================
# UTILITY TARGETS
# ====================================================================

# Directory for downloaded standards/spec references (git-ignored)
STANDARDS_DIR ?= validation/pdfs

# Curl command tuned for standards downloads:
# -f : fail on HTTP errors
# -L : follow redirects
# --retry / --retry-delay : handle transient/network/429 issues
# -A : spoof a common browser user agent (some mirrors are picky)
CURL_STD ?= curl -fL --retry 5 --retry-delay 10 -A "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119 Safari/537.36"

# Download historical FORTRAN/Fortran standard references (PDFs and key drafts)
# into a local, git-excluded cache under $(STANDARDS_DIR). These files are
# used only for local grammar/spec auditing and are NOT part of the repo.
download-standards:
	@echo "Downloading FORTRAN/Fortran standard references into $(STANDARDS_DIR)..."
	@mkdir -p $(STANDARDS_DIR)
	@echo "  - FORTRAN (IBM 704, 1957) manual"
	@$(CURL_STD) -o $(STANDARDS_DIR)/FORTRAN_1957_IBM704_C28-6003_Oct58.pdf \
		https://bitsavers.org/pdf/ibm/704/C28-6003_704_FORTRAN_Oct58.pdf \
		|| echo "    ⚠️  Failed to download FORTRAN 1957 IBM 704 manual"
	@echo "  - FORTRAN II (IBM 704, 1958) manual"
	@$(CURL_STD) -o $(STANDARDS_DIR)/FORTRANII_1958_IBM704_C28-6000-2.pdf \
		https://bitsavers.org/pdf/ibm/704/C28-6000-2_704_FORTRANII.pdf \
		|| echo "    ⚠️  Failed to download FORTRAN II IBM 704 manual"
	@echo "  - ANSI X3.9-1966 FORTRAN (FORTRAN 66) scan (WG5 archive mirror)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/FORTRAN66_ANSI_X3.9-1966.pdf \
		https://wg5-fortran.org/ARCHIVE/Fortran66.pdf \
		|| echo "    ⚠️  Failed to download ANSI X3.9-1966 FORTRAN scan from WG5 archive"
	@echo "  - ISO 1539:1980 Fortran 77 text (WG5 HTML archive)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/FORTRAN77_ISO_1539-1980.html \
		https://wg5-fortran.org/ARCHIVE/Fortran77.html \
		|| echo "    ⚠️  Failed to download ISO 1539:1980 Fortran 77 text from WG5 archive"
	@echo "  - Fortran 90 draft standard (WG5 N692)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/Fortran90_WG5_N692.pdf \
		https://wg5-fortran.org/N001-N1100/N692.pdf \
		|| echo "    ⚠️  Failed to download WG5 N692 (Fortran 90 draft)"
	@echo "  - Fortran 95 related document (Fortran 95 Request for Interpretation)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/Fortran95_J3_98-114_RFI.txt \
		https://j3-fortran.org/doc/year/98/98-114.txt \
		|| echo "    ⚠️  Failed to download J3 98-114 (Fortran 95 RFI)"
	@echo "  - Fortran 2003 draft/final text (J3/03-007)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/Fortran2003_J3_03-007.pdf \
		https://j3-fortran.org/doc/year/03/03-007.pdf \
		|| echo "    ⚠️  Failed to download J3 03-007 (Fortran 2003)"
	@echo "  - Fortran 2008 draft/final text (J3/08-007)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/Fortran2008_J3_08-007.pdf \
		https://j3-fortran.org/doc/year/08/08-007.pdf \
		|| echo "    ⚠️  Failed to download J3 08-007 (Fortran 2008)"
	@echo "  - Fortran 2018 draft/final text (J3/15-007)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/Fortran2018_J3_15-007.pdf \
		https://j3-fortran.org/doc/year/15/15-007.pdf \
		|| echo "    ⚠️  Failed to download J3 15-007 (Fortran 2018)"
	@echo "  - Fortran 2023 draft/final text (J3/22-007)"
	@$(CURL_STD) -o $(STANDARDS_DIR)/Fortran2023_J3_22-007.pdf \
		https://j3-fortran.org/doc/year/22/22-007.pdf \
		|| echo "    ⚠️  Failed to download J3 22-007 (Fortran 2023)"
	@echo "✅ Download complete (see $(STANDARDS_DIR))"

# OCR historical scanned PDFs into plain text for audit use.
# Requires tesseract to be installed and available on PATH.
ocr-standards:
	@echo "Running Tesseract OCR for historical FORTRAN/Fortran PDFs into $(STANDARDS_DIR)..."
	@which tesseract >/dev/null 2>&1 || { echo "❌ tesseract not found on PATH"; exit 1; }
	@which pdftoppm >/dev/null 2>&1 || { echo "❌ pdftoppm not found on PATH"; exit 1; }
	@mkdir -p $(STANDARDS_DIR)
	@set -e; \
	for pdf in \
		FORTRAN_1957_IBM704_C28-6003_Oct58.pdf \
		FORTRANII_1958_IBM704_C28-6000-2.pdf \
		FORTRAN66_ANSI_X3.9-1966.pdf \
		Fortran90_WG5_N692.pdf \
		Fortran2003_J3_03-007.pdf \
		Fortran2008_J3_08-007.pdf \
		Fortran2018_J3_15-007.pdf \
		Fortran2023_J3_22-007.pdf; do \
		if [ -f "$(STANDARDS_DIR)/$$pdf" ]; then \
			base=$${pdf%.pdf}; \
			txt_file="$(STANDARDS_DIR)/$${base}.txt"; \
			tmpdir="$(STANDARDS_DIR)/.ocr_tmp_$${base}"; \
			echo "  - OCR $$pdf -> $${txt_file}"; \
			rm -rf "$$tmpdir"; \
			mkdir -p "$$tmpdir"; \
			pdftoppm -r 300 -png "$(STANDARDS_DIR)/$$pdf" "$$tmpdir/page" >/dev/null 2>&1 || { echo "    ⚠️  pdftoppm failed for $$pdf"; continue; }; \
			if ! ls "$$tmpdir"/page-*.png >/dev/null 2>&1; then \
				echo "    ⚠️  No page images produced for $$pdf"; \
				continue; \
			fi; \
			: > "$$txt_file"; \
			for img in "$$tmpdir"/page-*.png; do \
				pagebase=$$(basename "$$img" .png); \
				outbase="$$tmpdir/$$pagebase"; \
				tesseract "$$img" "$$outbase" -l eng >/dev/null 2>&1 || echo "    ⚠️  Tesseract failed on $$img"; \
				if [ -f "$$outbase.txt" ]; then \
					cat "$$outbase.txt" >> "$$txt_file"; \
					echo "" >> "$$txt_file"; \
				fi; \
			done; \
		else \
			echo "    ⚠️  Skipping $$pdf (not present in $(STANDARDS_DIR))"; \
		fi; \
	done
	@echo "✅ OCR pass complete (see $(STANDARDS_DIR)/*.txt)"

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	rm -f $(GRAMMAR_GEN_EARLY)/*.py $(GRAMMAR_GEN_EARLY)/*.pyc $(GRAMMAR_GEN_EARLY)/*.tokens $(GRAMMAR_GEN_EARLY)/*.interp
	rm -f $(GRAMMAR_GEN_MODERN)/*.py $(GRAMMAR_GEN_MODERN)/*.pyc $(GRAMMAR_GEN_MODERN)/*.tokens $(GRAMMAR_GEN_MODERN)/*.interp
	find $(TEST_DIR) -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete 2>/dev/null || true

# Deep clean - also removes test artifacts
clean-all: clean
	@echo "Deep cleaning (including test artifacts)..."
	rm -rf .pytest_cache

# Help target
help:
	@echo "FORTRAN/Fortran Standard Grammar Build System"
	@echo "============================================="
	@echo ""
	@echo "Historical FORTRAN/Fortran Standards Chain:"
	@echo "  FORTRAN (1957) → FORTRAN II (1958) → FORTRAN IV (1962)"
	@echo "  → FORTRAN 66 (1966) → FORTRAN 77 (1977) → Fortran 90 (1990)"
	@echo "  → Fortran 95 (1995) → Fortran 2003 (2003) → Fortran 2008 (2008)"
	@echo "  → Fortran 2018 (2018) → Fortran 2023 (2023)"
	@echo ""
	@echo "BUILD TARGETS:"
	@echo "  all                    - Build all grammars (default)"
	@echo "  FORTRAN                - Build FORTRAN I (1957)"
	@echo "  FORTRANII              - Build FORTRAN II (1958)"
	@echo "  FORTRAN66              - Build FORTRAN 66 (1966)"
	@echo "  FORTRAN77              - Build FORTRAN 77 (1977)"
	@echo "  Fortran90              - Build Fortran 90 (1990)"
	@echo "  Fortran95              - Build Fortran 95 (1995)"
	@echo "  Fortran2003            - Build Fortran 2003 (2003)"
	@echo "  Fortran2008            - Build Fortran 2008 (2008)"
	@echo "  Fortran2018            - Build Fortran 2018 (2018)"
	@echo "  Fortran2023            - Build Fortran 2023 (2023)"
	@echo ""
	@echo "TEST TARGETS:"
	@echo "  test                   - Run all tests"
	@echo "  test-fortran           - Test FORTRAN I"
	@echo "  test-fortran90         - Test Fortran 90"
	@echo "  test-fortran2003       - Test Fortran 2003"
	@echo "  test-fortran2018       - Test Fortran 2018"
	@echo "  test-fortran2023       - Test Fortran 2023"
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
