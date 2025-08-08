# Makefile for LazyFortran2025 Standard Grammar Implementation
# Historical FORTRAN/Fortran grammar inheritance chain: 1957 → 2025

# Configuration
ANTLR4 = antlr4
ANTLR4_PYTHON = -Dlanguage=Python3
GRAMMAR_DIR = grammars
BUILD_DIR = build

# Grammar inheritance chain (build order matters!)
GRAMMARS = FORTRAN FORTRANII FORTRANIV FORTRAN66 FORTRAN77 Fortran90 Fortran95 Fortran2003 Fortran2008 Fortran2018

# Default target
.PHONY: all clean test help

all: $(GRAMMARS)

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

# Test target
test: all
	@echo "Running comprehensive grammar tests..."
	python -m pytest tests/ -v

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	rm -rf $(BUILD_DIR)

# Help target
help:
	@echo "LazyFortran2025 Grammar Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all         Build all grammars in dependency order"
	@echo "  FORTRAN     Build FORTRAN I (1957) foundation"
	@echo "  FORTRANII   Build FORTRAN II (1958) procedural features"
	@echo "  FORTRANIV   Build FORTRAN IV (1962) data types"
	@echo "  FORTRAN66   Build FORTRAN 66 (1966) first standard"
	@echo "  FORTRAN77   Build FORTRAN 77 (1977) structured programming"
	@echo "  Fortran90   Build Fortran 90 (1990) modern foundation"
	@echo "  Fortran95   Build Fortran 95 (1995) minor update"
	@echo "  Fortran2003 Build Fortran 2003 (2003) object-oriented programming"
	@echo "  Fortran2008 Build Fortran 2008 (2008) enhanced parallel programming"
	@echo "  Fortran2018 Build Fortran 2018 (2018) modern Fortran revolution"
	@echo "  test        Run comprehensive test suite"
	@echo "  clean       Remove all generated files"
	@echo "  help        Show this help"
	@echo ""
	@echo "Grammar Inheritance Chain:"
	@echo "  FORTRAN → FORTRANII → FORTRANIV → FORTRAN66 → FORTRAN77 → Fortran90 → Fortran95 → Fortran2003 → Fortran2008 → Fortran2018"
	@echo ""
	@echo "Historical Timeline:"
	@echo "  1957: FORTRAN I  (IBM 704, monolithic programs)"
	@echo "  1958: FORTRAN II (procedures: CALL, SUBROUTINE, FUNCTION)"
	@echo "  1962: FORTRAN IV (data types: LOGICAL, DOUBLE PRECISION, COMPLEX)"
	@echo "  1966: FORTRAN 66 (first standard: X3.9-1966, machine-independent)"
	@echo "  1977: FORTRAN 77 (structured: PROGRAM, IF-THEN-ELSE, CHARACTER)"
	@echo "  1990: Fortran 90 (modern: modules, arrays, derived types)"
	@echo "  1995: Fortran 95 (minor updates and clarifications)"
	@echo "  2003: Fortran 2003 (object-oriented: CLASS, EXTENDS, C interop)"
	@echo "  2008: Fortran 2008 (coarrays, submodules, DO CONCURRENT)"
	@echo "  2018: Fortran 2018 (collective ops, SELECT RANK, teams, events)"

# Force rebuild
.PHONY: $(GRAMMARS) force-rebuild
force-rebuild: clean all