#!/usr/bin/env python3
"""
Strict fixed-form preprocessor for FORTRAN (1957) and FORTRAN II (1958).

This module provides validation and transformation of strict IBM 704 card-image
source files according to the original FORTRAN manual (Form C28-6003, 1958) and
FORTRAN II manual (Form C28-6000-2, 1958).

Card layout per C28-6003 Chapter I.B (Coding for FORTRAN) and
C28-6000-2 Part I, Chapter 2 (Coding for FORTRAN II):
- Columns 1-5:  Statement labels (numeric, right-justified)
                FORTRAN 1957: 1-32767 (per C28-6003)
                FORTRAN II:   1-99999 (per C28-6000-2)
- Column 6:     Continuation mark (non-blank = continuation)
- Columns 7-72: Statement text
- Columns 73-80: Sequence/identification field (ignored by compiler)
- Column 1:     Comment card markers:
                FORTRAN 1957: C only (per C28-6003)
                FORTRAN II:   C or * (per C28-6000-2)

The preprocessor validates card layout and produces layout-lenient output
suitable for parsing by FORTRANParser (1957) or FORTRANIIParser (1958).

References:
- FORTRAN Automatic Coding System for the IBM 704 (Form C28-6003, Oct 1958)
- IBM FORTRAN II for the IBM 704 Data Processing System (Form C28-6000-2, 1958)
"""

from dataclasses import dataclass
from enum import Enum
from typing import List, Literal, Optional, Tuple

from f57_numeric_constant_validator import (
    validate_fortran1957_numeric_constants,
)

FortranDialect = Literal["1957", "II", "66", "77"]


class CardType(Enum):
    """Card classification per C28-6003 and C28-6000-2."""
    BLANK = "blank"
    COMMENT = "comment"
    STATEMENT = "statement"
    CONTINUATION = "continuation"


DIALECT_CONFIG = {
    "1957": {
        "comment_markers": ("C",),
        "label_max": 32767,
        "reference": "C28-6003",
        "parser": "FORTRANParser",
    },
    "II": {
        "comment_markers": ("C", "*"),
        "label_max": 99999,
        "reference": "C28-6000-2",
        "parser": "FORTRANIIParser",
    },
    "66": {
        "comment_markers": ("C", "*"),
        "label_max": 99999,
        "reference": "X3.9-1966",
        "parser": "FORTRAN66Parser",
    },
    "77": {
        "comment_markers": ("C", "*"),
        "label_max": 99999,
        "reference": "X3.9-1978",
        "parser": "FORTRAN77Parser",
    },
}


@dataclass
class Card:
    """Represents a single 80-column punch card image."""
    line_number: int
    raw_text: str
    card_type: CardType
    label: Optional[str]
    continuation: bool
    statement_text: str
    sequence_field: str

    @property
    def column_1_5(self) -> str:
        """Label field (columns 1-5)."""
        return self.raw_text[:5] if len(self.raw_text) >= 5 else self.raw_text.ljust(5)

    @property
    def column_6(self) -> str:
        """Continuation field (column 6)."""
        return self.raw_text[5] if len(self.raw_text) > 5 else " "

    @property
    def column_7_72(self) -> str:
        """Statement field (columns 7-72)."""
        if len(self.raw_text) <= 6:
            return ""
        return self.raw_text[6:72] if len(self.raw_text) > 72 else self.raw_text[6:]

    @property
    def column_73_80(self) -> str:
        """Sequence field (columns 73-80)."""
        return self.raw_text[72:80] if len(self.raw_text) > 72 else ""


@dataclass
class ValidationError:
    """Validation error with location and message."""
    line_number: int
    column: int
    message: str
    severity: str = "error"


@dataclass
class ValidationResult:
    """Result of strict fixed-form validation."""
    valid: bool
    errors: List[ValidationError]
    warnings: List[ValidationError]
    cards: List[Card]


class StrictFixedFormProcessor:
    """
    Validates and processes strict IBM 704 fixed-form FORTRAN source.

    Supports both FORTRAN 1957 (C28-6003) and FORTRAN II (C28-6000-2).

    Card layout (common to both standards):
    - Each card has 80 columns
    - Labels in columns 1-5 must be numeric or blank
      (1957: 1-32767, II: 1-99999)
    - Column 6 non-blank indicates continuation of previous statement
    - Statement text occupies columns 7-72
    - Columns 73-80 are for sequence numbers (ignored by compiler)
    - Comment card markers in column 1:
      (1957: C only, II: C or *)
    """

    def __init__(self, strict_width: bool = True, allow_tabs: bool = False,
                 dialect: FortranDialect = "II"):
        """
        Initialize the processor.

        Args:
            strict_width: Enforce exactly 80 columns per card (pad short lines)
            allow_tabs: Allow tab characters (not historical but common)
            dialect: FORTRAN dialect ("1957" or "II")
        """
        self.strict_width = strict_width
        self.allow_tabs = allow_tabs
        self.dialect = dialect
        self.config = DIALECT_CONFIG[dialect]

    def parse_card(self, line: str, line_number: int) -> Card:
        """
        Parse a single line as an 80-column card image.

        Args:
            line: Raw text of the card
            line_number: Line number for error reporting (1-based)

        Returns:
            Card object with parsed fields
        """
        raw = line.rstrip("\r\n")
        original_empty = len(raw) == 0
        if self.strict_width:
            raw = raw.ljust(80)[:80]

        if original_empty or raw.strip() == "":
            return Card(
                line_number=line_number,
                raw_text=raw,
                card_type=CardType.BLANK,
                label=None,
                continuation=False,
                statement_text="",
                sequence_field=""
            )

        col1 = raw[0] if raw else " "
        all_comment_markers = ("C", "*")
        if col1.upper() in all_comment_markers:
            return Card(
                line_number=line_number,
                raw_text=raw,
                card_type=CardType.COMMENT,
                label=None,
                continuation=False,
                statement_text=raw,
                sequence_field=""
            )

        label_field = raw[:5] if len(raw) >= 5 else raw.ljust(5)
        label = label_field.strip() if label_field.strip() else None

        cont_char = raw[5] if len(raw) > 5 else " "
        is_continuation = cont_char not in (" ", "0")

        stmt_text = raw[6:72] if len(raw) > 6 else ""
        seq_field = raw[72:80] if len(raw) > 72 else ""

        card_type = CardType.CONTINUATION if is_continuation else CardType.STATEMENT

        return Card(
            line_number=line_number,
            raw_text=raw,
            card_type=card_type,
            label=label,
            continuation=is_continuation,
            statement_text=stmt_text.rstrip(),
            sequence_field=seq_field
        )

    def _validate_1957_numeric_constants(self, card: Card) -> List[ValidationError]:
        """Validate numeric constants for FORTRAN 1957 dialect."""
        violations = validate_fortran1957_numeric_constants(
            card.statement_text,
            line_number=card.line_number,
            column_offset=7,
        )
        return [
            ValidationError(
                line_number=v.line_number,
                column=v.column,
                message=v.message,
            )
            for v in violations
        ]

    def validate_cards(self, cards: List[Card]) -> Tuple[List[ValidationError],
                                                         List[ValidationError]]:
        """
        Validate a list of parsed cards against dialect-specific rules.

        For FORTRAN 1957: C28-6003 rules (labels 1-32767, C comments only)
        For FORTRAN II: C28-6000-2 rules (labels 1-99999, C/* comments)

        Args:
            cards: List of parsed Card objects

        Returns:
            Tuple of (errors, warnings)
        """
        errors = []
        warnings = []
        label_max = self.config["label_max"]
        comment_markers = self.config["comment_markers"]

        for card in cards:
            if card.card_type in (CardType.BLANK, CardType.COMMENT):
                if card.card_type == CardType.COMMENT:
                    col1 = card.raw_text[0].upper() if card.raw_text else ""
                    if col1 == "*" and "*" not in comment_markers:
                        warnings.append(ValidationError(
                            line_number=card.line_number,
                            column=1,
                            message="Star (*) comment not historical for "
                                    "FORTRAN 1957; use C in column 1",
                            severity="warning"
                        ))
                continue

            if card.label is not None:
                if not card.label.isdigit():
                    errors.append(ValidationError(
                        line_number=card.line_number,
                        column=1,
                        message=f"Invalid label in columns 1-5: "
                                f"expected numeric (1-{label_max}), "
                                f"got '{card.label}'"
                    ))
                elif card.label:
                    label_val = int(card.label)
                    if label_val < 1 or label_val > label_max:
                        errors.append(ValidationError(
                            line_number=card.line_number,
                            column=1,
                            message=f"Label {label_val} out of range "
                                    f"(1-{label_max})"
                        ))
                if card.continuation:
                    errors.append(ValidationError(
                        line_number=card.line_number,
                        column=1,
                        message="Continuation card (col 6 non-blank) "
                                "must not have a label"
                    ))

            if not self.allow_tabs and "\t" in card.raw_text:
                warnings.append(ValidationError(
                    line_number=card.line_number,
                    column=card.raw_text.index("\t") + 1,
                    message="Tab character found (not historical); "
                            "use spaces for strict conformance",
                    severity="warning"
                ))

            if self.dialect == "1957":
                errors.extend(self._validate_1957_numeric_constants(card))

        for i, card in enumerate(cards):
            if card.card_type == CardType.CONTINUATION and i == 0:
                errors.append(ValidationError(
                    line_number=card.line_number,
                    column=6,
                    message="First card cannot be a continuation"
                ))
            elif card.card_type == CardType.CONTINUATION:
                prev_stmt_cards = [c for c in cards[:i]
                                   if c.card_type in (CardType.STATEMENT,
                                                      CardType.CONTINUATION)]
                if not prev_stmt_cards:
                    errors.append(ValidationError(
                        line_number=card.line_number,
                        column=6,
                        message="Continuation card has no preceding statement"
                    ))

        return errors, warnings

    def process(self, source: str) -> ValidationResult:
        """
        Process source code, validating and parsing as fixed-form cards.

        Args:
            source: Complete source code text

        Returns:
            ValidationResult with validation status, errors, and parsed cards
        """
        lines = source.split("\n")
        cards = []

        for i, line in enumerate(lines, start=1):
            card = self.parse_card(line, i)
            cards.append(card)

        errors, warnings = self.validate_cards(cards)

        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
            cards=cards
        )

    def to_lenient_form(self, result: ValidationResult,
                        strip_comments: bool = False) -> str:
        """
        Convert validated cards to layout-lenient source for parsing.

        This joins continuation cards with their base statements and
        produces output suitable for FORTRANIIParser.

        Args:
            result: ValidationResult from process()
            strip_comments: If True, omit comment lines from output

        Returns:
            Layout-lenient source text
        """
        output_lines = []
        current_stmt = None
        current_label = None

        for card in result.cards:
            if card.card_type == CardType.BLANK:
                if current_stmt is not None:
                    if current_label:
                        output_lines.append(f"{current_label} {current_stmt}")
                    else:
                        output_lines.append(current_stmt)
                    current_stmt = None
                    current_label = None
                if not strip_comments:
                    output_lines.append("")
            elif card.card_type == CardType.COMMENT:
                if current_stmt is not None:
                    if current_label:
                        output_lines.append(f"{current_label} {current_stmt}")
                    else:
                        output_lines.append(current_stmt)
                    current_stmt = None
                    current_label = None
                if not strip_comments:
                    comment_text = card.raw_text[1:].strip() \
                                   if len(card.raw_text) > 1 else ""
                    output_lines.append(f"! {comment_text}")
            elif card.card_type == CardType.STATEMENT:
                if current_stmt is not None:
                    if current_label:
                        output_lines.append(f"{current_label} {current_stmt}")
                    else:
                        output_lines.append(current_stmt)
                current_stmt = card.statement_text.rstrip()
                current_label = card.label
            elif card.card_type == CardType.CONTINUATION:
                if current_stmt is not None:
                    current_stmt += " " + card.statement_text.strip()

        if current_stmt is not None:
            if current_label:
                output_lines.append(f"{current_label} {current_stmt}")
            else:
                output_lines.append(current_stmt)

        return "\n".join(output_lines)


def validate_strict_fixed_form(source: str,
                               strict_width: bool = True,
                               allow_tabs: bool = False,
                               dialect: FortranDialect = "II") -> ValidationResult:
    """
    Validate source code as strict IBM 704 fixed-form FORTRAN.

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters
        dialect: FORTRAN dialect ("1957" or "II")

    Returns:
        ValidationResult with validation status, errors, and parsed cards
    """
    processor = StrictFixedFormProcessor(strict_width=strict_width,
                                         allow_tabs=allow_tabs,
                                         dialect=dialect)
    return processor.process(source)


def validate_strict_fixed_form_1957(source: str,
                                    strict_width: bool = True,
                                    allow_tabs: bool = False) -> ValidationResult:
    """
    Validate source code as strict IBM 704 fixed-form FORTRAN 1957.

    Convenience wrapper for validate_strict_fixed_form with dialect="1957".

    Per C28-6003 (FORTRAN for the IBM 704):
    - Labels must be 1-32767
    - Only C in column 1 marks a comment card
    - * comments generate a warning (not historical)

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters

    Returns:
        ValidationResult with validation status, errors, and parsed cards
    """
    return validate_strict_fixed_form(source, strict_width, allow_tabs,
                                      dialect="1957")


def convert_to_lenient(source: str,
                       strict_width: bool = True,
                       allow_tabs: bool = False,
                       strip_comments: bool = False,
                       dialect: FortranDialect = "II") -> Tuple[str, ValidationResult]:
    """
    Validate and convert strict fixed-form source to layout-lenient form.

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters
        strip_comments: If True, omit comment lines from output
        dialect: FORTRAN dialect ("1957" or "II")

    Returns:
        Tuple of (lenient_source, validation_result)
    """
    processor = StrictFixedFormProcessor(strict_width=strict_width,
                                         allow_tabs=allow_tabs,
                                         dialect=dialect)
    result = processor.process(source)
    lenient = processor.to_lenient_form(result, strip_comments=strip_comments)
    return lenient, result


def convert_to_lenient_1957(source: str,
                            strict_width: bool = True,
                            allow_tabs: bool = False,
                            strip_comments: bool = False
                            ) -> Tuple[str, ValidationResult]:
    """
    Validate and convert FORTRAN 1957 strict fixed-form to lenient form.

    Convenience wrapper for convert_to_lenient with dialect="1957".

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters
        strip_comments: If True, omit comment lines from output

    Returns:
        Tuple of (lenient_source, validation_result)
    """
    return convert_to_lenient(source, strict_width, allow_tabs,
                              strip_comments, dialect="1957")


def validate_strict_fixed_form_66(source: str,
                                  strict_width: bool = True,
                                  allow_tabs: bool = False) -> ValidationResult:
    """
    Validate source code as strict fixed-form FORTRAN 66.

    Convenience wrapper for validate_strict_fixed_form with dialect="66".

    Per ANSI X3.9-1966 Section 3.3 (Source Program Format):
    - Labels must be 1-99999
    - C or * in column 1 marks a comment card
    - Column 6 continuation mark
    - Columns 7-72 contain statement text
    - Columns 73-80 contain sequence field

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters

    Returns:
        ValidationResult with validation status, errors, and parsed cards
    """
    return validate_strict_fixed_form(source, strict_width, allow_tabs,
                                      dialect="66")


def convert_to_lenient_66(source: str,
                          strict_width: bool = True,
                          allow_tabs: bool = False,
                          strip_comments: bool = False
                          ) -> Tuple[str, ValidationResult]:
    """
    Validate and convert FORTRAN 66 strict fixed-form to lenient form.

    Convenience wrapper for convert_to_lenient with dialect="66".

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters
        strip_comments: If True, omit comment lines from output

    Returns:
        Tuple of (lenient_source, validation_result)
    """
    return convert_to_lenient(source, strict_width, allow_tabs,
                              strip_comments, dialect="66")


def validate_strict_fixed_form_77(source: str,
                                  strict_width: bool = True,
                                  allow_tabs: bool = False) -> ValidationResult:
    """
    Validate source code as strict fixed-form FORTRAN 77.

    Convenience wrapper for validate_strict_fixed_form with dialect="77".

    Per ANSI X3.9-1978 Section 3.3 (Source Program Format):
    - Labels must be 1-99999
    - C or * in column 1 marks a comment card
    - Column 6 continuation mark
    - Columns 7-72 contain statement text
    - Columns 73-80 contain sequence field

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters

    Returns:
        ValidationResult with validation status, errors, and parsed cards
    """
    return validate_strict_fixed_form(source, strict_width, allow_tabs,
                                      dialect="77")


def convert_to_lenient_77(source: str,
                          strict_width: bool = True,
                          allow_tabs: bool = False,
                          strip_comments: bool = False
                          ) -> Tuple[str, ValidationResult]:
    """
    Validate and convert FORTRAN 77 strict fixed-form to lenient form.

    Convenience wrapper for convert_to_lenient with dialect="77".

    Args:
        source: Source code text
        strict_width: Enforce 80-column cards
        allow_tabs: Allow tab characters
        strip_comments: If True, omit comment lines from output

    Returns:
        Tuple of (lenient_source, validation_result)
    """
    return convert_to_lenient(source, strict_width, allow_tabs,
                              strip_comments, dialect="77")


if __name__ == "__main__":
    sample_1957 = """\
C     THIS IS A COMMENT CARD
C     FORTRAN 1957 SAMPLE PROGRAM (IBM 704)
      READ 100, A, B                                                    00000010
C     COMPUTE SUM
      C = A + B                                                         00000020
      PRINT 200, C                                                      00000030
      STOP                                                              00000040
  100 FORMAT (2F10.2)                                                   00000050
  200 FORMAT (F15.4)                                                    00000060
      END                                                               00000070
"""

    sample_ii = """\
C     THIS IS A COMMENT CARD
*     FORTRAN II SAMPLE PROGRAM (IBM 704)
      SUBROUTINE SWAP(X, Y)                                             00000010
C     SWAP TWO VALUES
      TEMP = X                                                          00000020
      X = Y                                                             00000030
      Y = TEMP                                                          00000040
      RETURN                                                            00000050
      END                                                               00000060
"""

    for name, sample, dialect in [("FORTRAN 1957", sample_1957, "1957"),
                                  ("FORTRAN II", sample_ii, "II")]:
        print(f"Strict Fixed-Form Validator for {name}")
        print("=" * 60)
        print(f"\nSample input ({dialect}):")
        print(sample)

        result = validate_strict_fixed_form(sample, dialect=dialect)

        print(f"\nValidation result: {'VALID' if result.valid else 'INVALID'}")
        print(f"Cards parsed: {len(result.cards)}")

        if result.errors:
            print("\nErrors:")
            for err in result.errors:
                print(f"  Line {err.line_number}, Col {err.column}: {err.message}")

        if result.warnings:
            print("\nWarnings:")
            for warn in result.warnings:
                print(f"  Line {warn.line_number}, Col {warn.column}: {warn.message}")

        lenient, _ = convert_to_lenient(sample, dialect=dialect)
        print("\nConverted to lenient form:")
        print(lenient)
        print("\n")
