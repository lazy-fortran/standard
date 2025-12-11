#!/usr/bin/env python3
"""
Semantic validation for Hollerith constants per ANSI X3.9-1966 Section 4.7

Hollerith format: nHxxx where:
- n is an unsigned integer constant (1 or more digits)
- H is the letter H (case-insensitive in some dialects)
- xxx are exactly n characters (any characters including spaces)

Example:
- Valid: 5HHELLO (5 chars)
- Invalid: 5HHELL (4 chars, should be 4H or HHHELLO)
- Invalid: 3HHELLO (5 chars, should be 5H)
"""

import re
from typing import List, Tuple, Optional


class HollerthValidationError(Exception):
    """Raised when Hollerith format violates ANSI X3.9-1966 Section 4.7"""
    pass


def validate_hollerith(hollerith_text: str) -> Tuple[bool, Optional[str]]:
    """
    Validate a Hollerith constant format.

    Args:
        hollerith_text: The Hollerith token as a string (e.g., "5HHELLO")

    Returns:
        Tuple[bool, Optional[str]]: (is_valid, error_message)
        - (True, None) if valid
        - (False, error_msg) if invalid

    Raises:
        None - Returns error in tuple instead
    """
    # Pattern: one or more digits, followed by H, followed by any characters
    match = re.match(r'^(\d+)H(.*)$', hollerith_text)

    if not match:
        return False, f"Not a valid Hollerith format: {hollerith_text}"

    count_str, chars = match.groups()
    declared_count = int(count_str)
    actual_count = len(chars)

    # Check: declared count must be at least 1
    if declared_count == 0:
        return (
            False,
            f"Hollerith count must be >= 1, got 0 in {hollerith_text}"
        )

    # Check: declared count must match actual character count
    if declared_count != actual_count:
        return (
            False,
            f"Hollerith count mismatch in {hollerith_text}: "
            f"declared {declared_count}, but {actual_count} characters"
        )

    return True, None


def validate_hollerith_in_format(format_text: str) -> List[Tuple[str, str]]:
    """
    Find and validate all Hollerith constants in a FORMAT statement.

    Args:
        format_text: The FORMAT statement text

    Returns:
        List of (hollerith_token, error_message) for invalid Hollerith
    """
    # Find all Hollerith patterns in the format text
    pattern = r'\d+H[^,)\r\n]*'
    holleritems = re.findall(pattern, format_text)

    errors = []
    for holleri in holleritems:
        is_valid, error_msg = validate_hollerith(holleri)
        if not is_valid:
            errors.append((holleri, error_msg))

    return errors


def report_hollerith_violations(format_text: str) -> str:
    """
    Generate a human-readable report of Hollerith violations.

    Args:
        format_text: The FORMAT statement text

    Returns:
        Report string (empty if no violations)
    """
    errors = validate_hollerith_in_format(format_text)
    if not errors:
        return ""

    lines = ["Hollerith validation errors (ANSI X3.9-1966 Section 4.7):"]
    for holleri, error_msg in errors:
        lines.append(f"  {holleri}: {error_msg}")

    return "\n".join(lines)


if __name__ == "__main__":
    # Test cases
    test_cases = [
        ("5HHELLO", True),
        ("1H*", True),
        ("10HFORTRAN 66", True),
        ("5HHELL", False),  # Count too high
        ("3HHELLO", False),  # Count too low
        ("0H", False),  # Count is zero
        ("10HTEST TEXT", True),
    ]

    for holleri, expected_valid in test_cases:
        is_valid, error_msg = validate_hollerith(holleri)
        status = "✓" if is_valid == expected_valid else "✗"
        print(f"{status} {holleri}: {error_msg if error_msg else 'valid'}")
