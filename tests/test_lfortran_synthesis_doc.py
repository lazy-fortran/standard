"""Check that every Synthesis example in docs/lfortran-synthesis.md parses.

The checker mirrors the normative grammar of the document line by line
(statements are NEWLINE terminated). Ordinary Fortran inside a model or a
procedure is opaque, but a line starting with a Synthesis keyword must match
its production, so a drift between grammar and examples fails the test.
"""
import re
from pathlib import Path

import pytest

DOC = Path(__file__).resolve().parent.parent / "docs" / "lfortran-synthesis.md"
KEYWORDS = {"symbolic", "assume", "derive", "prove", "generate",
            "requires", "ensures", "invariant"}
IDENT = r"[A-Za-z_]\w*"
SYMBOLIC = re.compile(
    rf"symbolic\s+(?:real|integer)(?:\(\s*\w+\s*\))?\s*::\s*"
    rf"{IDENT}(?:\([^()]*\))?(?:\s*,\s*{IDENT}(?:\([^()]*\))?)*$", re.I)
DERIVE = re.compile(rf"derive\s+{IDENT}\s*=\s*(.+)$", re.I)
GENERATE = re.compile(rf"generate\s+{IDENT}\s+from\s*\[\s*{IDENT}"
                      rf"(?:\s*,\s*{IDENT})*\s*\]$", re.I)
MODEL_BEGIN = re.compile(rf"(?:synthesis\s+)?model\s+({IDENT})$", re.I)
MODEL_END = re.compile(rf"end\s+(?:synthesis\s+)?model(?:\s+({IDENT}))?$", re.I)
PROC_BEGIN = re.compile(
    r"(?:(?:pure|elemental|impure|recursive)\s+)*(function|subroutine)\s+\w+",
    re.I)
PAIR_BEGIN = re.compile(rf"(specification|implementation)\s+(.+)$", re.I)
PAIR_END = re.compile(rf"end\s+(specification|implementation)(?:\s+{IDENT})?$",
                      re.I)


class ParseError(Exception):
    pass


def balanced(expr):
    depth = 0
    for ch in expr:
        depth += ch == "(" or ch == "["
        depth -= ch == ")" or ch == "]"
        if depth < 0:
            return False
    return depth == 0


def expr_ok(expr):
    return bool(expr.strip()) and balanced(expr)


def statement(line):
    """Check one body line; ordinary Fortran is accepted as opaque."""
    word = re.match(r"\w+", line)
    key = word.group(0).lower() if word else ""
    if key not in KEYWORDS:
        return
    rest = line[len(key):]
    if key == "symbolic":
        ok = SYMBOLIC.match(line)
    elif key == "derive":
        m = DERIVE.match(line)
        ok = m and expr_ok(m.group(1))
    elif key == "generate":
        ok = GENERATE.match(line)
    else:  # assume, prove, requires, ensures, invariant: KEYWORD expr
        ok = expr_ok(rest) and not rest.lstrip().startswith("=")
    if not ok:
        raise ParseError(f"does not match its production: {line!r}")


def clean(lines):
    out = []
    for raw in lines:
        raw = re.sub(r"^\s*!\$synthesis\s+", "", raw)
        raw = re.sub(r"\s*!.*$", "", raw).strip()
        if raw:
            out.append(raw)
    return out


def parse_unit(lines):
    """Parse one fenced block as model, specification/implementation or
    ordinary procedure with contract statements."""
    lines = clean(lines)
    if not lines:
        return
    first = lines[0]
    m = MODEL_BEGIN.match(first)
    if m:
        end = MODEL_END.match(lines[-1])
        if not end:
            raise ParseError(f"model without matching end: {lines[-1]!r}")
        if end.group(1) and end.group(1).lower() != m.group(1).lower():
            raise ParseError("end model name differs from model name")
        for line in lines[1:-1]:
            statement(line)
        return
    pair = PAIR_BEGIN.match(first)
    if pair and PROC_BEGIN.match(pair.group(2)):
        stop = next((i for i, l in enumerate(lines) if PAIR_END.match(l)), None)
        if stop is None or PAIR_END.match(lines[stop]).group(1).lower() != \
                pair.group(1).lower():
            raise ParseError(f"unterminated {pair.group(1)}")
        for line in lines[1:stop]:
            statement(line)
        parse_unit(lines[stop + 1:])  # further units in the same block
        return
    for line in lines:
        statement(line)


def fortran_blocks():
    text = DOC.read_text()
    for m in re.finditer(r"^```fortran\n(.*?)^```", text, re.S | re.M):
        body = m.group(1)
        line_no = text[:m.start()].count("\n") + 1
        yield line_no, body.splitlines()


def blocks_to_check():
    # A block containing '...' is a schematic fragment, not an example.
    return [(n, b) for n, b in fortran_blocks()
            if not any(l.strip().endswith("...") for l in b)]


def test_document_has_examples():
    assert len(blocks_to_check()) >= 10


@pytest.mark.parametrize("line_no,block", blocks_to_check(),
                         ids=lambda v: str(v) if isinstance(v, int) else "")
def test_example_parses(line_no, block):
    try:
        parse_unit(block)
    except ParseError as e:
        pytest.fail(f"docs/lfortran-synthesis.md:{line_no}: {e}")


@pytest.mark.parametrize("source", [
    "synthesis model m\n  derive q diff(h, p)\nend synthesis model",
    "synthesis model m\n  generate k from qdot\nend synthesis model",
    "synthesis model m\n  prove (a == b\nend synthesis model",
    "synthesis model m\n  prove a == b",
    "model a\n  symbolic real q\nend model a",
])
def test_invalid_examples_rejected(source):
    with pytest.raises(ParseError):
        parse_unit(source.splitlines())


def test_grammar_block_declares_prefix_and_end_phrase():
    text = DOC.read_text()
    assert "SYNTHESIS_KW? MODEL_KW identifier" in text
    assert "END SYNTHESIS MODEL" in text
