from pathlib import Path


def _repo_root() -> Path:
  return Path(__file__).resolve().parents[1]


def test_fixed_form_docs_exist() -> None:
  """Ensure the fixed-form support documentation is present."""
  path = _repo_root() / "docs" / "fixed_form_support.md"
  assert path.is_file(), "docs/fixed_form_support.md must exist"


def test_fixed_form_docs_reference_subset_and_strict_mode() -> None:
  """Fixed-form docs should describe subset mode and strict validation mode."""
  path = _repo_root() / "docs" / "fixed_form_support.md"
  content = path.read_text(encoding="utf-8")

  assert "layout\u2011lenient model" in content
  assert "80-column card layout" in content
  assert "strict fixed-form preprocessor" in content
  assert "fortran_2003_audit.md" not in content
