from pathlib import Path


def _repo_root() -> Path:
  return Path(__file__).resolve().parents[1]


def test_docs_index_html_exists() -> None:
  index_path = _repo_root() / "docs" / "index.html"
  assert index_path.is_file(), "docs/index.html must exist for GitHub Pages"


def test_docs_index_references_design_markdown() -> None:
  index_path = _repo_root() / "docs" / "index.html"
  content = index_path.read_text(encoding="utf-8")

  assert "lazyfortran2025-design.md" in content
  assert "Lazy Fortran 2025 Design" in content

