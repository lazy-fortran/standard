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


def test_design_doc_has_work_in_progress_notice() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "Work in progress" in content


def test_design_doc_covers_all_lazy_feature_issues() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  for issue in range(51, 58):
    marker = f"#{issue}"
    assert (
      marker in content
    ), f"Design document should reference Lazy Fortran issue #{issue}"


def test_type_inference_doc_spells_out_numeric_kinds() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "bytes per component" in content
  assert "`double precision` maps to `real(8)`" in content
  assert "`double complex` maps to `complex(16)`" in content


def test_type_inference_doc_covers_automatic_inference_and_promotion() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "automatic type inference" in content.lower()
  assert "first assignment" in content.lower()
  assert "`implicit none`: undeclared names are errors" in content
  assert "integer + integer | integer" in content
  assert "real + complex | complex" in content


def test_world_specializations_doc_spells_out_precedence_and_ambiguity() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "LF-SYN-03" in content
  assert "World-Wide Automatic Specializations" in content
  assert "User-written specifics win" in content
  assert "Most specific candidate wins" in content
  assert "Ambiguity is an error" in content


def test_world_specializations_doc_mentions_iso_generic_resolution() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "ISO/IEC 1539-1:2018" in content
  assert "15.4.3.4" in content
  assert "standard-compliant" in content
