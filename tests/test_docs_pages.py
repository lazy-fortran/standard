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

  assert "WORK IN PROGRESS" in content


def test_design_doc_covers_all_lazy_feature_issues() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  for issue in range(51, 58):
    marker = f"Issue #{issue}"
    assert (
      marker in content
    ), f"Design document should reference Lazy Fortran issue #{issue}"


def test_type_inference_doc_spells_out_numeric_kinds() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "Kinds represent **bytes per numeric component**" in content
  assert "double precision => real(8)" in content
  assert "double complex => complex(8)" in content


def test_type_inference_doc_covers_implicit_modes_and_promotion() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert (
    "In plain `.lf` without `implicit`, undeclared names are allowed"
    in content
  )
  assert "With `implicit none`, undeclared names are errors" in content
  assert "integer division stays integer" in content
  assert "complex dominates real" in content


def test_world_specializations_doc_spells_out_precedence_and_ambiguity() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert (
    "LF‑SYN‑03 – World‑Wide Automatic Specializations (Issue #51)"
    in content
  )
  assert "explicit, user-written specific procedures always" in content
  assert "win over generated specializations" in content
  assert "most specific candidate" in content
  assert "compile-time ambiguity error" in content


def test_world_specializations_doc_mentions_iso_generic_resolution() -> None:
  design_path = _repo_root() / "docs" / "lazyfortran2025-design.md"
  content = design_path.read_text(encoding="utf-8")

  assert "ISO/IEC 1539-1:2018" in content
  assert "15.4.3.4" in content
  assert "STANDARD-COMPLIANT" in content
