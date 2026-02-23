#!/usr/bin/env python3
"""Sync external standards/proposal sources used by this repository.

Downloads (fail-fast):
1. Traits-for-Fortran repository (tracking master)
2. Fortran 2028 working draft PDF (26-007 primary, standing-doc #007 fallback)

All data is placed under git-ignored validation directories.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
import urllib.error
import urllib.request
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

TRAITS_REPO_URL = "https://github.com/difference-scheme/Traits-for-Fortran.git"
TRAITS_BRANCH = "master"
F2028_PRIMARY_URL = "https://j3-fortran.org/doc/year/26/26-007.pdf"
J3_STANDING_URL = "https://j3-fortran.org/doc/standing"


@dataclass
class SyncPaths:
    repo_root: Path
    external_dir: Path
    traits_dir: Path
    pdfs_dir: Path
    cache_dir: Path
    manifest_path: Path
    f2028_pdf_path: Path


class SyncError(RuntimeError):
    pass


def run(cmd: list[str], cwd: Path | None = None) -> str:
    proc = subprocess.run(
        cmd,
        cwd=str(cwd) if cwd else None,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        raise SyncError(
            f"Command failed ({proc.returncode}): {' '.join(cmd)}\n{proc.stderr.strip()}"
        )
    return proc.stdout.strip()


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        while True:
            chunk = f.read(1024 * 1024)
            if not chunk:
                break
            h.update(chunk)
    return h.hexdigest()


def download_file(url: str, dest: Path) -> None:
    req = urllib.request.Request(
        url,
        headers={
            "User-Agent": (
                "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                "(KHTML, like Gecko) Chrome/122 Safari/537.36"
            )
        },
    )
    with urllib.request.urlopen(req, timeout=60) as resp:
        data = resp.read()
    tmp = dest.with_suffix(dest.suffix + ".tmp")
    tmp.write_bytes(data)
    tmp.replace(dest)


def resolve_standing_007_pdf() -> str:
    req = urllib.request.Request(
        J3_STANDING_URL,
        headers={
            "User-Agent": (
                "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                "(KHTML, like Gecko) Chrome/122 Safari/537.36"
            )
        },
    )
    with urllib.request.urlopen(req, timeout=60) as resp:
        html = resp.read().decode("utf-8", errors="replace")

    # Example href pattern: /doc/year/26/26-007.pdf
    matches = re.findall(r'href="(/doc/year/\d+/\d+-007(?:r\d+)?\.pdf)"', html)
    if not matches:
        raise SyncError("Could not resolve standing document #007 PDF URL from J3 standing page")

    # Prefer non-revision doc when present, else first match.
    preferred = None
    for m in matches:
        if re.search(r"-007\.pdf$", m):
            preferred = m
            break
    selected = preferred or matches[0]
    return f"https://j3-fortran.org{selected}"


def sync_traits_repo(paths: SyncPaths) -> dict:
    paths.external_dir.mkdir(parents=True, exist_ok=True)

    if not paths.traits_dir.exists():
        run(
            [
                "git",
                "clone",
                "--depth",
                "1",
                "--branch",
                TRAITS_BRANCH,
                TRAITS_REPO_URL,
                str(paths.traits_dir),
            ]
        )
    else:
        run(["git", "fetch", "origin", TRAITS_BRANCH, "--depth", "1"], cwd=paths.traits_dir)
        run(["git", "checkout", "-B", TRAITS_BRANCH, "FETCH_HEAD"], cwd=paths.traits_dir)
        run(["git", "reset", "--hard", "FETCH_HEAD"], cwd=paths.traits_dir)

    commit = run(["git", "rev-parse", "HEAD"], cwd=paths.traits_dir)
    return {
        "repo_url": TRAITS_REPO_URL,
        "branch": TRAITS_BRANCH,
        "commit": commit,
        "path": str(paths.traits_dir.relative_to(paths.repo_root)),
    }


def sync_f2028_pdf(paths: SyncPaths) -> dict:
    paths.pdfs_dir.mkdir(parents=True, exist_ok=True)
    resolved_url = F2028_PRIMARY_URL

    try:
        download_file(F2028_PRIMARY_URL, paths.f2028_pdf_path)
    except (urllib.error.URLError, urllib.error.HTTPError) as exc:
        try:
            resolved_url = resolve_standing_007_pdf()
            download_file(resolved_url, paths.f2028_pdf_path)
        except Exception as fallback_exc:  # noqa: BLE001
            raise SyncError(
                "Failed to download Fortran 2028 working draft from both primary "
                f"({F2028_PRIMARY_URL}) and standing-doc fallback. "
                f"Primary error: {exc}; fallback error: {fallback_exc}"
            ) from fallback_exc

    return {
        "primary_url": F2028_PRIMARY_URL,
        "resolved_url": resolved_url,
        "sha256": sha256_file(paths.f2028_pdf_path),
        "path": str(paths.f2028_pdf_path.relative_to(paths.repo_root)),
    }


def write_manifest(paths: SyncPaths, traits_meta: dict | None, f2028_meta: dict | None) -> None:
    paths.cache_dir.mkdir(parents=True, exist_ok=True)
    payload = {
        "synced_at_utc": datetime.now(timezone.utc).isoformat(),
        "traits": traits_meta,
        "fortran2028": f2028_meta,
    }
    paths.manifest_path.write_text(json.dumps(payload, indent=2) + "\n")


def build_paths(repo_root: Path) -> SyncPaths:
    validation = repo_root / "validation"
    return SyncPaths(
        repo_root=repo_root,
        external_dir=validation / "external",
        traits_dir=validation / "external" / "traits-for-fortran",
        pdfs_dir=validation / "pdfs",
        cache_dir=validation / "cache",
        manifest_path=validation / "cache" / "external_sources_manifest.json",
        f2028_pdf_path=validation / "pdfs" / "Fortran2028_J3_26-007.pdf",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Sync external traits + standards references")
    parser.add_argument(
        "repo_root",
        nargs="?",
        default=str(Path(__file__).resolve().parents[2]),
        help="Repository root path",
    )
    parser.add_argument("--traits-only", action="store_true", help="Sync only Traits-for-Fortran")
    parser.add_argument("--f2028-only", action="store_true", help="Sync only Fortran 2028 draft PDF")
    args = parser.parse_args()

    if args.traits_only and args.f2028_only:
        print("Cannot use --traits-only and --f2028-only together", file=sys.stderr)
        return 2

    repo_root = Path(args.repo_root).resolve()
    paths = build_paths(repo_root)

    do_traits = not args.f2028_only
    do_f2028 = not args.traits_only

    try:
        traits_meta = sync_traits_repo(paths) if do_traits else None
        f2028_meta = sync_f2028_pdf(paths) if do_f2028 else None
        write_manifest(paths, traits_meta, f2028_meta)
    except SyncError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    if do_traits:
        print(f"Synced traits repo: {paths.traits_dir}")
    if do_f2028:
        print(f"Synced Fortran 2028 draft: {paths.f2028_pdf_path}")
    print(f"Wrote manifest: {paths.manifest_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
