# External Sources

This repository keeps third-party proposal materials out of git and syncs
 them on demand into git-ignored `validation/` directories.

## Synced Sources

1. Traits proposal examples:
   - Repository: `https://github.com/difference-scheme/Traits-for-Fortran.git`
   - Branch policy: track `master`
   - Local path: `validation/external/traits-for-fortran/`

2. Fortran 2028 working draft:
   - Primary URL: `https://j3-fortran.org/doc/year/26/26-007.pdf`
   - Fallback: resolve current standing document `007` from
     `https://j3-fortran.org/doc/standing`
   - Local path: `validation/pdfs/Fortran2028_J3_26-007.pdf`

## Commands

```bash
# Sync both sources
make sync-external-specs

# Sync only traits repository
make download-traits

# Sync only Fortran 2028 draft PDF
make download-f2028-draft
```

## Manifest

Each sync writes:

- `validation/cache/external_sources_manifest.json`

The manifest records source URLs, commit/checksum metadata, and sync timestamp.

## Failure Policy

Sync is fail-fast. If any required source cannot be fetched, the command exits
with a non-zero status.
