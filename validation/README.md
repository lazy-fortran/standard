# Validation Framework

This directory contains tools and scripts for validating the Fortran grammar implementations against external references.

## Quick Setup

To download all external validation references:

```bash
# Run from repository root
make sync-external-specs

# Or run from validation directory
./setup_validation.sh
```

This downloads:

- [kaby76/fortran](https://github.com/kaby76/fortran.git) reference grammar sources
- [difference-scheme/Traits-for-Fortran](https://github.com/difference-scheme/Traits-for-Fortran.git) proposal examples
- Fortran 2028 working draft PDF (J3 standing document `007`)

## Directory Structure

```
validation/
├── tools/              # Validation utilities
│   ├── download_kaby76.py      # Downloads kaby76/fortran repo
│   ├── sync_external_specs.py  # Sync traits repo + F2028 working draft
│   └── ...
├── external/           # External resources (git-ignored)
│   ├── kaby76-fortran/ # Downloaded on-demand from GitHub
│   └── traits-for-fortran/ # Downloaded proposal examples
├── pdfs/              # Downloaded standards drafts (git-ignored)
├── cache/             # Processing cache (git-ignored) 
├── auto-generated/    # Generated reference files (git-ignored)
└── setup_validation.sh # One-click setup script
```

## Git Policy

- `cache/`, `auto-generated/`, `external/`, and `pdfs/` are git-ignored local artifacts.
- Sync scripts refresh external references on demand.

## Usage

1. Run `make sync-external-specs` or `./setup_validation.sh` to download reference materials
2. Compare our grammars against `external/kaby76-fortran/*.g4`
3. Test with example files in `external/kaby76-fortran/examples/` and `external/traits-for-fortran/Code/Fortran/`
4. Validation artifacts are cached locally but not committed

The validation environment is completely self-contained and reproducible.
