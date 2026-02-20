# Validation Framework

This directory contains tools and scripts for validating the Fortran grammar implementations against external references.

## Quick Setup

To download the kaby76/fortran reference repository:

```bash
# Run from validation directory
./setup_validation.sh
```

This ensures the [kaby76/fortran](https://github.com/kaby76/fortran.git)
reference material is available under `external/kaby76-fortran/` for
comparison with our grammar implementation.

## Directory Structure

```
validation/
├── tools/              # Validation utilities
│   ├── download_kaby76.py      # Downloads kaby76/fortran repo
│   └── ...
├── external/           # External resources (some committed, some synced)
│   ├── flang-grammar/  # Committed reference material
│   ├── FortranAS/      # Committed extraction framework mirror
│   └── kaby76-fortran/ # Synced/updated by setup scripts
├── cache/             # Processing cache (git-ignored) 
├── auto-generated/    # Generated reference files (git-ignored)
└── setup_validation.sh # One-click setup script
```

## Git Policy

- `cache/` and `auto-generated/` are git-ignored local artifacts.
- `external/` contains committed reference materials plus content that setup
  scripts may refresh locally.
- `validation/pdfs/` is used by `make download-standards` as a local cache.

## Usage

1. Run `./setup_validation.sh` to download reference materials
2. Compare our grammars against `external/kaby76-fortran/*.g4`
3. Test with example files in `external/kaby76-fortran/examples/`
4. Validation artifacts are cached locally but not committed

The validation environment is completely self-contained and reproducible.
