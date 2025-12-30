# Validation Framework

This directory contains tools and scripts for validating the Fortran grammar implementations against external references.

## Quick Setup

To download the kaby76/fortran reference repository:

```bash
# Run from validation directory
./setup_validation.sh
```

This will download the [kaby76/fortran](https://github.com/kaby76/fortran.git) repository into `external/kaby76-fortran/` for comparison with our grammar implementation.

## Directory Structure

```
validation/
├── tools/              # Validation utilities
│   ├── download_kaby76.py      # Downloads kaby76/fortran repo
│   └── ...
├── external/           # External resources (git-ignored)
│   └── kaby76-fortran/ # Downloaded on-demand from GitHub
├── cache/             # Processing cache (git-ignored) 
├── auto-generated/    # Generated reference files (git-ignored)
└── setup_validation.sh # One-click setup script
```

## Git Policy

All content in `external/`, `cache/`, `auto-generated/`, and `pdfs/` directories is **git-ignored** and downloaded on-demand. This keeps the repository clean while providing access to reference materials for validation.

## Usage

1. Run `./setup_validation.sh` to download reference materials
2. Compare our grammars against `external/kaby76-fortran/*.g4`
3. Test with example files in `external/kaby76-fortran/examples/`
4. Validation artifacts are cached locally but not committed

The validation environment is completely self-contained and reproducible.