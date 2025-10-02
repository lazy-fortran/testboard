# testboard

Fortran-powered dashboard generator for GitHub Actions. testboard collects image
artifacts from CI runs, builds responsive HTML reports, and publishes them to
GitHub Pages with minimal configuration.

[![Build, Test, and Deploy Dashboard](https://github.com/lazy-fortran/testboard/actions/workflows/ci.yml/badge.svg)](https://github.com/lazy-fortran/testboard/actions/workflows/ci.yml)

Live dashboard: https://lazy-fortran.github.io/testboard/test/

## Features

- Modern Fortran implementation with zero Python dependencies
- Single `fpm` build step, no external tooling required
- Responsive gallery layout with branch metadata and workflow links
- Tracks history for every branch and pull request
- Self-hosted: the project publishes its own dashboard as proof

## Installation

### Download Pre-built Binary (Recommended)

```bash
# For x86_64
curl -LO https://github.com/lazy-fortran/testboard/releases/latest/download/testboard-x86_64
chmod +x testboard-x86_64
./testboard-x86_64 --help

# For aarch64
curl -LO https://github.com/lazy-fortran/testboard/releases/latest/download/testboard-aarch64
chmod +x testboard-aarch64
./testboard-aarch64 --help
```

Binaries are built on Ubuntu 22.04 and require glibc ≥ 2.35.

### Build from Source

```bash
git clone https://github.com/lazy-fortran/testboard.git
cd testboard
make templates
fpm build
fpm run testboard -- --help
```

## Quick Start

```bash
# Using pre-built binary
./testboard-x86_64 \
  --image-root ./image-artifacts \
  --output ./dashboard \
  --branch main \
  --commit abc123def456 \
  --run-id 1234567890 \
  --repo "owner/repo" \
  --project-name "My Project"

# Inspect the result
open dashboard/test/index.html  # or xdg-open on Linux
```

## GitHub Actions Integration

### Option 1: Use Latest Release Binary (Recommended)

```yaml
jobs:
  dashboard:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      pages: write
      id-token: write
      pull-requests: read
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: test-images
          path: image-artifacts

      - name: Download testboard
        run: |
          curl -LO https://github.com/lazy-fortran/testboard/releases/latest/download/testboard-x86_64
          chmod +x testboard-x86_64

      - name: Generate dashboard
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          ./testboard-x86_64 \
            --image-root image-artifacts \
            --output dashboard \
            --branch "${{ github.ref_name }}" \
            --commit "${{ github.sha }}" \
            --run-id "${{ github.run_id }}" \
            --repo "${{ github.repository }}" \
            --project-name "${{ github.repository }}"

      - uses: actions/upload-pages-artifact@v3
        with:
          path: dashboard
```

### Option 2: Use Pinned Version

Replace `latest` with a specific tag like `v2025.10.03`:

```yaml
- name: Download testboard
  run: |
    curl -LO https://github.com/lazy-fortran/testboard/releases/download/v2025.10.03/testboard-x86_64
    chmod +x testboard-x86_64
```

### Option 3: Build from Source

```yaml
- name: Install dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y gfortran make
    wget -q https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12
    chmod +x fpm-0.12.0-linux-x86_64-gcc-12
    sudo mv fpm-0.12.0-linux-x86_64-gcc-12 /usr/local/bin/fpm

- name: Checkout testboard
  uses: actions/checkout@v4
  with:
    repository: lazy-fortran/testboard
    path: testboard

- name: Build and run testboard
  run: |
    cd testboard
    make templates
    fpm run testboard -- \
      --image-root ../image-artifacts \
      --output ../dashboard \
      --branch "${{ github.ref_name }}" \
      --commit "${{ github.sha }}" \
      --run-id "${{ github.run_id }}" \
      --repo "${{ github.repository }}" \
      --project-name "${{ github.repository }}"
```

## Command Line Options

| Option | Required | Description |
|--------|----------|-------------|
| `--image-root DIR` | No | Image artifacts directory (default: `image-artifacts`) |
| `--output DIR` | No | Output directory (default: `dashboard`) |
| `--branch NAME` | **Yes** | Branch name |
| `--commit SHA` | **Yes** | Commit SHA |
| `--run-id ID` | **Yes** | CI run identifier |
| `--repo OWNER/REPO` | **Yes** | Repository in `owner/repo` format |
| `--project-name NAME` | No | Project display name (default: `testboard`) |
| `--github-pages-url URL` | No | Base URL for GitHub Pages |
| `--help, -h` | No | Show help message |

Supported formats: PNG (`.png`), JPEG (`.jpg`, `.jpeg`).

## Requirements

- make (GNU Make or compatible)
- fpm (Fortran Package Manager)
- A modern Fortran compiler (gfortran 8+ tested)
- gh CLI (optional, for metadata lookups)

## Development

```bash
make templates
fpm build   # compile
fpm test    # unit + integration tests
```

## License

MIT. See LICENSE for full text.
