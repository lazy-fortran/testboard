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

## Quick Start

```bash
# Generate embedded template module, then build dashboard from artifacts
make templates
fpm run -- \
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

### GitHub Action snippet

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run tests and create images
        run: make test
      - uses: actions/upload-artifact@v4
        with:
          name: test-images
          path: build/test/**/*.{png,jpg,jpeg}

  dashboard:
    needs: build
    runs-on: ubuntu-latest
    container:
      image: ghcr.io/fortran-lang/fpm:latest
    permissions:
      contents: read
      pages: write
      id-token: write
      pull-requests: read
    steps:
      - uses: actions/checkout@v4
        with:
          repository: lazy-fortran/testboard
          path: testboard
      - uses: actions/download-artifact@v4
        with:
          name: test-images
          path: image-artifacts
      - name: Build + run testboard
        run: |
          cd testboard
          fpm run -- \
            --image-root ../image-artifacts \
            --output ../dashboard \
            --branch "${{ github.ref_name }}" \
            --commit "${{ github.sha }}" \
            --run-id "${{ github.run_id }}" \
            --repo "${{ github.repository }}" \
            --project-name "${{ github.repository }}"
      - uses: actions/configure-pages@v5
      - uses: actions/upload-pages-artifact@v3
        with:
          path: dashboard

  deploy:
    needs: dashboard
    runs-on: ubuntu-latest
    permissions:
      pages: write
      id-token: write
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - uses: actions/deploy-pages@v4
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
