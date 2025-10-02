# 📊 testboard

A **Fortran-powered** test dashboard generator for GitHub Actions. Automatically generates beautiful HTML dashboards from test artifacts (PNG images) and publishes them to GitHub Pages.

[![Build, Test, and Deploy Dashboard](https://github.com/lazy-fortran/testboard/actions/workflows/ci.yml/badge.svg)](https://github.com/lazy-fortran/testboard/actions/workflows/ci.yml)

🔗 **[View Live Dashboard](https://lazy-fortran.github.io/testboard/test/)**

## ✨ Features

- 🚀 **Pure Fortran** implementation using modern Fortran 2008+
- 📦 **fpm-based** build system for easy integration
- 🎨 **Beautiful HTML dashboards** with responsive grid layouts
- 🔄 **Multi-branch support** - tracks test results for all branches
- 🔗 **GitHub integration** - automatically links to PRs, commits, and workflow runs
- 📸 **Image galleries** - displays PNG and JPG test outputs in an organized grid
- 💾 **Persistent history** - preserves previous branch results across runs
- 🎯 **Self-hosted** - testboard uses itself to display its own test dashboard!

## 🛠️ Usage

### As a GitHub Action

Add this to your workflow after your test job:

```yaml
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run tests and generate images
        run: make test
      - uses: actions/upload-artifact@v4
        with:
          name: test-plots
          path: |
            build/test/**/*.png
            build/test/**/*.jpg
            build/test/**/*.jpeg

  dashboard:
    needs: test
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

      - name: Build testboard
        run: cd testboard && fpm build

      - uses: actions/download-artifact@v4
        with:
          name: test-plots
          path: image-artifacts

      - name: Restore previous dashboard
        run: |
          mkdir -p dashboard
          # Download existing site if available
          wget -r -nH --cut-dirs=2 -P dashboard \
            https://youruser.github.io/yourproject/test/ 2>/dev/null || true

      - name: Generate dashboard
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          testboard/build/gfortran_*/app/testboard \
            --image-root image-artifacts \
            --output dashboard \
            --branch "${{ github.ref_name }}" \
            --commit "${{ github.sha }}" \
            --run-id "${{ github.run_id }}" \
            --repo "${{ github.repository }}" \
            --project-name "My Project"

      - uses: actions/configure-pages@v5
      - uses: actions/upload-pages-artifact@v3
        with:
          path: './dashboard'

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

### Command Line

```bash
# Build testboard
fpm build

# Generate dashboard
fpm run testboard -- \
  --image-root ./test_output \
  --output ./dashboard \
  --branch main \
  --commit abc123def456 \
  --run-id 1234567890 \
  --repo "owner/repo" \
  --project-name "My Project"

# View dashboard
open dashboard/test/index.html
```

## 📋 Requirements

- **fpm** (Fortran Package Manager)
- **gfortran** 8.0+ or other modern Fortran compiler
- **gh CLI** (optional, for PR integration)

## 🏗️ Building

```bash
fpm build
```

## 🧪 Testing

testboard includes comprehensive tests:

```bash
fpm test
```

Test suite includes:
- ✅ Unit tests for HTML escaping and generation
- ✅ Unit tests for string utilities
- ✅ Integration tests for full dashboard generation
- ✅ Multi-branch scenario tests

## 📖 API

### Command Line Options

| Option | Required | Description |
|--------|----------|-------------|
| `--image-root DIR` | No | Image artifacts directory (default: `image-artifacts`) |
| `--output DIR` | No | Output directory (default: `dashboard`) |
| `--branch NAME` | **Yes** | Branch name |
| `--commit SHA` | **Yes** | Commit SHA |
| `--run-id ID` | **Yes** | CI run identifier |
| `--repo OWNER/REPO` | **Yes** | Repository in `owner/repo` format |
| `--project-name NAME` | No | Project display name (default: `Test Dashboard`) |
| `--github-pages-url URL` | No | Base URL for GitHub Pages |
| `--help, -h` | No | Show help message |

### Supported Image Formats

testboard supports the following image formats:
- PNG (`.png`)
- JPEG (`.jpg`, `.jpeg`)

## 🎯 Design Goals

1. **Zero Python dependencies** - Pure Fortran for maximum portability
2. **Simple integration** - Works with any CI system that can run executables
3. **Persistent history** - Dashboard accumulates results across branches
4. **Beautiful output** - Professional HTML with modern CSS
5. **Self-documenting** - testboard hosts its own test dashboard

## 🤝 Contributing

Contributions welcome! Please feel free to submit a Pull Request.

## 📜 License

MIT License - see LICENSE file for details

## 🙏 Acknowledgments

- Inspired by the Python-based test dashboard in [libneo-coil](https://github.com/itpplasma/libneo)
- Built with love using modern Fortran and [fpm](https://fpm.fortran-lang.org)
- Part of the [lazy-fortran](https://github.com/lazy-fortran) organization

---

**Note**: This is a fun twist on the traditional Python/JavaScript dashboard generators - proving that Fortran can do web development too! 🚀
