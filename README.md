# testboard

testboard builds a static HTML dashboard from the image artifacts of a CI run
(plots, rendered figures, visual test output) and is meant to be published on
GitHub Pages. It is written in Fortran.

Status: experimental; options and output may change.

Live dashboard of this repository: https://lazy-fortran.github.io/testboard/test/

## Features

- One gallery page per branch, with commit, run and pull-request links.
- Images that differ from the base branch are marked.
- Earlier branch pages are kept by reading the already published site
  (`--github-pages-url`), so the dashboard accumulates history across runs.
- PNG and JPEG input.
- Single static binary for x86_64 and aarch64 Linux.

## Install

Download a release binary (built on Ubuntu 22.04, requires glibc 2.35 or newer):

```bash
curl -LO https://github.com/lazy-fortran/testboard/releases/latest/download/testboard-x86_64
chmod +x testboard-x86_64
```

Replace `x86_64` with `aarch64` on ARM. To build from source you need GNU Make,
Python 3 (to embed the HTML templates), [fpm](https://fpm.fortran-lang.org) and
gfortran:

```bash
git clone https://github.com/lazy-fortran/testboard.git
cd testboard
make templates
fpm build
fpm test
```

## Usage

```bash
./testboard-x86_64 \
  --image-root ./image-artifacts \
  --output ./dashboard \
  --branch main \
  --commit abc123def456 \
  --run-id 1234567890 \
  --repo owner/repo \
  --project-name "My Project"
```

The result is in `dashboard/test/index.html`. `--branch`, `--commit`, `--run-id`
and `--repo` are required. Optional:

| Option | Default | Meaning |
|---|---|---|
| `--image-root DIR` | `image-artifacts` | directory with the images |
| `--output DIR` | `dashboard` | output directory |
| `--base-branch NAME` | `main` | branch that images are compared against |
| `--project-name NAME` | `testboard` | name shown on the dashboard |
| `--github-pages-url URL` | none | published site to read existing pages from |
| `--template-root DIR` | `templates` | directory with HTML templates |

`gh` (GitHub CLI) is used, if available, to look up pull-request metadata.

## Documentation

- [GitHub Actions integration](docs/github-actions.md)
- [Example workflow](example/example-workflow.yml)

## License

MIT. See [LICENSE](LICENSE).
