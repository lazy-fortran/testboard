# testboard Architecture

## Overview

testboard is a pure Fortran implementation of a test dashboard generator, ported from the Python implementation in libneo-coil. It generates static HTML dashboards from test artifacts and publishes them to GitHub Pages.

## Module Structure

```
testboard/
├── src/
│   ├── string_utils.f90      - String manipulation (join_path, str(), ends_with, etc.)
│   ├── html_utils.f90         - HTML generation and escaping
│   ├── datetime_utils.f90     - ISO 8601 timestamp generation
│   ├── file_utils.f90         - File operations (copy, find, create dirs)
│   ├── json_utils.f90         - Simple JSON metadata handling
│   ├── gh_api.f90            - GitHub CLI integration for PR info
│   └── dashboard.f90          - Main orchestration logic
├── app/
│   └── main.f90               - Command-line interface
└── test/
    ├── test_html_utils.f90    - Unit tests for HTML utilities
    ├── test_string_utils.f90  - Unit tests for string utilities
    └── test_integration.f90   - Integration tests for full workflow
```

## Design Principles

### 1. Zero Dependencies
- Pure Fortran 2008+ standard library only
- No external libraries required
- Optional: `gh` CLI for PR integration

### 2. Self-Contained HTML Generation
- All HTML is generated programmatically in Fortran
- CSS is embedded directly in HTML
- No template files needed

### 3. Persistent Multi-Branch History
- Metadata stored in `branches.json`
- Each branch has its own directory: `test/{branch-name}/`
- Dashboard preserves previous branches across runs
- Root page redirects to `/test/` overview

### 4. GitHub Integration
- Automatic PR linking via `gh` CLI
- Workflow run links
- Commit references with truncated SHAs

## Data Flow

```
1. Test Job (CI)
   └─> Generates PNG artifacts
       └─> Uploads to GitHub Actions artifacts

2. Dashboard Job (CI)
   ├─> Downloads PNG artifacts
   ├─> Restores previous dashboard from GitHub Pages
   ├─> Runs testboard executable:
   │   ├─> Copies PNGs to branch directory
   │   ├─> Fetches PR info via gh CLI
   │   ├─> Generates branch HTML page
   │   ├─> Updates branches.json metadata
   │   ├─> Generates overview HTML page
   │   └─> Creates root redirect page
   └─> Uploads to GitHub Pages

3. Deploy Job (CI)
   └─> Publishes dashboard to GitHub Pages
```

## Directory Structure (Generated)

```
dashboard/
├── index.html              - Root redirect to /test/
└── test/
    ├── index.html          - Overview of all branches
    ├── branches.json       - Metadata for all branches
    ├── main/
    │   ├── index.html      - Branch-specific dashboard
    │   └── images/
    │       ├── test1.png
    │       └── test2.png
    └── feature-branch/
        ├── index.html
        └── images/
            └── plot.png
```

## Key Features

### HTML Escaping
All user-provided strings (branch names, commit messages, PR titles) are properly escaped to prevent XSS vulnerabilities.

### Responsive Grid Layout
Gallery uses CSS Grid with `auto-fill` and `minmax()` for responsive image display:
```css
.gallery {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  gap: 1.5rem;
}
```

### PR Draft Detection
The dashboard shows a "DRAFT" badge for draft pull requests, fetched via GitHub CLI.

### Incremental Updates
The workflow restores the previous dashboard state and only updates the current branch, preserving history for other branches.

## Testing Strategy

### Unit Tests
- **test_html_utils.f90**: HTML escaping, gallery item generation
- **test_string_utils.f90**: String operations, path joining

### Integration Tests
- **test_integration.f90**:
  - Full dashboard generation with mock data
  - Multi-branch scenario testing
  - File creation verification

### System Tests (CI)
- Real workflow execution
- Actual PNG generation
- Dashboard deployment to GitHub Pages
- Self-hosted validation (testboard tests itself!)

## Future Enhancements

1. **Full JSON Parser**: Currently uses simplified JSON reading
2. **Multiple Image Formats**: Support JPG, SVG, etc.
3. **Configurable Layouts**: Allow custom CSS themes
4. **Local Preview Server**: Built-in HTTP server for local testing
5. **Artifact Type Detection**: Auto-detect different artifact types
6. **Comparison View**: Side-by-side comparison of branches

## Comparison with Python Version

| Feature | Python (libneo-coil) | Fortran (testboard) |
|---------|---------------------|---------------------|
| Dependencies | Python 3.x | None (pure Fortran) |
| Build System | Native Python | fpm |
| HTML Generation | String templates | Programmatic |
| JSON Handling | json module | Custom parser |
| Testing | pytest | fpm test |
| Lines of Code | ~330 | ~1800 (with tests) |
| Deployment | Direct execution | Compiled binary |
| Portability | Python required | Any platform with Fortran compiler |

## Performance Characteristics

- **Compilation**: ~5 seconds on modern systems
- **Execution**: <1 second for typical dashboard generation
- **Memory**: Minimal (~10 MB for small dashboards)
- **Disk I/O**: Efficient single-pass file operations

## Contributing Guidelines

When adding features:
1. Add unit tests in `test/`
2. Update module documentation
3. Ensure all tests pass: `fpm test`
4. Update this architecture doc
5. Test with CI workflow

## License

MIT - See LICENSE file
