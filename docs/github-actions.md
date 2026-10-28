# GitHub Actions integration

testboard runs as one step in a job that has already collected the image
artifacts of a CI run. The generated `dashboard/` directory is then uploaded as a
GitHub Pages artifact. `gh` is used for pull-request metadata, so pass
`GH_TOKEN`.

A complete workflow that runs tests, collects images and deploys the dashboard is
in [example/example-workflow.yml](../example/example-workflow.yml).

## Latest release binary

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
          ARCH=$(uname -m)
          curl -LO https://github.com/lazy-fortran/testboard/releases/latest/download/testboard-${ARCH}
          chmod +x testboard-${ARCH}
          mv testboard-${ARCH} testboard

      - name: Generate dashboard
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          ./testboard \
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

## Pinned release

Replace `latest` with a specific tag like `v2025.10.03`:

```yaml
- name: Download testboard
  run: |
    ARCH=$(uname -m)
    curl -LO https://github.com/lazy-fortran/testboard/releases/download/v2025.10.03/testboard-${ARCH}
    chmod +x testboard-${ARCH}
    mv testboard-${ARCH} testboard
```

## Build from source

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

