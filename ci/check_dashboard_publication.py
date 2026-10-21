#!/usr/bin/env python3
"""Check the generated gallery path and its native validation provenance."""
import json
from pathlib import Path
import sys


def check_dashboard(root, branch, commit, run_id):
    root = Path(root)
    metadata = json.loads((root / 'test/branches.json').read_text())
    current = metadata[branch]
    if current['path'] != branch or current['commit'] != commit or current['run_id'] != run_id:
        raise RuntimeError('Generated gallery has incorrect branch or validation provenance')
    gallery = root / 'test' / branch
    if not (gallery / 'index.html').is_file() or not (gallery / 'images').is_dir():
        raise RuntimeError('Generated gallery is missing at the publication path')
    if not current['has_pngs'] or not any((gallery / 'images').rglob('*.*')):
        raise RuntimeError('Generated gallery does not contain the admitted native images')


if __name__ == '__main__':
    check_dashboard(*sys.argv[1:])
