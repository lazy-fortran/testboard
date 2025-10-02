#!/usr/bin/env python3
"""Generate Fortran module embedding HTML templates as int8 arrays."""
from __future__ import annotations
import sys
from pathlib import Path

PER_LINE = 12

def sanitize(name: str) -> str:
    return ''.join(ch.lower() if ch.isalnum() else '_' for ch in name)

def to_int8_list(data: bytes) -> list[int]:
    return [b - 256 if b >= 128 else b for b in data]

def format_int8_array(values: list[int]) -> list[str]:
    if not values:
        return ['        0 ]']

    lines: list[str] = []
    chunk: list[str] = []
    total = len(values)

    for idx, value in enumerate(values):
        chunk.append(f"{value:>4d}")
        is_last = idx == total - 1
        chunk_full = (idx % PER_LINE == PER_LINE - 1) or is_last

        if chunk_full:
            line = '        ' + ', '.join(chunk)
            if not is_last:
                line += ', &'
            else:
                line += ' ]'
            lines.append(line)
            chunk = []
    return lines

def main() -> int:
    if len(sys.argv) != 3:
        print('usage: embed_templates.py <template-dir> <output-file>', file=sys.stderr)
        return 1
    template_dir = Path(sys.argv[1])
    output_path = Path(sys.argv[2])
    if not template_dir.is_dir():
        print(f"error: template directory '{template_dir}' not found", file=sys.stderr)
        return 1

    files = sorted(template_dir.glob('*.html'))
    if not files:
        print('error: no templates found', file=sys.stderr)
        return 1

    output_path.parent.mkdir(parents=True, exist_ok=True)

    with output_path.open('w', encoding='ascii') as f:
        f.write('module embedded_templates\n')
        f.write('    use, intrinsic :: iso_fortran_env, only: int8, int32\n')
        f.write('    implicit none\n')
        f.write('    private\n')
        f.write('    public :: get_embedded_template\n')
        f.write('\n')

        for file in files:
            name = file.name
            symbol = sanitize(name)
            data = file.read_bytes()
            values = to_int8_list(data)
            length = len(values)

            f.write(f'    integer(int32), parameter :: {symbol}_len = {length}\n')
            f.write(f'    integer(int8), parameter :: {symbol}_data({symbol}_len) = [integer(int8) :: &\n')
            array_lines = format_int8_array(values)
            for line in array_lines:
                f.write(f'{line}\n')
            f.write('\n')

        f.write('contains\n')
        f.write('\n')
        f.write('    function get_embedded_template(name) result(content)\n')
        f.write('        character(len=*), intent(in) :: name\n')
        f.write('        character(len=:), allocatable :: content\n')
        f.write('        character(len=:), allocatable :: key\n')
        f.write('\n')
        f.write('        key = trim(name)\n')
        f.write('        if (len(key) == 0) then\n')
        f.write("            content = ''\n")
        f.write('            return\n')
        f.write('        end if\n')
        f.write('\n')
        f.write('        select case (key)\n')
        for file in files:
            name = file.name
            symbol = sanitize(name)
            f.write(f"        case ('{name}')\n")
            f.write(f'            content = from_int8({symbol}_data)\n')
        f.write('        case default\n')
        f.write("            content = ''\n")
        f.write('        end select\n')
        f.write('    end function get_embedded_template\n')
        f.write('\n')
        f.write('    pure function from_int8(data) result(str)\n')
        f.write('        integer(int8), intent(in) :: data(:)\n')
        f.write('        character(len=:), allocatable :: str\n')
        f.write('        integer :: i\n')
        f.write('        integer(int32) :: val\n')
        f.write('\n')
        f.write('        if (size(data) == 0) then\n')
        f.write("            str = ''\n")
        f.write('            return\n')
        f.write('        end if\n')
        f.write('\n')
        f.write('        allocate (character(len=size(data)) :: str)\n')
        f.write('        do i = 1, size(data)\n')
        f.write('            val = int(data(i), kind=int32)\n')
        f.write('            if (val < 0) val = val + 256\n')
        f.write('            str(i:i) = achar(val)\n')
        f.write('        end do\n')
        f.write('    end function from_int8\n')
        f.write('\n')
        f.write('end module embedded_templates\n')

    return 0

if __name__ == '__main__':
    sys.exit(main())
