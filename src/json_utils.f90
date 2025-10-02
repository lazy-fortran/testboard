module json_utils
  !! Simple JSON reading/writing for metadata
  !! This is a minimal implementation for our specific use case
  use string_utils, only: trim_null
  implicit none
  private

  public :: json_write_metadata, json_read_metadata, branch_metadata

  type :: branch_metadata
    character(len=256) :: branch_name = ''
    character(len=256) :: commit = ''
    character(len=256) :: timestamp = ''
    character(len=64) :: run_id = ''
    character(len=256) :: repo = ''
    logical :: has_pngs = .false.
    integer :: pr_number = 0
    character(len=512) :: pr_url = ''
    character(len=512) :: pr_title = ''
    logical :: pr_draft = .false.
  end type branch_metadata

contains

  subroutine json_write_metadata(filepath, branches, n_branches)
    !! Write metadata to JSON file
    character(len=*), intent(in) :: filepath
    type(branch_metadata), intent(in) :: branches(:)
    integer, intent(in) :: n_branches
    integer :: unit, i

    open(newunit=unit, file=trim(filepath), status='replace', action='write')

    write(unit, '(A)') '{'
    do i = 1, n_branches
      call write_branch_entry(unit, branches(i), i == n_branches)
    end do
    write(unit, '(A)') '}'

    close(unit)
  end subroutine json_write_metadata

  subroutine write_branch_entry(unit, branch, is_last)
    !! Write a single branch entry
    integer, intent(in) :: unit
    type(branch_metadata), intent(in) :: branch
    logical, intent(in) :: is_last
    character(len=1) :: comma

    if (is_last) then
      comma = ' '
    else
      comma = ','
    end if

    write(unit, '(A)') '  "' // trim(branch%branch_name) // '": {'
    write(unit, '(A)') '    "commit": "' // trim(branch%commit) // '",'
    write(unit, '(A)') '    "has_pngs": ' // merge('true ', 'false', branch%has_pngs) // ','
    write(unit, '(A)') '    "path": "' // trim(branch%branch_name) // '",'
    write(unit, '(A)') '    "repo": "' // trim(branch%repo) // '",'
    write(unit, '(A)') '    "run_id": "' // trim(branch%run_id) // '",'
    write(unit, '(A)') '    "updated": "' // trim(branch%timestamp) // '"'

    if (branch%pr_number > 0) then
      write(unit, '(A)') '    ,'
      write(unit, '(A)') '    "pr_info": {'
      write(unit, '(A,I0,A)') '      "number": ', branch%pr_number, ','
      write(unit, '(A)') '      "url": "' // trim(branch%pr_url) // '",'
      write(unit, '(A)') '      "title": "' // trim(branch%pr_title) // '",'
      write(unit, '(A)') '      "draft": ' // merge('true ', 'false', branch%pr_draft)
      write(unit, '(A)') '    }'
    end if

    write(unit, '(A)') '  }' // comma
  end subroutine write_branch_entry

  subroutine json_read_metadata(filepath, branches, n_branches, max_branches)
    !! Read metadata from JSON file (simplified parser)
    character(len=*), intent(in) :: filepath
    type(branch_metadata), intent(out) :: branches(:)
    integer, intent(out) :: n_branches
    integer, intent(in), optional :: max_branches
    integer :: unit, ios, max_n
    character(len=2048) :: line
    logical :: exists

    if (present(max_branches)) then
      max_n = max_branches
    else
      max_n = size(branches)
    end if

    inquire(file=trim(filepath), exist=exists)
    if (.not. exists) then
      n_branches = 0
      return
    end if

    ! Simple implementation: count entries first
    n_branches = 0
    open(newunit=unit, file=trim(filepath), status='old', action='read')
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (index(line, '"path":') > 0) n_branches = n_branches + 1
      if (n_branches >= max_n) exit
    end do
    close(unit)

    ! For now, we'll keep this simple and regenerate from scratch
    ! A full JSON parser would be needed for proper merging
  end subroutine json_read_metadata

end module json_utils
