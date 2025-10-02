module gh_api
  !! GitHub API interactions via gh CLI
  use string_utils, only: trim_null
  use json_utils, only: branch_metadata
  implicit none
  private

  public :: get_pr_info

contains

  subroutine get_pr_info(branch, repo, metadata, success)
    !! Fetch PR information for a branch using gh CLI
    character(len=*), intent(in) :: branch, repo
    type(branch_metadata), intent(inout) :: metadata
    logical, intent(out) :: success
    character(len=1024) :: cmd, temp_file, line
    integer :: unit, stat, ios
    character(len=512) :: gh_token

    success = .false.

    ! Check if GH_TOKEN is set
    call get_environment_variable('GH_TOKEN', gh_token, status=stat)
    if (stat /= 0 .or. len_trim(gh_token) == 0) then
      return
    end if

    ! Create temp file for output
    temp_file = '/tmp/testboard_pr.json'

    ! Call gh CLI
    write(cmd, '(A)') 'gh pr list --repo "' // trim(repo) // '" --head "' // &
                      trim(branch) // '" --json number,url,title,isDraft --jq ".[0]" > "' // &
                      trim(temp_file) // '" 2>/dev/null'

    call execute_command_line(trim(cmd), exitstat=stat)

    if (stat /= 0) return

    ! Read JSON output (simple parsing)
    open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
    if (ios /= 0) return

    success = parse_pr_json(unit, metadata)
    close(unit)

    ! Clean up
    open(newunit=unit, file=temp_file, status='old')
    close(unit, status='delete')
  end subroutine get_pr_info

  function parse_pr_json(unit, metadata) result(success)
    !! Parse JSON from gh pr list (simple parser for specific fields)
    integer, intent(in) :: unit
    type(branch_metadata), intent(inout) :: metadata
    logical :: success
    character(len=2048) :: line
    integer :: ios, pos1, pos2

    success = .false.

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit

      ! Parse "number": 123
      if (index(line, '"number":') > 0) then
        pos1 = index(line, ':') + 1
        read(line(pos1:), *, iostat=ios) metadata%pr_number
        if (ios == 0) success = .true.
      end if

      ! Parse "url": "..."
      if (index(line, '"url":') > 0) then
        pos1 = index(line, '"', back=.true.) ! last quote
        pos2 = index(line(:pos1-1), '"', back=.true.) ! second-to-last quote
        if (pos2 > 0 .and. pos1 > pos2) then
          metadata%pr_url = line(pos2+1:pos1-1)
        end if
      end if

      ! Parse "title": "..."
      if (index(line, '"title":') > 0) then
        pos1 = index(line, '"', back=.true.)
        pos2 = index(line(:pos1-1), '"', back=.true.)
        if (pos2 > 0 .and. pos1 > pos2) then
          metadata%pr_title = line(pos2+1:pos1-1)
        end if
      end if

      ! Parse "isDraft": true/false
      if (index(line, '"isDraft":') > 0) then
        metadata%pr_draft = (index(line, 'true') > 0)
      end if
    end do
  end function parse_pr_json

end module gh_api
