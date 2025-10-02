module gh_api
  !! GitHub API interactions via gh CLI
   use string_utils, only: starts_with, str
   use json_utils, only: branch_metadata
   implicit none
   private

   public :: get_pr_info, get_pr_state

contains

   subroutine get_pr_info(branch, repo, metadata, success)
    !! Fetch PR information for a branch using gh CLI
      character(len=*), intent(in) :: branch, repo
      type(branch_metadata), intent(inout) :: metadata
      logical, intent(out) :: success
      character(len=1024) :: command
      character(len=512) :: gh_token
      character(len=256) :: branch_query, normalized_branch, github_ref
      character(len=256) :: head_ref, pr_number_env, pr_title_env, pr_draft_env
      integer :: stat, pr_number, env_pr_number, ios
      logical :: has_token, link_parsed, command_ok, got_env_number

      success = .false.
      normalized_branch = trim(branch)
      branch_query = normalized_branch

      call get_environment_variable('GITHUB_HEAD_REF', head_ref, status=stat)
      if (stat == 0 .and. len_trim(head_ref) > 0) then
         branch_query = trim(head_ref)
      end if

      pr_number = parse_pr_number_from_ref(normalized_branch)

      if (pr_number <= 0) then
         call get_environment_variable('GITHUB_REF', github_ref, status=stat)
         if (stat == 0 .and. len_trim(github_ref) > 0) then
            pr_number = parse_pr_number_from_ref(github_ref)
         end if
      end if

      got_env_number = .false.
      if (pr_number <= 0) then
         call get_environment_variable('GITHUB_PR_NUMBER', pr_number_env, status=stat)
         if (stat == 0 .and. len_trim(pr_number_env) > 0) then
            pr_number_env = adjustl(trim(pr_number_env))
            read (pr_number_env, *, iostat=ios) env_pr_number
            if (ios == 0) then
               pr_number = env_pr_number
               got_env_number = .true.
            end if
         end if
      end if

      if (pr_number > 0) then
         if (metadata%pr_number <= 0) metadata%pr_number = pr_number
         if (len_trim(repo) > 0) then
            metadata%pr_url = 'https://github.com/'//trim(repo)//'/pull/'// &
                              trim(str(pr_number))
         end if
      end if

      call get_environment_variable('GITHUB_PR_TITLE', pr_title_env, status=stat)
      if (stat == 0 .and. len_trim(pr_title_env) > 0) then
         metadata%pr_title = trim(pr_title_env)
      end if

      call get_environment_variable('GITHUB_PR_DRAFT', pr_draft_env, status=stat)
      if (stat == 0 .and. len_trim(pr_draft_env) > 0) then
         pr_draft_env = adjustl(trim(pr_draft_env))
         read (pr_draft_env, *, iostat=ios) metadata%pr_draft
         if (ios /= 0) metadata%pr_draft = .false.
      end if

      link_parsed = (metadata%pr_number > 0 .and. len_trim(metadata%pr_url) > 0)

      call get_environment_variable('GH_TOKEN', gh_token, status=stat)
      has_token = (stat == 0 .and. len_trim(gh_token) > 0)

      if (.not. has_token) then
         success = link_parsed
         return
      end if

      if (starts_with(branch_query, 'refs/heads/')) then
         branch_query = trim(branch_query(len('refs/heads/') + 1:))
      end if

      if (pr_number > 0 .and. .not. got_env_number) then
         write (command, '(A,I0,A)') 'gh pr view ', pr_number, ' --repo "'// &
            trim(repo)//'" --json number,url,title,isDraft'
         command_ok = run_pr_command(trim(command), metadata)
      else
         write (command, '(A)') 'gh pr list --repo "'//trim(repo)// &
            '" --head "'//trim(branch_query)// &
            '" --json number,url,title,isDraft --jq ".[0]"'
         command_ok = run_pr_command(trim(command), metadata)

         if (.not. command_ok) then
            pr_number = parse_pr_number_from_ref(branch_query)
            if (pr_number > 0) then
               write (command, '(A,I0,A)') 'gh pr view ', pr_number, ' --repo "'// &
                  trim(repo)//'" --json number,url,title,isDraft'
               command_ok = run_pr_command(trim(command), metadata)
            end if
         end if
      end if

      if (command_ok) then
         success = .true.
         if (metadata%pr_number == 0 .and. pr_number > 0) then
            metadata%pr_number = pr_number
         end if
         if (len_trim(metadata%pr_url) == 0 .and. pr_number > 0 .and. &
             len_trim(repo) > 0) then
            metadata%pr_url = 'https://github.com/'//trim(repo)//'/pull/'// &
                              trim(str(pr_number))
         end if
      else
         success = link_parsed
      end if
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
         read (unit, '(A)', iostat=ios) line
         if (ios /= 0) exit

         ! Parse "number": 123
         if (index(line, '"number":') > 0) then
            pos1 = index(line, ':') + 1
            read (line(pos1:), *, iostat=ios) metadata%pr_number
            if (ios == 0) success = .true.
         end if

         ! Parse "url": "..."
         if (index(line, '"url":') > 0) then
            pos1 = index(line, '"', back=.true.) ! last quote
            pos2 = index(line(:pos1 - 1), '"', back=.true.) ! second-to-last quote
            if (pos2 > 0 .and. pos1 > pos2) then
               metadata%pr_url = line(pos2 + 1:pos1 - 1)
            end if
         end if

         ! Parse "title": "..."
         if (index(line, '"title":') > 0) then
            pos1 = index(line, '"', back=.true.)
            pos2 = index(line(:pos1 - 1), '"', back=.true.)
            if (pos2 > 0 .and. pos1 > pos2) then
               metadata%pr_title = line(pos2 + 1:pos1 - 1)
            end if
         end if

         ! Parse "isDraft": true/false
         if (index(line, '"isDraft":') > 0) then
            metadata%pr_draft = (index(line, 'true') > 0)
         end if
      end do
   end function parse_pr_json

   logical function run_pr_command(command, metadata) result(success)
    !! Execute a gh command and parse PR metadata JSON output
      character(len=*), intent(in) :: command
      type(branch_metadata), intent(inout) :: metadata
      character(len=1024) :: cmd, temp_file
      integer :: unit, stat, ios

      temp_file = '/tmp/testboard_pr.json'
      write (cmd, '(A)') trim(command)//' > "'//trim(temp_file)// &
         '" 2>/dev/null'

      call execute_command_line(trim(cmd), exitstat=stat)
      if (stat /= 0) then
         success = .false.
         call cleanup_temp_file(temp_file)
         return
      end if

      open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         success = .false.
         call cleanup_temp_file(temp_file)
         return
      end if

      success = parse_pr_json(unit, metadata)
      close (unit)
      call cleanup_temp_file(temp_file)
   end function run_pr_command

   subroutine cleanup_temp_file(path)
    !! Delete temporary file if it exists
      character(len=*), intent(in) :: path
      integer :: unit, ios

      open (newunit=unit, file=path, status='old', action='read', iostat=ios)
      if (ios == 0) then
         close (unit, status='delete')
      end if
   end subroutine cleanup_temp_file

   integer function parse_pr_number_from_ref(branch) result(pr_number)
    !! Extract PR number from GitHub ref like refs/pull/123/merge
      character(len=*), intent(in) :: branch
      character(len=256) :: trimmed_branch, digits
      integer :: prefix_len, slash_pos, ios

      pr_number = 0
      trimmed_branch = trim(branch)

      if (starts_with(trimmed_branch, 'refs/pull/')) then
         prefix_len = len('refs/pull/')
      else if (starts_with(trimmed_branch, 'pull/')) then
         prefix_len = len('pull/')
      else
         return
      end if

      slash_pos = index(trimmed_branch(prefix_len + 1:), '/')
      if (slash_pos > 0) then
         digits = trimmed_branch(prefix_len + 1:prefix_len + slash_pos - 1)
      else
         digits = trimmed_branch(prefix_len + 1:)
      end if

      digits = trim(digits)
      if (.not. is_all_digits(digits)) return

      read (digits, *, iostat=ios) pr_number
      if (ios /= 0) pr_number = 0
   end function parse_pr_number_from_ref

   logical function is_all_digits(text) result(all_digits)
    !! Check whether the provided text is composed only of digits
      character(len=*), intent(in) :: text
      integer :: i, code

      all_digits = (len_trim(text) > 0)
      if (.not. all_digits) return

      do i = 1, len_trim(text)
         code = iachar(text(i:i))
         if (code < iachar('0') .or. code > iachar('9')) then
            all_digits = .false.
            return
         end if
      end do
   end function is_all_digits

   function get_pr_state(pr_number, repo, state, success) result(is_open)
    !! Check if a PR is open using gh CLI
    !! Returns .true. if PR is open, .false. if closed/merged
      integer, intent(in) :: pr_number
      character(len=*), intent(in) :: repo
      character(len=16), intent(out) :: state
      logical, intent(out) :: success
      logical :: is_open
      character(len=1024) :: cmd
      character(len=256) :: temp_file
      character(len=512) :: gh_token
      character(len=64) :: line
      integer :: unit, stat, ios, pos1, pos2

      success = .false.
      is_open = .true.  ! Default to open (fail-safe)
      state = 'unknown'

      ! Check if GH_TOKEN is set
      call get_environment_variable('GH_TOKEN', gh_token, status=stat)
      if (stat /= 0 .or. len_trim(gh_token) == 0) then
         return
      end if

      ! Create temp file for output
      temp_file = '/tmp/testboard_pr_state.json'

      ! Call gh CLI to get PR state
      write (cmd, '(A,I0,A)') 'gh pr view ', pr_number, ' --repo "'//trim(repo)// &
         '" --json state --jq ".state" > "'//trim(temp_file)//'" 2>/dev/null'

      call execute_command_line(trim(cmd), exitstat=stat)

      if (stat /= 0) return

      ! Read state from file
      open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
      if (ios /= 0) return

      read (unit, '(A)', iostat=ios) line
      if (ios == 0) then
         ! Remove quotes and newlines
         line = adjustl(trim(line))
         if (line(1:1) == '"') then
            pos1 = 2
            pos2 = index(line(2:), '"')
            if (pos2 > 0) then
               state = trim(line(pos1:pos1 + pos2 - 2))
            else
               state = trim(line(2:))
            end if
         else
            state = trim(line)
         end if

         ! Check if state is OPEN
         is_open = (trim(state) == 'OPEN')
         success = .true.
      end if

      close (unit)

      ! Clean up
      open (newunit=unit, file=temp_file, status='old')
      close (unit, status='delete')
   end function get_pr_state

end module gh_api
