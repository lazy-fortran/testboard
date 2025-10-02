module json_utils
  !! Simple JSON reading/writing for metadata
  !! This is a minimal implementation for our specific use case
   use string_utils, only: str, starts_with
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
      integer :: diff_count = 0
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

      open (newunit=unit, file=trim(filepath), status='replace', action='write')

      write (unit, '(A)') '{'
      do i = 1, n_branches
         call write_branch_entry(unit, branches(i), i == n_branches)
      end do
      write (unit, '(A)') '}'

      close (unit)
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

      write (unit, '(A)') '  "'//trim(branch%branch_name)//'": {'
      write (unit, '(A)') '    "commit": "'//trim(branch%commit)//'",'
      write (unit, '(A)') '    "has_pngs": '// &
         merge('true ', 'false', branch%has_pngs)//','
      write (unit, '(A)') '    "diff_count": '//trim(str(branch%diff_count))//','
      write (unit, '(A)') '    "path": "'//trim(branch%branch_name)//'",'
      write (unit, '(A)') '    "repo": "'//trim(branch%repo)//'",'
      write (unit, '(A)') '    "run_id": "'//trim(branch%run_id)//'",'
      write (unit, '(A)') '    "updated": "'//trim(branch%timestamp)//'"'

      if (branch%pr_number > 0) then
         write (unit, '(A)') '    ,'
         write (unit, '(A)') '    "pr_info": {'
         write (unit, '(A,I0,A)') '      "number": ', branch%pr_number, ','
         write (unit, '(A)') '      "url": "'//trim(branch%pr_url)//'",'
         write (unit, '(A)') '      "title": "'//trim(branch%pr_title)//'",'
         write (unit, '(A)') '      "draft": '//merge('true ', 'false', branch%pr_draft)
         write (unit, '(A)') '    }'
      end if

      write (unit, '(A)') '  }'//comma
   end subroutine write_branch_entry

   subroutine json_read_metadata(filepath, branches, n_branches, max_branches)
    !! Read metadata from JSON file written by json_write_metadata
      character(len=*), intent(in) :: filepath
      type(branch_metadata), intent(out) :: branches(:)
      integer, intent(out) :: n_branches
      integer, intent(in), optional :: max_branches
      integer :: unit, ios, max_n, current_index
      character(len=2048) :: line
      character(len=:), allocatable :: trimmed
      logical :: exists, in_branch, in_pr_info

      if (present(max_branches)) then
         max_n = max_branches
      else
         max_n = size(branches)
      end if

      inquire (file=trim(filepath), exist=exists)
      if (.not. exists) then
         n_branches = 0
         return
      end if

      n_branches = 0
      current_index = 0
      in_branch = .false.
      in_pr_info = .false.

      open (newunit=unit, file=trim(filepath), status='old', action='read')
      do
         read (unit, '(A)', iostat=ios) line
         if (ios /= 0) exit

         trimmed = adjustl(trim(line))

         if (.not. in_branch) then
            if (is_branch_header(trimmed)) then
               if (n_branches >= max_n) exit
               n_branches = n_branches + 1
               current_index = n_branches
               branches(current_index) = branch_metadata()
               call parse_branch_name(trimmed, branches(current_index)%branch_name)
               in_branch = .true.
            end if
            cycle
         end if

         if (.not. in_pr_info .and. starts_with(trimmed, '"pr_info"')) then
            in_pr_info = .true.
            cycle
         end if

         if (in_pr_info) then
            if (is_section_end(trimmed)) then
               in_pr_info = .false.
               cycle
            end if
            call parse_pr_field(trimmed, branches(current_index))
            cycle
         end if

         if (is_section_end(trimmed)) then
            in_branch = .false.
            current_index = 0
            cycle
         end if

         call parse_branch_field(trimmed, branches(current_index))
      end do
      close (unit)

      if (n_branches > max_n) n_branches = max_n
   end subroutine json_read_metadata

   logical function is_branch_header(line) result(header)
      character(len=*), intent(in) :: line

      header = (index(line, '": {') > 0 .and. index(line, '"pr_info"') == 0 .and. &
                index(line, '{') > 0)
   end function is_branch_header

   logical function is_section_end(line) result(ended)
      character(len=*), intent(in) :: line

      ended = (trim(line) == '}' .or. trim(line) == '},' .or. trim(line) == '}'//char(0))
   end function is_section_end

   subroutine parse_branch_name(line, name)
      character(len=*), intent(in) :: line
      character(len=*), intent(inout) :: name
      integer :: first_quote, second_quote

      first_quote = index(line, '"')
      if (first_quote <= 0) then
         name = ''
         return
      end if

      second_quote = index(line(first_quote + 1:), '"')
      if (second_quote <= 0) then
         name = ''
         return
      end if

      name = line(first_quote + 1:first_quote + second_quote - 1)
   end subroutine parse_branch_name

   subroutine parse_branch_field(line, branch)
      character(len=*), intent(in) :: line
      type(branch_metadata), intent(inout) :: branch
      character(len=512) :: value
      integer :: number, ios
      logical :: logical_value

      if (extract_json_string(line, '"commit"', value)) then
         branch%commit = trim(value)
      else if (extract_json_string(line, '"path"', value)) then
         branch%branch_name = trim(value)
      else if (extract_json_string(line, '"repo"', value)) then
         branch%repo = trim(value)
      else if (extract_json_string(line, '"run_id"', value)) then
         branch%run_id = trim(value)
      else if (extract_json_string(line, '"updated"', value)) then
         branch%timestamp = trim(value)
      else if (extract_json_integer(line, '"diff_count"', number)) then
         branch%diff_count = number
      else if (extract_json_integer(line, '"diff"', number)) then
         branch%diff_count = number
      else if (extract_json_logical(line, '"has_pngs"', logical_value)) then
         branch%has_pngs = logical_value
      end if
   end subroutine parse_branch_field

   subroutine parse_pr_field(line, branch)
      character(len=*), intent(in) :: line
      type(branch_metadata), intent(inout) :: branch
      character(len=512) :: value
      integer :: number
      logical :: logical_value

      if (extract_json_integer(line, '"number"', number)) then
         branch%pr_number = number
      else if (extract_json_string(line, '"url"', value)) then
         branch%pr_url = trim(value)
      else if (extract_json_string(line, '"title"', value)) then
         branch%pr_title = trim(value)
      else if (extract_json_logical(line, '"draft"', logical_value)) then
         branch%pr_draft = logical_value
      end if
   end subroutine parse_pr_field

   logical function extract_json_string(line, field, value) result(found)
      character(len=*), intent(in) :: line
      character(len=*), intent(in) :: field
      character(len=*), intent(out) :: value
      integer :: pos, first_quote, second_quote, colon_pos

      value = ''
      pos = index(line, field)
      if (pos == 0) then
         found = .false.
         return
      end if

      colon_pos = index(line(pos:), ':')
      if (colon_pos == 0) then
         found = .false.
         return
      end if
      pos = pos + colon_pos - 1

      first_quote = index(line(pos + 1:), '"')
      if (first_quote == 0) then
         found = .false.
         return
      end if
      first_quote = pos + first_quote
      second_quote = index(line(first_quote + 1:), '"')
      if (second_quote == 0) then
         found = .false.
         return
      end if
      second_quote = first_quote + second_quote

      value = line(first_quote + 1:second_quote - 1)
      found = .true.
   end function extract_json_string

   logical function extract_json_integer(line, field, value) result(found)
      character(len=*), intent(in) :: line
      character(len=*), intent(in) :: field
      integer, intent(out) :: value
      integer :: pos, colon_pos, ios
      character(len=256) :: buffer

      value = 0
      pos = index(line, field)
      if (pos == 0) then
         found = .false.
         return
      end if

      colon_pos = index(line(pos:), ':')
      if (colon_pos == 0) then
         found = .false.
         return
      end if

      colon_pos = pos + colon_pos - 1

      buffer = adjustl(trim(line(colon_pos + 1:)))
      call strip_trailing_comma(buffer)
      read (buffer, *, iostat=ios) value
      found = (ios == 0)
   end function extract_json_integer

   logical function extract_json_logical(line, field, value) result(found)
      character(len=*), intent(in) :: line
      character(len=*), intent(in) :: field
      logical, intent(out) :: value
      integer :: pos, colon_pos, ios
      character(len=256) :: buffer

      value = .false.
      pos = index(line, field)
      if (pos == 0) then
         found = .false.
         return
      end if

      colon_pos = index(line(pos:), ':')
      if (colon_pos == 0) then
         found = .false.
         return
      end if

      colon_pos = pos + colon_pos - 1

      buffer = adjustl(trim(line(colon_pos + 1:)))
      call strip_trailing_comma(buffer)
      read (buffer, *, iostat=ios) value
      found = (ios == 0)
   end function extract_json_logical

   subroutine strip_trailing_comma(text)
      character(len=*), intent(inout) :: text
      integer :: len_trimmed

      len_trimmed = len_trim(text)
      if (len_trimmed <= 0) return
      if (text(len_trimmed:len_trimmed) == ',') then
         text(len_trimmed:len_trimmed) = ' '
      end if
   end subroutine strip_trailing_comma

end module json_utils
