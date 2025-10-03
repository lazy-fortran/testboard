module site_sync
  !! Synchronize dashboard output with existing GitHub Pages content
    use, intrinsic :: iso_fortran_env, only: int32, dp => real64
    use string_utils, only: string_array, append_string, clear_string_array, contains_string
    use file_utils, only: create_directory, copy_file, read_text_file
    use json_utils, only: branch_metadata, json_read_metadata
    implicit none
    private

    public :: bootstrap_existing_site

contains

    subroutine bootstrap_existing_site(github_pages_url, output_root, current_branch, metadata_file)
    !! Import existing site state so new runs preserve remote branches
        character(len=*), intent(in) :: github_pages_url
        character(len=*), intent(in) :: output_root
        character(len=*), intent(in) :: current_branch
        character(len=*), intent(in) :: metadata_file
        character(len=:), allocatable :: base_url, branches_url
        character(len=:), allocatable :: test_root
        character(len=1024) :: temp_metadata
        type(branch_metadata) :: remote_branches(100)
        integer :: n_remote, i
        logical :: ok, metadata_exists

        if (len_trim(github_pages_url) == 0) return

        inquire (file=trim(metadata_file), exist=metadata_exists)
        if (metadata_exists) return

        base_url = normalize_base_url(github_pages_url)
        if (len_trim(base_url) == 0) return

        branches_url = trim(base_url)//'/branches.json'
        temp_metadata = build_temp_path('branches.json')

        ok = download_to_file(branches_url, temp_metadata)
        if (.not. ok) then
            call delete_file_if_exists(temp_metadata)
            return
        end if

        call json_read_metadata(temp_metadata, remote_branches, n_remote)
        if (n_remote == 0) then
            call delete_file_if_exists(temp_metadata)
            return
        end if

        call copy_file(temp_metadata, metadata_file, ok)
        call delete_file_if_exists(temp_metadata)
        if (.not. ok) return

        test_root = trim(output_root)//'/test'
        do i = 1, n_remote
            call mirror_remote_branch(base_url, trim(remote_branches(i)%branch_name), &
                 trim(test_root), trim(current_branch))
        end do
    end subroutine bootstrap_existing_site

    subroutine mirror_remote_branch(base_url, branch_name, test_root, current_branch)
        character(len=*), intent(in) :: base_url
        character(len=*), intent(in) :: branch_name
        character(len=*), intent(in) :: test_root
        character(len=*), intent(in) :: current_branch
        character(len=:), allocatable :: branch_dir, images_dir
        character(len=:), allocatable :: branch_base_url
        character(len=:), allocatable :: index_path, diff_path
        character(len=:), allocatable :: index_url, diff_url
        character(len=:), allocatable :: html_content
        type(string_array) :: image_refs
        integer :: j
        logical :: ok

        if (len_trim(branch_name) == 0) return
        if (trim(branch_name) == trim(current_branch)) return

        branch_dir = trim(test_root)//'/'//trim(branch_name)
        images_dir = trim(branch_dir)//'/images'
        branch_base_url = trim(base_url)//'/'//trim(branch_name)

        call create_directory(branch_dir, ok)
        if (.not. ok) return
        call create_directory(images_dir, ok)
        if (.not. ok) return

        index_path = trim(branch_dir)//'/index.html'
        diff_path = trim(branch_dir)//'/diff.html'
        index_url = trim(branch_base_url)//'/index.html'
        diff_url = trim(branch_base_url)//'/diff.html'

        ok = download_to_file(index_url, index_path)
        if (.not. ok) call delete_file_if_exists(index_path)

        ok = download_to_file(diff_url, diff_path)
        if (.not. ok) call delete_file_if_exists(diff_path)

        call clear_string_array(image_refs)

        html_content = read_file_safely(index_path)
        call collect_image_references(html_content, image_refs)

        html_content = read_file_safely(diff_path)
        call collect_image_references(html_content, image_refs)

        do j = 1, image_refs%count
            call mirror_remote_image(branch_base_url, trim(image_refs%items(j)), &
                 images_dir)
        end do
    end subroutine mirror_remote_branch

    subroutine mirror_remote_image(branch_base_url, relative_path, images_dir)
        character(len=*), intent(in) :: branch_base_url
        character(len=*), intent(in) :: relative_path
        character(len=*), intent(in) :: images_dir
        character(len=:), allocatable :: cleaned, filename
        character(len=:), allocatable :: target_path, url
        logical :: ok

        if (.not. starts_with_images(relative_path)) return

        cleaned = trim(relative_path)
        filename = extract_filename(cleaned)
        if (len_trim(filename) == 0) return

        target_path = trim(images_dir)//'/'//trim(filename)
        call ensure_parent_directory(target_path)

        url = trim(branch_base_url)//'/'//trim(cleaned)
        ok = download_to_file(url, target_path)
        if (.not. ok) call delete_file_if_exists(target_path)
    end subroutine mirror_remote_image

    subroutine collect_image_references(html, images)
        character(len=*), intent(in) :: html
        type(string_array), intent(inout) :: images
        integer :: pos, end_pos, search_start, next_pos
        character(len=512) :: candidate

        if (len_trim(html) == 0) return

        pos = index(html, 'images/')
        do while (pos > 0)
            end_pos = find_path_end(html, pos)
            if (end_pos > pos) then
                candidate = trim(html(pos:end_pos - 1))
                if (.not. contains_string(images, trim(candidate))) then
                    call append_string(images, trim(candidate))
                end if
            end if
            search_start = pos + 1
            next_pos = index(html(search_start:), 'images/')
            if (next_pos > 0) then
                pos = search_start + next_pos - 1
            else
                pos = 0
            end if
        end do
    end subroutine collect_image_references

    integer function find_path_end(text, start_pos) result(path_end)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos
        integer :: i, text_len
        character(len=1) :: ch

        text_len = len(text)
        path_end = start_pos
        do i = start_pos, text_len
            ch = text(i:i)
            if (ch == '"' .or. ch == '''' .or. ch == ' ' .or. ch == '<') then
                path_end = i
                return
            end if
        end do
        path_end = text_len + 1
    end function find_path_end

    logical function starts_with_images(path) result(is_image)
        character(len=*), intent(in) :: path

        is_image = (index(path, 'images/') == 1)
    end function starts_with_images

    function extract_filename(path) result(filename)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: filename

        if (len_trim(path) <= 7) then
            filename = ''
            return
        end if

        filename = trim(path(8:))
    end function extract_filename

    pure function normalize_base_url(url) result(base)
        character(len=*), intent(in) :: url
        character(len=:), allocatable :: base
        integer :: len_trimmed

        base = trim(url)
        len_trimmed = len_trim(base)
        do while (len_trimmed > 0 .and. base(len_trimmed:len_trimmed) == '/')
            base(len_trimmed:len_trimmed) = ' '
            len_trimmed = len_trim(base)
        end do
        base = base(1:len_trimmed)
    end function normalize_base_url

    function build_temp_path(suffix) result(path)
        character(len=*), intent(in) :: suffix
        character(len=:), allocatable :: path
        character(len=12) :: token

        token = random_token()
        path = '/tmp/testboard_'//trim(token)//'_'//trim(suffix)
    end function build_temp_path

    function random_token() result(token)
        character(len=12) :: token
        integer :: i, idx
        real(dp) :: r
        character(len=*), parameter :: alphabet = &
            'abcdefghijklmnopqrstuvwxyz0123456789'
        logical, save :: seeded = .false.

        if (.not. seeded) then
            call random_seed()
            seeded = .true.
        end if

        do i = 1, len(token)
            call random_number(r)
            idx = int(r*len(alphabet)) + 1
            if (idx > len(alphabet)) idx = len(alphabet)
            token(i:i) = alphabet(idx:idx)
        end do
    end function random_token

    logical function download_to_file(url, destination) result(success)
        character(len=*), intent(in) :: url
        character(len=*), intent(in) :: destination
        character(len=2048) :: cmd
        integer :: stat

        write (cmd, '(A)') 'curl -fsSL "'//trim(url)//'" -o "'// &
            trim(destination)//'"'
        call execute_command_line(trim(cmd), exitstat=stat)
        success = (stat == 0)
    end function download_to_file

    function read_file_safely(path) result(contents)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: contents
        logical :: ok

        contents = ''
        contents = read_text_file(path, ok)
        if (.not. ok) contents = ''
    end function read_file_safely

    subroutine delete_file_if_exists(path)
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: delete_unit

        inquire (file=trim(path), exist=exists)
        if (exists) then
            open (newunit=delete_unit, file=trim(path))
            close (delete_unit, status='delete')
        end if
    end subroutine delete_file_if_exists

    subroutine ensure_parent_directory(path)
        character(len=*), intent(in) :: path
        integer :: slash_pos
        character(len=:), allocatable :: parent
        logical :: ok

        slash_pos = index(trim(path), '/', back=.true.)
        if (slash_pos <= 0) return
        parent = trim(path(1:slash_pos - 1))
        if (len_trim(parent) == 0) return
        call create_directory(parent, ok)
    end subroutine ensure_parent_directory

end module site_sync
