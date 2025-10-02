module file_utils
  !! File and directory utilities
    use, intrinsic :: iso_fortran_env, only: iostat_end
    use string_utils, only: ends_with, join_path, string_array, append_string, &
                            clear_string_array
    implicit none
    private

    public :: copy_file, create_directory, directory_exists, file_exists
    public :: find_files, find_image_files, remove_directory
    public :: read_text_file, compute_sha256

contains

    subroutine copy_file(src, dest, success)
    !! Copy file using system command
        character(len=*), intent(in) :: src, dest
        logical, intent(out) :: success
        character(len=1024) :: cmd
        integer :: stat

        write (cmd, '(A)') 'cp "'//trim(src)//'" "'//trim(dest)//'"'
        call execute_command_line(trim(cmd), exitstat=stat)
        success = (stat == 0)
    end subroutine copy_file

    subroutine create_directory(path, success)
    !! Create directory including parents
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=1024) :: cmd
        integer :: stat

        write (cmd, '(A)') 'mkdir -p "'//trim(path)//'"'
        call execute_command_line(trim(cmd), exitstat=stat)
        success = (stat == 0)
    end subroutine create_directory

    function directory_exists(path) result(exists)
    !! Check if directory exists
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: stat
        character(len=1024) :: cmd

        write (cmd, '(A)') 'test -d "'//trim(path)//'"'
        call execute_command_line(trim(cmd), exitstat=stat)
        exists = (stat == 0)
    end function directory_exists

    function file_exists(path) result(exists)
    !! Check if file exists
        character(len=*), intent(in) :: path
        logical :: exists

        inquire (file=trim(path), exist=exists)
    end function file_exists

    subroutine remove_directory(path, success)
    !! Remove directory recursively
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=1024) :: cmd
        integer :: stat

        write (cmd, '(A)') 'rm -rf "'//trim(path)//'"'
        call execute_command_line(trim(cmd), exitstat=stat)
        success = (stat == 0)
    end subroutine remove_directory

    subroutine find_files(root_dir, pattern, files)
    !! Find all files matching pattern recursively
        character(len=*), intent(in) :: root_dir, pattern
        type(string_array), intent(out) :: files
        character(len=1024) :: cmd, temp_file, line
        integer :: unit, stat, ios

        temp_file = '/tmp/testboard_find_'//trim(str_random())//'.txt'

        write (cmd, '(A)') 'find "'//trim(root_dir)//'" -type f -name "'// &
            trim(pattern)//'" > "'//trim(temp_file)//'" 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=stat)

        call clear_string_array(files)
        if (file_exists(temp_file)) then
            open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
            if (ios == 0) then
                do
                    read (unit, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    if (len_trim(line) > 0) call append_string(files, trim(line))
                end do
                close (unit)
            end if
            open (newunit=unit, file=temp_file, status='old')
            close (unit, status='delete')
        end if
    end subroutine find_files

    subroutine find_image_files(root_dir, files)
    !! Find all image files (PNG, JPG, JPEG) recursively
        character(len=*), intent(in) :: root_dir
        type(string_array), intent(out) :: files
        type(string_array) :: png_files, png_files_upper
        type(string_array) :: jpg_files, jpg_files_upper
        type(string_array) :: jpeg_files, jpeg_files_upper
        type(string_array) :: jpe_files, jpe_files_upper
        integer :: i

        call clear_string_array(files)

        call find_files(root_dir, '*.png', png_files)
        call find_files(root_dir, '*.PNG', png_files_upper)
        call find_files(root_dir, '*.jpg', jpg_files)
        call find_files(root_dir, '*.JPG', jpg_files_upper)
        call find_files(root_dir, '*.jpeg', jpeg_files)
        call find_files(root_dir, '*.JPEG', jpeg_files_upper)
        call find_files(root_dir, '*.jpe', jpe_files)
        call find_files(root_dir, '*.JPE', jpe_files_upper)

        do i = 1, png_files%count
            call append_string(files, png_files%items(i))
        end do
        do i = 1, png_files_upper%count
            call append_string(files, png_files_upper%items(i))
        end do
        do i = 1, jpg_files%count
            call append_string(files, jpg_files%items(i))
        end do
        do i = 1, jpg_files_upper%count
            call append_string(files, jpg_files_upper%items(i))
        end do
        do i = 1, jpeg_files%count
            call append_string(files, jpeg_files%items(i))
        end do
        do i = 1, jpeg_files_upper%count
            call append_string(files, jpeg_files_upper%items(i))
        end do
        do i = 1, jpe_files%count
            call append_string(files, jpe_files%items(i))
        end do
        do i = 1, jpe_files_upper%count
            call append_string(files, jpe_files_upper%items(i))
        end do
    end subroutine find_image_files

    function read_text_file(path, success) result(content)
    !! Read full file contents as a single string preserving newlines
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=:), allocatable :: content
        integer :: unit, ios
        character(len=2048) :: line
        logical :: exists, first_line

        content = ''
        success = .false.
        inquire (file=trim(path), exist=exists)
        if (.not. exists) return

        open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return

        first_line = .true.
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (first_line) then
                content = trim(line)
                first_line = .false.
            else
                content = content//new_line('a')//trim(line)
            end if
        end do
        close (unit)

        if (ios == 0 .or. ios == iostat_end) success = .true.
    end function read_text_file

    subroutine compute_sha256(path, hash, success)
    !! Compute SHA-256 checksum using available command-line tools
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: hash
        logical, intent(out) :: success
        character(len=1024) :: temp_file
        character(len=2048) :: line
        integer :: unit, ios

        temp_file = '/tmp/testboard_sha_'//trim(str_random())//'.txt'

        success = run_command('shasum -a 256', temp_file)
        if (.not. success) success = run_command('sha256sum', temp_file)
        if (.not. success) then
            hash = ''
            call cleanup_temp()
            return
        end if

        open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            hash = ''
            success = .false.
            call cleanup_temp()
            return
        end if

        read (unit, '(A)', iostat=ios) line
        close (unit)
        if (ios /= 0) then
            hash = ''
            success = .false.
            call cleanup_temp()
            return
        end if

        call extract_hash(line, hash, success)
        call cleanup_temp()

    contains

        logical function run_command(tool, outfile) result(ok)
            character(len=*), intent(in) :: tool, outfile
            character(len=1024) :: cmd
            integer :: stat

write (cmd, '(A)') trim(tool)//' "'//trim(path)//'" > "'//trim(outfile)//'" 2>/dev/null'
            call execute_command_line(trim(cmd), exitstat=stat)
            ok = (stat == 0)
        end function run_command

        subroutine extract_hash(source_line, value, ok)
            character(len=*), intent(in) :: source_line
            character(len=:), allocatable, intent(out) :: value
            logical, intent(out) :: ok
            integer :: space_pos
            character(len=:), allocatable :: token

            token = adjustl(source_line)
            space_pos = index(token, ' ')
            if (space_pos == 0) space_pos = index(token, char(9))
            if (space_pos > 0) token = token(1:space_pos - 1)

            if (len_trim(token) < 64) then
                value = ''
                ok = .false.
            else
                allocate (character(len=len_trim(token)) :: value)
                value = trim(token)
                ok = .true.
            end if
        end subroutine extract_hash

        subroutine cleanup_temp()
            integer :: del_unit

            if (file_exists(temp_file)) then
       open (newunit=del_unit, file=temp_file, status='old', action='write', iostat=ios)
                if (ios == 0) close (del_unit, status='delete')
            end if
        end subroutine cleanup_temp

    end subroutine compute_sha256

    function str_random() result(s)
    !! Generate random string for temp files
        character(len=8) :: s
        real :: r
        integer :: i, val
        character(len=36), parameter :: chars = '0123456789abcdefghijklmnopqrstuvwxyz'

        call random_number(r)
        call random_seed()

        do i = 1, 8
            call random_number(r)
            val = int(r*36) + 1
            s(i:i) = chars(val:val)
        end do
    end function str_random

end module file_utils
