module file_utils
  !! File and directory utilities
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use string_utils, only: ends_with, join_path, string_array, append_string
    implicit none
    private

    public :: copy_file, create_directory, directory_exists, file_exists
    public :: find_files, find_image_files, remove_directory, is_tiny_image

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

        ! Create temporary file for results
        temp_file = '/tmp/testboard_find_'//trim(str_random())//'.txt'

        write (cmd, '(A)') 'find "'//trim(root_dir)//'" -type f -name "'// &
            trim(pattern)//'" > "'//trim(temp_file)//'" 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=stat)

        ! Read results
        files%count = 0
        if (file_exists(temp_file)) then
            open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
            if (ios == 0) then
                do
                    read (unit, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    if (len_trim(line) > 0) then
                        call append_string(files, trim(line))
                    end if
                end do
                close (unit)
            end if

            ! Clean up
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

        if (allocated(files%items)) then
            deallocate (files%items)
        end if
        files%count = 0

        ! Find PNG files (case-insensitive)
        call find_files(root_dir, '*.png', png_files)
        call find_files(root_dir, '*.PNG', png_files_upper)

        ! Find JPG files (case-insensitive)
        call find_files(root_dir, '*.jpg', jpg_files)
        call find_files(root_dir, '*.JPG', jpg_files_upper)

        ! Find JPEG files (case-insensitive, including .jpe)
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

    logical function is_tiny_image(path) result(tiny)
    !! Determine if an image should be treated as too small to keep
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=8) :: signature
        character(len=4) :: chunk_len_bytes, chunk_type
        character(len=4) :: width_bytes, height_bytes
        integer(kind=int32) :: width32, height32
        integer(kind=int64) :: size_bytes

        tiny = .false.

        if (.not. file_exists(path)) then
            tiny = .true.
            return
        end if

        inquire (file=trim(path), size=size_bytes)

        open (newunit=unit, file=trim(path), status='old', access='stream', &
              form='unformatted', action='read', iostat=ios)
        if (ios /= 0) then
            tiny = (size_bytes <= 128_int64)
            return
        end if

        read (unit, iostat=ios) signature
        if (ios /= 0) then
            close (unit)
            tiny = (size_bytes <= 128_int64)
            return
        end if

        if (signature /= char(137)//'PNG'//char(13)//char(10)//char(26)//char(10)) then
            close (unit)
            tiny = (size_bytes <= 128_int64)
            return
        end if

        read (unit, iostat=ios) chunk_len_bytes
        if (ios /= 0) then
            close (unit)
            tiny = (size_bytes <= 128_int64)
            return
        end if

        read (unit, iostat=ios) chunk_type
        if (ios /= 0) then
            close (unit)
            tiny = (size_bytes <= 128_int64)
            return
        end if

        if (chunk_type /= 'IHDR') then
            close (unit)
            tiny = (size_bytes <= 128_int64)
            return
        end if

        read (unit, iostat=ios) width_bytes
        if (ios /= 0) then
            close (unit)
            tiny = (size_bytes <= 128_int64)
            return
        end if

        read (unit, iostat=ios) height_bytes
        close (unit)
        if (ios /= 0) then
            tiny = (size_bytes <= 128_int64)
            return
        end if

        width32 = uint32_from_bytes(width_bytes)
        height32 = uint32_from_bytes(height_bytes)

        if (width32 <= 0_int32 .or. height32 <= 0_int32) then
            tiny = .true.
            return
        end if

        if (int(width32, int64)*int(height32, int64) <= 256_int64) then
            tiny = .true.
        else
            tiny = .false.
        end if
    end function is_tiny_image

    pure function uint32_from_bytes(bytes) result(value)
    !! Convert 4-byte big-endian sequence to int32
        character(len=4), intent(in) :: bytes
        integer(kind=int32) :: value
        integer :: b1, b2, b3, b4

        b1 = iachar(bytes(1:1))
        b2 = iachar(bytes(2:2))
        b3 = iachar(bytes(3:3))
        b4 = iachar(bytes(4:4))

        value = int(shiftl(b1, 24) + shiftl(b2, 16) + shiftl(b3, 8) + b4, int32)
    end function uint32_from_bytes

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
