module file_utils
  !! File and directory utilities
  use string_utils, only: ends_with, join_path, string_array, append_string
  implicit none
  private

  public :: copy_file, create_directory, directory_exists, file_exists
  public :: find_files, find_image_files, remove_directory

contains

  subroutine copy_file(src, dest, success)
    !! Copy file using system command
    character(len=*), intent(in) :: src, dest
    logical, intent(out) :: success
    character(len=1024) :: cmd
    integer :: stat

    write(cmd, '(A)') 'cp "' // trim(src) // '" "' // trim(dest) // '"'
    call execute_command_line(trim(cmd), exitstat=stat)
    success = (stat == 0)
  end subroutine copy_file

  subroutine create_directory(path, success)
    !! Create directory including parents
    character(len=*), intent(in) :: path
    logical, intent(out) :: success
    character(len=1024) :: cmd
    integer :: stat

    write(cmd, '(A)') 'mkdir -p "' // trim(path) // '"'
    call execute_command_line(trim(cmd), exitstat=stat)
    success = (stat == 0)
  end subroutine create_directory

  function directory_exists(path) result(exists)
    !! Check if directory exists
    character(len=*), intent(in) :: path
    logical :: exists
    integer :: stat
    character(len=1024) :: cmd

    write(cmd, '(A)') 'test -d "' // trim(path) // '"'
    call execute_command_line(trim(cmd), exitstat=stat)
    exists = (stat == 0)
  end function directory_exists

  function file_exists(path) result(exists)
    !! Check if file exists
    character(len=*), intent(in) :: path
    logical :: exists
    inquire(file=trim(path), exist=exists)
  end function file_exists

  subroutine remove_directory(path, success)
    !! Remove directory recursively
    character(len=*), intent(in) :: path
    logical, intent(out) :: success
    character(len=1024) :: cmd
    integer :: stat

    write(cmd, '(A)') 'rm -rf "' // trim(path) // '"'
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
    temp_file = '/tmp/testboard_find_' // trim(str_random()) // '.txt'

    write(cmd, '(A)') 'find "' // trim(root_dir) // '" -type f -name "' // &
                      trim(pattern) // '" > "' // trim(temp_file) // '" 2>/dev/null'
    call execute_command_line(trim(cmd), exitstat=stat)

    ! Read results
    files%count = 0
    if (file_exists(temp_file)) then
      open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
      if (ios == 0) then
        do
          read(unit, '(A)', iostat=ios) line
          if (ios /= 0) exit
          if (len_trim(line) > 0) then
            call append_string(files, trim(line))
          end if
        end do
        close(unit)
      end if

      ! Clean up
      open(newunit=unit, file=temp_file, status='old')
      close(unit, status='delete')
    end if
  end subroutine find_files

  subroutine find_image_files(root_dir, files)
    !! Find all image files (PNG, JPG, JPEG) recursively
    character(len=*), intent(in) :: root_dir
    type(string_array), intent(out) :: files
    type(string_array) :: png_files, jpg_files, jpeg_files
    integer :: i

    ! Find all PNG files
    call find_files(root_dir, '*.png', png_files)

    ! Find all JPG files
    call find_files(root_dir, '*.jpg', jpg_files)

    ! Find all JPEG files
    call find_files(root_dir, '*.jpeg', jpeg_files)

    ! Combine all results
    files%count = 0
    do i = 1, png_files%count
      call append_string(files, png_files%items(i))
    end do
    do i = 1, jpg_files%count
      call append_string(files, jpg_files%items(i))
    end do
    do i = 1, jpeg_files%count
      call append_string(files, jpeg_files%items(i))
    end do
  end subroutine find_image_files

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
      val = int(r * 36) + 1
      s(i:i) = chars(val:val)
    end do
  end function str_random

end module file_utils
