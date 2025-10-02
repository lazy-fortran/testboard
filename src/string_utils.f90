module string_utils
  !! String manipulation utilities
  implicit none
  private

  public :: str, trim_null, join_path, ends_with, starts_with
  public :: string_array, append_string

  type :: string_array
    character(len=:), allocatable :: items(:)
    integer :: count = 0
  end type string_array

contains

  function str(i) result(s)
    !! Convert integer to string
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=32) :: buffer
    write(buffer, '(I0)') i
    s = trim(buffer)
  end function str

  function trim_null(input) result(output)
    !! Trim null characters and whitespace
    character(len=*), intent(in) :: input
    character(len=:), allocatable :: output
    integer :: null_pos

    null_pos = index(input, char(0))
    if (null_pos > 0) then
      output = trim(input(1:null_pos-1))
    else
      output = trim(input)
    end if
  end function trim_null

  function join_path(parts) result(path)
    !! Join path components with /
    character(len=*), intent(in) :: parts(:)
    character(len=:), allocatable :: path
    integer :: i, total_len

    if (size(parts) == 0) then
      path = ''
      return
    end if

    ! Calculate total length
    total_len = len_trim(parts(1))
    do i = 2, size(parts)
      total_len = total_len + 1 + len_trim(parts(i))  ! +1 for /
    end do

    allocate(character(len=total_len) :: path)
    path = trim(parts(1))

    do i = 2, size(parts)
      path = trim(path) // '/' // trim(parts(i))
    end do
  end function join_path

  function ends_with(str, suffix) result(match)
    !! Check if string ends with suffix
    character(len=*), intent(in) :: str, suffix
    logical :: match
    integer :: str_len, suf_len

    str_len = len_trim(str)
    suf_len = len(suffix)

    if (suf_len > str_len) then
      match = .false.
    else
      match = (str(str_len-suf_len+1:str_len) == suffix)
    end if
  end function ends_with

  function starts_with(str, prefix) result(match)
    !! Check if string starts with prefix
    character(len=*), intent(in) :: str, prefix
    logical :: match
    integer :: pre_len

    pre_len = len(prefix)
    if (pre_len > len_trim(str)) then
      match = .false.
    else
      match = (str(1:pre_len) == prefix)
    end if
  end function starts_with

  subroutine append_string(arr, str)
    !! Append string to dynamic array
    type(string_array), intent(inout) :: arr
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: temp(:)
    integer :: i, max_len

    if (.not. allocated(arr%items)) then
      allocate(character(len=len(str)) :: arr%items(10))
      arr%count = 1
      arr%items(1) = str
      return
    end if

    if (arr%count >= size(arr%items)) then
      ! Grow array
      max_len = max(len(arr%items), len(str))
      allocate(character(len=max_len) :: temp(size(arr%items) * 2))
      do i = 1, arr%count
        temp(i) = arr%items(i)
      end do
      call move_alloc(temp, arr%items)
    end if

    arr%count = arr%count + 1
    arr%items(arr%count) = str
  end subroutine append_string

end module string_utils
