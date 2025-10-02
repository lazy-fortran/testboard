module string_utils
  !! String manipulation utilities
    implicit none
    private

    public :: str, trim_null, join_path, ends_with, starts_with
    public :: string_array, append_string, clear_string_array
    public :: contains_string, replace_all

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

        write (buffer, '(I0)') i
        s = trim(buffer)
    end function str

    function trim_null(input) result(output)
    !! Trim null characters and whitespace
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: null_pos

        null_pos = index(input, char(0))
        if (null_pos > 0) then
            output = trim(input(1:null_pos - 1))
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

        total_len = len_trim(parts(1))
        do i = 2, size(parts)
            total_len = total_len + 1 + len_trim(parts(i))
        end do

        allocate (character(len=total_len) :: path)
        path = trim(parts(1))

        do i = 2, size(parts)
            path = trim(path)//'/'//trim(parts(i))
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
            match = (str(str_len - suf_len + 1:str_len) == suffix)
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

    subroutine clear_string_array(arr)
    !! Reset a string_array to empty
        type(string_array), intent(inout) :: arr

        if (allocated(arr%items)) then
            deallocate (arr%items)
        end if
        arr%count = 0
    end subroutine clear_string_array

    subroutine ensure_capacity(arr, new_len)
    !! Ensure array can store elements of at least new_len characters
        type(string_array), intent(inout) :: arr
        integer, intent(in) :: new_len
        character(len=:), allocatable :: temp(:)
        integer :: i

        if (.not. allocated(arr%items)) return
        if (new_len <= len(arr%items)) return

        allocate (character(len=new_len) :: temp(size(arr%items)))
        do i = 1, arr%count
            temp(i) = arr%items(i)
        end do
        call move_alloc(temp, arr%items)
    end subroutine ensure_capacity

    subroutine grow_array(arr, new_len)
    !! Grow array capacity when needed
        type(string_array), intent(inout) :: arr
        integer, intent(in) :: new_len
        character(len=:), allocatable :: temp(:)
        integer :: i, capacity, element_len

        if (.not. allocated(arr%items)) return

        element_len = max(len(arr%items), new_len)
        capacity = max(2*size(arr%items), 1)

        allocate (character(len=element_len) :: temp(capacity))
        do i = 1, arr%count
            temp(i) = arr%items(i)
        end do
        call move_alloc(temp, arr%items)
    end subroutine grow_array

    subroutine append_string(arr, str)
    !! Append string to dynamic array
        type(string_array), intent(inout) :: arr
        character(len=*), intent(in) :: str
        integer :: required_len

        required_len = len_trim(str)
        if (required_len == 0) required_len = 1

        if (.not. allocated(arr%items)) then
            allocate (character(len=required_len) :: arr%items(10))
            arr%count = 0
        end if

        if (len(arr%items) < required_len) then
            call ensure_capacity(arr, required_len)
        end if

        if (arr%count >= size(arr%items)) then
            call grow_array(arr, required_len)
        end if

        arr%count = arr%count + 1
        arr%items(arr%count) = trim(str)
    end subroutine append_string

    logical function contains_string(arr, value) result(found)
    !! Check if value exists in string_array
        type(string_array), intent(in) :: arr
        character(len=*), intent(in) :: value
        integer :: i

        found = .false.
        if (.not. allocated(arr%items)) return

        do i = 1, arr%count
            if (trim(arr%items(i)) == trim(value)) then
                found = .true.
                return
            end if
        end do
    end function contains_string

    function replace_all(input, pattern, replacement) result(output)
    !! Replace all occurrences of pattern with replacement
        character(len=*), intent(in) :: input, pattern, replacement
        character(len=:), allocatable :: output
        integer :: start_idx, found_idx, pattern_len, input_len

        if (len(pattern) == 0) then
            output = input
            return
        end if

        output = ''
        pattern_len = len(pattern)
        input_len = len_trim(input)
        start_idx = 1

        do
            found_idx = index(input(start_idx:input_len), pattern)
            if (found_idx == 0) then
                output = output//input(start_idx:input_len)
                exit
            end if

            if (found_idx > 1) then
                output = output//input(start_idx:start_idx + found_idx - 2)
            end if
            output = output//replacement
            start_idx = start_idx + found_idx - 1 + pattern_len
            if (start_idx > input_len) exit
        end do
    end function replace_all

end module string_utils
