program test_string_utils
  !! Unit tests for string_utils module
  use string_utils
  implicit none

  integer :: num_tests, num_passed

  num_tests = 0
  num_passed = 0

  call test_str()
  call test_trim_null()
  call test_join_path()
  call test_ends_with()
  call test_starts_with()

  print *, ''
  print *, 'String Utils Tests: ', num_passed, '/', num_tests, ' passed'

  if (num_passed /= num_tests) then
    stop 1
  end if

contains

  subroutine test_str()
    character(len=:), allocatable :: result

    num_tests = num_tests + 1
    result = str(42)
    if (result == '42') then
      num_passed = num_passed + 1
      print *, '[PASS] str: positive number'
    else
      print *, '[FAIL] str: positive number, got: ', result
    end if

    num_tests = num_tests + 1
    result = str(0)
    if (result == '0') then
      num_passed = num_passed + 1
      print *, '[PASS] str: zero'
    else
      print *, '[FAIL] str: zero, got: ', result
    end if
  end subroutine test_str

  subroutine test_trim_null()
    character(len=:), allocatable :: result
    character(len=20) :: input

    num_tests = num_tests + 1
    input = 'hello' // char(0) // 'world'
    result = trim_null(input)
    if (result == 'hello') then
      num_passed = num_passed + 1
      print *, '[PASS] trim_null: null character'
    else
      print *, '[FAIL] trim_null: null character, got: ', result
    end if
  end subroutine test_trim_null

  subroutine test_join_path()
    character(len=:), allocatable :: result
    character(len=32) :: parts(3)

    num_tests = num_tests + 1
    parts(1) = 'usr'
    parts(2) = 'local'
    parts(3) = 'bin'
    result = join_path(parts)
    if (result == 'usr/local/bin') then
      num_passed = num_passed + 1
      print *, '[PASS] join_path: three parts'
    else
      print *, '[FAIL] join_path: three parts, got: ', result
    end if
  end subroutine test_join_path

  subroutine test_ends_with()
    num_tests = num_tests + 1
    if (ends_with('test.png', '.png')) then
      num_passed = num_passed + 1
      print *, '[PASS] ends_with: matches'
    else
      print *, '[FAIL] ends_with: matches'
    end if

    num_tests = num_tests + 1
    if (.not. ends_with('test.jpg', '.png')) then
      num_passed = num_passed + 1
      print *, '[PASS] ends_with: does not match'
    else
      print *, '[FAIL] ends_with: does not match'
    end if
  end subroutine test_ends_with

  subroutine test_starts_with()
    num_tests = num_tests + 1
    if (starts_with('prefix_test', 'prefix')) then
      num_passed = num_passed + 1
      print *, '[PASS] starts_with: matches'
    else
      print *, '[FAIL] starts_with: matches'
    end if

    num_tests = num_tests + 1
    if (.not. starts_with('test', 'prefix')) then
      num_passed = num_passed + 1
      print *, '[PASS] starts_with: does not match'
    else
      print *, '[FAIL] starts_with: does not match'
    end if
  end subroutine test_starts_with

end program test_string_utils
