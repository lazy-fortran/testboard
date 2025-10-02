program test_html_utils
  !! Unit tests for html_utils module
  use html_utils
  implicit none

  integer :: num_tests, num_passed

  num_tests = 0
  num_passed = 0

  call test_html_escape()
  call test_build_gallery_item()
  call test_build_html_page()

  print *, ''
  print *, 'HTML Utils Tests: ', num_passed, '/', num_tests, ' passed'

  if (num_passed /= num_tests) then
    stop 1
  end if

contains

  subroutine test_html_escape()
    character(len=:), allocatable :: result

    num_tests = num_tests + 1
    result = html_escape('Hello <world>')
    if (result == 'Hello &lt;world&gt;') then
      num_passed = num_passed + 1
      print *, '[PASS] html_escape: basic < >'
    else
      print *, '[FAIL] html_escape: basic < >, got: ', result
    end if

    num_tests = num_tests + 1
    result = html_escape('A & B')
    if (result == 'A &amp; B') then
      num_passed = num_passed + 1
      print *, '[PASS] html_escape: ampersand'
    else
      print *, '[FAIL] html_escape: ampersand, got: ', result
    end if

    num_tests = num_tests + 1
    result = html_escape('"quoted"')
    if (result == '&quot;quoted&quot;') then
      num_passed = num_passed + 1
      print *, '[PASS] html_escape: quotes'
    else
      print *, '[FAIL] html_escape: quotes, got: ', result
    end if

    num_tests = num_tests + 1
    result = html_escape("O'Brien")
    if (result == 'O&#x27;Brien') then
      num_passed = num_passed + 1
      print *, '[PASS] html_escape: apostrophe'
    else
      print *, '[FAIL] html_escape: apostrophe, got: ', result
    end if
  end subroutine test_html_escape

  subroutine test_build_gallery_item()
    character(len=:), allocatable :: result

    num_tests = num_tests + 1
    result = build_gallery_item('test.png', 'images/test.png')
    if (index(result, '<figure>') > 0 .and. &
        index(result, 'images/test.png') > 0 .and. &
        index(result, '<figcaption>test.png</figcaption>') > 0) then
      num_passed = num_passed + 1
      print *, '[PASS] build_gallery_item: basic structure'
    else
      print *, '[FAIL] build_gallery_item: basic structure'
    end if
  end subroutine test_build_gallery_item

  subroutine test_build_html_page()
    character(len=:), allocatable :: result

    num_tests = num_tests + 1
    result = build_html_page('Test Page', '<p>Hello</p>')
    if (index(result, '<!DOCTYPE html>') > 0 .and. &
        index(result, '<title>Test Page</title>') > 0 .and. &
        index(result, '<p>Hello</p>') > 0) then
      num_passed = num_passed + 1
      print *, '[PASS] build_html_page: basic structure'
    else
      print *, '[FAIL] build_html_page: basic structure'
    end if
  end subroutine test_build_html_page

end program test_html_utils
