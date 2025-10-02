module html_utils
  !! Utilities for HTML generation and escaping
  implicit none
  private

  public :: html_escape, build_html_page, build_gallery_item

contains

  function html_escape(input) result(output)
    !! Escape HTML special characters
    character(len=*), intent(in) :: input
    character(len=:), allocatable :: output
    integer :: i, n, new_len
    character(len=1) :: c

    ! First pass: calculate required length
    new_len = 0
    do i = 1, len(input)
      c = input(i:i)
      select case(c)
        case('&')
          new_len = new_len + 5  ! &amp;
        case('<')
          new_len = new_len + 4  ! &lt;
        case('>')
          new_len = new_len + 4  ! &gt;
        case('"')
          new_len = new_len + 6  ! &quot;
        case("'")
          new_len = new_len + 6  ! &#x27;
        case default
          new_len = new_len + 1
      end select
    end do

    ! Second pass: build escaped string
    allocate(character(len=new_len) :: output)
    n = 0
    do i = 1, len(input)
      c = input(i:i)
      select case(c)
        case('&')
          output(n+1:n+5) = '&amp;'
          n = n + 5
        case('<')
          output(n+1:n+4) = '&lt;'
          n = n + 4
        case('>')
          output(n+1:n+4) = '&gt;'
          n = n + 4
        case('"')
          output(n+1:n+6) = '&quot;'
          n = n + 6
        case("'")
          output(n+1:n+6) = '&#x27;'
          n = n + 6
        case default
          output(n+1:n+1) = c
          n = n + 1
      end select
    end do
  end function html_escape

  function build_gallery_item(rel_path, img_path) result(html)
    !! Build HTML for a single gallery item
    character(len=*), intent(in) :: rel_path, img_path
    character(len=:), allocatable :: html
    character(len=:), allocatable :: escaped_path, escaped_rel

    escaped_path = html_escape(img_path)
    escaped_rel = html_escape(rel_path)

    html = '<figure>' // &
           '<a href="' // escaped_path // '" target="_blank">' // &
           '<img src="' // escaped_path // '" alt="' // escaped_rel // '" loading="lazy">' // &
           '</a>' // &
           '<figcaption>' // escaped_rel // '</figcaption>' // &
           '</figure>'
  end function build_gallery_item

  function build_html_page(title, body_content, extra_style) result(html)
    !! Build a complete HTML page
    character(len=*), intent(in) :: title, body_content
    character(len=*), intent(in), optional :: extra_style
    character(len=:), allocatable :: html
    character(len=:), allocatable :: style_section
    character(len=*), parameter :: base_style = &
      "body { font-family: Arial, sans-serif; margin: 2rem; }" // new_line('a') // &
      "a { color: #0366d6; text-decoration: none; }" // new_line('a') // &
      "a:hover { text-decoration: underline; }" // new_line('a') // &
      "code { font-family: Consolas, Monaco, monospace; background: #f6f8fa; " // &
      "padding: 2px 4px; border-radius: 3px; }"

    if (present(extra_style)) then
      style_section = base_style // new_line('a') // extra_style
    else
      style_section = base_style
    end if

    html = '<!DOCTYPE html>' // new_line('a') // &
           '<html lang="en">' // new_line('a') // &
           '<head>' // new_line('a') // &
           '  <meta charset="utf-8">' // new_line('a') // &
           '  <title>' // html_escape(title) // '</title>' // new_line('a') // &
           '  <style>' // new_line('a') // &
           style_section // new_line('a') // &
           '  </style>' // new_line('a') // &
           '</head>' // new_line('a') // &
           '<body>' // new_line('a') // &
           body_content // new_line('a') // &
           '</body>' // new_line('a') // &
           '</html>'
  end function build_html_page

end module html_utils
