module template_engine
  !! Lightweight template rendering utilities (Mustache-style placeholders)
    use string_utils, only: replace_all
    use file_utils, only: read_text_file
    implicit none
    private

    public :: template_context, init_template_context, add_template_value
    public :: render_template, load_template

    type :: template_entry
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
    end type template_entry

    type :: template_context
        type(template_entry), allocatable :: entries(:)
    end type template_context

contains

    subroutine init_template_context(ctx)
        type(template_context), intent(inout) :: ctx

        if (allocated(ctx%entries)) then
            deallocate (ctx%entries)
        end if
    end subroutine init_template_context

    subroutine add_template_value(ctx, key, value)
        type(template_context), intent(inout) :: ctx
        character(len=*), intent(in) :: key, value
        type(template_entry), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(ctx%entries)) then
            allocate (ctx%entries(1))
            ctx%entries(1)%key = trim(key)
            ctx%entries(1)%value = trim(value)
            return
        end if

        n = size(ctx%entries)
        allocate (temp(n + 1))
        temp(1:n) = ctx%entries
        temp(n + 1)%key = trim(key)
        temp(n + 1)%value = trim(value)
        call move_alloc(temp, ctx%entries)
    end subroutine add_template_value

    function load_template(path, success) result(content)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=:), allocatable :: content

        content = read_text_file(trim(path), success)
        if (.not. success) then
            content = ''
        end if
    end function load_template

    function render_template(template, ctx) result(output)
        character(len=*), intent(in) :: template
        type(template_context), intent(in) :: ctx
        character(len=:), allocatable :: output
        integer :: i, j, count
        integer, allocatable :: order(:)
        character(len=:), allocatable :: token, value

        output = template
        if (.not. allocated(ctx%entries)) return

        count = size(ctx%entries)
        if (count <= 0) return

        allocate (order(count))
        do i = 1, count
            order(i) = i
        end do

        do i = 1, count - 1
            do j = i + 1, count
                if (len_trim(ctx%entries(order(j))%key) > &
                    len_trim(ctx%entries(order(i))%key)) then
                    call swap_int(order(i), order(j))
                end if
            end do
        end do

        do i = 1, count
            token = '{{'//trim(ctx%entries(order(i))%key)//'}}'
            value = trim(ctx%entries(order(i))%value)
            output = replace_all(output, token, value)
        end do

        deallocate (order)
    end function render_template

    subroutine swap_int(a, b)
        integer, intent(inout) :: a, b
        integer :: tmp

        tmp = a
        a = b
        b = tmp
    end subroutine swap_int

end module template_engine
