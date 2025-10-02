program test_integration
  !! Integration tests - test dashboard generation with mock data
    use dashboard
    use file_utils
    implicit none

    integer :: num_tests, num_passed
    integer, parameter :: basic_png_bytes(68) = [integer :: &
                                                 137, 80, 78, 71, 13, 10, 26, 10, &
                                                 0, 0, 0, 13, 73, 72, 68, 82, &
                                                 0, 0, 0, 1, 0, 0, 0, 1, &
                                                 8, 4, 0, 0, 0, 181, 28, 12, &
                                                 2, 0, 0, 0, 11, 73, 68, 65, &
                                                 84, 120, 218, 99, 252, 255, 31, 0, &
                                                 3, 3, 2, 0, 238, 103, 208, 90, &
                                                 0, 0, 0, 0, 73, 69, 78, 68, &
                                                 174, 66, 96, 130]
    integer, parameter :: fancy_png_bytes(104) = [integer :: &
                                                  137, 80, 78, 71, 13, 10, 26, 10, &
                                                  0, 0, 0, 13, 73, 72, 68, 82, &
                                                  0, 0, 0, 32, 0, 0, 0, 32, &
                                                  8, 6, 0, 0, 0, 115, 122, 122, &
                                                  244, 0, 0, 0, 47, 73, 68, 65, &
                                                  84, 120, 156, 237, 206, 49, 1, 0, &
                                                  48, 12, 128, 48, 54, 255, 158, 91, &
                                                  25, 125, 130, 1, 242, 166, 166, 195, &
                                                  254, 229, 28, 0, 0, 0, 0, 0, &
                                                  0, 0, 0, 0, 0, 0, 0, 0, &
                                                  0, 160, 106, 1, 48, 227, 2, 62, &
                                                  54, 153, 94, 177, 0, 0, 0, 0, &
                                                  73, 69, 78, 68, 174, 66, 96, 130]
    logical :: success

    num_tests = 0
    num_passed = 0

    call test_dashboard_generation()
    call test_multiple_branches()
    call test_nested_branch_gallery()

    print *, ''
    print *, 'Integration Tests: ', num_passed, '/', num_tests, ' passed'

    if (num_passed /= num_tests) then
        stop 1
    end if

contains

    subroutine test_dashboard_generation()
        type(dashboard_config) :: config
        logical :: exists

        num_tests = num_tests + 1

        ! Setup test config
        config%image_root = 'test_artifacts'
        config%output_dir = 'test_output'
        config%branch_name = 'test-branch'
        config%commit_sha = 'abc123def456'
        config%run_id = '12345'
        config%repo = 'test-org/test-repo'
        config%project_name = 'Test Project'

        ! Create mock image directory (empty for this test)
        call create_directory(config%image_root, success)

        ! Generate dashboard
        call generate_dashboard(config, success)

        if (success .and. &
            file_exists(trim(config%output_dir)//'/test/test-branch/index.html') .and. &
            file_exists(trim(config%output_dir)//'/test/index.html') .and. &
            file_exists(trim(config%output_dir)//'/index.html')) then
            num_passed = num_passed + 1
            print *, '[PASS] dashboard_generation: creates all files'
        else
            print *, '[FAIL] dashboard_generation: creates all files'
            print *, '  Success: ', success
            print *, '  Branch page: ', &
                file_exists(trim(config%output_dir)//'/test/test-branch/index.html')
            print *, '  Overview page: ', &
                file_exists(trim(config%output_dir)//'/test/index.html')
            print *, '  Root page: ', &
                file_exists(trim(config%output_dir)//'/index.html')
        end if

        ! Cleanup
        call remove_directory(config%image_root, success)
        call remove_directory(config%output_dir, success)
    end subroutine test_dashboard_generation

    subroutine test_multiple_branches()
        type(dashboard_config) :: config
        logical :: exists

        num_tests = num_tests + 1

        ! Setup test config
        config%image_root = 'test_artifacts'
        config%output_dir = 'test_output'
        config%repo = 'test-org/test-repo'
        config%project_name = 'Test Project'

        call create_directory(config%image_root, success)

        ! Generate dashboard for branch 1
        config%branch_name = 'main'
        config%commit_sha = 'abc123'
        config%run_id = '11111'
        call generate_dashboard(config, success)

        ! Generate dashboard for branch 2
        config%branch_name = 'feature'
        config%commit_sha = 'def456'
        config%run_id = '22222'
        call generate_dashboard(config, success)

        ! Check both branches exist
        if (success .and. &
            file_exists(trim(config%output_dir)//'/test/main/index.html') .and. &
            file_exists(trim(config%output_dir)//'/test/feature/index.html') .and. &
            file_exists(trim(config%output_dir)//'/test/branches.json')) then
            num_passed = num_passed + 1
            print *, '[PASS] multiple_branches: both branches exist'
        else
            print *, '[FAIL] multiple_branches: both branches exist'
        end if

        ! Cleanup
        call remove_directory(config%image_root, success)
        call remove_directory(config%output_dir, success)
    end subroutine test_multiple_branches

    subroutine test_nested_branch_gallery()
        type(dashboard_config) :: config
        character(len=:), allocatable :: branch_dir, branch_file
        character(len=:), allocatable :: fancy_file, basic_file
        logical :: run_success, fancy_exists, basic_exists, link_ok, style_ok
        integer :: unit, ios
        character(len=1024) :: line

        num_tests = num_tests + 1

        config%image_root = 'test_artifacts'
        config%output_dir = 'test_output'
        config%branch_name = 'feat/support-png-jpg-images'
        config%commit_sha = 'feedfacecafebeef'
        config%run_id = '33333'
        config%repo = 'test-org/test-repo'
        config%project_name = 'Test Project'

        call create_directory(trim(config%image_root)//'/basic', run_success)
        call create_directory(trim(config%image_root)//'/fancy', run_success)

        call write_png_fixture(trim(config%image_root)//'/basic/chart.png', &
                               basic_png_bytes)
        call write_png_fixture(trim(config%image_root)//'/fancy/chart.PNG', &
                               fancy_png_bytes)

        call generate_dashboard(config, run_success)

        branch_dir = trim(config%output_dir)//'/test/feat/support-png-jpg-images'
        branch_file = trim(branch_dir)//'/index.html'
        fancy_file = trim(branch_dir)//'/images/fancy/chart.PNG'
        basic_file = trim(branch_dir)//'/images/basic/chart.png'

        fancy_exists = file_exists(fancy_file)
        basic_exists = file_exists(basic_file)

        link_ok = .false.
        style_ok = .false.
        open (newunit=unit, file=branch_file, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, 'href="../../index.html"') > 0) then
                    link_ok = .true.
                end if
                if (index(line, 'img { width: min(100%, 320px); }') > 0) then
                    style_ok = .true.
                end if
                if (link_ok .and. style_ok) exit
            end do
            close (unit)
        end if

        if (run_success .and. fancy_exists .and. basic_exists .and. link_ok .and. &
            style_ok) then
            num_passed = num_passed + 1
            print *, '[PASS] nested_branch_gallery: preserves fancy outputs and link'
        else
            print *, '[FAIL] nested_branch_gallery: preserves fancy outputs and link'
            print *, '  Success: ', run_success
            print *, '  Fancy exists: ', fancy_exists
            print *, '  Basic exists: ', basic_exists
            print *, '  Link ok: ', link_ok
            print *, '  Style ok: ', style_ok
        end if

        call remove_directory(config%image_root, run_success)
        call remove_directory(config%output_dir, run_success)
    end subroutine test_nested_branch_gallery

    subroutine write_png_fixture(path, data)
        character(len=*), intent(in) :: path
        integer, intent(in) :: data(:)
        integer :: unit, ios, i
        character(len=1) :: ch

        open (newunit=unit, file=trim(path), status='replace', access='stream', &
              form='unformatted', action='write', iostat=ios)
        if (ios /= 0) return

        do i = 1, size(data)
            ch = achar(data(i))
            write (unit, iostat=ios) ch
            if (ios /= 0) exit
        end do

        close (unit)
    end subroutine write_png_fixture

end program test_integration
