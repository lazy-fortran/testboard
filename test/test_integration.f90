program test_integration
  !! Integration tests - test dashboard generation with mock data
  use dashboard
  use file_utils
  implicit none

  integer :: num_tests, num_passed
  logical :: success

  num_tests = 0
  num_passed = 0

  call test_dashboard_generation()
  call test_multiple_branches()

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
        file_exists(trim(config%output_dir) // '/test/test-branch/index.html') .and. &
        file_exists(trim(config%output_dir) // '/test/index.html') .and. &
        file_exists(trim(config%output_dir) // '/index.html')) then
      num_passed = num_passed + 1
      print *, '[PASS] dashboard_generation: creates all files'
    else
      print *, '[FAIL] dashboard_generation: creates all files'
      print *, '  Success: ', success
      print *, '  Branch page: ', file_exists(trim(config%output_dir) // '/test/test-branch/index.html')
      print *, '  Overview page: ', file_exists(trim(config%output_dir) // '/test/index.html')
      print *, '  Root page: ', file_exists(trim(config%output_dir) // '/index.html')
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
        file_exists(trim(config%output_dir) // '/test/main/index.html') .and. &
        file_exists(trim(config%output_dir) // '/test/feature/index.html') .and. &
        file_exists(trim(config%output_dir) // '/test/branches.json')) then
      num_passed = num_passed + 1
      print *, '[PASS] multiple_branches: both branches exist'
    else
      print *, '[FAIL] multiple_branches: both branches exist'
    end if

    ! Cleanup
    call remove_directory(config%image_root, success)
    call remove_directory(config%output_dir, success)
  end subroutine test_multiple_branches

end program test_integration
