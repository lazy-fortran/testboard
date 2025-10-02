program testboard_app
  !! Generate test dashboards from CI artifacts
    use dashboard
    implicit none

    type(dashboard_config) :: config
    logical :: success
    integer :: i, nargs
    character(len=512) :: arg, value

    ! Parse command line arguments
    nargs = command_argument_count()

    i = 1
    do while (i <= nargs)
        call get_command_argument(i, arg)

        select case (trim(arg))
        case ('--image-root')
            i = i + 1
            call get_command_argument(i, value)
            config%image_root = trim(value)

        case ('--output')
            i = i + 1
            call get_command_argument(i, value)
            config%output_dir = trim(value)

        case ('--branch')
            i = i + 1
            call get_command_argument(i, value)
            config%branch_name = trim(value)

        case ('--commit')
            i = i + 1
            call get_command_argument(i, value)
            config%commit_sha = trim(value)

        case ('--run-id')
            i = i + 1
            call get_command_argument(i, value)
            config%run_id = trim(value)

        case ('--repo')
            i = i + 1
            call get_command_argument(i, value)
            config%repo = trim(value)

        case ('--project-name')
            i = i + 1
            call get_command_argument(i, value)
            config%project_name = trim(value)

        case ('--github-pages-url')
            i = i + 1
            call get_command_argument(i, value)
            config%github_pages_url = trim(value)

        case ('--help', '-h')
            call print_help()
            stop

        case default
            print *, 'Unknown argument: ', trim(arg)
            call print_help()
            stop 1
        end select

        i = i + 1
    end do

    ! Validate required arguments
    if (len_trim(config%branch_name) == 0) then
        print *, 'Error: --branch is required'
        call print_help()
        stop 1
    end if

    if (len_trim(config%commit_sha) == 0) then
        print *, 'Error: --commit is required'
        call print_help()
        stop 1
    end if

    if (len_trim(config%run_id) == 0) then
        print *, 'Error: --run-id is required'
        call print_help()
        stop 1
    end if

    if (len_trim(config%repo) == 0) then
        print *, 'Error: --repo is required'
        call print_help()
        stop 1
    end if

    ! Generate dashboard
    print *, 'Generating dashboard for branch: ', trim(config%branch_name)
    print *, 'Project name: ', trim(config%project_name)
    print *, 'Output directory: ', trim(config%output_dir)

    call generate_dashboard(config, success)

    if (success) then
        print *, 'Dashboard generated successfully!'
    else
        print *, 'Error: Dashboard generation failed'
        stop 1
    end if

contains

    subroutine print_help()
        print *, 'testboard - Generate test dashboards from CI artifacts'
        print *, ''
        print *, 'Usage: testboard [OPTIONS]'
        print *, ''
        print *, 'Required options:'
        print *, '  --branch BRANCH        Branch name'
        print *, '  --commit SHA           Commit SHA'
        print *, '  --run-id ID            CI run identifier'
        print *, '  --repo OWNER/REPO      Repository in owner/repo format'
        print *, ''
        print *, 'Optional:'
        print *, '  --image-root DIR       Image dir (default: image-artifacts)'
        print *, '  --output DIR           Output directory (default: dashboard)'
        print *, '  --project-name NAME    Project name (default: testboard)'
        print *, '  --github-pages-url URL Base URL for GitHub Pages'
        print *, '  --help, -h             Show this help'
    end subroutine print_help

end program testboard_app
