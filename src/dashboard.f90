module dashboard
  !! Main dashboard generation logic
  use html_utils
  use string_utils
  use file_utils
  use json_utils
  use gh_api
  use datetime_utils
  implicit none
  private

  public :: generate_dashboard, dashboard_config

  type :: dashboard_config
    character(len=512) :: png_root = 'png-artifacts'
    character(len=512) :: output_dir = 'dashboard'
    character(len=256) :: branch_name = ''
    character(len=64) :: commit_sha = ''
    character(len=64) :: run_id = ''
    character(len=256) :: repo = ''
    character(len=256) :: project_name = 'Test Dashboard'
    character(len=512) :: github_pages_url = ''
  end type dashboard_config

contains

  subroutine generate_dashboard(config, success)
    !! Generate complete dashboard from configuration
    type(dashboard_config), intent(in) :: config
    logical, intent(out) :: success
    type(string_array) :: png_files
    type(branch_metadata) :: current_branch, all_branches(100)
    integer :: n_branches
    character(len=512) :: test_root, branch_path, images_path
    character(len=512) :: metadata_file, branch_html_file, index_file
    character(len=:), allocatable :: gallery_html, branch_html, overview_html
    character(len=25) :: timestamp
    logical :: stat

    success = .false.

    ! Create output directories
    test_root = trim(config%output_dir) // '/test'
    branch_path = trim(test_root) // '/' // trim(config%branch_name)
    images_path = trim(branch_path) // '/images'

    call create_directory(images_path, stat)
    if (.not. stat) then
      print *, 'Error: Failed to create directory: ', trim(images_path)
      return
    end if

    ! Copy PNG files
    call find_files(config%png_root, '*.png', png_files)
    call copy_png_files(config%png_root, images_path, png_files)

    ! Get current timestamp and PR info
    timestamp = get_iso8601_timestamp()
    current_branch%branch_name = config%branch_name
    current_branch%commit = config%commit_sha
    current_branch%timestamp = timestamp
    current_branch%run_id = config%run_id
    current_branch%repo = config%repo
    current_branch%has_pngs = (png_files%count > 0)

    call get_pr_info(config%branch_name, config%repo, current_branch, stat)

    ! Generate branch page
    gallery_html = build_gallery(png_files)
    branch_html = build_branch_page(config, current_branch, gallery_html)

    branch_html_file = trim(branch_path) // '/index.html'
    call write_file(branch_html_file, branch_html, stat)
    if (.not. stat) return

    ! Update metadata
    metadata_file = trim(test_root) // '/branches.json'
    call update_metadata(metadata_file, current_branch, all_branches, n_branches)

    ! Generate overview page
    overview_html = build_overview_page(config, all_branches, n_branches, timestamp)
    index_file = trim(test_root) // '/index.html'
    call write_file(index_file, overview_html, stat)
    if (.not. stat) return

    ! Generate root redirect
    call generate_root_redirect(config, timestamp, stat)
    if (.not. stat) return

    success = .true.
  end subroutine generate_dashboard

  subroutine copy_png_files(src_root, dest_root, files)
    !! Copy PNG files preserving directory structure
    character(len=*), intent(in) :: src_root, dest_root
    type(string_array), intent(in) :: files
    integer :: i
    logical :: stat

    do i = 1, files%count
      call copy_file(trim(files%items(i)), &
                     trim(dest_root) // '/' // trim(basename(files%items(i))), stat)
    end do
  end subroutine copy_png_files

  function basename(path) result(name)
    !! Extract filename from path
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: name
    integer :: pos

    pos = index(path, '/', back=.true.)
    if (pos > 0) then
      name = trim(path(pos+1:))
    else
      name = trim(path)
    end if
  end function basename

  function build_gallery(files) result(html)
    !! Build HTML gallery from file list
    type(string_array), intent(in) :: files
    character(len=:), allocatable :: html
    integer :: i
    character(len=512) :: rel_path

    if (files%count == 0) then
      html = '<p>No PNG outputs were produced for this run.</p>'
      return
    end if

    html = '<div class="gallery">'
    do i = 1, files%count
      rel_path = 'images/' // trim(basename(files%items(i)))
      html = html // build_gallery_item(trim(basename(files%items(i))), trim(rel_path))
    end do
    html = html // '</div>'
  end function build_gallery

  function build_branch_page(config, branch, gallery) result(html)
    !! Build HTML page for a specific branch
    type(dashboard_config), intent(in) :: config
    type(branch_metadata), intent(in) :: branch
    character(len=*), intent(in) :: gallery
    character(len=:), allocatable :: html, body, title, pr_section, extra_style
    character(len=2048) :: buffer

    title = trim(config%project_name) // ' – ' // trim(branch%branch_name)

    ! Build PR section if available
    pr_section = ''
    if (branch%pr_number > 0) then
      write(buffer, '(A)') '<p><strong>Pull Request:</strong> <a href="' // &
        trim(branch%pr_url) // '">#' // trim(str(branch%pr_number)) // ' ' // &
        trim(html_escape(branch%pr_title)) // '</a>'
      if (branch%pr_draft) then
        buffer = trim(buffer) // ' <span style="background:#6a737d;color:white;' // &
                 'padding:2px 6px;border-radius:3px;font-size:0.85em">DRAFT</span>'
      end if
      pr_section = trim(buffer) // '</p>'
    end if

    ! Build body
    body = '<a href="../index.html" class="back-link">← Back to all branches</a>' // &
           '<h1>Test Dashboard – ' // html_escape(branch%branch_name) // '</h1>' // &
           '<div class="meta">' // &
           '<p><strong>Branch:</strong> ' // html_escape(branch%branch_name) // '</p>' // &
           trim(pr_section) // &
           '<p><strong>Commit:</strong> <code>' // html_escape(branch%commit) // '</code></p>' // &
           '<p><strong>Workflow run:</strong> <a href="https://github.com/' // &
           trim(config%repo) // '/actions/runs/' // trim(branch%run_id) // '">' // &
           trim(branch%run_id) // '</a></p>' // &
           '<p><strong>Generated:</strong> ' // html_escape(branch%timestamp) // '</p>' // &
           '</div>' // &
           gallery

    extra_style = &
      '.meta { margin-bottom: 1.5rem; }' // new_line('a') // &
      '.gallery { display: grid; grid-template-columns: repeat(auto-fill, minmax(320px, 1fr)); gap: 1.5rem; }' // new_line('a') // &
      'figure { margin: 0; }' // new_line('a') // &
      'figure a { display: block; cursor: pointer; }' // new_line('a') // &
      'figcaption { margin-top: 0.5rem; font-size: 0.9rem; word-break: break-word; }' // new_line('a') // &
      'img { width: 100%; height: auto; border: 1px solid #ccd; box-shadow: 0 2px 4px rgba(0,0,0,0.1); ' // &
      'transition: box-shadow 0.2s, transform 0.2s; }' // new_line('a') // &
      'img:hover { box-shadow: 0 4px 8px rgba(0,0,0,0.2); transform: scale(1.02); }' // new_line('a') // &
      '.back-link { display: inline-block; margin-bottom: 1rem; }'

    html = build_html_page(title, body, extra_style)
  end function build_branch_page

  function build_overview_page(config, branches, n, timestamp) result(html)
    !! Build overview page with all branches
    type(dashboard_config), intent(in) :: config
    type(branch_metadata), intent(in) :: branches(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: timestamp
    character(len=:), allocatable :: html, body, title, rows, extra_style
    integer :: i
    character(len=2048) :: row

    title = trim(config%project_name) // ' Dashboards'

    rows = ''
    do i = 1, n
      rows = rows // build_branch_row(branches(i), config)
    end do

    if (n == 0) then
      rows = '<tr><td colspan="5">No branch dashboards published yet.</td></tr>'
    end if

    body = '<h1>Test Dashboards</h1>' // &
           '<p class="subtitle">Automated test artifacts for all branches and pull requests</p>' // &
           '<p>Generated: ' // html_escape(timestamp) // '</p>' // &
           '<table><thead><tr>' // &
           '<th>Branch</th><th>Updated (UTC)</th><th>Commit</th><th>Workflow</th><th>Status</th>' // &
           '</tr></thead><tbody>' // rows // '</tbody></table>'

    extra_style = &
      'h1 { margin-bottom: 0.5rem; }' // new_line('a') // &
      '.subtitle { color: #586069; margin-bottom: 1rem; }' // new_line('a') // &
      'table { border-collapse: collapse; width: 100%; margin-top: 1rem; }' // new_line('a') // &
      'th, td { border: 1px solid #ccc; padding: 0.5rem; text-align: left; }' // new_line('a') // &
      'th { background-color: #f5f5f5; font-weight: 600; }'

    html = build_html_page(title, body, extra_style)
  end function build_overview_page

  function build_branch_row(branch, config) result(row)
    !! Build table row for branch overview
    type(branch_metadata), intent(in) :: branch
    type(dashboard_config), intent(in) :: config
    character(len=:), allocatable :: row
    character(len=2048) :: buffer
    character(len=512) :: branch_display, workflow_cell, status

    branch_display = html_escape(branch%branch_name)
    if (branch%pr_number > 0) then
      write(buffer, '(A)') trim(branch_display) // ' (<a href="' // trim(branch%pr_url) // &
        '">#' // trim(str(branch%pr_number)) // '</a>)'
      branch_display = trim(buffer)
    end if

    workflow_cell = '<a href="https://github.com/' // trim(config%repo) // &
                    '/actions/runs/' // trim(branch%run_id) // '">run ' // &
                    trim(branch%run_id) // '</a>'

    if (branch%has_pngs) then
      status = 'available'
    else
      status = 'no artifacts'
    end if

    row = '<tr>' // &
          '<td><a href="' // trim(branch%branch_name) // '/">' // trim(branch_display) // '</a></td>' // &
          '<td>' // html_escape(branch%timestamp) // '</td>' // &
          '<td><code>' // html_escape(branch%commit(1:min(12,len_trim(branch%commit)))) // '</code></td>' // &
          '<td>' // trim(workflow_cell) // '</td>' // &
          '<td>' // html_escape(status) // '</td>' // &
          '</tr>'
  end function build_branch_row

  subroutine generate_root_redirect(config, timestamp, success)
    !! Generate root index.html with redirect
    type(dashboard_config), intent(in) :: config
    character(len=*), intent(in) :: timestamp
    logical, intent(out) :: success
    character(len=:), allocatable :: html, body
    character(len=512) :: filepath

    body = '<h1>' // html_escape(config%project_name) // ' GitHub Pages</h1>' // &
           '<p>This site hosts automatically generated test dashboards.</p>' // &
           '<p>You will be redirected to <a href="./test/">/test/</a> momentarily.</p>' // &
           '<p>Generated: ' // html_escape(timestamp) // '</p>'

    html = '<!DOCTYPE html>' // new_line('a') // &
           '<html lang="en">' // new_line('a') // &
           '<head>' // new_line('a') // &
           '  <meta charset="utf-8">' // new_line('a') // &
           '  <title>' // html_escape(config%project_name) // '</title>' // new_line('a') // &
           '  <meta http-equiv="refresh" content="0; url=./test/" />' // new_line('a') // &
           '</head>' // new_line('a') // &
           '<body>' // body // '</body>' // new_line('a') // &
           '</html>'

    filepath = trim(config%output_dir) // '/index.html'
    call write_file(filepath, html, success)
  end subroutine generate_root_redirect

  subroutine update_metadata(filepath, current, all_branches, n_branches)
    !! Update or create metadata file
    character(len=*), intent(in) :: filepath
    type(branch_metadata), intent(in) :: current
    type(branch_metadata), intent(out) :: all_branches(:)
    integer, intent(out) :: n_branches
    integer :: i
    logical :: found

    ! Read existing metadata
    call json_read_metadata(filepath, all_branches, n_branches)

    ! Update or append current branch
    found = .false.
    do i = 1, n_branches
      if (trim(all_branches(i)%branch_name) == trim(current%branch_name)) then
        all_branches(i) = current
        found = .true.
        exit
      end if
    end do

    if (.not. found) then
      n_branches = n_branches + 1
      all_branches(n_branches) = current
    end if

    ! Write updated metadata
    call json_write_metadata(filepath, all_branches, n_branches)
  end subroutine update_metadata

  subroutine write_file(filepath, content, success)
    !! Write string content to file
    character(len=*), intent(in) :: filepath, content
    logical, intent(out) :: success
    integer :: unit, ios

    open(newunit=unit, file=trim(filepath), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      success = .false.
      return
    end if

    write(unit, '(A)') trim(content)
    close(unit)
    success = .true.
  end subroutine write_file

end module dashboard
