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
        character(len=512) :: image_root = 'image-artifacts'
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
        type(string_array) :: image_files, gallery_files
        type(branch_metadata) :: current_branch, all_branches(100)
        integer :: n_branches
        character(len=512) :: test_root, branch_path, images_path
        character(len=512) :: metadata_file, branch_html_file, index_file
        character(len=:), allocatable :: gallery_html, branch_html, overview_html
        character(len=25) :: timestamp
        logical :: stat

        success = .false.

        ! Create output directories
        test_root = trim(config%output_dir)//'/test'
        branch_path = trim(test_root)//'/'//trim(config%branch_name)
        images_path = trim(branch_path)//'/images'

        call create_directory(images_path, stat)
        if (.not. stat) then
            print *, 'Error: Failed to create directory: ', trim(images_path)
            return
        end if

        ! Copy image files (PNG, JPG, JPEG)
        call find_image_files(config%image_root, image_files)
        call copy_image_files(config%image_root, images_path, image_files, &
                              gallery_files)

        ! Get current timestamp and PR info
        timestamp = get_iso8601_timestamp()
        current_branch%branch_name = config%branch_name
        current_branch%commit = config%commit_sha
        current_branch%timestamp = timestamp
        current_branch%run_id = config%run_id
        current_branch%repo = config%repo
        current_branch%has_pngs = (gallery_files%count > 0)

        call get_pr_info(config%branch_name, config%repo, current_branch, stat)

        ! Generate branch page
        gallery_html = build_gallery(gallery_files)
        branch_html = build_branch_page(config, current_branch, gallery_html)

        branch_html_file = trim(branch_path)//'/index.html'
        call write_file(branch_html_file, branch_html, stat)
        if (.not. stat) return

        ! Update metadata
        metadata_file = trim(test_root)//'/branches.json'
        call update_metadata(metadata_file, current_branch, all_branches, n_branches)

        ! Generate overview page
        overview_html = build_overview_page(config, all_branches, n_branches, timestamp)
        index_file = trim(test_root)//'/index.html'
        call write_file(index_file, overview_html, stat)
        if (.not. stat) return

        ! Generate root redirect
        call generate_root_redirect(config, timestamp, stat)
        if (.not. stat) return

        success = .true.
    end subroutine generate_dashboard

    subroutine copy_image_files(src_root, dest_root, files, copied)
    !! Copy image files while preserving structure and filtering unwanted assets
        character(len=*), intent(in) :: src_root, dest_root
        type(string_array), intent(in) :: files
        type(string_array), intent(out) :: copied
        integer :: i
        logical :: stat
        character(len=:), allocatable :: rel_path, dest_path, dest_dir

        if (allocated(copied%items)) then
            deallocate (copied%items)
        end if
        copied%count = 0

        do i = 1, files%count
            rel_path = trim(get_relative_path(files%items(i), src_root))

            if (len_trim(rel_path) == 0) cycle
            if (skip_basic_image(rel_path)) cycle

            dest_path = trim(dest_root)//'/'//trim(rel_path)
            dest_dir = get_parent_directory(dest_path)

            if (len_trim(dest_dir) > 0) then
                call create_directory(dest_dir, stat)
                if (.not. stat) then
                    print *, 'Error: Failed to create directory: ', trim(dest_dir)
                    cycle
                end if
            end if

            call copy_file(trim(files%items(i)), dest_path, stat)
            if (.not. stat) then
                print *, 'Error: Failed to copy image: ', trim(files%items(i))
                cycle
            end if

            call append_string(copied, trim(rel_path))
        end do
    end subroutine copy_image_files

    function basename(path) result(name)
    !! Extract filename from path
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: name
        integer :: pos

        pos = index(path, '/', back=.true.)
        if (pos > 0) then
            name = trim(path(pos + 1:))
        else
            name = trim(path)
        end if
    end function basename

    function build_gallery(files) result(html)
        !! Build HTML gallery from file list
        type(string_array), intent(in) :: files
        character(len=:), allocatable :: html
        integer :: i
        character(len=:), allocatable :: rel_path, img_src

        if (files%count == 0) then
            html = '<p>No image outputs were produced for this run.</p>'
            return
        end if

        html = '<div class="gallery">'
        do i = 1, files%count
            rel_path = trim(files%items(i))
            img_src = 'images/'//rel_path
            html = html//build_gallery_item(rel_path, img_src)
        end do
        html = html//'</div>'
    end function build_gallery

    function build_branch_page(config, branch, gallery) result(html)
        !! Build HTML page for a specific branch
        type(dashboard_config), intent(in) :: config
        type(branch_metadata), intent(in) :: branch
        character(len=*), intent(in) :: gallery
        character(len=:), allocatable :: html, body, title, pr_section, extra_style
        character(len=:), allocatable :: back_href
        character(len=2048) :: buffer
        integer :: depth, idx, name_len

        title = trim(config%project_name)//' – '//trim(branch%branch_name)

        ! Build PR section if available
        pr_section = ''
        if (branch%pr_number > 0) then
            write (buffer, '(A)') '<p><strong>Pull Request:</strong> <a href="'// &
                trim(branch%pr_url)//'">#'//trim(str(branch%pr_number))//' '// &
                trim(html_escape(branch%pr_title))//'</a>'
            if (branch%pr_draft) then
                buffer = trim(buffer)//' <span style="background:#6a737d;color:white;'
                buffer = buffer//'padding:2px 6px;border-radius:3px;'
                buffer = buffer//'font-size:0.85em">DRAFT</span>'
            end if
            pr_section = trim(buffer)//'</p>'
        end if

        depth = 1
        name_len = len_trim(branch%branch_name)
        do idx = 1, name_len
            if (branch%branch_name(idx:idx) == '/') depth = depth + 1
        end do
        back_href = repeat('../', depth)//'index.html'

        ! Build body content
        body = '<a href="'//trim(back_href)//'" class="back-link">'// &
               '← Back to all branches</a>'
        body = body//'<h1>Test Dashboard – '// &
               html_escape(branch%branch_name)//'</h1>'
        body = body//'<div class="meta">'
        body = body//'<p><strong>Branch:</strong> '// &
               html_escape(branch%branch_name)//'</p>'
        body = body//trim(pr_section)
        body = body//'<p><strong>Commit:</strong> <code>'// &
               html_escape(branch%commit)//'</code></p>'
        body = body//'<p><strong>Workflow run:</strong> <a href="'
        body = body//'https://github.com/'
        body = body//trim(config%repo)//'/actions/runs/'
        body = body//trim(branch%run_id)//'">'
        body = body//trim(branch%run_id)//'</a></p>'
        body = body//'<p><strong>Generated:</strong> '// &
               html_escape(branch%timestamp)//'</p>'
        body = body//'</div>'//gallery

        call append_line(extra_style, '.meta { margin-bottom: 1.5rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, '.gallery { display: grid; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'grid-template-columns: repeat(auto-fill, ')
        call append_line(extra_style, 'minmax(320px, 1fr));')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'gap: 1.5rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'figure { margin: 0; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'figure a { display: block; cursor: pointer; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'figcaption { margin-top: 0.5rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'figcaption { font-size: 0.9rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'figcaption { word-break: break-word; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'img { width: 100%; height: auto; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'img { border: 1px solid #ccd; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'img { box-shadow: 0 2px 4px rgba(0,0,0,0.1);')
        call append_line(extra_style, ' }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'img { transition: box-shadow 0.2s,')
        call append_line(extra_style, ' transform 0.2s; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'img:hover { box-shadow: 0 4px 8px ')
        call append_line(extra_style, 'rgba(0,0,0,0.2);')
        call append_line(extra_style, ' }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'img:hover { transform: scale(1.02); }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, '.back-link { display: inline-block; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, '.back-link { margin-bottom: 1rem; }')

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

        title = trim(config%project_name)//' Dashboards'

        rows = ''
        do i = 1, n
            rows = rows//build_branch_row(branches(i), config)
        end do

        if (n == 0) then
            rows = '<tr><td colspan="5">No branch dashboards published yet.</td></tr>'
        end if

        body = '<h1>Test Dashboards</h1>'
        body = body//'<p class="subtitle">Automated test artifacts for all branches '
        body = body//'and pull requests</p>'
        body = body//'<p>Generated: '//html_escape(timestamp)//'</p>'
        body = body//'<table><thead><tr>'
        body = body//'<th>Branch</th><th>Updated (UTC)</th><th>Commit</th>'// &
               '<th>Workflow</th><th>Status</th>'
        body = body//'</tr></thead><tbody>'//rows//'</tbody></table>'

        extra_style = ''
        call append_line(extra_style, 'h1 { margin-bottom: 0.5rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, '.subtitle { color: #586069; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, '.subtitle { margin-bottom: 1rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'table { border-collapse: collapse; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'table { width: 100%; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'table { margin-top: 1rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'th, td { border: 1px solid #ccc; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'th, td { padding: 0.5rem; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'th, td { text-align: left; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'th { background-color: #f5f5f5; }')
        call append_line(extra_style, new_line('a'))
        call append_line(extra_style, 'th { font-weight: 600; }')

        html = build_html_page(title, body, extra_style)
    end function build_overview_page

    subroutine append_line(target, text)
    !! Append text to an allocatable deferred-length string
        character(len=:), allocatable, intent(inout) :: target
        character(len=*), intent(in) :: text

        if (.not. allocated(target)) then
            target = text
        else
            target = target//text
        end if
    end subroutine append_line

    function get_relative_path(path, root) result(relative)
    !! Compute relative path of file with respect to root directory
        character(len=*), intent(in) :: path, root
        character(len=:), allocatable :: relative
        character(len=:), allocatable :: norm_path, norm_root
        integer :: root_len

        norm_path = trim(path)
        norm_root = trim(root)

        if (len_trim(norm_root) == 0) then
            relative = norm_path
            return
        end if

        if (norm_root(len_trim(norm_root):len_trim(norm_root)) == '/') then
            norm_root = norm_root(1:len_trim(norm_root) - 1)
        end if

        root_len = len_trim(norm_root)

        if (root_len > 0 .and. starts_with(norm_path, norm_root//'/')) then
            relative = norm_path(root_len + 2:)
        else if (norm_path == norm_root) then
            relative = ''
        else
            relative = norm_path
        end if

        relative = trim(relative)
    end function get_relative_path

    function get_parent_directory(path) result(parent)
    !! Return parent directory for a provided path
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: parent
        character(len=:), allocatable :: trimmed_path
        integer :: pos

        trimmed_path = trim(path)
        pos = index(trimmed_path, '/', back=.true.)

        if (pos > 0) then
            parent = trimmed_path(1:pos - 1)
        else
            parent = ''
        end if
    end function get_parent_directory

    logical function skip_basic_image(path) result(skip)
    !! Determine whether image should be excluded from gallery
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: lowered

        lowered = trim(to_lower_ascii(path))

        if (len_trim(lowered) == 0) then
            skip = .true.
            return
        end if

        if (starts_with(lowered, 'basic/')) then
            skip = .true.
            return
        end if

        if (index(lowered, '/basic/') > 0) then
            skip = .true.
            return
        end if

        if (starts_with(lowered, 'basic-')) then
            skip = .true.
            return
        end if

        if (index(lowered, '/basic-') > 0) then
            skip = .true.
            return
        end if

        if (lowered == 'basic.png') then
            skip = .true.
            return
        end if

        skip = .false.
    end function skip_basic_image

    pure function to_lower_ascii(text) result(lowered)
    !! Lowercase ASCII characters without touching other bytes
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lowered
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lowered(i:i) = achar(code + 32)
            else
                lowered(i:i) = text(i:i)
            end if
        end do
    end function to_lower_ascii

    function build_branch_row(branch, config) result(row)
    !! Build table row for branch overview
        type(branch_metadata), intent(in) :: branch
        type(dashboard_config), intent(in) :: config
        character(len=:), allocatable :: row
        character(len=2048) :: buffer
        character(len=512) :: branch_display, workflow_cell, status

        branch_display = html_escape(branch%branch_name)
        if (branch%pr_number > 0) then
            buffer = trim(branch_display)//' (<a href="'
            buffer = buffer//trim(branch%pr_url)//'">#'
            buffer = buffer//trim(str(branch%pr_number))//'</a>)'
            branch_display = trim(buffer)
        end if

        workflow_cell = '<a href="https://github.com/'//trim(config%repo)// &
                        '/actions/runs/'//trim(branch%run_id)//'">run '// &
                        trim(branch%run_id)//'</a>'

        if (branch%has_pngs) then
            status = 'available'
        else
            status = 'no artifacts'
        end if

        row = '<tr>'
        row = row//'<td><a href="'//trim(branch%branch_name)//'/">'// &
              trim(branch_display)//'</a></td>'
        row = row//'<td>'//html_escape(branch%timestamp)//'</td>'
        row = row//'<td><code>'// &
              html_escape(branch%commit(1:min(12, len_trim(branch%commit))))// &
              '</code></td>'
        row = row//'<td>'//trim(workflow_cell)//'</td>'
        row = row//'<td>'//html_escape(status)//'</td>'
        row = row//'</tr>'
    end function build_branch_row

    subroutine generate_root_redirect(config, timestamp, success)
    !! Generate root index.html with redirect
        type(dashboard_config), intent(in) :: config
        character(len=*), intent(in) :: timestamp
        logical, intent(out) :: success
        character(len=:), allocatable :: html, body
        character(len=512) :: filepath

        body = '<h1>'//html_escape(config%project_name)//' GitHub Pages</h1>'
        body = body//'<p>This site hosts automatically generated test dashboards.</p>'
        body = body//'<p>You will be redirected to <a href="./test/">/test/</a> '// &
               'momentarily.</p>'
        body = body//'<p>Generated: '//html_escape(timestamp)//'</p>'

        html = '<!DOCTYPE html>'//new_line('a')// &
               '<html lang="en">'//new_line('a')// &
               '<head>'//new_line('a')// &
               '  <meta charset="utf-8">'//new_line('a')// &
               '  <title>'//html_escape(config%project_name)//'</title>'// &
               new_line('a')// &
               '  <meta http-equiv="refresh" content="0; url=./test/" />'// &
               new_line('a')// &
               '</head>'//new_line('a')// &
               '<body>'//body//'</body>'//new_line('a')// &
               '</html>'

        filepath = trim(config%output_dir)//'/index.html'
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

        open (newunit=unit, file=trim(filepath), status='replace', &
              action='write', iostat=ios)
        if (ios /= 0) then
            success = .false.
            return
        end if

        write (unit, '(A)') trim(content)
        close (unit)
        success = .true.
    end subroutine write_file

end module dashboard
