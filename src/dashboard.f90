module dashboard
  !! Main dashboard generation logic using template files
    use html_utils, only: html_escape
    use string_utils, only: string_array, append_string, join_path, str, starts_with, &
         contains_string, clear_string_array, replace_all
    use file_utils, only: create_directory, copy_file, find_image_files, &
         read_text_file, compute_file_crc32, directory_exists, file_exists
    use json_utils
    use gh_api
    use datetime_utils
    use template_engine
    use embedded_templates, only: get_embedded_template
    implicit none
    private

    public :: generate_dashboard, dashboard_config

    type :: dashboard_config
        character(len=512) :: image_root = 'image-artifacts'
        character(len=512) :: output_dir = 'dashboard'
        character(len=256) :: branch_name = ''
        character(len=256) :: base_branch = 'main'
        character(len=64) :: commit_sha = ''
        character(len=64) :: run_id = ''
        character(len=256) :: repo = ''
        character(len=256) :: project_name = 'testboard'
        character(len=512) :: github_pages_url = ''
        character(len=512) :: template_root = 'templates'
    end type dashboard_config

    type :: dashboard_templates
        character(len=:), allocatable :: branch
        character(len=:), allocatable :: diff
        character(len=:), allocatable :: overview
        character(len=:), allocatable :: overview_row
        character(len=:), allocatable :: gallery_item
        character(len=:), allocatable :: root
    end type dashboard_templates

contains

    subroutine generate_dashboard(config, success)
    !! Generate complete dashboard from configuration
        type(dashboard_config), intent(in) :: config
        logical, intent(out) :: success
        type(string_array) :: image_files, gallery_files, diff_files
        type(branch_metadata) :: current_branch, all_branches(100)
        type(dashboard_templates) :: templates
        type(template_context) :: ctx
        integer :: n_branches
        character(len=512) :: test_root, branch_path, images_path, base_images_path
       character(len=512) :: metadata_file, branch_html_file, diff_html_file, index_file
        character(len=25) :: timestamp
        character(len=:), allocatable :: gallery_html, branch_html, diff_html
        character(len=:), allocatable :: overview_html, root_html
        logical :: stat, ok, same_branch

        success = .false.

        if (.not. load_dashboard_templates(config, templates)) then
  print *, 'Error: failed to load dashboard templates from ', trim(config%template_root)
            return
        end if

        ! Create output directories
        test_root = trim(config%output_dir)//'/test'
        branch_path = trim(test_root)//'/'//trim(config%branch_name)
        images_path = trim(branch_path)//'/images'
        base_images_path = trim(test_root)//'/'//trim(config%base_branch)//'/images'

        call create_directory(images_path, stat)
        if (.not. stat) then
            print *, 'Error: Failed to create directory: ', trim(images_path)
            return
        end if

        ! Copy image files (PNG, JPG, JPEG)
        call find_image_files(config%image_root, image_files)
       call copy_image_files(config%image_root, images_path, image_files, gallery_files)

        call clear_string_array(diff_files)
        same_branch = (trim(config%branch_name) == trim(config%base_branch))
        if (.not. same_branch) then
       call compute_diff_files(images_path, base_images_path, gallery_files, diff_files)
        end if

        ! Get current timestamp and PR info
        timestamp = get_iso8601_timestamp()
        current_branch%branch_name = config%branch_name
        current_branch%commit = config%commit_sha
        current_branch%timestamp = timestamp
        current_branch%run_id = config%run_id
        current_branch%repo = config%repo
        current_branch%has_pngs = (gallery_files%count > 0)
        current_branch%diff_count = diff_files%count

        call get_pr_info(config%branch_name, config%repo, current_branch, ok)

        ! Render branch gallery and pages
        gallery_html = render_gallery(templates%gallery_item, &
                                      gallery_files, diff_files, .false.)
        branch_html = render_branch_page(config, current_branch, gallery_html, &
                                         diff_files%count, templates%branch)
        diff_html = render_diff_page(config, current_branch, templates%gallery_item, &
                                     diff_files, templates%diff)

        branch_html_file = trim(branch_path)//'/index.html'
        call write_file(branch_html_file, branch_html, stat)
        if (.not. stat) return

        diff_html_file = trim(branch_path)//'/diff.html'
        call write_file(diff_html_file, diff_html, stat)
        if (.not. stat) return

        ! Update metadata
        metadata_file = trim(test_root)//'/branches.json'
        call update_metadata(metadata_file, current_branch, all_branches, n_branches)

        ! Render overview and root pages
        overview_html = render_overview_page(config, all_branches, n_branches, &
                                             timestamp, templates)
        index_file = trim(test_root)//'/index.html'
        call write_file(index_file, overview_html, stat)
        if (.not. stat) return

        root_html = render_root_redirect(config, timestamp, templates%root)
        call write_file(trim(config%output_dir)//'/index.html', root_html, stat)
        if (.not. stat) return

        success = .true.
    end subroutine generate_dashboard

    logical function load_dashboard_templates(config, templates) result(success)
        type(dashboard_config), intent(in) :: config
        type(dashboard_templates), intent(out) :: templates
        logical :: ok

        templates%branch = fetch_template(config, 'branch.html', ok)
        if (.not. ok) then
            success = .false.
            return
        end if

        templates%diff = fetch_template(config, 'diff.html', ok)
        if (.not. ok) then
            success = .false.
            return
        end if

        templates%overview = fetch_template(config, 'overview.html', ok)
        if (.not. ok) then
            success = .false.
            return
        end if

        templates%overview_row = fetch_template(config, 'overview_row.html', ok)
        if (.not. ok) then
            success = .false.
            return
        end if

        templates%gallery_item = fetch_template(config, 'gallery_item.html', ok)
        if (.not. ok) then
            success = .false.
            return
        end if

        templates%root = fetch_template(config, 'root.html', ok)
        success = ok
    end function load_dashboard_templates

    function fetch_template(config, filename, ok) result(content)
        type(dashboard_config), intent(in) :: config
        character(len=*), intent(in) :: filename
        logical, intent(out) :: ok
        character(len=:), allocatable :: content
        character(len=:), allocatable :: path
        logical :: disk_ok
        character(len=:), allocatable :: embedded

        path = template_path(config, filename)
        content = load_template(path, disk_ok)
        if (disk_ok) then
            ok = .true.
            return
        end if

        embedded = get_embedded_template(filename)
        if (len(embedded) > 0) then
            content = embedded
            ok = .true.
        else
            content = ''
            ok = .false.
        end if
    end function fetch_template

    function template_path(config, filename) result(path)
        type(dashboard_config), intent(in) :: config
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: path
        character(len=512) :: parts(2)

        parts(1) = trim(config%template_root)
        parts(2) = trim(filename)
        path = join_path(parts)
    end function template_path

    function render_gallery(item_template, files, diff_files, diff_only) result(html)
        character(len=*), intent(in) :: item_template
        type(string_array), intent(in) :: files
        type(string_array), intent(in) :: diff_files
        logical, intent(in) :: diff_only
        character(len=:), allocatable :: html
        integer :: i
        character(len=:), allocatable :: rel_path
        logical :: is_diff_item

        if (diff_only) then
            if (diff_files%count == 0) then
             html = '<p class="diff-empty">No differing artifacts compared to base.</p>'
                return
            end if
        else
            if (files%count == 0) then
                html = '<p>No image outputs were produced for this run.</p>'
                return
            end if
        end if

        html = '<div class="gallery">'//new_line('a')
        if (diff_only) then
            do i = 1, diff_files%count
                rel_path = trim(diff_files%items(i))
        html = html//render_gallery_item(item_template, rel_path, .true.)//new_line('a')
            end do
        else
            do i = 1, files%count
                rel_path = trim(files%items(i))
                is_diff_item = contains_string(diff_files, rel_path)
  html = html//render_gallery_item(item_template, rel_path, is_diff_item)//new_line('a')
            end do
        end if
        html = html//'</div>'
    end function render_gallery

    function render_gallery_item(template, rel_path, is_diff) result(html)
        character(len=*), intent(in) :: template
        character(len=*), intent(in) :: rel_path
        logical, intent(in) :: is_diff
        character(len=:), allocatable :: html
        type(template_context) :: ctx
        character(len=:), allocatable :: figure_class

        call init_template_context(ctx)
        call add_template_value(ctx, 'image_href', 'images/'//trim(rel_path))
        call add_template_value(ctx, 'image_src', 'images/'//trim(rel_path))
        call add_template_value(ctx, 'alt_text', html_escape(rel_path))
        call add_template_value(ctx, 'caption', html_escape(rel_path))
        if (is_diff) then
            figure_class = 'diff'
        else
            figure_class = ''
        end if
        call add_template_value(ctx, 'figure_class', figure_class)

        html = render_template(template, ctx)
    end function render_gallery_item

    function render_branch_page(config, branch, gallery_html, diff_count, template) &
        result(html)
        type(dashboard_config), intent(in) :: config
        type(branch_metadata), intent(in) :: branch
        character(len=*), intent(in) :: gallery_html
        integer, intent(in) :: diff_count
        character(len=*), intent(in) :: template
        character(len=:), allocatable :: html, pr_section, diff_summary, diff_nav_link
        type(template_context) :: ctx
        character(len=:), allocatable :: back_link, workflow_label
        character(len=:), allocatable :: page_title

        back_link = compute_back_link(branch%branch_name)
        pr_section = render_pr_section(branch)
     diff_summary = render_diff_summary(config%base_branch, branch%has_pngs, diff_count)
        if (branch%has_pngs) then
            diff_nav_link = '<a href="diff.html" class="diff-link">diff view</a>'
        else
            diff_nav_link = ''
        end if

        workflow_label = 'run '//trim(branch%run_id)

        page_title = trim(config%project_name)//' – '//trim(branch%branch_name)

        call init_template_context(ctx)
        call add_template_value(ctx, 'page_title', html_escape(page_title))
        call add_template_value(ctx, 'project_name', html_escape(config%project_name))
        call add_template_value(ctx, 'branch_name', html_escape(branch%branch_name))
        call add_template_value(ctx, 'back_link', back_link)
        call add_template_value(ctx, 'pr_section', pr_section)
        call add_template_value(ctx, 'commit_sha', html_escape(branch%commit))
        call add_template_value(ctx, 'commit_url', 'https://github.com/'// &
                               trim(config%repo)//'/commit/'//trim(branch%commit))
        call add_template_value(ctx, 'workflow_url', 'https://github.com/'// &
                               trim(config%repo)//'/actions/runs/'//trim(branch%run_id))
        call add_template_value(ctx, 'workflow_label', workflow_label)
        call add_template_value(ctx, 'generated_at', html_escape(branch%timestamp))
        call add_template_value(ctx, 'diff_summary', diff_summary)
        call add_template_value(ctx, 'diff_nav_link', diff_nav_link)
        call add_template_value(ctx, 'gallery', gallery_html)

        html = render_template(template, ctx)
    end function render_branch_page

    function render_pr_section(branch) result(section)
        type(branch_metadata), intent(in) :: branch
        character(len=:), allocatable :: section
        character(len=2048) :: buffer

        if (branch%pr_number <= 0) then
            section = ''
            return
        end if

        write (buffer, '(A)') '<p><strong>Pull Request:</strong> <a href="'// &
            trim(branch%pr_url)//'">#'//trim(str(branch%pr_number))//' '// &
            trim(html_escape(branch%pr_title))//'</a>'
        if (branch%pr_draft) then
            buffer = trim(buffer)//' <span style="'// &
                     'background:#6a737d;color:white;padding:2px 6px;"'// &
                     'border-radius:3px;font-size:0.85em">DRAFT</span>'
        end if
        section = trim(buffer)//'</p>'
    end function render_pr_section

    function render_diff_summary(base_branch, has_pngs, diff_count) result(summary)
        character(len=*), intent(in) :: base_branch
        logical, intent(in) :: has_pngs
        integer, intent(in) :: diff_count
        character(len=:), allocatable :: summary

        if (.not. has_pngs) then
            summary = ''
            return
        end if

        if (diff_count > 0) then
            summary = '<p class="diff-summary"><strong>'//trim(str(diff_count))// &
               ' differing artifacts</strong> vs '//html_escape(base_branch)//'</p>'// &
new_line('a')//'<p class="diff-summary-link"><a href="diff.html">Open diff view</a></p>'
        else
            summary = '<p class="diff-summary no-diff">No differences vs '// &
                      html_escape(base_branch)//'</p>'//new_line('a')// &
               '<p class="diff-summary-link"><a href="diff.html">Open diff view</a></p>'
        end if
    end function render_diff_summary

    function render_diff_page(config, branch, item_template, diff_files, template) &
        result(html)
        type(dashboard_config), intent(in) :: config
        type(branch_metadata), intent(in) :: branch
        character(len=*), intent(in) :: item_template
        type(string_array), intent(in) :: diff_files
        character(len=*), intent(in) :: template
        character(len=:), allocatable :: html, diff_content
        character(len=:), allocatable :: page_title
        type(template_context) :: ctx

        diff_content = render_gallery(item_template, diff_files, diff_files, .true.)

        page_title = trim(config%project_name)//' – '//trim(branch%branch_name)//' diffs'

        call init_template_context(ctx)
        call add_template_value(ctx, 'page_title', html_escape(page_title))
        call add_template_value(ctx, 'project_name', html_escape(config%project_name))
        call add_template_value(ctx, 'branch_name', html_escape(branch%branch_name))
        call add_template_value(ctx, 'base_branch', html_escape(config%base_branch))
        call add_template_value(ctx, 'back_link', compute_back_link(branch%branch_name))
        call add_template_value(ctx, 'diff_content', diff_content)

        html = render_template(template, ctx)
    end function render_diff_page

    function render_overview_page(config, branches, n_branches, timestamp, templates) &
        result(html)
        type(dashboard_config), intent(in) :: config
        type(branch_metadata), intent(in) :: branches(:)
        integer, intent(in) :: n_branches
        character(len=*), intent(in) :: timestamp
        type(dashboard_templates), intent(in) :: templates
        character(len=:), allocatable :: html, rows
        integer :: i

        rows = ''
        if (n_branches == 0) then
            rows = '<tr><td colspan="6">No branch dashboards published yet.</td></tr>'
        else
            do i = 1, n_branches
                rows = rows//render_overview_row(config, branches(i), &
                                                 templates%overview_row)//new_line('a')
            end do
        end if

        html = templates%overview
        html = replace_all(html, '{{page_title}}', html_escape(trim(config%project_name)// &
            ' Dashboards'))
        html = replace_all(html, '{{project_name}}', html_escape(config%project_name))
        html = replace_all(html, '{{generated_at}}', html_escape(timestamp))
        html = replace_all(html, '{{rows}}', trim(rows))
    end function render_overview_page

    function render_overview_row(config, branch, template) result(row)
        type(dashboard_config), intent(in) :: config
        type(branch_metadata), intent(in) :: branch
        character(len=*), intent(in) :: template
        character(len=:), allocatable :: row, pr_suffix, diff_status
        type(template_context) :: ctx
        character(len=:), allocatable :: workflow_label
        character(len=:), allocatable :: artifact_status

        if (branch%pr_number > 0) then
            pr_suffix = ' (<a href="'//trim(branch%pr_url)//'">#'// &
                        trim(str(branch%pr_number))//'</a>)'
        else
            pr_suffix = ''
        end if

        if (branch%has_pngs) then
            workflow_label = 'run '//trim(branch%run_id)
            if (branch%diff_count >= 0) then
         diff_status = '<a href="'//trim(branch%branch_name)//'/diff.html">diff</a>'// &
                              ' ('//trim(str(branch%diff_count))//')'
            else
             diff_status = '<a href="'//trim(branch%branch_name)//'/diff.html">diff</a>'
            end if
            artifact_status = 'available'
        else
            workflow_label = 'run '//trim(branch%run_id)
            diff_status = '&mdash;'
            artifact_status = 'no artifacts'
        end if

        call init_template_context(ctx)
        call add_template_value(ctx, 'branch_href', trim(branch%branch_name)//'/')
        call add_template_value(ctx, 'branch_name', html_escape(branch%branch_name))
        call add_template_value(ctx, 'pr_suffix', pr_suffix)
        call add_template_value(ctx, 'updated', html_escape(branch%timestamp))
        call add_template_value(ctx, 'commit', html_escape(branch%commit(1:min(12, &
                                                             len_trim(branch%commit)))))
        call add_template_value(ctx, 'commit_url', 'https://github.com/'// &
                               trim(config%repo)//'/commit/'//trim(branch%commit))
        call add_template_value(ctx, 'workflow_url', 'https://github.com/'// &
                               trim(config%repo)//'/actions/runs/'//trim(branch%run_id))
        call add_template_value(ctx, 'workflow_label', workflow_label)
        call add_template_value(ctx, 'diff_status', diff_status)
        call add_template_value(ctx, 'artifact_status', html_escape(artifact_status))

        row = render_template(template, ctx)
    end function render_overview_row

    function render_root_redirect(config, timestamp, template) result(html)
        type(dashboard_config), intent(in) :: config
        character(len=*), intent(in) :: timestamp
        character(len=*), intent(in) :: template
        character(len=:), allocatable :: html
        type(template_context) :: ctx

        call init_template_context(ctx)
        call add_template_value(ctx, 'project_name', html_escape(config%project_name))
        call add_template_value(ctx, 'generated_at', html_escape(timestamp))
        call add_template_value(ctx, 'test_index', './test/')

        html = render_template(template, ctx)
    end function render_root_redirect

    function compute_back_link(branch_name) result(back)
        character(len=*), intent(in) :: branch_name
        character(len=:), allocatable :: back
        integer :: depth, idx, name_len

        depth = 1
        name_len = len_trim(branch_name)
        do idx = 1, name_len
            if (branch_name(idx:idx) == '/') depth = depth + 1
        end do
        back = repeat('../', depth)//'index.html'
    end function compute_back_link

    subroutine compute_diff_files(current_root, base_root, files, diffs)
        character(len=*), intent(in) :: current_root, base_root
        type(string_array), intent(in) :: files
        type(string_array), intent(inout) :: diffs
        integer :: i
        character(len=:), allocatable :: rel_path, current_path, base_path
        character(len=8) :: current_hash, base_hash
        logical :: ok_current, ok_base

        call clear_string_array(diffs)

        do i = 1, files%count
            rel_path = trim(files%items(i))
            current_path = trim(current_root)//'/'//rel_path
            current_hash = compute_file_crc32(current_path, ok_current)
            if (.not. ok_current) then
                call append_string(diffs, rel_path)
                cycle
            end if

            base_path = trim(base_root)//'/'//rel_path
            if (.not. file_exists(base_path)) then
                call append_string(diffs, rel_path)
                cycle
            end if

            base_hash = compute_file_crc32(base_path, ok_base)
            if (.not. ok_base) then
                call append_string(diffs, rel_path)
                cycle
            end if

            if (trim(current_hash) /= trim(base_hash)) then
                call append_string(diffs, rel_path)
            end if
        end do
    end subroutine compute_diff_files

    subroutine copy_image_files(src_root, dest_root, files, copied)
    !! Copy image files while preserving structure and filtering unwanted assets
        character(len=*), intent(in) :: src_root, dest_root
        type(string_array), intent(in) :: files
        type(string_array), intent(out) :: copied
        integer :: i
        logical :: stat
        character(len=:), allocatable :: rel_path, dest_path, dest_dir

        call clear_string_array(copied)

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

    subroutine update_metadata(filepath, current, all_branches, n_branches)
    !! Update or create metadata file
        character(len=*), intent(in) :: filepath
        type(branch_metadata), intent(in) :: current
        type(branch_metadata), intent(out) :: all_branches(:)
        integer, intent(out) :: n_branches
        integer :: i
        logical :: found

        call json_read_metadata(filepath, all_branches, n_branches)

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

        call json_write_metadata(filepath, all_branches, n_branches)
    end subroutine update_metadata

    subroutine write_file(filepath, content, success)
    !! Write string content to file
        character(len=*), intent(in) :: filepath, content
        logical, intent(out) :: success
        integer :: unit, ios

  open (newunit=unit, file=trim(filepath), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            success = .false.
            return
        end if

        write (unit, '(A)') trim(content)
        close (unit)
        success = .true.
    end subroutine write_file

end module dashboard
