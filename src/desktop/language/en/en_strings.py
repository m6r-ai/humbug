"""English language strings for the UI."""

from desktop.language.language_strings import LanguageStrings


def get_english_strings() -> LanguageStrings:
    """
    Get the English language strings.

    Returns:
        LanguageStrings object with all English text
    """
    return LanguageStrings(
        # Window titles
        about_title="About Humbug",

        # Welcome widget
        welcome_message=(
            "Welcome to Humbug!  It looks like you don't have any AIs configured yet?\n\n"
            "To start using AI features, please open the application \"Settings\", scroll to\nthe "
            "\"AI Backend Configuration\" section and set up at least one AI backend."
        ),
        welcome_button="Open Settings",

        # Menu names
        humbug_menu="Humbug",
        file_menu="File",
        mindspace_menu="Mindspace",
        edit_menu="Edit",
        view_menu="View",

        # File menu items
        about_humbug="About Humbug",
        quit_humbug="Quit Humbug",
        new_mindspace="New Mindspace",
        new_conversation="New Conversation",
        new_file="New File",
        new_terminal="New Terminal",
        open_mindspace="Open Mindspace...",
        open_conversation="Open Conversation...",
        open_file="Open File...",
        open_preview="Open Preview...",
        open_diff="Open Diff...",
        save="Save",
        save_as="Save As...",
        close_mindspace="Close Mindspace",
        close_tab="Close Tab",
        settings="Settings",
        recent_mindspaces="Open Recent Mindspace",
        take_tour="Take a Tour",

        # Tab bar context menu items
        close_tabs_to_left="Close Tabs to the Left",
        close_tabs_to_right="Close Tabs to the Right",
        close_other_tabs="Close Other Tabs",
        close_column="Close Column",
        move_tab_to_left_column="Move Tab to Left Column",
        move_tab_to_right_column="Move Tab to Right Column",
        move_tab_to_new_column="Move Tab to New Column",
        move_tab_left="Move Tab Left",
        move_tab_right="Move Tab Right",
        move_tab_to_start="Move Tab to Start",
        move_tab_to_end="Move Tab to End",

        # Edit menu items
        submit_message="Submit Message",
        undo="Undo",
        redo="Redo",
        cut="Cut",
        copy="Copy",
        paste="Paste",
        select_all="Select All",
        find="Find",
        find_replace="Find and Replace",
        goto_line="Go to Line",
        mindspace_search="Mindspace Search",
        quick_switcher="Quick Switcher",
        quick_switcher_placeholder="Search files, conversations, and open tabs...",
        quick_switcher_prefix_hint="> commands   @ conversations   :line   file:line",
        quick_switcher_goto_line="Go to line {0}",
        quick_switcher_limited="Limited to the first {0} files",
        mindspace_settings="Mindspace Settings",
        conversation_settings="Conversation Settings",

        # View menu items
        display_theme="Display Theme",
        zoom_in="Zoom In",
        zoom_out="Zoom Out",
        reset_zoom="Reset Zoom",
        open_mindspace_log="Mindspace Log",
        open_humbug_shell="Humbug Shell",
        open_token_usage="Token Usage",
        show_tab_overview="Show Open Tabs",
        show_tab_carousel="Show Tab Carousel",
        show_all_columns="Show All Columns",
        split_column_left="Split Column Left",
        split_column_right="Split Column Right",
        merge_column_left="Merge Column Left",
        merge_column_right="Merge Column Right",
        swap_column_left="Swap Column Left",
        swap_column_right="Swap Column Right",
        next_message="Next Message",
        previous_message="Previous Message",
        next_hunk="Next Hunk",
        previous_hunk="Previous Hunk",
        prompt_markers="Prompt Markers",

        # Message roles
        role_you="You",
        role_you_queued="You (queued)",
        role_connected="{model} connected",
        role_assistant="{model} response",
        role_reasoning="{model} reasoning",
        role_system="Humbug",
        role_tool_call="Tool call",
        role_tool_result="Tool result",

        # Message labels
        highlighting="Highlighting: {syntax}",

        # Find widget
        find_placeholder="Find",
        find_no_matches="No matches",
        find_match_count="{current} of {total}",
        find_match_case="Match Case",
        find_use_whole_word="Match Whole Word",
        find_use_regexp="Use Regular Expression",
        find_invalid_regexp="Invalid Regexp",
        replace_placeholder="Replace",
        replace_button="Replace",
        replace_all_button="Replace All",
        replace_count="{count} replaced",

        # Go to line dialog
        goto_line_title="Go to Line",
        goto_line_label="Line number:",

        # Input widget
        processing_message="Chat with {model} (Esc to cancel, {key} to submit)",
        input_prompt="Chat with {model} ({key} to submit)",
        command_prompt="Command (Enter or {key} to submit)",
        editing_message="Editing message — submitting will remove this and all following messages ({key} to submit)",

        # AI thinking message
        ai_thinking="AI is thinking...",

        # Dialog and settings
        cancel="Cancel",
        ok="OK",
        apply="Apply",
        discard="Discard",
        yes="Yes",
        no="No",

        # User Settings dialog - AI backends section
        display_settings="Display Settings",
        ai_backend_config="AI Backend Configuration",
        enable_backend="Enable Backend",
        api_key="API Key",
        api_url="API URL (optional)",
        anthropic_backend="Anthropic",
        deepseek_backend="DeepSeek",
        google_backend="Google",
        zai_backend="Zai",
        mistral_backend="Mistral",
        openai_backend="OpenAI",
        ollama_backend="Ollama",
        ollama_cloud_backend="Ollama Cloud",
        xai_backend="xAI",
        vllm_backend="vLLM",

        # User Settings dialog - other settings
        select_language="Language",
        font_size="Font Size",
        theme_system="Automatic",
        font_ligatures="Enable font ligatures",
        theme_dark="Dark",
        theme_light="Light",
        theme_color_blind="Color Blind",
        theme_ocean_light="Ocean Light",
        theme_light_default="Default",
        theme_custom="Custom Theme",
        theme_custom_manually="Manual",
        customize_colors="Customize Colors…",
        file_sort_order="File Sort Order",
        sort_directories_first="Directories First",
        sort_alphabetical="Alphabetical",

        # User Settings dialog - external file access
        external_file_access="File Access",
        allow_external_file_access="Allow AI to read files outside mindspace",
        external_file_allowlist="Allowed paths (glob patterns, one per line):",
        external_file_denylist="Always denied paths (glob patterns, one per line):",

        # User Settings dialog - external HTTP access
        settings_network_access="Network Access",
        external_http_access="Network Access",
        allow_external_http_access="Allow AI to fetch allowlisted URLs without asking",
        external_http_allowlist="Auto-approved URLs (one per line, e.g. api.example.com or https://api.example.com:8443):",
        external_http_denylist="Always denied URLs (one per line, e.g. localhost or 10.0.0.0/8):",

        # Mindspace tree
        mindspace_label_none="No mindspace active",
        window_title="Humbug: {}",
        mindspace_name_tooltip="Click to switch mindspace",
        mindspace_files="Files",
        mindspace_vcs="Changed Files",
        mindspace_vcs_no_repo="No repository found",
        mindspace_vcs_root="Mindspace root",
        mindspace_conversations="Conversations",
        pinned_section="Pinned",
        mindspace_preview="Preview",
        mindspace_expand_sidebar="Expand sidebar",
        mindspace_collapse_sidebar="Collapse sidebar",
        mindspace_search_placeholder="Search",
        mindspace_search_options="Search options",
        mindspace_search_whole_word="Match whole word",
        mindspace_search_empty_state="Ready to search",
        mindspace_search_no_results="No results found",
        mindspace_search_path_match="Path match",
        mindspace_search_results="{0} matches in {1} files",
        mindspace_search_results_limited="Results limited to {0} matches",
        mindspace_search_include_hidden="Include hidden directories",

        # Mindspace folders dialog
        mindspace_folders_title="Configure Mindspace Folders",
        mindspace_path="Mindspace Path",
        humbug_folder='Create ".humbug" Folder',
        src_folder='Create "src" Folder',

        # Mindspace settings
        model_settings="Model Settings",
        editor_settings="Editor Settings",
        use_soft_tabs="Use Soft Tabs",
        tab_size="Tab Size",
        backup_settings="Backup Settings",
        auto_backup="Auto Backup",
        backup_interval="Backup Interval (seconds)",
        terminal_settings="Terminal Settings",
        terminal_settings_description=(
            "Configure terminal scrollback buffer behavior. The scrollback buffer stores terminal output "
            "history that you can scroll back through."
        ),
        terminal_fixed_width_enabled="Enable fixed width",
        terminal_fixed_width="Fixed width (columns)",
        terminal_scrollback_enabled="Limit Scrollback Buffer",
        terminal_scrollback_lines="Scrollback Lines",
        terminal_close_on_exit="Close terminal when shell exits",
        tool_settings="AI Tool Settings",
        tools_description="Enable or disable individual tools for this mindspace",

        # Diff view settings
        diff_settings="Diff Settings",
        diff_view_mode="Diff View",
        diff_view_inline="Inline",
        diff_view_side_by_side="Side by Side",
        diff_show_side_by_side="Show Side by Side",
        diff_show_inline="Show Inline",

        # Unified settings dialog
        settings_all_mindspaces="All Mindspaces",
        settings_this_mindspace="This Mindspace",
        settings_display="Display",
        settings_file_access="File Access",
        settings_ai_backends="AI Backends",
        settings_ai_model="AI Model",
        settings_ai_tools="AI Tools",
        settings_editor="Editor",
        settings_diff="Diff",
        settings_terminal="Terminal",
        settings_conversation="Conversation",
        settings_tabs="Tabs",
        settings_journal="Journal",

        journal_settings="Journal",
        journal_checkpoint_interval="Checkpoint Interval (seconds)",
        journal_watched_section="Watched Tree",
        journal_watched_section_description=(
            "Changes under an ignored entry are not recorded. Entries are glob patterns, "
            "matched as the File Access lists are: a name matches that whole name anywhere in "
            "the tree and never a longer one, so venv does not match venvity, and end an entry "
            "with /** to include everything below a path. .humbug and .git are always ignored."
        ),
        journal_pruned_paths="Ignored Files, Directories and Paths",

        # Conversation settings
        model_info="Model Information",
        settings_model_label="AI Model",
        settings_temp_label="Temperature",
        settings_context_label="Context Window",
        settings_max_output_label="Max Output Tokens",
        settings_tokens_label="tokens",
        settings_reasoning_label="Reasoning Capabilities",
        settings_no_reasoning="No Reasoning",
        settings_hidden_reasoning="Hidden Reasoning",
        settings_visible_reasoning="Visible Reasoning",
        settings_reasoning_effort_label="Reasoning Effort",
        settings_effort_none="None",
        settings_effort_minimal="Minimal",
        settings_effort_low="Low",
        settings_effort_medium="Medium",
        settings_effort_high="High",
        settings_effort_xhigh="Extra High",
        settings_effort_max="Maximum",
        settings_apply_to_all_conversations="Set as default for this and all open conversations",
        settings_apply_to_open_conversations="Apply to all open conversations",
        settings_applied_to_all_conversations="Applied",
        settings_apply_to_all_confirm_title="Apply to All Conversations?",
        settings_apply_to_all_confirm_message=(
            "This will change the model, provider, temperature and reasoning settings for every "
            "currently open conversation, and set them as the default for new conversations. "
            "This cannot be undone. Continue?"
        ),

        # File dialog titles
        file_dialog_open_conversation="Open Conversation",
        file_dialog_open_file="Open File",
        file_dialog_save_file="Save File",
        file_dialog_new_mindspace="Create New Mindspace",
        file_dialog_open_mindspace="Open Mindspace",
        file_dialog_attach_file="Attach File",

        # File dialog filters
        file_filter_all="All Files (*.*)",
        file_filter_conversation="Conversation Files (*.conv)",

        # Dialog titles
        mindspace_error_title="Mindspace Error",
        conversation_error_title="Conversation Error",
        settings_error_title="Settings Error",
        error_opening_file_title="Error Opening File",
        error_saving_file_title="Error Saving File",
        save_changes_title="Save Changes?",
        confirm_delete_title="Confirm Delete",
        file_error_title="File Error",
        preview_error_title="Preview Error",
        cancel_conversation_title="Cancel Conversation?",

        # Messages
        confirm_delete_message="Are you sure you want to delete {0}?",
        delete_warning_detail=(
            "Any open tab for it will be closed without saving. It moves to the trash, "
            "where it can be restored."
        ),
        delete_recorded_warning_detail=(
            "Any open tab for it will be closed without saving. Its content is recorded first, "
            "so the deletion can be reversed from the Changes panel."
        ),
        error_deleting_file="Could not delete file: {0}",
        error_title_rename="Rename Error",
        error_rename_exists="A conversation named '{0}' already exists.",
        error_rename_failed="Could not rename conversation: {0}",
        unsaved_changes="Do you want to save changes to {0}?",
        mindspace_exists_error="Mindspace already exists in selected directory.",
        mindspace_already_open_error="This mindspace is already open in another window.",
        close_button="Close",
        confirm_close="Confirm Close",
        could_not_open="Could not open {}: {}",
        could_not_save="Could not save {}: {}",
        error_creating_mindspace="Failed to create mindspace: {0}",
        error_opening_mindspace="Failed to open mindspace: {0}",
        error_restoring_mindspace="Failed to restore mindspace state: {0}",
        error_saving_mindspace="Failed to save mindspace state: {0}",
        error_creating_conversation="Failed to create conversation: {}",
        error_opening_conversation="Could not load {}: {}",
        error_forking_conversation="Could not fork conversation: {}",
        error_opening_preview="Could not open preview: {}",
        error_saving_mindspace_settings="Failed to save mindspace settings: {}",
        error_saving_user_settings="Failed to save user settings: {}",
        cancel_conversation=(
            "Are you sure you want to stop the current AI response? "
            "This will terminate the ongoing conversation and may result in incomplete responses."
        ),

        initialize_mindspace_title="Initialize Mindspace",
        initialize_mindspace_message=(
            "No mindspace was found at:\n\n{0}\n\n"
            "Do you want to initialize this folder as a mindspace?"
        ),

        open_in_new_window="Open Mindspace in New Window...",
        open_in_new_window_title="Open in New Window",
        open_in_new_window_already_open=(
            "This mindspace is already open in another Humbug window (process {}).\n\n"
            "Two windows cannot share one mindspace, so switch to the existing window instead."
        ),
        open_in_new_window_already_here="This mindspace is already open in this window.",
        open_in_new_window_failed="Could not start a new Humbug window:\n\n{}",
        open_in_new_window_choose_folder="Choose Folder...",
        mindspace_open_elsewhere_tooltip=(
            "{} is already open in another Humbug window, so it cannot be opened here"
        ),

        # Status bar
        editor_status="Line {line}, Column {column} | {encoding} | {line_ending} | {type}",
        conversation_status=(
            "Model: {model}  |  Provider: {provider}  |  {reasoning}  |  {temperature}  |  "
            "Input Tokens: {input_tokens} / {max_input_tokens}  |  "
            "Input Total: {total_input_tokens}  |  "
            "Output Tokens: {output_tokens} / {max_output_tokens}  |  "
            "Output Total: {total_output_tokens}"
        ),
        conversation_status_temperature="Temp: {temperature:.1f}",
        conversation_status_no_temperature="Temp: N/A",
        conversation_status_reasoning="Reasoning: {effort}",
        conversation_status_no_reasoning="Reasoning: N/A",
        conversation_status_rate_limited="Rate limited — retrying in {delay}s…",
        terminal_status="Terminal: {name} ({columns}x{rows})",
        system_status="System",
        log_status="Log",
        log_status_entries="Log: entries {first:,}-{last:,} of {total:,}",
        preview_status="Preview: {path}",
        diff_status="Diff: {path} | {rows} changed rows",

        # Mindspace File Tree Edit Menu Errors and Options
        pin="Pin",
        unpin="Unpin",
        rename="Rename",
        delete="Delete",
        open_in_editor="Open In Editor",
        open_in_preview="Open In Preview",
        open_in_diff="Open In Diff",
        open_in_conversation="Open In Conversation",
        duplicate="Duplicate",
        sort_by="Sort By",
        sort_by_name="Sort by name",
        sort_by_creation_time="Sort by creation time",
        file_name="File name",
        rename_error_exists="A file with this name already exists.",
        rename_error_generic="Could not rename file: ",
        file_creation_error_title="File Creation Error",
        file_creation_error="Could not create file: ",

        # File and folder creation
        new_folder="New Folder",
        new_folder_title="Create New Folder",
        folder_name="Folder name",
        new_file_title="Create New File",
        confirm_delete_item_message="Are you sure you want to delete {0}?",
        error_folder_not_empty="Cannot delete folder: folder is not empty.",
        error_creating_folder="Could not create folder: {0}",

        # Trash operations
        trash="Trash",
        trash_empty_message="Trash is empty.",
        trash_no_mindspace_message="No mindspace open.",
        trash_unknown_location="Unknown location",
        trash_restore="Restore",
        trash_delete_forever="Delete Forever",
        trash_empty_trash_action="Empty Trash",
        trash_confirm_delete_forever_title="Delete Forever",
        trash_confirm_delete_forever_message="Are you sure you want to permanently delete {0}? This cannot be undone.",
        trash_confirm_empty_title="Empty Trash",
        trash_confirm_empty_message=(
            "Are you sure you want to permanently delete everything in the trash? This cannot be undone."
        ),
        trash_restore_failed_title="Restore Failed",
        trash_restore_failed_message="Failed to restore {0}: {1}",
        trash_time_just_now="Just now",
        trash_time_minutes_ago="{0}m ago",
        trash_time_hours_ago="{0}h ago",
        trash_time_days_ago="{0}d ago",

        journal="Journal",
        changes_no_mindspace_message="Open a mindspace to see its recent changes.",
        changes_empty_message="No changes have been recorded yet.",
        changes_until="Changes on or before {0}",
        changes_since_action="Show changes on or before…",
        changes_show_all_action="Show all changes",
        changes_revert_file_action="Revert this change",
        changes_restore_to_point_action="Restore the mindspace to this change",
        changes_view_this_version_action="View the version this change produced",
        changes_view_replaced_action="View the version this change replaced",
        changes_compare_action="Compare the replaced version with this one",
        changes_version_unavailable=(
            "This version cannot be shown: its content is no longer stored, or it "
            "is not text and the file has changed since."
        ),
        changes_this_version_unavailable="This change removed the file, so it has no version of its own.",
        changes_revert_title="Revert changes",
        changes_revert_confirm=(
            "Revert {0} change(s)? Every affected file returns to the state it held before. "
            "Changes made outside Humbug are not recorded, so they are not reverted."
        ),
        changes_revert_result="{0} path(s) reverted, {1} could not be.",
        changes_restore_to_point_confirm=(
            "Undo {0} change(s) recorded after this one? Every affected file returns to "
            "the state it held at this change. Changes made outside Humbug are not "
            "recorded, so the mindspace may not be exactly as it was."
        ),
        changes_bucket_today="Today",
        changes_bucket_earlier="Earlier",
        changes_older_not_shown="{0} older change(s) not shown",
        changes_size_warning=(
            "The change journal is using {0:.0f} MB. Right-click here to forget old history."
        ),
        changes_forget_through_action="Forget this change and everything before it",
        changes_forget_all_action="Forget all changes",
        changes_forget_title="Forget changes",
        changes_forget_confirm=(
            "Forget {0}? Those changes can no longer be reverted, and any content held "
            "only by the journal is deleted. This cannot be undone."
        ),
        changes_forget_scope_through="this change and every change before it",
        changes_forget_scope_all="every recorded change",
        changes_forget_result="Forgot {0} change(s) and reclaimed {1:.0f} MB.",
        editor_read_only_marker=" (earlier version)",

        # Tool tips
        tooltip_copy_contents="Copy all contents",
        tooltip_save_contents="Save contents to a file",
        tooltip_copy_message="Copy message to clipboard",
        tooltip_save_message="Save message as markdown",
        tooltip_fork_message="Fork conversation at this message",
        tooltip_delete_from_message="Delete this message and all following messages",
        tooltip_edit_message="Edit from this message",
        tooltip_submit_message="Submit message",
        tooltip_stop_message="Stop current processing",
        tooltip_settings_message="Open conversation settings",
        tooltip_attach_file="Attach a file",
        tooltip_cancel_edit="Cancel editing",
        tooltip_expand_message="Expand message",
        tooltip_collapse_message="Collapse message",
        warning_file_too_large=(
            "'{filename}' is {size_kb}KB, which exceeds the recommended 100KB limit. "
            "Large files may use a lot of context window. Attach anyway?"
        ),
        tooltip_show_attachments="Show attachments",
        tooltip_hide_attachments="Hide attachments",
        error_pdf_unsupported=(
            "'{filename}' could not be attached. {reason}"
        ),
        error_pdf_extraction_failed=(
            "'{filename}' could not be attached: text extraction failed. {reason}"
        ),
        error_docx_unsupported=(
            "'{filename}' could not be attached. {reason}"
        ),
        error_docx_extraction_failed=(
            "'{filename}' could not be attached: text extraction failed. {reason}"
        ),

        delete_from_here_title="Delete Messages",
        delete_from_here_message=(
            "Are you sure you want to delete this message and all subsequent messages? This action cannot be undone."
        ),

        # Move operation strings
        move_file_title="Move File",
        move_folder_title="Move Folder",
        move_file_confirmation="Are you sure you want to move the file '{0}'?",
        move_folder_confirmation="Are you sure you want to move the folder '{0}'?",
        move_from_label="From:",
        move_to_label="To:",
        move_button="Move",
        move_error_title="Move Error",
        move_error_exists="A file or folder with this name already exists at the destination.",
        move_error_failed="Could not move item: {0}",
        move_error_protected="Cannot move system folders (conversations, .humbug).",

        # Inline editor validation errors
        error_empty_name="Name cannot be empty",
        error_invalid_characters="Name contains invalid characters: \\ / : * ? \" < > |",
        error_validation_failed="Unable to validate name",
        error_invalid_path="Invalid file path",
        error_title="Error",

        # File duplication errors
        error_duplicating_file="Could not duplicate file: {0}",

        # File extension change confirmation
        rename_change_extension_title="Change File Type?",
        rename_change_extension_message="You are changing the file extension from '{0}' to '{1}'."
            " This may change how the file is handled. Are you sure?",

        # Tool use
        approve_tool_call="Approve",
        i_am_unsure_about_tool_call="I'm Unsure About This",
        reject_tool_call="Reject",
        auto_approve_file_changes="Auto-approve file changes until your next message",
        interruptible_wait_message="Waiting for {duration} seconds \u2014 {remaining} remaining...",
        finish_waiting_now="Finish Waiting Now",
        retry_error="Retry",

        # Update checker
        check_for_updates="Check for Updates...",
        update_current_version="Current version: {0}",
        update_checking="checking...",
        update_up_to_date="You are up to date.",
        update_available_message="Version {0} is available — click to download.",
        update_check_failed="Could not check for updates. Please check your network connection.",
        update_tooltip="Humbug {0} available — click for release page",
        tab_overview_tooltip="Show all open tabs",
        tab_carousel_tooltip="Show tab carousel",
        check_for_updates_setting="Automatically check for updates",

        # Ollama pull
        ollama_pull_label="Pull Model",
        ollama_pull_placeholder="Model name (e.g. llama3.2)",
        ollama_pull_button="Pull",
        ollama_pull_pulling="Pulling {0}…",
        ollama_pull_success="'{0}' installed successfully.",
        ollama_pull_model_not_found="Model not found — check the name at ollama.com/library.",
        ollama_pull_not_running="Could not connect to Ollama — is it running?",
        ollama_pull_error="Pull failed: {0}",
        ollama_update_local_models="Update Local Models",
        update_models="Update Models",
        remove_fetched_models="Remove Fetched Models…",
        fetching_models="Fetching…",
        unknown_provider="Unknown provider.",
        fetch_added_models="Added {0} new model(s).",
        fetch_already_up_to_date="List already up to date.",
        ollama_no_installed_models="No installed models found.",
        ollama_added_models="Added {0} model(s).",
        ollama_already_up_to_date="Local model list already up to date.",
        test_connection="Test Connection",
        testing_connection="Testing connection…",
        connection_successful="Connection successful.",

        # Settings dialog - AI model page
        settings_provider="Provider",
        settings_all_providers="All Providers",

        # Fetched model manager dialog
        fetched_models_title="Fetched Models — {0}",
        no_fetched_models="No fetched models for this provider.",
        fetched_models_close="Close",

        # Onboarding product tour
        tour_step_of="Step {0} of {1}",
        tour_back_button="Back",
        tour_skip_button="Skip Tour",
        tour_next_button="Next",
        tour_finish_button="Start Using Humbug",
        tour_welcome_title="Welcome to Humbug",
        tour_welcome_description=(
            "Humbug helps you collaborate with AI on your projects — chat, edit files, run terminals, "
            "and review changes, all in one workspace. Let's take a quick tour."
        ),
        tour_start_here_title="Start Here",
        tour_start_here_description="Click here to create or open a mindspace — your project workspace in Humbug.",
        tour_open_conversation_title="Open a Conversation",
        tour_open_conversation_description=(
            "Open your conversations here — use + to start a new one, or the folder icon to organize them."
        ),
        tour_workspace_title="Your Workspace",
        tour_workspace_description="Conversations and other tabs open here. This is where you'll do most of your work.",
        tour_key_action_title="Send a Message",
        tour_key_action_description="Type a message and press Send (or {0}) to talk with the AI.",
        tour_output_title="Conversation History",
        tour_output_description="AI responses and your conversation history appear here as you chat.",
        tour_navigation_title="Navigation",
        tour_navigation_description="Switch between Conversations, Files, Search, Preview, and more using this sidebar.",
        tour_help_title="Settings & Help",
        tour_help_description="Open Settings here anytime. You can restart this tour from Humbug → Take a Tour.",

        fetched_models_remove="Remove",

        fetch_error_invalid_key="Invalid API key — please check your key and try again.",
        fetch_error_access_denied="Access denied ({0}) — your key may not have model-list permission.",
        fetch_error_not_found="Models endpoint not found ({0}) — check the API URL.",
        fetch_error_rate_limited="Rate limited ({0}) — wait a moment then try again.",
        fetch_error_server_error="Provider server error ({0}) — try again later.",
        fetch_error_connection="Could not connect — check the URL and your network.",
        fetch_error_timeout="Request timed out — the provider took too long to respond.",
        fetch_error_generic="Failed: {0}",
        stream_error_interrupted="The connection was interrupted before the response finished. Please try again.",
        stream_error_no_backend="No AI backend is configured for this model's provider — check your AI Backend settings.",
        stream_error_access_denied="Access denied ({0}) — your account may not have access to this model.",

        mission_control="Mission Control",
        mission_subtitle="Plan, delegate and approve in one place",
        mission_hero_title="What would you like to achieve?",
        mission_hero_subtitle=(
            "Describe an objective. A lead AI plans the work, coordinates helper AIs and keeps you in "
            "control: anything that changes files or runs commands still waits for your approval."
        ),
        mission_examples_label="Try one of these",
        mission_example_1="Review this project and write a short architecture overview",
        mission_example_2="Find the failing tests and fix them",
        mission_example_3="Add input validation to the settings form, with tests",
        mission_new="New mission",
        mission_caption="MISSION",
        mission_status_planning="Planning",
        mission_status_running="Running",
        mission_status_needs_you="Needs you",
        mission_status_idle="Waiting for you",
        mission_status_completed="Completed",
        mission_status_stopped="Stopped",
        mission_progress="{0}% complete",
        mission_stop="Stop",
        mission_stat_tasks="Tasks",
        mission_stat_completed="Completed",
        mission_stat_in_progress="In progress",
        mission_stat_approvals="Need your approval",
        mission_stat_agents="Active agents",
        mission_plan_title="Mission Plan",
        mission_plan_empty="The plan appears here once the lead AI has drafted it.",
        mission_task_pending="Pending",
        mission_task_in_progress="In progress",
        mission_task_completed="Completed",
        mission_task_blocked="Blocked",
        mission_agents_title="Agents",
        mission_agents_active="{0} active",
        mission_agents_empty="No conversation is open for this mission. Open the lead conversation to continue.",
        mission_agent_lead="Mission lead",
        mission_agent_delegate="Delegated agent",
        mission_agent_thinking="Thinking…",
        mission_agent_working="Using {0}",
        mission_agent_waiting="Waiting for your approval",
        mission_agent_idle="Idle",
        mission_agent_error="Stopped with an error",
        mission_inbox_title="Decision Inbox",
        mission_inbox_empty="Nothing needs your decision. Approval requests from every open conversation appear here.",
        mission_approve="Approve",
        mission_unsure="Unsure",
        mission_reject="Reject",
        mission_destructive="Destructive",
        mission_decision_from="From {0}",
        mission_activity_title="Recent Activity",
        mission_activity_empty="Activity for this mission appears here.",
        mission_activity_started="Mission started",
        mission_activity_plan_updated="Plan updated",
        mission_activity_agent_joined="{0} joined the mission",
        mission_activity_turn_started="{0} started working",
        mission_activity_turn_finished="{0} finished",
        mission_activity_tool_used="{0} used {1}",
        mission_activity_approval_requested="{0} asked to use {1}",
        mission_activity_approved="You approved {0}",
        mission_activity_rejected="You declined {0}",
        mission_activity_guidance="You sent guidance",
        mission_activity_stopped="Mission stopped",
        mission_activity_error="{0} stopped with an error",
        mission_composer_new="Tell Humbug what you want to achieve… ({key} to submit)",
        mission_composer_guidance="Send guidance to the mission lead… ({key} to submit)",
        mission_send="Send",
        mission_error_start="Could not start the mission: {0}",
        mission_error_send="Could not send to the mission: {0}",
        mission_decision_stale="That request has already been answered.",
        mission_lead_message_title="Latest from the mission lead",
        mission_lead_waiting="Waiting for your reply",
        mission_reply="Reply",
        mission_composer_reply="Reply to the mission lead… ({key} to submit)",
        mission_task_progress="Progress",
        mission_task_evidence="Evidence",
        mission_task_worked_by="Worked on by {0}",
        mission_task_no_update="The lead has not reported progress for this step yet.",
        mission_task_open_agent="Open conversation",
        mission_plan_hint="Click a step to see its progress.",
        mission_plan_filter_empty="No steps match this filter.",
        mission_plan_show_all="Show all",
        mission_activity_auto_continued="Asked the lead to carry on",
        mission_activity_auto_paused="Paused: no step finished after {0} automatic continues",
        mission_status_interrupted="Interrupted",
        mission_resume="Resume",
        mission_interrupted_notice="Humbug was closed while this mission was running. Resume to carry on from where it stopped.",
        mission_auto_resume="Auto-resume after restart",
        mission_activity_resumed="Resumed after Humbug was closed",
        mission_error_resume="Could not resume the mission: {0}",
        mission_switch_to_autonomous="Switch to Autonomous mode",
        mission_switch_to_classic="Switch to Classic mode",
        mission_mode_autonomous="Autonomous",
        mission_mode_classic="Classic",
        mission_classic_title="This mission is in Classic mode",
        mission_classic_body=(
            "You are driving it from its conversation tab, so Mission Control does not carry it on. "
            "Switch to Autonomous mode to hand it back."
        ),
        mission_task_stop="Stop task",
        mission_task_stopped="Stopped",
        mission_activity_task_stopped="You stopped “{0}”",
        mission_activity_mode_classic="Switched to Classic mode",
        mission_activity_mode_autonomous="Switched to Autonomous mode",
        mission_error_switch="Could not switch the mission: {0}",
        mission_error_stop_task="Could not stop the task: {0}",
        mission_auto_approve="Auto-approve",
        mission_auto_approve_off="Off",
        mission_auto_approve_safe="Safe actions",
        mission_auto_approve_all="Everything",
        mission_auto_approve_tip=(
            "An AI reviewer approves this mission's requests when they clearly fit the objective. "
            "Anything it is unsure about still comes to you. Safe actions never includes deleting, "
            "moving, overwriting, terminal commands or network writes."
        ),
        mission_auto_approve_all_title="Approve everything automatically?",
        mission_auto_approve_all_confirm=(
            "An AI reviewer will approve this mission's requests without asking you, including "
            "deleting files, running terminal commands and sending data over the network. Requests it "
            "is unsure about still come to you, and every decision is logged. Turn this on only for "
            "work you are comfortable not supervising."
        ),
        mission_decision_reviewing="AI reviewing…",
        mission_decision_review_note="AI reviewer: {0}",
        mission_activity_auto_approved="AI reviewer approved {0}",
        mission_activity_review_deferred="AI reviewer left {0} for you",
        mission_auto_approve_note_classic="Auto-approve is off while this mission is in Classic mode.",
        mission_auto_approve_note_destructive="This request is destructive, and auto-approve is set to Safe actions.",
        mission_options="Mission options",
        mission_rename="Rename",
        mission_rename_title="Rename mission",
        mission_rename_label="Mission name:",
        mission_delete="Delete",
        mission_delete_title="Delete mission",
        mission_delete_confirm="Delete \"{0}\" from Mission Control? Its conversations are kept.",
        mission_error_rename="Could not rename the mission: {0}",
        mission_you="You",
        mission_plan_history="History ({0})",
        mission_plan_history_tip="Plans this mission finished before the current work",
        mission_plan_history_title="Earlier plans",
        mission_plan_empty_next="The lead AI is drafting the plan for the new work. Earlier plans are in History.",
        mission_phase_count="{0} of {1} steps done",
        mission_activity_plan_archived="Finished plan moved to history",
        mission_say_reading="Reading {0} for you…",
        mission_say_writing="Writing {0} for you…",
        mission_say_editing="Editing {0} for you…",
        mission_say_deleting="Removing {0}…",
        mission_say_files="Sorting out files for you…",
        mission_say_delegating="Bringing in a helper agent for this…",
        mission_say_terminal="Running a command for you: {0}",
        mission_say_web="Looking something up on the web for you…",
        mission_say_tool="Using {0} for you…",
        mission_say_task="Now working on “{0}” for you…",
        mission_say_thinking="Thinking about the next step…",
        mission_say_planning="Hi, I'm {0}! Drafting a plan for you…",
        mission_say_hello="Hi, I'm {0}! Getting started on my part…",
        mission_say_approval="I need your approval: {0}",
        mission_say_reviewing="The AI reviewer is checking my request: {0}",
        mission_say_done="All done! Anything else I can do for you?",
        mission_say_reply="I've left you a message. Reply below when you're ready.",
        mission_say_stopped="I've stopped. Send me a message to carry on.",
        mission_say_interrupted="Humbug closed while I was working. Press Resume and I'll pick up where I left off.",
        mission_say_helper_waiting="One of my helpers needs your approval.",
        mission_say_ready="Ready when you are.",
        mission_say_error="Something went wrong. Open my conversation to see why.",
        mission_say_sleeping="Taking a planned pause of {0}. I'll carry on when it's up…",
        mission_finish_waiting="Finish waiting",
        mission_wait_left="{0} left",
        mission_activity_started_prompt="Mission started: “{0}”",
        mission_activity_guidance_prompt="You: “{0}”",
        mission_composer_hint_enter="Enter for a new line"
    )
