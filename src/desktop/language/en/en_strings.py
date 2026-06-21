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

        # Tab bar context menu items
        close_tabs_to_left="Close Tabs to the Left",
        close_tabs_to_right="Close Tabs to the Right",
        close_other_tabs="Close Other Tabs",

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
        theme_custom="Custom Theme",
        customize_colors="Customize Colors…",
        file_sort_order="File Sort Order",
        sort_directories_first="Directories First",
        sort_alphabetical="Alphabetical",

        # User Settings dialog - external file access
        external_file_access="File Access",
        allow_external_file_access="Allow AI to read files outside mindspace",
        external_file_allowlist="Allowed paths (glob patterns, one per line):",
        external_file_denylist="Always denied paths (glob patterns, one per line):",

        # Mindspace tree
        mindspace_label_none="No mindspace active",
        mindspace_name_tooltip="Click to open a different mindspace",
        mindspace_files="Files",
        mindspace_vcs="Changed Files",
        mindspace_vcs_no_repo="No repository found",
        mindspace_conversations="Conversations",
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
        conversations_folder='Create "conversations" Folder',
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

        # Unified settings dialog
        settings_all_mindspaces="All Mindspaces",
        settings_this_mindspace="This Mindspace",
        settings_display="Display",
        settings_file_access="File Access",
        settings_ai_backends="AI Backends",
        settings_ai_model="AI Model",
        settings_ai_tools="AI Tools",
        settings_editor="Editor",
        settings_terminal="Terminal",
        settings_tabs="Tabs",

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
        delete_warning_detail="Any open tab for this file will be closed without saving.",
        error_deleting_file="Could not delete file: {0}",
        error_title_rename="Rename Error",
        error_rename_exists="A conversation named '{0}' already exists.",
        error_rename_failed="Could not rename conversation: {0}",
        unsaved_changes="Do you want to save changes to {0}?",
        mindspace_exists_error="Mindspace already exists in selected directory.",
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
        cancel_conversation="Are you sure you want to stop the current AI response? "
            "This will terminate the ongoing conversation and may result in incomplete responses.",

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
        preview_status="Preview: {path}",
        diff_status="Diff: {path} | {rows} changed rows",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="Rename",
        delete="Delete",
        open_in_editor="Open In Editor",
        open_in_preview="Open In Preview",
        open_in_diff="Open In Diff",
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

        # Tool tips
        tooltip_copy_contents="Copy all contents",
        tooltip_save_contents="Save contents to a file",
        tooltip_copy_message="Copy message to clipboard",
        tooltip_save_message="Save message as markdown",
        tooltip_fork_message="Fork conversation at this message",
        tooltip_delete_from_message="Delete conversation from this point",
        tooltip_edit_message="Edit this message",
        tooltip_edit_file="Edit this file",
        tooltip_submit_message="Submit message",
        tooltip_stop_message="Stop current processing",
        tooltip_settings_message="Open conversation settings",
        tooltip_attach_file="Attach a file",
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

        # Fetch-models errors
        fetch_error_invalid_key="Invalid API key — please check your key and try again.",
        fetch_error_access_denied="Access denied ({0}) — your key may not have model-list permission.",
        fetch_error_not_found="Models endpoint not found ({0}) — check the API URL.",
        fetch_error_rate_limited="Rate limited ({0}) — wait a moment then try again.",
        fetch_error_server_error="Provider server error ({0}) — try again later.",
        fetch_error_connection="Could not connect — check the URL and your network.",
        fetch_error_timeout="Request timed out — the provider took too long to respond.",
        fetch_error_generic="Failed: {0}"
    )
