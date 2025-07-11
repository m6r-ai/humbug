"""English language strings for the UI."""

from humbug.language.language_strings import LanguageStrings


def get_english_strings() -> LanguageStrings:
    """
    Get the English language strings.

    Returns:
        LanguageStrings object with all English text
    """
    return LanguageStrings(
        # Window titles
        about_title="About Humbug",

        # Menu names
        humbug_menu="Humbug",
        file_menu="File",
        edit_menu="Edit",
        view_menu="View",

        # File menu items
        about_humbug="About Humbug",
        quit_humbug="Quit Humbug",
        new_mindspace="New Mindspace",
        new_conversation="New Conversation",
        new_metaphor_conversation="New Metaphor Conversation...",
        new_file="New File",
        new_terminal="New Terminal",
        open_mindspace="Open Mindspace...",
        open_conversation="Open Conversation...",
        open_file="Open File...",
        open_wiki="Open Wiki",
        fork_conversation="Fork Conversation",
        save="Save",
        save_as="Save As...",
        close_mindspace="Close Mindspace",
        close_tab="Close Tab",
        user_settings="User Settings",

        # Edit menu items
        submit_message="Submit Message",
        undo="Undo",
        redo="Redo",
        cut="Cut",
        copy="Copy",
        paste="Paste",
        find="Find",
        mindspace_settings="Mindspace Settings",
        conversation_settings="Conversation Settings",

        # View menu items
        display_theme="Display Theme",
        zoom_in="Zoom In",
        zoom_out="Zoom Out",
        reset_zoom="Reset Zoom",
        show_system_log="Show Mindspace Log",
        show_system_shell="Show Humbug Shell",
        show_all_columns="Show All Columns",
        split_column_left="Split Column Left",
        split_column_right="Split Column Right",
        merge_column_left="Merge Column Left",
        merge_column_right="Merge Column Right",
        swap_column_left="Swap Column Left",
        swap_column_right="Swap Column Right",
        next_message="Next Message",
        previous_message="Previous Message",

        # Message roles
        role_you="You",
        role_assistant="{model} response",
        role_reasoning="{model} reasoning",
        role_system="Humbug",
        role_tool_call="Tool call",
        role_tool_result="Tool result",

        # Message labels
        highlighting="Highlighting: {language}",

        # Find widget
        find_placeholder="Find",
        find_no_matches="No matches",
        find_match_count="{current} of {total}",

        # Input widget
        processing_message="Processing your request (Esc to cancel)",
        input_prompt="Chat with {model}... ({key} to submit)",
        command_prompt="Command... (Enter or {key} to submit)",

        # Dialog and settings
        cancel="Cancel",
        ok="OK",
        apply="Apply",
        discard="Discard",
        yes="Yes",
        no="No",

        # User Settings dialog - AI backends section
        general_settings="General Settings",
        display_settings="Display Settings",
        ai_backend_config="AI Backend Configuration",
        enable_backend="Enable Backend",
        api_key="API Key",
        api_url="API URL (optional)",
        anthropic_backend="Anthropic",
        deepseek_backend="DeepSeek",
        google_backend="Google",
        m6r_backend="M6R",
        mistral_backend="Mistral",
        openai_backend="OpenAI",
        ollama_backend="Ollama (local)",
        xai_backend="xAI",

        # User Settings dialog - other settings
        select_language="Language",
        font_size="Font Size",
        theme_dark="Dark",
        theme_light="Light",

        # Mindspace tree
        mindspace_label_none="No mindspace active",

        # Mindspace folders dialog
        mindspace_folders_title="Configure Mindspace Folders",
        mindspace_path="Mindspace Path",
        conversations_folder='Create "conversations" Folder',
        metaphor_folder='Create "metaphor" Folder',
        src_folder='Create "src" Folder',

        # Mindspace settings
        model_settings="Model Settings",
        editor_settings="Editor Settings",
        use_soft_tabs="Use Soft Tabs",
        tab_size="Tab Size",
        backup_settings="Backup Settings",
        auto_backup="Auto Backup",
        backup_interval="Backup Interval (seconds)",

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

        # File dialog titles
        file_dialog_open_metaphor="Open Metaphor File",
        file_dialog_open_conversation="Open Conversation",
        file_dialog_open_file="Open File",
        file_dialog_save_file="Save File",
        file_dialog_new_mindspace="Create New Mindspace",
        file_dialog_open_mindspace="Open Mindspace",

        # File dialog filters
        file_filter_all="All Files (*.*)",
        file_filter_metaphor="Metaphor Files (*.m6r)",
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
        error_processing_metaphor="Failed to process Metaphor file:\n\n{}",
        error_saving_mindspace_settings="Failed to save mindspace settings: {}",
        error_saving_user_settings="Failed to save user settings: {}",

        # Status bar
        editor_status="Line {line}, Column {column} | {encoding} | {line_ending} | {type}",
        conversation_status=(
            "Model: {model}  |  {temperature}  |  "
            "Input Tokens: {input_tokens} / {max_input_tokens}  |  "
            "Input Total: {total_input_tokens}  |  "
            "Output Tokens: {output_tokens} / {max_output_tokens}  |  "
            "Output Total: {total_output_tokens}"
        ),
        conversation_status_temperature="Temp: {temperature:.1f}",
        conversation_status_no_temperature="Temp: N/A",
        terminal_status="Terminal: {name} ({columns}x{rows})",
        system_status="System",
        log_status="Log",

        # Bookmark-related strings
        bookmark_section="Toggle Bookmark",
        next_bookmark="Next Bookmark",
        previous_bookmark="Previous Bookmark",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="Rename",
        delete="Delete",
        edit="Edit",
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
        tooltip_fork_message="Fork conversation after this message",
        tooltip_delete_from_message="Delete conversation from this point",
        tooltip_edit_file="Edit this file",
        tooltip_submit_message="Submit message",
        tooltip_stop_message="Stop processing message",
        tooltip_expand_message="Expand message",
        tooltip_collapse_message="Collapse message",

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
        move_error_protected="Cannot move system folders (conversations, metaphor, .humbug).",

        # Inline editor validation errors
        error_empty_name="Name cannot be empty",
        error_invalid_characters="Name contains invalid characters: \\ / : * ? \" < > |",
        error_validation_failed="Unable to validate name",
        error_invalid_path="Invalid file path",
        error_title="Error",

        # File duplication errors
        error_duplicating_file="Could not duplicate file: {0}",

        # Tool use
        approve_tool_call="Approve Tool Call",
        reject_tool_call="Reject Tool Call"
    )
