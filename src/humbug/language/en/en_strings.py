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
        open_mindspace="Open Mindspace",
        open_conversation="Open Conversation...",
        open_file="Open File...",
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
        role_assistant="Assistant Response",
        role_reasoning="Assistant Reasoning",
        role_system="System Message",

        # Message labels
        highlighting="Highlighting: {language}",

        # Find widget
        find_placeholder="Find",
        find_no_matches="No matches",
        find_match_count="{current} of {total}",

        # Input widget
        processing_message="Processing your request (Esc to cancel)",
        input_prompt="Add your next message... ({key} to submit)",

        # File tree messages
        rename_conversation="Rename Conversation",
        conversation_name="Conversation Name",
        delete_file="Delete",

        # Dialog and settings
        cancel="Cancel",
        ok="OK",
        apply="Apply",
        discard="Discard",
        yes="Yes",
        no="No",

        # User Settings dialog
        anthropic_api_key="Anthropic API Key",
        deepseek_api_key="DeepSeek API Key",
        google_api_key="Google API Key",
        m6r_api_key="M6R API Key",
        mistral_api_key="Mistral API Key",
        openai_api_key="OpenAI API Key",
        xai_api_key="xAI API Key",
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
        use_soft_tabs="Use Soft Tabs",
        tab_size="Tab Size",
        auto_backup="Auto Backup",
        backup_interval="Backup Interval (seconds)",

        # Conversation settings
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
        file_filter_text="Text Files (*.txt)",

        # Dialog titles
        mindspace_error_title="Mindspace Error",
        conversation_error_title="Conversation Error",
        metaphor_error_title="Metaphor Processing Error",
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
        delete_file_warning="Are you sure you want to delete this file?",
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
            "Model: {model} | {temperature} | Last response - Input: {input_tokens} "
            "({max_input_tokens}) | Output: {output_tokens} ({max_output_tokens})"
        ),
        conversation_status_temperature="Temp: {temperature:.1f}",
        conversation_status_no_temperature="Temp: N/A",
        terminal_status="Terminal: {name} ({columns}x{rows})",

        # Bookmark-related strings
        bookmark_section="Toggle Bookmark",
        next_bookmark="Next Bookmark",
        previous_bookmark="Previous Bookmark",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="Rename",
        delete="Delete",
        new="New",
        new_folder="Folder",
        rename_file_title="Rename File",
        rename_file_prompt="Enter new name",
        rename_error_title="Rename Error",
        rename_error_exists="A file with this name already exists.",
        rename_error_generic="Could not rename file: ",
        file_creation_error_title="File Creation Error",
        file_creation_error="Could not create file: ",

        # Tool tips
        tooltip_copy_contents="Copy all contents",
        tooltip_save_contents="Save contents to a file",
        tooltip_copy_message="Copy message to clipboard",
        tooltip_save_message="Save message as markdown"
    )
