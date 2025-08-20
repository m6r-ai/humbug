"""Base language strings class."""

from dataclasses import dataclass


@dataclass
class LanguageStrings:
    """Strings for UI elements in different languages."""
    # Window titles
    about_title: str

    # Menu names
    humbug_menu: str
    file_menu: str
    edit_menu: str
    view_menu: str

    # Menu items
    about_humbug: str
    quit_humbug: str
    new_mindspace: str
    new_conversation: str
    new_metaphor_conversation: str
    new_file: str
    new_terminal: str
    open_mindspace: str
    open_conversation: str
    open_file: str
    open_wiki: str
    fork_conversation: str
    save: str
    save_as: str
    close_mindspace: str
    close_tab: str
    user_settings: str

    # Edit menu items
    submit_message: str
    undo: str
    redo: str
    cut: str
    copy: str
    paste: str
    find: str
    mindspace_settings: str
    conversation_settings: str

    # View menu items
    display_theme: str
    zoom_in: str
    zoom_out: str
    reset_zoom: str
    show_system_log: str
    show_system_shell: str
    show_all_columns: str
    split_column_left: str
    split_column_right: str
    merge_column_left: str
    merge_column_right: str
    swap_column_left: str
    swap_column_right: str
    next_message: str
    previous_message: str

    # Message roles and labels
    role_you: str
    role_assistant: str
    role_reasoning: str
    role_system: str
    role_tool_call: str
    role_tool_result: str

    # Conversation labels
    highlighting: str

    # Find widget
    find_placeholder: str
    find_no_matches: str
    find_match_count: str  # Format: "{current} of {total}"

    # Input widget
    processing_message: str
    input_prompt: str  # Format: "Chat with {model}... ({key} to submit)"
    command_prompt: str  # Format: "Command... (Enter or {key} to submit)"

    # Dialog and settings
    cancel: str
    ok: str
    apply: str
    discard: str
    yes: str
    no: str

    # User Settings dialog - AI backends section
    general_settings: str
    display_settings: str
    ai_backend_config: str
    enable_backend: str
    api_key: str
    api_url: str
    anthropic_backend: str
    deepseek_backend: str
    google_backend: str
    zai_backend: str
    mistral_backend: str
    openai_backend: str
    ollama_backend: str
    xai_backend: str

    # User Settings dialog - other settings
    select_language: str
    font_size: str
    theme_dark: str
    theme_light: str

    # Mindspace tree
    mindspace_label_none: str
    mindspace_files: str
    mindspace_conversations: str
    mindspace_wiki: str

    # Mindspace folders dialog
    mindspace_folders_title: str
    mindspace_path: str
    conversations_folder: str
    metaphor_folder: str
    src_folder: str

    # Mindspace settings
    model_settings: str
    editor_settings: str
    use_soft_tabs: str
    tab_size: str
    backup_settings: str
    auto_backup: str
    backup_interval: str
    tool_settings: str
    tools_description: str

    # Conversation settings
    model_info: str
    settings_model_label: str
    settings_temp_label: str
    settings_context_label: str
    settings_max_output_label: str
    settings_tokens_label: str
    settings_reasoning_label: str
    settings_no_reasoning: str
    settings_hidden_reasoning: str
    settings_visible_reasoning: str

    # File dialog titles
    file_dialog_open_metaphor: str
    file_dialog_open_conversation: str
    file_dialog_open_file: str
    file_dialog_save_file: str
    file_dialog_new_mindspace: str
    file_dialog_open_mindspace: str

    # File dialog filters
    file_filter_all: str
    file_filter_metaphor: str
    file_filter_conversation: str

    # Dialog titles
    mindspace_error_title: str
    conversation_error_title: str
    settings_error_title: str
    error_opening_file_title: str
    error_saving_file_title: str
    save_changes_title: str
    confirm_delete_title: str
    file_error_title: str
    wiki_error_title: str
    cancel_conversation_title: str

    # Messages
    confirm_delete_message: str
    delete_warning_detail: str
    error_deleting_file: str
    error_title_rename: str
    error_rename_exists: str
    error_rename_failed: str
    unsaved_changes: str
    mindspace_exists_error: str
    close_button: str
    confirm_close: str
    could_not_open: str
    could_not_save: str
    error_creating_mindspace: str
    error_opening_mindspace: str
    error_restoring_mindspace: str
    error_saving_mindspace: str
    error_creating_conversation: str
    error_opening_conversation: str
    error_forking_conversation: str
    error_processing_metaphor: str
    error_saving_mindspace_settings: str
    error_saving_user_settings: str
    cancel_conversation: str

    # Status bar
    editor_status: str
    conversation_status: str
    conversation_status_temperature: str
    conversation_status_no_temperature: str
    terminal_status: str
    system_status: str
    log_status: str
    wiki_status: str

    # Bookmark-related strings
    bookmark_section: str
    next_bookmark: str
    previous_bookmark: str

    # Mindspace File Tree Edit Menu Errors and Options
    rename: str
    delete: str
    edit: str
    wiki_view: str
    duplicate: str
    sort_by: str
    sort_by_name: str
    sort_by_creation_time: str
    file_name: str
    rename_error_exists: str
    rename_error_generic: str
    file_creation_error_title: str
    file_creation_error: str

    # File and folder creation
    new_folder: str
    new_folder_title: str
    folder_name: str
    new_file_title: str
    confirm_delete_item_message: str  # Generic message for file/folder deletion
    error_folder_not_empty: str
    error_creating_folder: str

    # Tool tips
    tooltip_copy_contents: str
    tooltip_save_contents: str
    tooltip_copy_message: str
    tooltip_save_message: str
    tooltip_fork_message: str
    tooltip_delete_from_message: str
    tooltip_edit_file: str
    tooltip_submit_message: str
    tooltip_stop_message: str
    tooltip_expand_message: str
    tooltip_collapse_message: str

    delete_from_here_title: str
    delete_from_here_message: str

    # Move operation strings
    move_file_title: str
    move_folder_title: str
    move_file_confirmation: str
    move_folder_confirmation: str
    move_from_label: str
    move_to_label: str
    move_button: str
    move_error_title: str
    move_error_exists: str
    move_error_failed: str
    move_error_protected: str

    # Inline editor validation errors
    error_empty_name: str
    error_invalid_characters: str
    error_validation_failed: str
    error_invalid_path: str
    error_title: str

    # File duplication errors
    error_duplicating_file: str

    # Tool use
    approve_tool_call: str
    reject_tool_call: str
