"""Base language strings class."""

from dataclasses import dataclass


@dataclass
class LanguageStrings:
    """Strings for UI elements in different languages."""
    # Window titles
    about_title: str

    # Welcome widget
    welcome_message: str
    welcome_button: str

    # Menu names
    humbug_menu: str
    file_menu: str
    mindspace_menu: str
    edit_menu: str
    view_menu: str

    # Menu items
    about_humbug: str
    quit_humbug: str
    new_mindspace: str
    new_conversation: str
    new_file: str
    new_terminal: str
    open_mindspace: str
    open_conversation: str
    open_file: str
    open_preview: str
    open_diff: str
    save: str
    save_as: str
    close_mindspace: str
    close_tab: str
    settings: str

    # Tab bar context menu items
    close_tabs_to_left: str
    close_tabs_to_right: str
    close_other_tabs: str

    # Edit menu items
    submit_message: str
    undo: str
    redo: str
    cut: str
    copy: str
    paste: str
    select_all: str
    find: str
    find_replace: str
    goto_line: str
    mindspace_search: str
    mindspace_settings: str
    conversation_settings: str

    # View menu items
    display_theme: str
    zoom_in: str
    zoom_out: str
    reset_zoom: str
    open_mindspace_log: str
    open_humbug_shell: str
    show_tab_overview: str
    show_tab_carousel: str
    show_all_columns: str
    split_column_left: str
    split_column_right: str
    merge_column_left: str
    merge_column_right: str
    swap_column_left: str
    swap_column_right: str
    next_message: str
    previous_message: str
    next_hunk: str
    previous_hunk: str

    # Message roles and labels
    role_you: str
    role_you_queued: str
    role_connected: str
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
    find_match_case: str
    find_use_whole_word: str
    find_use_regexp: str
    find_invalid_regexp: str
    replace_placeholder: str
    replace_button: str
    replace_all_button: str
    replace_count: str  # Format: "{count} replaced"

    # Go to line dialog
    goto_line_title: str
    goto_line_label: str

    # Input widget
    processing_message: str
    input_prompt: str  # Format: "Chat with {model}... ({key} to submit)"
    command_prompt: str  # Format: "Command... (Enter or {key} to submit)"

    # AI thinking message
    ai_thinking: str

    # Dialog and settings
    cancel: str
    ok: str
    apply: str
    discard: str
    yes: str
    no: str

    # User Settings dialog - AI backends section
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
    ollama_cloud_backend: str
    xai_backend: str
    vllm_backend: str

    # User Settings dialog - other settings
    select_language: str
    font_size: str
    theme_system: str
    font_ligatures: str
    theme_dark: str
    theme_light: str
    theme_color_blind: str
    theme_custom: str
    customize_colors: str
    file_sort_order: str
    sort_directories_first: str
    sort_alphabetical: str

    # User Settings dialog - external file access
    external_file_access: str
    allow_external_file_access: str
    external_file_allowlist: str
    external_file_denylist: str

    # Mindspace tree
    mindspace_label_none: str
    mindspace_name_tooltip: str
    mindspace_files: str
    mindspace_vcs: str
    mindspace_vcs_no_repo: str
    mindspace_conversations: str
    mindspace_preview: str
    mindspace_expand_sidebar: str
    mindspace_collapse_sidebar: str
    mindspace_search_placeholder: str
    mindspace_search_options: str
    mindspace_search_whole_word: str
    mindspace_search_empty_state: str
    mindspace_search_no_results: str
    mindspace_search_path_match: str
    mindspace_search_results: str
    mindspace_search_results_limited: str
    mindspace_search_include_hidden: str

    # Mindspace folders dialog
    mindspace_folders_title: str
    mindspace_path: str
    conversations_folder: str
    src_folder: str

    # Mindspace settings
    model_settings: str
    editor_settings: str
    use_soft_tabs: str
    tab_size: str
    backup_settings: str
    auto_backup: str
    backup_interval: str
    terminal_settings: str
    terminal_settings_description: str
    terminal_fixed_width_enabled: str
    terminal_fixed_width: str
    terminal_scrollback_enabled: str
    terminal_scrollback_lines: str
    terminal_close_on_exit: str
    tool_settings: str
    tools_description: str

    # Unified settings dialog
    settings_all_mindspaces: str
    settings_this_mindspace: str
    settings_display: str
    settings_file_access: str
    settings_ai_backends: str
    settings_ai_model: str
    settings_ai_tools: str
    settings_editor: str
    settings_terminal: str
    settings_tabs: str

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
    settings_reasoning_effort_label: str
    settings_effort_none: str
    settings_effort_minimal: str
    settings_effort_low: str
    settings_effort_medium: str
    settings_effort_high: str
    settings_effort_xhigh: str
    settings_effort_max: str

    # File dialog titles
    file_dialog_open_conversation: str
    file_dialog_open_file: str
    file_dialog_save_file: str
    file_dialog_new_mindspace: str
    file_dialog_open_mindspace: str
    file_dialog_attach_file: str

    # File dialog filters
    file_filter_all: str
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
    preview_error_title: str
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
    error_opening_preview: str
    error_saving_mindspace_settings: str
    error_saving_user_settings: str
    cancel_conversation: str

    # Status bar
    editor_status: str
    conversation_status: str
    conversation_status_temperature: str
    conversation_status_no_temperature: str
    conversation_status_reasoning: str
    conversation_status_no_reasoning: str
    terminal_status: str
    system_status: str
    log_status: str
    preview_status: str
    diff_status: str

    # Mindspace File Tree Edit Menu Errors and Options
    rename: str
    delete: str
    open_in_editor: str
    open_in_preview: str
    open_in_diff: str
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
    tooltip_edit_message: str
    tooltip_edit_file: str
    tooltip_submit_message: str
    tooltip_stop_message: str
    tooltip_settings_message: str
    tooltip_attach_file: str
    tooltip_expand_message: str
    tooltip_collapse_message: str
    warning_file_too_large: str
    tooltip_show_attachments: str
    tooltip_hide_attachments: str
    error_pdf_unsupported: str
    error_pdf_extraction_failed: str
    error_docx_unsupported: str
    error_docx_extraction_failed: str

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

    # File extension change confirmation
    rename_change_extension_title: str
    rename_change_extension_message: str

    # Tool use
    approve_tool_call: str
    i_am_unsure_about_tool_call: str
    reject_tool_call: str
    retry_error: str

    # Update checker
    check_for_updates: str
    update_current_version: str  # Format: "Current version: {0}"
    update_checking: str
    update_up_to_date: str
    update_available_message: str  # Format: "Version {0} is available"
    update_check_failed: str
    update_tooltip: str  # Format: "Humbug {0} available"
    tab_overview_tooltip: str
    tab_carousel_tooltip: str
    check_for_updates_setting: str

    # Fetch-models error messages ({0} = HTTP status code or error text)
    fetch_error_invalid_key: str
    fetch_error_access_denied: str
    fetch_error_not_found: str
    fetch_error_rate_limited: str
    fetch_error_server_error: str
    fetch_error_connection: str
    fetch_error_timeout: str
    fetch_error_generic: str

    # Ollama pull model UI ({0} = model name or detail)
    ollama_pull_label: str
    ollama_pull_placeholder: str
    ollama_pull_button: str
    ollama_pull_pulling: str
    ollama_pull_success: str
    ollama_pull_model_not_found: str
    ollama_pull_not_running: str
    ollama_pull_error: str
    ollama_update_local_models: str
