"""French language strings for the UI."""

from humbug.language.language_strings import LanguageStrings


def get_french_strings() -> LanguageStrings:
    """
    Get the French language strings.

    Returns:
        LanguageStrings object with all French text
    """
    return LanguageStrings(
        # Window titles
        about_title="À propos de Humbug",

        # Menu names
        humbug_menu="Humbug",
        file_menu="Fichier",
        edit_menu="Édition",
        view_menu="Affichage",

        # File menu items
        about_humbug="À propos de Humbug",
        quit_humbug="Quitter Humbug",
        new_mindspace="Nouvel espace mental",
        new_conversation="Nouvelle conversation",
        new_metaphor_conversation="Nouvelle conversation Metaphor...",
        new_file="Nouveau fichier",
        new_terminal="Nouveau terminal",
        open_mindspace="Ouvrir espace mental...",
        open_conversation="Ouvrir conversation...",
        open_file="Ouvrir fichier...",
        open_wiki="Ouvrir wiki",
        fork_conversation="Dupliquer la conversation",
        save="Enregistrer",
        save_as="Enregistrer sous...",
        close_mindspace="Fermer l'espace mental",
        close_tab="Fermer l'onglet",
        user_settings="Paramètres utilisateur",

        # Edit menu items
        submit_message="Envoyer le message",
        undo="Annuler",
        redo="Rétablir",
        cut="Couper",
        copy="Copier",
        paste="Coller",
        find="Rechercher",
        mindspace_settings="Paramètres de l'espace mental",
        conversation_settings="Paramètres de conversation",

        # View menu items
        display_theme="Thème d'affichage",
        zoom_in="Zoom avant",
        zoom_out="Zoom arrière",
        reset_zoom="Réinitialiser le zoom",
        show_system_log="Afficher le journal de l'espace mental",
        show_system_shell="Afficher le shell Humbug",
        show_all_columns="Afficher toutes les colonnes",
        split_column_left="Diviser la colonne à gauche",
        split_column_right="Diviser la colonne à droite",
        merge_column_left="Fusionner avec la colonne de gauche",
        merge_column_right="Fusionner avec la colonne de droite",
        swap_column_left="Échanger la colonne gauche",
        swap_column_right="Échanger la colonne de droite",
        next_message="Message Suivant",
        previous_message="Message Précédent",

        # Message roles
        role_you="Vous",
        role_assistant="Réponse de {model}",
        role_reasoning="Raisonnement de {model}",
        role_system="Humbug",
        role_tool_call="Appel d'outil",
        role_tool_result="Résultat de l'outil",

        # Message labels
        highlighting="Mise en évidence : {language}",

        # Find widget
        find_placeholder="Rechercher",
        find_no_matches="Aucun résultat",
        find_match_count="{current} sur {total}",

        # Input widget
        processing_message="Traitement de votre requête (Échap pour annuler)",
        input_prompt="Discuter avec {model}... ({key} pour envoyer)",
        command_prompt="Commande... (Entrée ou {key} pour envoyer)",

        # Dialog and settings
        cancel="Annuler",
        ok="OK",
        apply="Appliquer",
        discard="Abandonner",
        yes="Oui",
        no="Non",

        # User Settings dialog - AI backends section
        general_settings="Paramètres généraux",
        display_settings="Paramètres d'affichage",
        ai_backend_config="Configuration des backends d'IA",
        enable_backend="Activer le backend",
        api_key="Clé API",
        api_url="URL API (optionnel)",
        anthropic_backend="Anthropic",
        deepseek_backend="DeepSeek",
        google_backend="Google",
        m6r_backend="M6R",
        mistral_backend="Mistral",
        openai_backend="OpenAI",
        ollama_backend="Ollama",
        xai_backend="xAI",

        # User Settings dialog - other settings
        select_language="Langue",
        font_size="Taille de police",
        theme_dark="Sombre",
        theme_light="Clair",

        # Mindspace tree
        mindspace_label_none="Aucun espace mental actif",

        # Mindspace folders dialog
        mindspace_folders_title="Configurer les dossiers de l'espace mental",
        mindspace_path="Chemin de l'espace mental",
        conversations_folder='Créer le dossier "conversations"',
        metaphor_folder='Créer le dossier "metaphor"',
        src_folder='Créer le dossier "src"',

        # Mindspace settings
        model_settings="Paramètres du modèle",
        editor_settings="Paramètres de l'éditeur",
        use_soft_tabs="Utiliser des tabulations douces",
        tab_size="Taille de tabulation",
        backup_settings="Paramètres de sauvegarde",
        auto_backup="Sauvegarde automatique",
        backup_interval="Intervalle de sauvegarde (secondes)",
        tool_settings="Paramètres des outils IA",
        tools_description="Activer ou désactiver des outils individuels pour cet espace mental",

        # Conversation settings
        model_info="Informations sur le modèle",
        settings_model_label="Modèle IA",
        settings_temp_label="Température",
        settings_context_label="Fenêtre de contexte",
        settings_max_output_label="Jetons de sortie maximum",
        settings_tokens_label="jetons",
        settings_reasoning_label="Capacités de raisonnement",
        settings_no_reasoning="Sans raisonnement",
        settings_hidden_reasoning="Raisonnement caché",
        settings_visible_reasoning="Raisonnement visible",

        # File dialog titles
        file_dialog_open_metaphor="Ouvrir un fichier Metaphor",
        file_dialog_open_conversation="Ouvrir une conversation",
        file_dialog_open_file="Ouvrir un fichier",
        file_dialog_save_file="Enregistrer le fichier",
        file_dialog_new_mindspace="Créer un nouvel espace mental",
        file_dialog_open_mindspace="Ouvrir un espace mental",

        # File dialog filters
        file_filter_all="Tous les fichiers (*.*)",
        file_filter_metaphor="Fichiers Metaphor (*.m6r)",
        file_filter_conversation="Fichiers de conversation (*.conv)",

        # Dialog titles
        mindspace_error_title="Erreur d'espace mental",
        conversation_error_title="Erreur de conversation",
        settings_error_title="Erreur de paramètres",
        error_opening_file_title="Erreur lors de l'ouverture du fichier",
        error_saving_file_title="Erreur lors de l'enregistrement du fichier",
        save_changes_title="Enregistrer les modifications ?",
        confirm_delete_title="Confirmer la suppression",
        file_error_title="Erreur de fichier",
        wiki_error_title="Erreur de Wiki",

        # Messages
        confirm_delete_message="Êtes-vous sûr de vouloir supprimer {0} ?",
        delete_warning_detail="Tout onglet ouvert pour ce fichier sera fermé sans enregistrement.",
        error_deleting_file="Impossible de supprimer le fichier : {0}",
        error_title_rename="Erreur de renommage",
        error_rename_exists="Une conversation nommée '{0}' existe déjà.",
        error_rename_failed="Impossible de renommer la conversation : {0}",
        unsaved_changes="Voulez-vous enregistrer les modifications de {0} ?",
        mindspace_exists_error="L'espace mental existe déjà dans le répertoire sélectionné.",
        close_button="Fermer",
        confirm_close="Confirmer la fermeture",
        could_not_open="Impossible d'ouvrir {} : {}",
        could_not_save="Impossible d'enregistrer {} : {}",
        error_creating_mindspace="Échec de la création de l'espace mental: {0}",
        error_opening_mindspace="Échec de l'ouverture de l'espace mental: {0}",
        error_restoring_mindspace="Échec de la restauration de l'état de l'espace mental: {0}",
        error_saving_mindspace="Échec de l'enregistrement de l'état de l'espace mental: {0}",
        error_creating_conversation="Échec de la création de la conversation : {}",
        error_opening_conversation="Impossible de charger {} : {}",
        error_forking_conversation="Impossible de dupliquer la conversation : {}",
        error_processing_metaphor="Échec du traitement du fichier Metaphor :\n\n{}",
        error_saving_mindspace_settings="Échec de l'enregistrement des paramètres : {}",
        error_saving_user_settings="Échec de l'enregistrement des paramètres utilisateur : {}",

        # Status bar
        editor_status="Ligne {line}, Colonne {column} | {encoding} | {line_ending} | {type}",
        conversation_status=(
            "Modèle : {model}  |  {temperature}  |  "
            "Jetons d'Entrée : {input_tokens} / {max_input_tokens}  |  "
            "Total Entrée : {total_input_tokens}  |  "
            "Jetons de Sortie : {output_tokens} / {max_output_tokens}  |  "
            "Total Sortie : {total_output_tokens}"
        ),
        conversation_status_temperature="Temp : {temperature:.1f}",
        conversation_status_no_temperature="Temp : N/D",
        terminal_status="Terminal : {name} ({columns}x{rows})",
        system_status="Statut du système",
        log_status="Journal",

        # Bookmark Actions
        bookmark_section="Marquer/Démarquer",
        next_bookmark="Signet suivant",
        previous_bookmark="Signet précédent",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="Renommer",
        delete="Supprimer",
        edit="Modifier",
        duplicate="Dupliquer",
        sort_by="Trier par",
        sort_by_name="Trier par nom",
        sort_by_creation_time="Trier par date de création",
        file_name="Nom du fichier",
        rename_error_exists="Un fichier avec ce nom existe déjà.",
        rename_error_generic="Impossible de renommer le fichier : ",
        file_creation_error_title="Erreur de création de fichier",
        file_creation_error="Impossible de créer le fichier : ",

        # File and folder creation
        new_folder="Nouveau Dossier",
        new_folder_title="Créer un Nouveau Dossier",
        folder_name="Nom du dossier",
        new_file_title="Créer un Nouveau Fichier",
        confirm_delete_item_message="Êtes-vous sûr de vouloir supprimer {0} ?",
        error_folder_not_empty="Impossible de supprimer le dossier : le dossier n'est pas vide.",
        error_creating_folder="Impossible de créer le dossier : {0}",

        # Tool tips
        tooltip_copy_contents="Copier tout le contenu",
        tooltip_save_contents="Enregistrer le contenu dans un fichier",
        tooltip_copy_message="Copier le message dans le presse-papiers",
        tooltip_save_message="Enregistrer le message au format markdown",
        tooltip_fork_message="Créer une nouvelle conversation après ce message",
        tooltip_delete_from_message="Supprimer la conversation à partir d'ici",
        tooltip_edit_file="Modifier ce fichier",
        tooltip_submit_message="Envoyer le message",
        tooltip_stop_message="Arrêter le traitement du message",
        tooltip_expand_message="Étendre le message",
        tooltip_collapse_message="Réduire le message",

        delete_from_here_title="Supprimer les messages",
        delete_from_here_message=(
            "Êtes-vous sûr de vouloir supprimer ce message et tous les messages suivants ? Cette action ne peut pas être annulée."
        ),

        # Move operation strings
        move_file_title="Déplacer le fichier",
        move_folder_title="Déplacer le dossier",
        move_file_confirmation="Êtes-vous sûr de vouloir déplacer le fichier '{0}' ?",
        move_folder_confirmation="Êtes-vous sûr de vouloir déplacer le dossier '{0}' ?",
        move_from_label="De :",
        move_to_label="Vers :",
        move_button="Déplacer",
        move_error_title="Erreur de déplacement",
        move_error_exists="Un fichier ou dossier avec ce nom existe déjà à la destination.",
        move_error_failed="Impossible de déplacer l'élément : {0}",
        move_error_protected="Impossible de déplacer les dossiers système (conversations, metaphor, .humbug).",

        # Inline editor validation errors
        error_empty_name="Le nom ne peut pas être vide",
        error_invalid_characters="Le nom contient des caractères invalides : \\ / : * ? \" < > |",
        error_validation_failed="Impossible de valider le nom",
        error_invalid_path="Chemin de fichier invalide",
        error_title="Erreur",

        # File duplication errors
        error_duplicating_file="Impossible de dupliquer le fichier : {0}",

        # Tool use
        approve_tool_call="Approuver les appels d'outils",
        reject_tool_call="Rejeter les appels d'outils"
    )
