"""French language strings for the UI."""

from desktop.language.language_strings import LanguageStrings


def get_french_strings() -> LanguageStrings:
    """
    Get the French language strings.

    Returns:
        LanguageStrings object with all French text
    """
    return LanguageStrings(
        # Window titles
        about_title="À propos de Humbug",

        # Welcome widget
        welcome_message=(
            "Bienvenue dans Humbug ! Il semble que vous n'ayez pas encore configuré d'IA ?\n\n"
            "Pour commencer à utiliser les fonctionnalités IA, veuillez ouvrir les « Paramètres » de l'application,\n"
            "faites défiler jusqu'à la section « Configuration des backends IA » et configurez au moins un backend IA."
        ),
        welcome_button="Ouvrir les Paramètres",

        # Menu names
        humbug_menu="Humbug",
        file_menu="Fichier",
        mindspace_menu="Espace mental",
        edit_menu="Édition",
        view_menu="Affichage",

        # File menu items
        about_humbug="À propos de Humbug",
        quit_humbug="Quitter Humbug",
        new_mindspace="Nouvel espace mental",
        new_conversation="Nouvelle conversation",
        new_file="Nouveau fichier",
        new_terminal="Nouveau terminal",
        open_mindspace="Ouvrir espace mental...",
        open_conversation="Ouvrir conversation...",
        open_file="Ouvrir fichier...",
        open_preview="Ouvrir aperçu...",
        open_diff="Ouvrir diff...",
        save="Enregistrer",
        save_as="Enregistrer sous...",
        close_mindspace="Fermer l'espace mental",
        close_tab="Fermer l'onglet",
        settings="Paramètres",

        # Tab bar context menu items
        close_tabs_to_left="Fermer les onglets à gauche",
        close_tabs_to_right="Fermer les onglets à droite",
        close_other_tabs="Fermer les autres onglets",

        # Edit menu items
        submit_message="Envoyer le message",
        undo="Annuler",
        redo="Rétablir",
        cut="Couper",
        copy="Copier",
        paste="Coller",
        select_all="Tout sélectionner",
        find="Rechercher",
        find_replace="Rechercher et remplacer",
        goto_line="Aller à la ligne",
        mindspace_search="Recherche globale",
        mindspace_settings="Paramètres de l'espace mental",
        conversation_settings="Paramètres de conversation",

        # View menu items
        display_theme="Thème d'affichage",
        zoom_in="Zoom avant",
        zoom_out="Zoom arrière",
        reset_zoom="Réinitialiser le zoom",
        open_mindspace_log="Journal de l'espace mental",
        open_humbug_shell="Shell Humbug",
        open_token_usage="Utilisation des tokens",
        show_tab_overview="Afficher les onglets ouverts",
        show_tab_carousel="Afficher le carrousel d'onglets",
        show_all_columns="Afficher toutes les colonnes",
        split_column_left="Diviser la colonne à gauche",
        split_column_right="Diviser la colonne à droite",
        merge_column_left="Fusionner avec la colonne de gauche",
        merge_column_right="Fusionner avec la colonne de droite",
        swap_column_left="Échanger la colonne gauche",
        swap_column_right="Échanger la colonne de droite",
        next_message="Message Suivant",
        previous_message="Message Précédent",
        next_hunk="Modification Suivante",
        previous_hunk="Modification Précédente",

        # Message roles
        role_you="Vous",
        role_you_queued="Vous (en attente)",
        role_connected="{model} connecté",
        role_assistant="Réponse de {model}",
        role_reasoning="Raisonnement de {model}",
        role_system="Humbug",
        role_tool_call="Appel d'outil",
        role_tool_result="Résultat de l'outil",

        # Message labels
        highlighting="Mise en évidence : {syntax}",

        # Find widget
        find_placeholder="Rechercher",
        find_no_matches="Aucun résultat",
        find_match_count="{current} sur {total}",
        find_match_case="Respecter la casse",
        find_use_whole_word="Mot entier",
        find_use_regexp="Utiliser une expression régulière",
        find_invalid_regexp="Expression invalide",
        replace_placeholder="Remplacer",
        replace_button="Remplacer",
        replace_all_button="Remplacer tout",
        replace_count="{count} remplacé(s)",

        # Go to line dialog
        goto_line_title="Aller à la ligne",
        goto_line_label="Numéro de ligne :",

        # Input widget
        processing_message="Discuter avec {model} (Échap pour annuler, {key} pour envoyer)",
        input_prompt="Discuter avec {model} ({key} pour envoyer)",
        command_prompt="Commande (Entrée ou {key} pour envoyer)",

        # AI thinking message
        ai_thinking="L'IA réfléchit...",

        # Dialog and settings
        cancel="Annuler",
        ok="OK",
        apply="Appliquer",
        discard="Abandonner",
        yes="Oui",
        no="Non",

        # User Settings dialog - AI backends section
        display_settings="Paramètres d'affichage",
        ai_backend_config="Configuration des backends d'IA",
        enable_backend="Activer le backend",
        api_key="Clé API",
        api_url="URL API (optionnel)",
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
        select_language="Langue",
        font_size="Taille de police",
        theme_system="Automatique",
        font_ligatures="Activer les ligatures de police",
        theme_dark="Sombre",
        theme_light="Clair",
        theme_color_blind="Daltonien",
        theme_ocean_light="Océan Clair",
        theme_custom="Thème personnalisé",
        customize_colors="Personnaliser les couleurs…",
        file_sort_order="Ordre de tri des fichiers",
        sort_directories_first="Dossiers en premier",
        sort_alphabetical="Alphabétique",

        # User Settings dialog - external file access
        external_file_access="Accès aux fichiers",
        allow_external_file_access="Autoriser l'IA à lire des fichiers en dehors du mindspace",
        external_file_allowlist="Chemins autorisés (motifs glob, un par ligne) :",
        external_file_denylist="Chemins toujours refusés (motifs glob, un par ligne) :",

        # Mindspace tree
        mindspace_label_none="Aucun espace mental actif",
        mindspace_name_tooltip="Cliquer pour ouvrir un autre espace mental",
        mindspace_files="Fichiers",
        mindspace_vcs="Fichiers modifiés",
        mindspace_vcs_no_repo="Aucun dépôt trouvé",
        mindspace_conversations="Conversations",
        mindspace_preview="Aperçu",
        mindspace_expand_sidebar="Développer la barre latérale",
        mindspace_collapse_sidebar="Réduire la barre latérale",
        mindspace_search_placeholder="Rechercher",
        mindspace_search_options="Options de recherche",
        mindspace_search_whole_word="Mot entier",
        mindspace_search_empty_state="Prêt à rechercher",
        mindspace_search_no_results="Aucun résultat",
        mindspace_search_path_match="Correspondance dans le chemin",
        mindspace_search_results="{0} correspondances dans {1} fichiers",
        mindspace_search_results_limited="Résultats limités à {0} correspondances",
        mindspace_search_include_hidden="Inclure les dossiers cachés",

        # Mindspace folders dialog
        mindspace_folders_title="Configurer les dossiers de l'espace mental",
        mindspace_path="Chemin de l'espace mental",
        conversations_folder='Créer le dossier "conversations"',
        src_folder='Créer le dossier "src"',

        # Mindspace settings
        model_settings="Paramètres du modèle",
        editor_settings="Paramètres de l'éditeur",
        use_soft_tabs="Utiliser des tabulations douces",
        tab_size="Taille de tabulation",
        backup_settings="Paramètres de sauvegarde",
        auto_backup="Sauvegarde automatique",
        backup_interval="Intervalle de sauvegarde (secondes)",
        terminal_settings="Paramètres du terminal",
        terminal_settings_description=(
            "Configurer le comportement du tampon de défilement du terminal. Le tampon de défilement "
            "stocke l'historique de sortie du terminal que vous pouvez faire défiler."
        ),
        terminal_fixed_width_enabled="Activer la largeur fixe",
        terminal_fixed_width="Largeur fixe (colonnes)",
        terminal_scrollback_enabled="Limiter le tampon de défilement",
        terminal_scrollback_lines="Lignes de défilement",
        terminal_close_on_exit="Fermer le terminal à la sortie du shell",
        tool_settings="Paramètres des outils IA",
        tools_description="Activer ou désactiver des outils individuels pour cet espace mental",

        # Unified settings dialog
        settings_all_mindspaces="Tous les espaces mentaux",
        settings_this_mindspace="Cet espace mental",
        settings_display="Affichage",
        settings_file_access="Accès aux fichiers",
        settings_ai_backends="Backends IA",
        settings_ai_model="Modèle IA",
        settings_ai_tools="Outils IA",
        settings_editor="Éditeur",
        settings_terminal="Terminal",
        settings_tabs="Tabulations",

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
        settings_reasoning_effort_label="Effort de raisonnement",
        settings_effort_none="Aucun",
        settings_effort_minimal="Minimal",
        settings_effort_low="Faible",
        settings_effort_medium="Moyen",
        settings_effort_high="Élevé",
        settings_effort_xhigh="Très élevé",
        settings_effort_max="Maximum",

        # File dialog titles
        file_dialog_open_conversation="Ouvrir une conversation",
        file_dialog_open_file="Ouvrir un fichier",
        file_dialog_save_file="Enregistrer le fichier",
        file_dialog_new_mindspace="Créer un nouvel espace mental",
        file_dialog_open_mindspace="Ouvrir un espace mental",
        file_dialog_attach_file="Joindre un fichier",

        # File dialog filters
        file_filter_all="Tous les fichiers (*.*)",
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
        preview_error_title="Erreur d'aperçu",
        cancel_conversation_title="Annuler la conversation ?",

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
        error_opening_preview="Impossible d'ouvrir l'aperçu : {}",
        error_saving_mindspace_settings="Échec de l'enregistrement des paramètres : {}",
        error_saving_user_settings="Échec de l'enregistrement des paramètres utilisateur : {}",
        cancel_conversation="Êtes-vous sûr de vouloir arrêter la réponse actuelle de l'IA ? "
            "Cela mettra fin à la conversation en cours et peut entraîner des réponses incomplètes.",

        # Status bar
        editor_status="Ligne {line}, Colonne {column} | {encoding} | {line_ending} | {type}",
        conversation_status=(
            "Modèle : {model}  |  Fournisseur : {provider}  |  {reasoning}  |  {temperature}  |  "
            "Jetons d'Entrée : {input_tokens} / {max_input_tokens}  |  "
            "Total Entrée : {total_input_tokens}  |  "
            "Jetons de Sortie : {output_tokens} / {max_output_tokens}  |  "
            "Total Sortie : {total_output_tokens}"
        ),
        conversation_status_temperature="Temp : {temperature:.1f}",
        conversation_status_no_temperature="Temp : N/D",
        conversation_status_reasoning="Raisonnement : {effort}",
        conversation_status_no_reasoning="Raisonnement : N/D",
        conversation_status_rate_limited="Limite de requêtes atteinte — nouvelle tentative dans {delay}s…",
        terminal_status="Terminal : {name} ({columns}x{rows})",
        system_status="Statut du système",
        log_status="Journal",
        preview_status="Aperçu : {path}",
        diff_status="Diff : {path} | {rows} lignes modifiées",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="Renommer",
        delete="Supprimer",
        open_in_editor="Ouvrir dans l'éditeur",
        open_in_preview="Ouvrir dans l'aperçu",
        open_in_diff="Ouvrir dans le diff",
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
        tooltip_fork_message="Dupliquer la conversation à partir de ce message",
        tooltip_delete_from_message="Supprimer la conversation à partir d'ici",
        tooltip_edit_message="Modifier ce message",
        tooltip_edit_file="Modifier ce fichier",
        tooltip_submit_message="Envoyer le message",
        tooltip_stop_message="Arrêter le traitement du message",
        tooltip_settings_message="Ouvrir les paramètres de la conversation",
        tooltip_attach_file="Joindre un fichier",
        tooltip_expand_message="Étendre le message",
        tooltip_collapse_message="Réduire le message",
        warning_file_too_large=(
            "'{filename}' fait {size_kb}Ko, ce qui dépasse la limite recommandée de 100Ko. "
            "Les fichiers volumineux peuvent utiliser beaucoup de fenêtre de contexte. Joindre quand même ?"
        ),
        tooltip_show_attachments="Afficher les pièces jointes",
        tooltip_hide_attachments="Masquer les pièces jointes",
        error_pdf_unsupported=(
            "Impossible de joindre '{filename}'. {reason}"
        ),
        error_pdf_extraction_failed=(
            "Impossible de joindre '{filename}' : l'extraction du texte a échoué. {reason}"
        ),
        error_docx_unsupported=(
            "Impossible de joindre '{filename}'. {reason}"
        ),
        error_docx_extraction_failed=(
            "Impossible de joindre '{filename}' : l'extraction du texte a échoué. {reason}"
        ),

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
        move_error_protected="Impossible de déplacer les dossiers système (conversations, .humbug).",

        # Inline editor validation errors
        error_empty_name="Le nom ne peut pas être vide",
        error_invalid_characters="Le nom contient des caractères invalides : \\ / : * ? \" < > |",
        error_validation_failed="Impossible de valider le nom",
        error_invalid_path="Chemin de fichier invalide",
        error_title="Erreur",

        # File duplication errors
        error_duplicating_file="Impossible de dupliquer le fichier : {0}",

        # File extension change confirmation
        rename_change_extension_title="Changer le type de fichier ?",
        rename_change_extension_message="Vous modifiez l'extension de fichier de '{0}' en '{1}'."
            " Cela peut changer la façon dont le fichier est traité. Êtes-vous sûr ?",

        # Tool use
        approve_tool_call="Approuver",
        i_am_unsure_about_tool_call="Je ne suis pas sûr à propos",
        reject_tool_call="Rejeter",
        retry_error="Réessayer",

        # Update checker
        check_for_updates="Vérifier les mises à jour...",
        update_current_version="Version actuelle : {0}",
        update_checking="vérification...",
        update_up_to_date="Vous êtes à jour.",
        update_available_message="La version {0} est disponible — cliquez pour télécharger.",
        update_check_failed="Impossible de vérifier les mises à jour. Vérifiez votre connexion réseau.",
        update_tooltip="Humbug {0} disponible — cliquez pour la page de version",
        tab_overview_tooltip="Afficher tous les onglets ouverts",
        tab_carousel_tooltip="Afficher le carrousel d'onglets",
        check_for_updates_setting="Vérifier automatiquement les mises à jour",

        # Ollama pull
        ollama_pull_label="Télécharger un modèle",
        ollama_pull_placeholder="Nom du modèle (ex. llama3.2)",
        ollama_pull_button="Télécharger",
        ollama_pull_pulling="Téléchargement de {0}…",
        ollama_pull_success="« {0} » installé avec succès.",
        ollama_pull_model_not_found="Modèle introuvable — vérifiez le nom sur ollama.com/library.",
        ollama_pull_not_running="Impossible de se connecter à Ollama — est-il en cours d'exécution ?",
        ollama_pull_error="Échec du téléchargement : {0}",
        ollama_update_local_models="Mettre à jour les modèles locaux",

        # Fetch-models errors
        fetch_error_invalid_key="Clé API invalide — veuillez vérifier votre clé et réessayer.",
        fetch_error_access_denied="Accès refusé ({0}) — votre clé n'a peut-être pas la permission de lister les modèles.",
        fetch_error_not_found="Point de terminaison des modèles introuvable ({0}) — vérifiez l'URL de l'API.",
        fetch_error_rate_limited="Limite de requêtes atteinte ({0}) — patientez un moment puis réessayez.",
        fetch_error_server_error="Erreur serveur du fournisseur ({0}) — réessayez plus tard.",
        fetch_error_connection="Impossible de se connecter — vérifiez l'URL et votre réseau.",
        fetch_error_timeout="Délai d'attente dépassé — le fournisseur a mis trop de temps à répondre.",
        fetch_error_generic="Échec : {0}"
    )
