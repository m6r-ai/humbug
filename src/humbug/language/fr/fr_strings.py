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
        settings_dialog_title="Paramètres",

        # Menu names
        humbug_menu="Humbug",
        file_menu="Fichier",
        edit_menu="Édition",
        view_menu="Affichage",

        # File menu items
        new_mindspace="Nouvel espace mental",
        new_conversation="Nouvelle conversation",
        new_metaphor_conversation="Nouvelle conversation Metaphor...",
        new_file="Nouveau fichier",
        new_terminal="Nouveau terminal",
        open_mindspace="Ouvrir espace mental",
        open_conversation="Ouvrir conversation...",
        open_file="Ouvrir fichier...",
        fork_conversation="Dupliquer la conversation",
        save="Enregistrer",
        save_as="Enregistrer sous...",
        close_mindspace="Fermer l'espace mental",
        close_tab="Fermer l'onglet",

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
        dark_mode="Mode sombre",
        zoom_in="Zoom avant",
        zoom_out="Zoom arrière",
        reset_zoom="Réinitialiser le zoom",
        show_all_columns="Afficher toutes les colonnes",
        split_column_left="Diviser la colonne à gauche",
        split_column_right="Diviser la colonne à droite",
        merge_column_left="Fusionner avec la colonne de gauche",
        merge_column_right="Fusionner avec la colonne de droite",
        swap_column_left="Échanger la colonne gauche",
        swap_column_right="Échanger la colonne de droite",

        # Message roles
        role_you="Vous",
        role_assistant="Réponse de l'assistant",
        role_reasoning="Raisonnement de l'assistant",
        role_system="Message système",

        # Find widget
        find_placeholder="Rechercher",
        find_no_matches="Aucun résultat",
        find_match_count="{current} sur {total}",

        # Input widget
        processing_message="Traitement de votre requête (Échap pour annuler)",
        input_prompt="Ajoutez votre message... ({key} pour envoyer)",

        # File tree messages
        rename_conversation="Renommer la conversation",
        conversation_name="Nom de la conversation",
        delete_file="Supprimer",

        # Dialog and settings
        cancel="Annuler",
        ok="OK",
        apply="Appliquer",
        discard="Abandonner",
        yes="Oui",
        no="Non",

        # Mindspace tree
        mindspace_label_none="Aucun espace mental actif",

        # Mindspace folders dialog
        mindspace_folders_title="Configurer les dossiers de l'espace mental",
        mindspace_path="Chemin de l'espace mental",
        conversations_folder='Créer le dossier "conversations"',
        metaphor_folder='Créer le dossier "metaphor"',
        src_folder='Créer le dossier "src"',

        # Mindspace settings
        select_language="Langue",
        font_size="Taille de police",
        use_soft_tabs="Utiliser des tabulations douces",
        tab_size="Taille de tabulation",
        auto_backup="Sauvegarde automatique",
        backup_interval="Intervalle de sauvegarde (secondes)",

        # Conversation settings
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
        file_filter_text="Fichiers texte (*.txt)",

        # Dialog titles
        mindspace_error_title="Erreur d'espace mental",
        conversation_error_title="Erreur de conversation",
        metaphor_error_title="Erreur de traitement Metaphor",
        settings_error_title="Erreur de paramètres",
        error_opening_file_title="Erreur lors de l'ouverture du fichier",
        error_saving_file_title="Erreur lors de l'enregistrement du fichier",
        save_changes_title="Enregistrer les modifications ?",
        confirm_delete_title="Confirmer la suppression",
        file_error_title="Erreur de fichier",

        # Messages
        confirm_delete_message="Êtes-vous sûr de vouloir supprimer {0} ?",
        delete_warning_detail="Tout onglet ouvert pour ce fichier sera fermé sans enregistrement.",
        error_deleting_file="Impossible de supprimer le fichier : {0}",
        error_title_rename="Erreur de renommage",
        error_rename_exists="Une conversation nommée '{0}' existe déjà.",
        error_rename_failed="Impossible de renommer la conversation : {0}",
        unsaved_changes="Voulez-vous enregistrer les modifications de {0} ?",
        delete_file_warning="Êtes-vous sûr de vouloir supprimer ce fichier ?",
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

        # Status bar
        editor_status="Ligne {line}, Colonne {column} | {encoding} | {line_ending} | {type}",
        conversation_status="Modèle : {model} | {temperature} | Dernière réponse - Entrée : {input_tokens} ({max_input_tokens}) | Sortie : {output_tokens} ({max_output_tokens})",
        conversation_status_temperature="Temp : {temperature:.1f}",
        conversation_status_no_temperature="Temp : N/D",
        terminal_status="Terminal : {name} ({columns}x{rows})",

        # Bookmark Actions
        bookmark_section="Marquer/Démarquer",
        next_bookmark="Signet suivant",
        previous_bookmark="Signet précédent",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="Renommer",
        delete="Supprimer",
        new="Nouveau",
        new_folder="Dossier",
        rename_file_title="Renommer le fichier",
        rename_file_prompt="Entrez un nouveau nom",
        rename_error_title="Erreur de renommage",
        rename_error_exists="Un fichier avec ce nom existe déjà.",
        rename_error_generic="Impossible de renommer le fichier : ",
        file_creation_error_title="Erreur de création de fichier",
        file_creation_error="Impossible de créer le fichier : ",

        # Tool tips
        tooltip_copy_contents="Copier tout le contenu",
        tooltip_save_contents="Enregistrer le contenu dans un fichier"
    )
