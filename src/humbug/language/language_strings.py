from dataclasses import dataclass
from typing import Dict

from humbug.language.language_code import LanguageCode


@dataclass
class LanguageStrings:
    """Strings for UI elements in different languages."""
    # Window titles
    about_title: str
    settings_dialog_title: str

    # Menu names
    humbug_menu: str
    file_menu: str
    edit_menu: str
    view_menu: str

    # File menu items
    new_mindspace: str
    new_conversation: str
    new_metaphor_conversation: str
    new_file: str
    open_mindspace: str
    open_conversation: str
    open_file: str
    fork_conversation: str
    save: str
    save_as: str
    close_mindspace: str
    close_tab: str

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
    dark_mode: str
    zoom_in: str
    zoom_out: str
    reset_zoom: str
    show_all_columns: str
    split_column_left: str
    split_column_right: str
    merge_column_left: str
    merge_column_right: str

    # Message roles and labels
    role_you: str
    role_assistant: str
    role_system: str

    # Find widget
    find_placeholder: str
    find_no_matches: str
    find_match_count: str  # Format: "{current} of {total}"

    # Input widget
    processing_message: str
    input_prompt: str  # Format: "Add your next message... ({key} to submit)"

    # File tree messages
    rename_conversation: str
    conversation_name: str
    delete_file: str

    # Dialog and settings
    cancel: str
    ok: str
    apply: str
    discard: str
    yes: str
    no: str

    # Mindspace settings
    select_language: str
    font_size: str
    use_soft_tabs: str
    tab_size: str
    auto_backup: str
    backup_interval: str

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
    file_filter_text: str

    # Dialog titles
    mindspace_error_title: str
    conversation_error_title: str
    metaphor_error_title: str
    settings_error_title: str
    error_opening_file_title: str
    error_saving_file_title: str
    save_changes_title: str
    confirm_delete_title: str
    file_error_title: str

    # Messages
    confirm_delete_message: str
    delete_warning_detail: str
    error_deleting_file: str
    error_title_rename: str
    error_rename_exists: str
    error_rename_failed: str
    unsaved_changes: str
    delete_file_warning: str
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

    # Status bar
    editor_status: str
    conversation_status: str
    conversation_status_temperature: str
    conversation_status_no_temperature: str

    @classmethod
    def get_strings(cls, code: LanguageCode) -> 'LanguageStrings':
        """Get strings for specified language code."""
        return _LANGUAGE_MAPPINGS.get(code, _LANGUAGE_MAPPINGS[LanguageCode.EN])


# Language mapping dictionary
_LANGUAGE_MAPPINGS: Dict[LanguageCode, LanguageStrings] = {
    LanguageCode.EN: LanguageStrings(
        # Window titles
        about_title="About Humbug",
        settings_dialog_title="Settings",

        # Menu names
        humbug_menu="Humbug",
        file_menu="File",
        edit_menu="Edit",
        view_menu="View",

        # File menu items
        new_mindspace="New Mindspace",
        new_conversation="New Conversation",
        new_metaphor_conversation="New Metaphor Conversation...",
        new_file="New File",
        open_mindspace="Open Mindspace",
        open_conversation="Open Conversation...",
        open_file="Open File...",
        fork_conversation="Fork Conversation",
        save="Save",
        save_as="Save As...",
        close_mindspace="Close Mindspace",
        close_tab="Close Tab",

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
        dark_mode="Dark Mode",
        zoom_in="Zoom In",
        zoom_out="Zoom Out",
        reset_zoom="Reset Zoom",
        show_all_columns="Show All Columns",
        split_column_left="Split Column Left",
        split_column_right="Split Column Right",
        merge_column_left="Merge Column Left",
        merge_column_right="Merge Column Right",

        # Message roles
        role_you="You",
        role_assistant="Assistant",
        role_system="System Message",

        # Find widget
        find_placeholder="Find",
        find_no_matches="No matches",
        find_match_count="{current} of {total}",

        # Input widget
        processing_message="Processing your request (Esc to cancel)",
        input_prompt="Add your next message... ({key} to submit)",

        # File tree messages
        rename_conversation="Rename Conversation",
        conversation_name="Conversation Name:",
        delete_file="Delete",

        # Dialog and settings
        cancel="Cancel",
        ok="OK",
        apply="Apply",
        discard="Discard",
        yes="Yes",
        no="No",

        # Mindspace settings
        select_language="Language",
        font_size="Font Size",
        use_soft_tabs="Use Soft Tabs",
        tab_size="Tab Size",
        auto_backup="Auto Backup",
        backup_interval="Backup Interval (seconds)",

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
        file_error_title = "File Error",

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

        # Status bar
        editor_status="Line {line}, Column {column} | {encoding} | {line_ending} | {type}",
        conversation_status="Model: {model} | {temperature} | Last response - Input: {input_tokens} ({max_tokens}) | Output: {output_tokens}",
        conversation_status_temperature="Temp: {temperature:.1f}",
        conversation_status_no_temperature="Temp: N/A"
    ),

    LanguageCode.FR: LanguageStrings(
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

        # Message roles
        role_you="Vous",
        role_assistant="Assistant",
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
        conversation_name="Nom de la conversation :",
        delete_file="Supprimer",

        # Dialog and settings
        cancel="Annuler",
        ok="OK",
        apply="Appliquer",
        discard="Abandonner",
        yes="Oui",
        no="Non",

        # Mindspace settings
        select_language="Langue",
        font_size="Taille de police",
        use_soft_tabs="Utiliser des tabulations douces",
        tab_size="Taille de tabulation",
        auto_backup="Sauvegarde automatique",
        backup_interval="Intervalle de sauvegarde (secondes)",

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
        file_error_title = "Erreur de fichier",

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
        conversation_status="Modèle : {model} | {temperature} | Dernière réponse - Entrée : {input_tokens} ({max_tokens}) | Sortie : {output_tokens}",
        conversation_status_temperature="Temp : {temperature:.1f}",
        conversation_status_no_temperature="Temp : N/D"
    ),

    LanguageCode.AR: LanguageStrings(
        # Window titles
        about_title="حول هامبج",
        settings_dialog_title="الإعدادات",

        # Menu names
        humbug_menu="هامبج",
        file_menu="ملف",
        edit_menu="تحرير",
        view_menu="عرض",

        # File menu items
        new_mindspace="مساحة ذهنية جديدة",
        new_conversation="محادثة جديدة",
        new_metaphor_conversation="محادثة ميتافور جديدة...",
        new_file="ملف جديد",
        open_mindspace="فتح مساحة ذهنية",
        open_conversation="فتح محادثة...",
        open_file="فتح ملف...",
        fork_conversation="نسخ المحادثة",
        save="حفظ",
        save_as="حفظ باسم...",
        close_mindspace="إغلاق المساحة الذهنية",
        close_tab="إغلاق التبويب",

        # Edit menu items
        submit_message="إرسال الرسالة",
        undo="تراجع",
        redo="إعادة",
        cut="قص",
        copy="نسخ",
        paste="لصق",
        find="بحث",
        mindspace_settings="إعدادات المساحة الذهنية",
        conversation_settings="إعدادات المحادثة",

        # View menu items
        dark_mode="الوضع الداكن",
        zoom_in="تكبير",
        zoom_out="تصغير",
        reset_zoom="إعادة تعيين التكبير",
        show_all_columns="عرض كل الأعمدة",
        split_column_left="تقسيم العمود لليسار",
        split_column_right="تقسيم العمود لليمين",
        merge_column_left="دمج مع العمود الأيسر",
        merge_column_right="دمج مع العمود الأيمن",

        # Message roles
        role_you="أنت",
        role_assistant="المساعد",
        role_system="رسالة النظام",

        # Find widget
        find_placeholder="بحث",
        find_no_matches="لا توجد نتائج",
        find_match_count="{current} من {total}",

        # Input widget
        processing_message="معالجة طلبك (Esc للإلغاء)",
        input_prompt="أضف رسالتك... ({key} للإرسال)",

        # File tree messages
        rename_conversation="إعادة تسمية المحادثة",
        conversation_name="اسم المحادثة:",
        delete_file="حذف",

        # Dialog and settings
        cancel="إلغاء",
        ok="موافق",
        apply="تطبيق",
        discard="تجاهل",
        yes="نعم",
        no="لا",

        # Mindspace settings
        select_language="اللغة",
        font_size="حجم الخط",
        use_soft_tabs="استخدام المسافات البادئة",
        tab_size="حجم المسافة البادئة",
        auto_backup="نسخ احتياطي تلقائي",
        backup_interval="فترة النسخ الاحتياطي (ثواني)",

        # File dialog titles
        file_dialog_open_metaphor="فتح ملف ميتافور",
        file_dialog_open_conversation="فتح محادثة",
        file_dialog_open_file="فتح ملف",
        file_dialog_save_file="حفظ الملف",
        file_dialog_new_mindspace="إنشاء مساحة ذهنية جديدة",
        file_dialog_open_mindspace="فتح مساحة ذهنية",

        # File dialog filters
        file_filter_all="كل الملفات (*.*)",
        file_filter_metaphor="ملفات ميتافور (*.m6r)",
        file_filter_conversation="ملفات المحادثة (*.conv)",
        file_filter_text="الملفات النصية (*.txt)",

        # Dialog titles
        mindspace_error_title="خطأ في المساحة الذهنية",
        conversation_error_title="خطأ في المحادثة",
        metaphor_error_title="خطأ في معالجة ميتافور",
        settings_error_title="خطأ في الإعدادات",
        error_opening_file_title="خطأ في فتح الملف",
        error_saving_file_title="خطأ في حفظ الملف",
        save_changes_title="حفظ التغييرات؟",
        confirm_delete_title="تأكيد الحذف",
        file_error_title = "خطأ في الملف",

        # Messages
        confirm_delete_message="هل أنت متأكد من حذف {0}؟",
        delete_warning_detail="سيتم إغلاق أي تبويب مفتوح لهذا الملف دون حفظ.",
        error_deleting_file="تعذر حذف الملف: {0}",
        error_title_rename="خطأ في إعادة التسمية",
        error_rename_exists="توجد محادثة باسم '{0}' بالفعل.",
        error_rename_failed="تعذر إعادة تسمية المحادثة: {0}",
        unsaved_changes="هل تريد حفظ التغييرات في {0}؟",
        delete_file_warning="هل أنت متأكد من حذف هذا الملف؟",
        mindspace_exists_error="المساحة الذهنية موجودة بالفعل في المجلد المحدد.",
        close_button="إغلاق",
        confirm_close="تأكيد الإغلاق",
        could_not_open="لا يمكن فتح {}: {}",
        could_not_save="لا يمكن حفظ {}: {}",
        error_creating_mindspace="فشل في إنشاء المساحة الذهنية: {0}",
        error_opening_mindspace="فشل في فتح المساحة الذهنية: {0}",
        error_restoring_mindspace="فشل في استعادة حالة المساحة الذهنية: {0}",
        error_saving_mindspace="فشل في حفظ حالة المساحة الذهنية: {0}",
        error_creating_conversation="فشل في إنشاء المحادثة: {}",
        error_opening_conversation="لا يمكن تحميل {}: {}",
        error_forking_conversation="لا يمكن نسخ المحادثة: {}",
        error_processing_metaphor="فشل في معالجة ملف ميتافور:\n\n{}",
        error_saving_mindspace_settings="فشل في حفظ الإعدادات: {}",

        # Status bar
        editor_status="سطر {line}، عمود {column} | {encoding} | {line_ending} | {type}",
        conversation_status="النموذج: {model} | {temperature} | آخر رد - المدخلات: {input_tokens} ({max_tokens}) | المخرجات: {output_tokens}",
        conversation_status_temperature="درجة الحرارة: {temperature:.1f}",
        conversation_status_no_temperature="درجة الحرارة: غير متوفر"
    )
}
