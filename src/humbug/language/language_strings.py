from dataclasses import dataclass
from typing import Dict

from humbug.language.language_code import LanguageCode


@dataclass
class LanguageStrings:
    """Strings for UI elements in different languages."""
    # Window titles
    about_title: str
    file_dialog_title: str
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

    # Dialog and settings
    cancel: str
    ok: str
    apply: str
    select_language: str
    font_size: str
    use_soft_tabs: str
    tab_size: str
    auto_backup: str
    backup_interval: str

    # Messages
    confirm_delete: str
    delete_file_warning: str
    error_opening_file: str
    error_saving_file: str
    mindspace_exists_error: str
    close_button: str
    save_changes: str
    discard_button: str
    confirm_close: str
    unsaved_changes: str
    could_not_open: str
    could_not_save: str
    error_creating_conversation: str
    error_opening_conversation: str
    error_forking_conversation: str
    error_processing_metaphor: str
    error_saving_settings: str

    @classmethod
    def get_strings(cls, code: LanguageCode) -> 'LanguageStrings':
        """Get strings for specified language code."""
        return _LANGUAGE_MAPPINGS.get(code, _LANGUAGE_MAPPINGS[LanguageCode.EN])


# Language mapping dictionary
_LANGUAGE_MAPPINGS: Dict[LanguageCode, LanguageStrings] = {
    LanguageCode.EN: LanguageStrings(
        # Window titles
        about_title="About Humbug",
        file_dialog_title="Select File",
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

        # Dialog and settings
        cancel="Cancel",
        ok="OK",
        apply="Apply",
        select_language="Language",
        font_size="Font Size",
        use_soft_tabs="Use Soft Tabs",
        tab_size="Tab Size",
        auto_backup="Auto Backup",
        backup_interval="Backup Interval (seconds)",

        # Messages
        confirm_delete="Confirm Delete",
        delete_file_warning="Are you sure you want to delete this file?",
        error_opening_file="Error Opening File",
        error_saving_file="Error Saving File",
        mindspace_exists_error="Mindspace already exists in selected directory.",
        close_button="Close",
        save_changes="Save Changes?",
        discard_button="Discard",
        confirm_close="Confirm Close",
        unsaved_changes="Do you want to save your changes?",
        could_not_open="Could not open {}: {}",
        could_not_save="Could not save {}: {}",
        error_creating_conversation="Failed to create conversation: {}",
        error_opening_conversation="Could not load {}: {}",
        error_forking_conversation="Could not fork conversation: {}",
        error_processing_metaphor="Failed to process Metaphor file:\n\n{}",
        error_saving_settings="Failed to save mindspace settings: {}"
    ),

    LanguageCode.FR: LanguageStrings(
        # Window titles
        about_title="À propos de Humbug",
        file_dialog_title="Sélectionner un fichier",
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

        # Dialog and settings
        cancel="Annuler",
        ok="OK",
        apply="Appliquer",
        select_language="Langue",
        font_size="Taille de police",
        use_soft_tabs="Utiliser des tabulations douces",
        tab_size="Taille de tabulation",
        auto_backup="Sauvegarde automatique",
        backup_interval="Intervalle de sauvegarde (secondes)",

        # Messages
        confirm_delete="Confirmer la suppression",
        delete_file_warning="Êtes-vous sûr de vouloir supprimer ce fichier ?",
        error_opening_file="Erreur lors de l'ouverture du fichier",
        error_saving_file="Erreur lors de l'enregistrement du fichier",
        mindspace_exists_error="L'espace mental existe déjà dans le répertoire sélectionné.",
        close_button="Fermer",
        save_changes="Enregistrer les modifications ?",
        discard_button="Abandonner",
        confirm_close="Confirmer la fermeture",
        unsaved_changes="Voulez-vous enregistrer vos modifications ?",
        could_not_open="Impossible d'ouvrir {} : {}",
        could_not_save="Impossible d'enregistrer {} : {}",
        error_creating_conversation="Échec de la création de la conversation : {}",
        error_opening_conversation="Impossible de charger {} : {}",
        error_forking_conversation="Impossible de dupliquer la conversation : {}",
        error_processing_metaphor="Échec du traitement du fichier Metaphor :\n\n{}",
        error_saving_settings="Échec de l'enregistrement des paramètres : {}"
    ),

    LanguageCode.AR: LanguageStrings(
        # Window titles
        about_title="حول هامبج",
        file_dialog_title="اختر ملف",
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

        # Dialog and settings
        cancel="إلغاء",
        ok="موافق",
        apply="تطبيق",
        select_language="اللغة",
        font_size="حجم الخط",
        use_soft_tabs="استخدام المسافات البادئة",
        tab_size="حجم المسافة البادئة",
        auto_backup="نسخ احتياطي تلقائي",
        backup_interval="فترة النسخ الاحتياطي (ثواني)",

        # Messages
        confirm_delete="تأكيد الحذف",
        delete_file_warning="هل أنت متأكد من حذف هذا الملف؟",
        error_opening_file="خطأ في فتح الملف",
        error_saving_file="خطأ في حفظ الملف",
        mindspace_exists_error="المساحة الذهنية موجودة بالفعل في المجلد المحدد.",
        close_button="إغلاق",
        save_changes="حفظ التغييرات؟",
        discard_button="تجاهل",
        confirm_close="تأكيد الإغلاق",
        unsaved_changes="هل تريد حفظ التغييرات؟",
        could_not_open="لا يمكن فتح {}: {}",
        could_not_save="لا يمكن حفظ {}: {}",
        error_creating_conversation="فشل في إنشاء المحادثة: {}",
        error_opening_conversation="لا يمكن تحميل {}: {}",
        error_forking_conversation="لا يمكن نسخ المحادثة: {}",
        error_processing_metaphor="فشل في معالجة ملف ميتافور:\n\n{}",
        error_saving_settings="فشل في حفظ الإعدادات: {}"
    )
}
