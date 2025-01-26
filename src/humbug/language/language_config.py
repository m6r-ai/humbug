from dataclasses import dataclass
from enum import Enum, auto
from typing import Dict


class LanguageCode(Enum):
    """Supported language codes."""
    EN = auto()  # English
    FR = auto()  # French
    AR = auto()  # Arabic


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

    # Dialog and settings
    cancel: str
    ok: str
    apply: str
    select_language: str
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

        # Dialog and settings
        cancel="Cancel",
        ok="OK",
        apply="Apply",
        select_language="Language",
        use_soft_tabs="Use Soft Tabs",
        tab_size="Tab Size",
        auto_backup="Auto Backup",
        backup_interval="Backup Interval (seconds)",

        # Messages
        confirm_delete="Confirm Delete",
        delete_file_warning="Are you sure you want to delete this file?",
        error_opening_file="Error Opening File",
        error_saving_file="Error Saving File",
        mindspace_exists_error="Mindspace already exists in selected directory."
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

        # Dialog and settings
        cancel="Annuler",
        ok="OK",
        apply="Appliquer",
        select_language="Langue",
        use_soft_tabs="Utiliser des tabulations douces",
        tab_size="Taille de tabulation",
        auto_backup="Sauvegarde automatique",
        backup_interval="Intervalle de sauvegarde (secondes)",

        # Messages
        confirm_delete="Confirmer la suppression",
        delete_file_warning="Êtes-vous sûr de vouloir supprimer ce fichier ?",
        error_opening_file="Erreur lors de l'ouverture du fichier",
        error_saving_file="Erreur lors de l'enregistrement du fichier",
        mindspace_exists_error="L'espace mental existe déjà dans le répertoire sélectionné."
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

        # Dialog and settings
        cancel="إلغاء",
        ok="موافق",
        apply="تطبيق",
        select_language="اللغة",
        use_soft_tabs="استخدام المسافات البادئة",
        tab_size="حجم المسافة البادئة",
        auto_backup="نسخ احتياطي تلقائي",
        backup_interval="فترة النسخ الاحتياطي (ثواني)",

        # Messages
        confirm_delete="تأكيد الحذف",
        delete_file_warning="هل أنت متأكد من حذف هذا الملف؟",
        error_opening_file="خطأ في فتح الملف",
        error_saving_file="خطأ في حفظ الملف",
        mindspace_exists_error="المساحة الذهنية موجودة بالفعل في المجلد المحدد."
    )
}
