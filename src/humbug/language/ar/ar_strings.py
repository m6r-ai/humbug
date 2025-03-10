"""Arabic language strings for the UI."""

from humbug.language.language_strings import LanguageStrings


def get_arabic_strings() -> LanguageStrings:
    """
    Get the Arabic language strings.

    Returns:
        LanguageStrings object with all Arabic text
    """
    return LanguageStrings(
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
        new_terminal="طرفية جديدة",
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
        swap_column_left="عمود المبادلة إلى اليسار",
        swap_column_right="عمود المبادلة لليمين",

        # Message roles
        role_you="أنت",
        role_assistant="رد المساعد",
        role_reasoning="تفكير المساعد",
        role_system="رسالة النظام",

        # Message labels
        highlighting="تمييز: {language}",

        # Find widget
        find_placeholder="بحث",
        find_no_matches="لا توجد نتائج",
        find_match_count="{current} من {total}",

        # Input widget
        processing_message="معالجة طلبك (Esc للإلغاء)",
        input_prompt="أضف رسالتك... ({key} للإرسال)",

        # File tree messages
        rename_conversation="إعادة تسمية المحادثة",
        conversation_name="اسم المحادثة",
        delete_file="حذف",

        # Dialog and settings
        cancel="إلغاء",
        ok="موافق",
        apply="تطبيق",
        discard="تجاهل",
        yes="نعم",
        no="لا",

        # Mindspace tree
        mindspace_label_none="لا توجد مساحة ذهنية نشطة",

        # Mindspace folders dialog
        mindspace_folders_title="تكوين مجلدات المساحة الذهنية",
        mindspace_path="مسار المساحة الذهنية",
        conversations_folder='إنشاء مجلد "conversations"',
        metaphor_folder='إنشاء مجلد "metaphor"',
        src_folder='إنشاء مجلد "src"',

        # Mindspace settings
        select_language="اللغة",
        font_size="حجم الخط",
        use_soft_tabs="استخدام المسافات البادئة",
        tab_size="حجم المسافة البادئة",
        auto_backup="نسخ احتياطي تلقائي",
        backup_interval="فترة النسخ الاحتياطي (ثواني)",

        # Conversation settings
        settings_model_label="نموذج الذكاء الاصطناعي",
        settings_temp_label="درجة الحرارة",
        settings_context_label="نافذة السياق",
        settings_max_output_label="الحد الأقصى للرموز الناتجة",
        settings_tokens_label="رمز",
        settings_reasoning_label="قدرات التفكير",
        settings_no_reasoning="بدون تفكير",
        settings_hidden_reasoning="تفكير مخفي",
        settings_visible_reasoning="تفكير مرئي",

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
        file_error_title="خطأ في الملف",

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
        conversation_status="النموذج: {model} | {temperature} | آخر رد - المدخلات: {input_tokens} ({max_input_tokens}) | المخرجات: {output_tokens} ({max_output_tokens})",
        conversation_status_temperature="درجة الحرارة: {temperature:.1f}",
        conversation_status_no_temperature="درجة الحرارة: غير متوفر",
        terminal_status="طرفية: {name} ({columns}x{rows})",

        # Bookmark
        bookmark_section="تبديل الإشارة",
        next_bookmark="الإشارة التالية",
        previous_bookmark="الإشارة السابقة",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="إعادة التسمية",
        delete="حذف",
        new="جديد",
        new_folder="مجلد",
        rename_file_title="إعادة تسمية الملف",
        rename_file_prompt="أدخل اسمًا جديدًا",
        rename_error_title="خطأ في إعادة التسمية",
        rename_error_exists="يوجد ملف بهذا الاسم بالفعل.",
        rename_error_generic="تعذر إعادة تسمية الملف: ",
        file_creation_error_title="خطأ في إنشاء الملف",
        file_creation_error="تعذر إنشاء الملف: ",

        # Tool tips
        tooltip_copy_contents="نسخ جميع المحتويات",
        tooltip_save_contents="حفظ المحتويات إلى ملف"
    )
