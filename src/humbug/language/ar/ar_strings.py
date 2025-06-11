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

        # Menu names
        humbug_menu="هامبج",
        file_menu="ملف",
        edit_menu="تحرير",
        view_menu="عرض",

        # File menu items
        about_humbug="حول هامبج",
        quit_humbug="خروج من هامبج",
        new_mindspace="مساحة ذهنية جديدة",
        new_conversation="محادثة جديدة",
        new_metaphor_conversation="محادثة ميتافور جديدة...",
        new_file="ملف جديد",
        new_terminal="طرفية جديدة",
        open_mindspace="فتح مساحة ذهنية...",
        open_conversation="فتح محادثة...",
        open_file="فتح ملف...",
        open_wiki="فتح ويكي",
        fork_conversation="نسخ المحادثة",
        save="حفظ",
        save_as="حفظ باسم...",
        close_mindspace="إغلاق المساحة الذهنية",
        close_tab="إغلاق التبويب",
        user_settings="إعدادات المستخدم",

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
        display_theme="مظهر العرض",
        zoom_in="تكبير",
        zoom_out="تصغير",
        reset_zoom="إعادة تعيين التكبير",
        show_system_shell="عرض الطرفية النظامية",
        show_all_columns="عرض كل الأعمدة",
        split_column_left="تقسيم العمود لليسار",
        split_column_right="تقسيم العمود لليمين",
        merge_column_left="دمج مع العمود الأيسر",
        merge_column_right="دمج مع العمود الأيمن",
        swap_column_left="عمود المبادلة إلى اليسار",
        swap_column_right="عمود المبادلة لليمين",
        next_message="الرسالة التالية",
        previous_message="الرسالة السابقة",

        # Message roles
        role_you="أنت",
        role_assistant="رد {model}",
        role_reasoning="تفكير {model}",
        role_system="هامبج",

        # Message labels
        highlighting="تمييز: {language}",

        # Find widget
        find_placeholder="بحث",
        find_no_matches="لا توجد نتائج",
        find_match_count="{current} من {total}",

        # Input widget
        processing_message="معالجة طلبك (Esc للإلغاء)",
        input_prompt="رسالة لـ {model}... ({key} للإرسال)",
        command_prompt="الأمر... (Enter أو {key} للإرسال)",

        # Dialog and settings
        cancel="إلغاء",
        ok="موافق",
        apply="تطبيق",
        discard="تجاهل",
        yes="نعم",
        no="لا",

        # User Settings dialog - AI backends section
        general_settings="الإعدادات العامة",
        display_settings="إعدادات العرض",
        ai_backend_config="إعدادات خلفيات الذكاء الاصطناعي",
        enable_backend="تفعيل الخلفية",
        api_key="مفتاح API",
        api_url="عنوان URL للواجهة (اختياري)",
        anthropic_backend="أنثروبيك",
        deepseek_backend="ديبسيك",
        google_backend="جوجل",
        m6r_backend="M6R",
        mistral_backend="ميسترال",
        openai_backend="أوبن إي آي",
        ollama_backend="أولاما",
        xai_backend="xAI",

        # User Settings dialog - other settings
        select_language="اللغة",
        font_size="حجم الخط",
        theme_dark="داكن",
        theme_light="فاتح",

        # Mindspace tree
        mindspace_label_none="لا توجد مساحة ذهنية نشطة",

        # Mindspace folders dialog
        mindspace_folders_title="تكوين مجلدات المساحة الذهنية",
        mindspace_path="مسار المساحة الذهنية",
        conversations_folder='إنشاء مجلد "conversations"',
        metaphor_folder='إنشاء مجلد "metaphor"',
        src_folder='إنشاء مجلد "src"',

        # Mindspace settings
        model_settings="إعدادات النموذج",
        editor_settings="إعدادات المحرر",
        use_soft_tabs="استخدام المسافات البادئة",
        tab_size="حجم المسافة البادئة",
        backup_settings="إعدادات النسخ الاحتياطي",
        auto_backup="نسخ احتياطي تلقائي",
        backup_interval="فترة النسخ الاحتياطي (ثواني)",

        # Conversation settings
        model_info="معلومات النموذج",
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
        error_saving_user_settings="فشل في حفظ إعدادات المستخدم: {}",

        # Status bar
        editor_status="سطر {line}، عمود {column} | {encoding} | {line_ending} | {type}",
        conversation_status=(
            "النموذج: {model} | {temperature} | آخر رد - المدخلات: {input_tokens} "
            "({max_input_tokens}) | المخرجات: {output_tokens} ({max_output_tokens})"
        ),
        conversation_status_temperature="درجة الحرارة: {temperature:.1f}",
        conversation_status_no_temperature="درجة الحرارة: غير متوفر",
        terminal_status="طرفية: {name} ({columns}x{rows})",
        system_status="نظام",

        # Bookmark
        bookmark_section="تبديل الإشارة",
        next_bookmark="الإشارة التالية",
        previous_bookmark="الإشارة السابقة",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="إعادة التسمية",
        delete="حذف",
        edit="تحرير",
        duplicate="تكرار",
        file_name="اسم الملف",
        rename_error_exists="يوجد ملف بهذا الاسم بالفعل.",
        rename_error_generic="تعذر إعادة تسمية الملف: ",
        file_creation_error_title="خطأ في إنشاء الملف",
        file_creation_error="تعذر إنشاء الملف: ",

        # File and folder creation
        new_folder="مجلد جديد",
        new_folder_title="إنشاء مجلد جديد",
        folder_name="اسم المجلد",
        new_file_title="إنشاء ملف جديد",
        confirm_delete_item_message="هل أنت متأكد من حذف {0}؟",
        error_folder_not_empty="لا يمكن حذف المجلد: المجلد ليس فارغاً.",
        error_creating_folder="تعذر إنشاء المجلد: {0}",

        # Tool tips
        tooltip_copy_contents="نسخ جميع المحتويات",
        tooltip_save_contents="حفظ المحتويات إلى ملف",
        tooltip_copy_message="نسخ الرسالة إلى الحافظة",
        tooltip_save_message="حفظ الرسالة كملف ماركداون",
        tooltip_fork_message="تفريع المحادثة بعد هذه الرسالة",
        tooltip_delete_from_message="حذف المحادثة من هذه النقطة",
        tooltip_edit_file="تحرير هذا الملف",
        tooltip_submit_message="إرسال الرسالة",

        delete_from_here_title="حذف الرسائل",
        delete_from_here_message="هل أنت متأكد من أنك تريد حذف هذه الرسالة وجميع الرسائل اللاحقة؟ لا يمكن التراجع عن هذا الإجراء.",

        # Move operation strings
        move_file_title="نقل الملف",
        move_folder_title="نقل المجلد",
        move_file_confirmation="هل أنت متأكد من أنك تريد نقل الملف '{0}'؟",
        move_folder_confirmation="هل أنت متأكد من أنك تريد نقل المجلد '{0}'؟",
        move_from_label="من:",
        move_to_label="إلى:",
        move_button="نقل",
        move_error_title="خطأ في النقل",
        move_error_exists="يوجد ملف أو مجلد بهذا الاسم بالفعل في الوجهة.",
        move_error_failed="تعذر نقل العنصر: {0}",
        move_error_protected="لا يمكن نقل مجلدات النظام (conversations, metaphor, .humbug).",

        # Inline editor validation errors
        error_empty_name="لا يمكن أن يكون الاسم فارغاً",
        error_invalid_characters="الاسم يحتوي على رموز غير صالحة: \\ / : * ? \" < > |",
        error_validation_failed="تعذر التحقق من صحة الاسم",
        error_invalid_path="مسار الملف غير صالح",
        error_title="خطأ",

        # File duplication errors
        error_duplicating_file="تعذر تكرار الملف: {0}",
    )
