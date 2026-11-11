"""Arabic language strings for the UI."""

from desktop.language.language_strings import LanguageStrings


def get_arabic_strings() -> LanguageStrings:
    """
    Get the Arabic language strings.

    Returns:
        LanguageStrings object with all Arabic text
    """
    return LanguageStrings(
        # Window titles
        about_title="حول هامبج",

        # Welcome widget
        welcome_message=(
            "مرحباً بك في هامبج! يبدو أنه لم يتم تكوين أي ذكاء اصطناعي بعد؟\n\n"
            "لبدء استخدام ميزات الذكاء الاصطناعي، يرجى فتح \"الإعدادات\" "
            "للتطبيق،\nوالتمرير إلى قسم \"تكوين الواجهة الخلفية للذكاء الاصطناعي\" "
            "وإعداد واجهة خلفية واحدة على الأقل للذكاء الاصطناعي."
        ),
        welcome_button="فتح الإعدادات",

        # Menu names
        humbug_menu="هامبج",
        file_menu="ملف",
        mindspace_menu="المساحة الذهنية",
        edit_menu="تحرير",
        view_menu="عرض",

        # File menu items
        about_humbug="حول هامبج",
        quit_humbug="خروج من هامبج",
        new_mindspace="مساحة ذهنية جديدة",
        new_conversation="محادثة جديدة",
        new_file="ملف جديد",
        new_terminal="طرفية جديدة",
        open_mindspace="فتح مساحة ذهنية...",
        open_conversation="فتح محادثة...",
        open_file="فتح ملف...",
        open_preview="فتح معاينة...",
        open_diff="فتح فروق...",
        save="حفظ",
        save_as="حفظ باسم...",
        close_mindspace="إغلاق المساحة الذهنية",
        close_tab="إغلاق التبويب",
        settings="الإعدادات",
        recent_mindspaces="فتح مساحة ذهنية حديثة",
        take_tour="أخذ جولة",

        # Tab bar context menu items
        close_tabs_to_left="إغلاق التبويبات إلى اليسار",
        close_tabs_to_right="إغلاق التبويبات إلى اليمين",
        close_other_tabs="إغلاق التبويبات الأخرى",
        close_column="إغلاق العمود",
        move_tab_to_left_column="نقل علامة التبويب إلى العمود الأيسر",
        move_tab_to_right_column="نقل علامة التبويب إلى العمود الأيمن",
        move_tab_to_new_column="نقل علامة التبويب إلى عمود جديد",
        move_tab_left="نقل علامة التبويب لليسار",
        move_tab_right="نقل علامة التبويب لليمين",
        move_tab_to_start="نقل علامة التبويب إلى البداية",
        move_tab_to_end="نقل علامة التبويب إلى النهاية",

        # Edit menu items
        submit_message="إرسال الرسالة",
        undo="تراجع",
        redo="إعادة",
        cut="قص",
        copy="نسخ",
        paste="لصق",
        select_all="تحديد الكل",
        find="بحث",
        find_replace="بحث واستبدال",
        goto_line="الانتقال إلى السطر",
        mindspace_search="بحث شامل",
        quick_switcher="التبديل السريع",
        quick_switcher_placeholder="ابحث في الملفات والمحادثات والعلامات المفتوحة...",
        quick_switcher_prefix_hint="> الأوامر   @ المحادثات   :السطر   ملف:سطر",
        quick_switcher_goto_line="الانتقال إلى السطر {0}",
        quick_switcher_limited="يقتصر على أول {0} ملف",
        mindspace_settings="إعدادات المساحة الذهنية",
        conversation_settings="إعدادات المحادثة",

        # View menu items
        display_theme="مظهر العرض",
        zoom_in="تكبير",
        zoom_out="تصغير",
        reset_zoom="إعادة تعيين التكبير",
        open_mindspace_log="سجل المساحة الذهنية",
        open_humbug_shell="صدفة هامبغ",
        open_token_usage="استخدام الرموز",
        show_tab_overview="عرض التبويبات المفتوحة",
        show_tab_carousel="عرض دوّار التبويبات",
        show_all_columns="عرض كل الأعمدة",
        split_column_left="تقسيم العمود لليسار",
        split_column_right="تقسيم العمود لليمين",
        merge_column_left="دمج مع العمود الأيسر",
        merge_column_right="دمج مع العمود الأيمن",
        swap_column_left="عمود المبادلة إلى اليسار",
        swap_column_right="عمود المبادلة لليمين",
        next_message="الرسالة التالية",
        previous_message="الرسالة السابقة",
        next_hunk="التعديل التالي",
        previous_hunk="التعديل السابق",
        prompt_markers="علامات المحث",

        # Message roles
        role_you="أنت",
        role_you_queued="أنت (قيد الانتظار)",
        role_connected="متصل {model}",
        role_assistant="رد {model}",
        role_reasoning="تفكير {model}",
        role_system="هامبج",
        role_tool_call="استدعاء الأداة",
        role_tool_result="نتيجة الأداة",

        # Message labels
        highlighting="تمييز: {syntax}",

        # Find widget
        find_placeholder="بحث",
        find_no_matches="لا توجد نتائج",
        find_match_count="{current} من {total}",
        find_match_case="مطابقة الحالة",
        find_use_whole_word="مطابقة الكلمة كاملة",
        find_use_regexp="استخدام التعبير النمطي",
        find_invalid_regexp="تعبير نمطي غير صالح",
        replace_placeholder="استبدال",
        replace_button="استبدال",
        replace_all_button="استبدال الكل",
        replace_count="{count} تم استبداله",

        # Go to line dialog
        goto_line_title="الانتقال إلى السطر",
        goto_line_label="رقم السطر:",

        # Input widget
        processing_message="محادثة مع {model} (Esc للإلغاء، {key} للإرسال)",
        input_prompt="تحدث مع {model} ({key} للإرسال)",
        command_prompt="الأمر (Enter أو {key} للإرسال)",
        editing_message="تحرير الرسالة — سيؤدي الإرسال إلى إزالة هذه الرسالة وكل الرسائل التالية ({key} للإرسال)",

        # AI thinking message
        ai_thinking="الذكاء الاصطناعي يفكر...",

        # Dialog and settings
        cancel="إلغاء",
        ok="موافق",
        apply="تطبيق",
        discard="تجاهل",
        yes="نعم",
        no="لا",

        # User Settings dialog - AI backends section
        display_settings="إعدادات العرض",
        ai_backend_config="إعدادات خلفيات الذكاء الاصطناعي",
        enable_backend="تفعيل الخلفية",
        api_key="مفتاح API",
        api_url="عنوان URL للواجهة (اختياري)",
        anthropic_backend="أنثروبيك",
        deepseek_backend="ديبسيك",
        google_backend="جوجل",
        zai_backend="زاي",
        mistral_backend="ميسترال",
        openai_backend="أوبن إي آي",
        ollama_backend="أولاما",
        ollama_cloud_backend="Ollama Cloud",
        xai_backend="xAI",
        vllm_backend="vLLM",

        # User Settings dialog - other settings
        select_language="اللغة",
        font_size="حجم الخط",
        theme_system="تلقائي",
        font_ligatures="تفعيل الروابط الخطية",
        theme_dark="داكن",
        theme_light="فاتح",
        theme_color_blind="عمى الألوان",
        theme_ocean_light="محيح فاتح",
        theme_light_default="افتراضي",
        theme_custom="مظهر مخصص",
        theme_custom_manually="يدوي",
        customize_colors="تخصيص الألوان…",
        file_sort_order="ترتيب الملفات",
        sort_directories_first="المجلدات أولاً",
        sort_alphabetical="أبجدي",

        # User Settings dialog - external file access
        external_file_access="الوصول إلى الملفات",
        allow_external_file_access="السماح للذكاء الاصطناعي بقراءة الملفات خارج المساحة الذهنية",
        external_file_allowlist="المسارات المسموح بها (أنماط glob، واحد لكل سطر):",
        external_file_denylist="المسارات المرفوضة دائماً (أنماط glob، واحد لكل سطر):",

        # User Settings dialog - external HTTP access
        settings_network_access="الوصول إلى الشبكة",
        external_http_access="الوصول إلى الشبكة",
        allow_external_http_access="السماح للذكاء الاصطناعي بجلب عناوين URL المسموح بها دون سؤال",
        external_http_allowlist="عناوين URL المعتمدة تلقائياً (واحد لكل سطر، مثل api.example.com أو https://api.example.com:8443):",
        external_http_denylist="عناوين URL المرفوضة دائماً (واحد لكل سطر، مثل localhost أو 10.0.0.0/8):",

        # Mindspace tree
        mindspace_label_none="لا توجد مساحة ذهنية نشطة",
        window_title="Humbug: {}",
        mindspace_name_tooltip="انقر لتبديل المساحة الذهنية",
        mindspace_files="ملفات",
        mindspace_vcs="الملفات المتغيرة",
        mindspace_vcs_no_repo="لم يتم العثور على مستودع",
        mindspace_vcs_root="جذر المساحة الذهنية",
        mindspace_conversations="محادثات",
        pinned_section="مثبت",
        mindspace_preview="معاينة",
        mindspace_expand_sidebar="توسيع الشريط الجانبي",
        mindspace_collapse_sidebar="طي الشريط الجانبي",
        mindspace_search_placeholder="بحث",
        mindspace_search_options="خيارات البحث",
        mindspace_search_whole_word="مطابقة الكلمة كاملة",
        mindspace_search_empty_state="جاهز للبحث",
        mindspace_search_no_results="لا توجد نتائج",
        mindspace_search_path_match="مطابقة في المسار",
        mindspace_search_results="{0} نتيجة في {1} ملف",
        mindspace_search_results_limited="النتائج محدودة بـ {0} تطابق",
        mindspace_search_include_hidden="تضمين المجلدات المخفية",

        # Mindspace folders dialog
        mindspace_folders_title="تكوين مجلدات المساحة الذهنية",
        mindspace_path="مسار المساحة الذهنية",
        humbug_folder='إنشاء مجلد ".humbug"',
        src_folder='إنشاء مجلد "src"',

        # Mindspace settings
        model_settings="إعدادات النموذج",
        editor_settings="إعدادات المحرر",
        use_soft_tabs="استخدام المسافات البادئة",
        tab_size="حجم المسافة البادئة",
        backup_settings="إعدادات النسخ الاحتياطي",
        auto_backup="نسخ احتياطي تلقائي",
        backup_interval="فترة النسخ الاحتياطي (ثواني)",
        terminal_settings="إعدادات الطرفية",
        terminal_settings_description=(
            "تكوين سلوك مخزن التمرير المؤقت للطرفية. يخزن مخزن التمرير المؤقت سجل إخراج الطرفية "
            "الذي يمكنك التمرير للرجوع إليه."
        ),
        terminal_fixed_width_enabled="تمكين العرض الثابت",
        terminal_fixed_width="العرض الثابت (أعمدة)",
        terminal_scrollback_enabled="تحديد مخزن التمرير المؤقت",
        terminal_scrollback_lines="خطوط التمرير",
        terminal_close_on_exit="إغلاق الطرفية عند خروج الصدفة",
        tool_settings="إعدادات أدوات الذكاء الاصطناعي",
        tools_description="تفعيل أو إلغاء تفعيل الأدوات الفردية لهذا المساحة الذهنية",

        # Diff view settings
        diff_settings="إعدادات الفروقات",
        diff_view_mode="عرض الفروقات",
        diff_view_inline="متسلسل",
        diff_view_side_by_side="جنباً إلى جنب",
        diff_show_side_by_side="عرض جنباً إلى جنب",
        diff_show_inline="عرض متسلسل",

        # Unified settings dialog
        settings_all_mindspaces="جميع المساحات الذهنية",
        settings_this_mindspace="هذه المساحة الذهنية",
        settings_display="العرض",
        settings_file_access="الوصول إلى الملفات",
        settings_ai_backends="خلفيات الذكاء الاصطناعي",
        settings_ai_model="نموذج الذكاء الاصطناعي",
        settings_ai_tools="أدوات الذكاء الاصطناعي",
        settings_editor="المحرر",
        settings_diff="الفروقات",
        settings_terminal="الطرفية",
        settings_conversation="المحادثة",
        settings_tabs="علامات التبويب",
        settings_journal="السجل",

        journal_settings="السجل",
        journal_checkpoint_interval="الفاصل الزمني لنقاط التحقق (ثوانٍ)",
        journal_watched_section="الشجرة المراقَبة",
        journal_watched_section_description=(
            "لا تُسجَّل التغييرات الواقعة تحت مُدخَل متجاهَل. المُدخَلات أنماط glob، تُطابَق كما "
            "في قوائم الوصول إلى الملفات: الاسم يُطابِق ذلك الاسم كاملًا في أي موضع من الشجرة "
            "ولا يُطابِق اسمًا أطول، فـ‎venv‎ لا يُطابِق ‎venvity‎، وأنهِ المُدخَل بـ‎/**‎ لتضمين "
            "كل ما يقع تحت مسار. يُتجاهَل دائمًا المجلد ‎.humbug‎ و‎.git‎."
        ),
        journal_pruned_paths="الملفات والمجلدات والمسارات المتجاهَلة",

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
        settings_reasoning_effort_label="جهد التفكير",
        settings_effort_none="لا شيء",
        settings_effort_minimal="الحد الأدنى",
        settings_effort_low="منخفض",
        settings_effort_medium="متوسط",
        settings_effort_high="عالٍ",
        settings_effort_xhigh="مرتفع جداً",
        settings_effort_max="الحد الأقصى",
        settings_apply_to_all_conversations="تعيين كافتراضي لهذه المحادثة وجميع المحادثات المفتوحة",
        settings_apply_to_open_conversations="تطبيق على جميع المحادثات المفتوحة",
        settings_applied_to_all_conversations="تم التطبيق",
        settings_apply_to_all_confirm_title="التطبيق على جميع المحادثات؟",
        settings_apply_to_all_confirm_message=(
            "سيؤدي هذا إلى تغيير النموذج والمزوّد ودرجة الحرارة وإعدادات التفكير لكل محادثة مفتوحة حاليًا، "
            "وتعيينها كافتراضي للمحادثات الجديدة. لا يمكن التراجع عن هذا الإجراء. هل تريد المتابعة؟"
        ),

        # File dialog titles
        file_dialog_open_conversation="فتح محادثة",
        file_dialog_open_file="فتح ملف",
        file_dialog_save_file="حفظ الملف",
        file_dialog_new_mindspace="إنشاء مساحة ذهنية جديدة",
        file_dialog_open_mindspace="فتح مساحة ذهنية",
        file_dialog_attach_file="إرفاق ملف",

        # File dialog filters
        file_filter_all="كل الملفات (*.*)",
        file_filter_conversation="ملفات المحادثة (*.conv)",

        # Dialog titles
        mindspace_error_title="خطأ في المساحة الذهنية",
        conversation_error_title="خطأ في المحادثة",
        settings_error_title="خطأ في الإعدادات",
        error_opening_file_title="خطأ في فتح الملف",
        error_saving_file_title="خطأ في حفظ الملف",
        save_changes_title="حفظ التغييرات؟",
        confirm_delete_title="تأكيد الحذف",
        file_error_title="خطأ في الملف",
        preview_error_title="خطأ في المعاينة",
        cancel_conversation_title="إلغاء المحادثة؟",

        # Messages
        confirm_delete_message="هل أنت متأكد من حذف {0}؟",
        delete_warning_detail=(
            "سيتم إغلاق أي تبويب مفتوح دون حفظ. يُنقل العنصر إلى سلة المحذوفات، "
            "حيث يمكن استعادته."
        ),
        delete_recorded_warning_detail=(
            "سيتم إغلاق أي تبويب مفتوح دون حفظ. يُسجَّل محتواه أولاً، "
            "بحيث يمكن التراجع عن الحذف من لوحة التغييرات."
        ),
        error_deleting_file="تعذر حذف الملف: {0}",
        error_title_rename="خطأ في إعادة التسمية",
        error_rename_exists="توجد محادثة باسم '{0}' بالفعل.",
        error_rename_failed="تعذر إعادة تسمية المحادثة: {0}",
        unsaved_changes="هل تريد حفظ التغييرات في {0}؟",
        mindspace_exists_error="المساحة الذهنية موجودة بالفعل في المجلد المحدد.",
        mindspace_already_open_error="هذه المساحة الذهنية مفتوحة بالفعل في نافذة أخرى.",
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
        error_opening_preview="لا يمكن فتح المعاينة: {}",
        error_saving_mindspace_settings="فشل في حفظ الإعدادات: {}",
        error_saving_user_settings="فشل في حفظ إعدادات المستخدم: {}",
        cancel_conversation="هل أنت متأكد من أنك تريد إيقاف استجابة الذكاء الاصطناعي الحالية؟ سيؤدي هذا "
            "إلى إنهاء المحادثة الجارية وقد يؤدي إلى استجابات غير مكتملة.",

        initialize_mindspace_title="تهيئة المساحة الذهنية",
        initialize_mindspace_message=(
            "لم يتم العثور على مساحة ذهنية في:\n\n{0}\n\n"
            "هل تريد تهيئة هذا المجلد كمساحة ذهنية؟"
        ),

        open_in_new_window="فتح مساحة ذهنية في نافذة جديدة...",
        open_in_new_window_title="فتح في نافذة جديدة",
        open_in_new_window_already_open=(
            "هذه المساحة الذهنية مفتوحة بالفعل في نافذة Humbug أخرى (العملية {}).\n\n"
            "لا يمكن لنافذتين مشاركة مساحة ذهنية واحدة، لذا انتقل إلى النافذة الموجودة بدلاً من ذلك."
        ),
        open_in_new_window_already_here="هذه المساحة الذهنية مفتوحة بالفعل في هذه النافذة.",
        open_in_new_window_failed="تعذّر بدء نافذة Humbug جديدة:\n\n{}",
        open_in_new_window_choose_folder="اختيار مجلد...",
        mindspace_open_elsewhere_tooltip=(
            "{} مفتوحة بالفعل في نافذة Humbug أخرى، لذا لا يمكن فتحها هنا"
        ),

        # Status bar
        editor_status="سطر {line}، عمود {column} | {encoding} | {line_ending} | {type}",
        conversation_status=(
            "النموذج: {model}  |  المزود: {provider}  |  {reasoning}  |  {temperature}  |  "
            "رموز الإدخال: {input_tokens} / {max_input_tokens}  |  "
            "إجمالي الإدخال: {total_input_tokens}  |  "
            "رموز الإخراج: {output_tokens} / {max_output_tokens}  |  "
            "إجمالي الإخراج: {total_output_tokens}"
        ),
        conversation_status_temperature="درجة الحرارة: {temperature:.1f}",
        conversation_status_no_temperature="درجة الحرارة: غير متوفر",
        conversation_status_reasoning="الاستدلال: {effort}",
        conversation_status_no_reasoning="الاستدلال: غير متوفر",
        conversation_status_rate_limited="تم تجاوز حد الطلبات — إعادة المحاولة خلال {delay}ث…",
        terminal_status="طرفية: {name} ({columns}x{rows})",
        system_status="نظام",
        log_status="سجل",
        log_status_entries="السجل: المدخلات {first:,}-{last:,} من {total:,}",
        preview_status="معاينة: {path}",
        diff_status="فرق: {path} | {rows} صفوف متغيرة",

        # Mindspace File Tree Edit Menu Errors and Options
        pin="تثبيت",
        unpin="إلغاء التثبيت",
        rename="إعادة التسمية",
        delete="حذف",
        open_in_editor="فتح في المحرر",
        open_in_preview="فتح في المعاينة",
        open_in_diff="فتح في الفرق",
        open_in_conversation="فتح في المحادثة",
        duplicate="تكرار",
        sort_by="ترتيب حسب",
        sort_by_name="ترتيب حسب الاسم",
        sort_by_creation_time="ترتيب حسب تاريخ الإنشاء",
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

        # Trash operations
        trash="سلة المهملات",
        trash_empty_message="سلة المهملات فارغة.",
        trash_no_mindspace_message="لا توجد مساحة عمل مفتوحة.",
        trash_unknown_location="موقع غير معروف",
        trash_restore="استعادة",
        trash_delete_forever="حذف نهائي",
        trash_empty_trash_action="إفراغ سلة المهملات",
        trash_confirm_delete_forever_title="حذف نهائي",
        trash_confirm_delete_forever_message="هل أنت متأكد من الحذف النهائي لـ {0}؟ لا يمكن التراجع عن هذا الإجراء.",
        trash_confirm_empty_title="إفراغ سلة المهملات",
        trash_confirm_empty_message="هل أنت متأكد من الحذف النهائي لكل ما في سلة المهملات؟ لا يمكن التراجع عن هذا الإجراء.",
        trash_restore_failed_title="فشلت الاستعادة",
        trash_restore_failed_message="فشلت استعادة {0}: {1}",
        trash_time_just_now="الآن",
        trash_time_minutes_ago="منذ {0} د",
        trash_time_hours_ago="منذ {0} س",
        trash_time_days_ago="منذ {0} ي",

        journal="السجل",
        changes_no_mindspace_message="افتح مساحة عمل لعرض تغييراتها الأخيرة.",
        changes_empty_message="لم يتم تسجيل أي تغيير بعد.",
        changes_until="التغييرات في {0} أو قبله",
        changes_since_action="عرض التغييرات حتى تاريخ محدد…",
        changes_show_all_action="عرض كل التغييرات",
        changes_revert_file_action="تراجع عن هذا التغيير",
        changes_restore_to_point_action="استعادة مساحة العمل إلى هذا التغيير",
        changes_revert_title="التراجع عن التغييرات",
        changes_view_replaced_action="عرض النسخة التي استبدلها هذا التغيير",
        changes_view_this_version_action="عرض النسخة التي أنتجها هذا التغيير",
        changes_compare_action="مقارنة النسخة المستبدلة بهذه النسخة",
        changes_version_unavailable=(
            "لا يمكن عرض هذه النسخة: محتواها لم يعد محفوظًا، أو أنها ليست نصًا وقد "
            "تغير الملف منذ ذلك الحين."
        ),
        changes_this_version_unavailable="حذف هذا التغيير الملف، لذا ليس له نسخة خاصة به.",
        changes_revert_confirm=(
            "هل تريد التراجع عن {0} تغيير؟ سيعود كل ملف متأثر إلى الحالة التي كان "
            "عليها من قبل. التغييرات التي تُجرى خارج Humbug لا تُسجَّل، لذلك لا "
            "يُتراجع عنها."
        ),
        changes_revert_result="تم التراجع عن {0} مسار، وتعذر التراجع عن {1}.",
        changes_restore_to_point_confirm=(
            "هل تريد التراجع عن {0} تغيير مسجل بعد هذا التغيير؟ سيعود كل ملف متأثر إلى "
            "الحالة التي كان عليها عند هذا التغيير. التغييرات التي تُجرى خارج Humbug "
            "لا تُسجَّل، لذا قد لا يعود كل شيء تمامًا كما كان."
        ),
        changes_bucket_today="اليوم",
        changes_bucket_earlier="في وقت سابق",
        changes_older_not_shown="{0} من التغييرات الأقدم غير معروضة",
        changes_size_warning=(
            "يستخدم سجل التغييرات {0:.0f} ميغابايت. انقر بزر الفأرة الأيمن هنا لنسيان "
            "السجل القديم."
        ),
        changes_forget_through_action="نسيان هذا التغيير وكل ما قبله",
        changes_forget_all_action="نسيان كل التغييرات",
        changes_forget_title="نسيان التغييرات",
        changes_forget_confirm=(
            "هل تريد نسيان {0}؟ لن يكون بالإمكان التراجع عن هذه التغييرات، وسيُحذف أي "
            "محتوى يحتفظ به السجل وحده. لا يمكن التراجع عن هذا الإجراء."
        ),
        changes_forget_scope_through="هذا التغيير وكل ما قبله",
        changes_forget_scope_all="كل تغيير مسجل",
        changes_forget_result="تم نسيان {0} تغيير واستعادة {1:.0f} ميغابايت.",
        editor_read_only_marker=" (نسخة سابقة)",

        # Tool tips
        tooltip_copy_contents="نسخ جميع المحتويات",
        tooltip_save_contents="حفظ المحتويات إلى ملف",
        tooltip_copy_message="نسخ الرسالة إلى الحافظة",
        tooltip_save_message="حفظ الرسالة كملف ماركداون",
        tooltip_fork_message="تفريع المحادثة من هذه الرسالة",
        tooltip_delete_from_message="حذف المحادثة من هذه النقطة",
        tooltip_edit_message="تحرير هذه الرسالة",
        tooltip_submit_message="إرسال الرسالة",
        tooltip_stop_message="إيقاف المعالجة الحالية",
        tooltip_settings_message="فتح إعدادات المحادثة",
        tooltip_attach_file="إرفاق ملف",
        tooltip_cancel_edit="إلغاء التحرير",
        tooltip_expand_message="توسيع الرسالة",
        tooltip_collapse_message="طي الرسالة",
        warning_file_too_large=(
            "'{filename}' حجمه {size_kb}كيلوبايت، وهو يتجاوز الحد الموصى به البالغ 100 كيلوبايت. قد تستهلك "
            "الملفات الكبيرة جزءاً كبيراً من نافذة السياق. هل تريد الإرفاق على أي حال؟"
        ),
        tooltip_show_attachments="عرض المرفقات",
        tooltip_hide_attachments="إخفاء المرفقات",
        error_pdf_unsupported=(
            "تعذّر إرفاق '{filename}'. {reason}"
        ),
        error_pdf_extraction_failed=(
            "تعذّر إرفاق '{filename}': فشل استخراج النص. {reason}"
        ),
        error_docx_unsupported=(
            "تعذّر إرفاق '{filename}'. {reason}"
        ),
        error_docx_extraction_failed=(
            "تعذّر إرفاق '{filename}': فشل استخراج النص. {reason}"
        ),

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
        move_error_protected="لا يمكن نقل مجلدات النظام (conversations, .humbug).",

        # Inline editor validation errors
        error_empty_name="لا يمكن أن يكون الاسم فارغاً",
        error_invalid_characters="الاسم يحتوي على رموز غير صالحة: \\ / : * ? \" < > |",
        error_validation_failed="تعذر التحقق من صحة الاسم",
        error_invalid_path="مسار الملف غير صالح",
        error_title="خطأ",

        # File duplication errors
        error_duplicating_file="تعذر تكرار الملف: {0}",

        # File extension change confirmation
        rename_change_extension_title="تغيير نوع الملف؟",
        rename_change_extension_message="أنت تغير امتداد الملف من '{0}' إلى '{1}'."
            " قد يؤثر ذلك على طريقة التعامل مع الملف. هل أنت متأكد؟",

        # Tool use
        approve_tool_call="موافقة",
        i_am_unsure_about_tool_call="لست متأكداً من هذا",
        reject_tool_call="رفض",
        auto_approve_file_changes="الموافقة التلقائية على تغييرات الملفات حتى رسالتك التالية",
        interruptible_wait_message="جارٍ الانتظار {duration} ثانية \u2014 تبقّى {remaining}...",
        finish_waiting_now="إنهاء الانتظار الآن",
        retry_error="إعادة المحاولة",

        # Update checker
        check_for_updates="التحقق من التحديثات...",
        update_current_version="الإصدار الحالي: {0}",
        update_checking="جارٍ التحقق...",
        update_up_to_date="أنت على أحدث إصدار.",
        update_available_message="الإصدار {0} متاح — انقر للتنزيل.",
        update_check_failed="تعذّر التحقق من التحديثات. يرجى التحقق من اتصالك بالشبكة.",
        update_tooltip="Humbug {0} متاح — انقر لصفحة الإصدار",
        tab_overview_tooltip="عرض جميع علامات التبويب المفتوحة",
        tab_carousel_tooltip="عرض دوّار علامات التبويب",
        check_for_updates_setting="التحقق التلقائي من التحديثات",

        # Ollama pull
        ollama_pull_label="تنزيل نموذج",
        ollama_pull_placeholder="اسم النموذج (مثال: llama3.2)",
        ollama_pull_button="تنزيل",
        ollama_pull_pulling="جارٍ تنزيل {0}…",
        ollama_pull_success="تم تثبيت '{0}' بنجاح.",
        ollama_pull_model_not_found="النموذج غير موجود — تحقق من الاسم على ollama.com/library.",
        ollama_pull_not_running="تعذّر الاتصال بـ Ollama — هل هو قيد التشغيل؟",
        ollama_pull_error="فشل التنزيل: {0}",
        ollama_update_local_models="تحديث النماذج المحلية",
        update_models="تحديث النماذج",
        remove_fetched_models="إزالة النماذج المسترجعة…",
        fetching_models="جارٍ الاسترجاع…",
        unknown_provider="مزود غير معروف.",
        fetch_added_models="تمت إضافة {0} نموذج جديد.",
        fetch_already_up_to_date="القائمة محدّثة بالفعل.",
        ollama_no_installed_models="لم يتم العثور على نماذج مثبتة.",
        ollama_added_models="تمت إضافة {0} نموذج.",
        ollama_already_up_to_date="قائمة النماذج المحلية محدّثة بالفعل.",
        test_connection="اختبار الاتصال",
        testing_connection="جارٍ اختبار الاتصال…",
        connection_successful="تم الاتصال بنجاح.",

        # Settings dialog - AI model page
        settings_provider="المزود",
        settings_all_providers="جميع المزودين",

        # Fetched model manager dialog
        fetched_models_title="النماذج المسترجعة — {0}",
        no_fetched_models="لا توجد نماذج مسترجعة لهذا المزود.",
        fetched_models_close="إغلاق",

        # Onboarding product tour
        tour_step_of="الخطوة {0} من {1}",
        tour_back_button="رجوع",
        tour_skip_button="تخطي الجولة",
        tour_next_button="التالي",
        tour_finish_button="ابدأ استخدام Humbug",
        tour_welcome_title="مرحباً بك في Humbug",
        tour_welcome_description=(
            "يساعدك Humbug على التعاون مع الذكاء الاصطناعي في مشاريعك — الدردشة، وتعديل الملفات، "
            "وتشغيل الطرفيات، ومراجعة التغييرات، كل ذلك في مساحة عمل واحدة. لنأخذ جولة سريعة."
        ),
        tour_start_here_title="ابدأ من هنا",
        tour_start_here_description="انقر هنا لإنشاء أو فتح مساحة ذهنية — مساحة عمل مشروعك في Humbug.",
        tour_open_conversation_title="فتح محادثة",
        tour_open_conversation_description=(
            "افتح محادثاتك هنا — استخدم + لبدء محادثة جديدة، أو أيقونة المجلد لتنظيمها."
        ),
        tour_workspace_title="مساحة عملك",
        tour_workspace_description="تُفتح المحادثات وعلامات التبويب الأخرى هنا. هذا هو المكان الذي ستقوم فيه بمعظم عملك.",
        tour_key_action_title="إرسال رسالة",
        tour_key_action_description="اكتب رسالة واضغط على إرسال (أو {0}) للتحدث مع الذكاء الاصطناعي.",
        tour_output_title="سجل المحادثة",
        tour_output_description="تظهر ردود الذكاء الاصطناعي وسجل محادثتك هنا أثناء الدردشة.",
        tour_navigation_title="التنقل",
        tour_navigation_description="بدّل بين المحادثات والملفات والبحث والمعاينة والمزيد باستخدام هذا الشريط الجانبي.",
        tour_help_title="الإعدادات والمساعدة",
        tour_help_description="افتح الإعدادات هنا في أي وقت. يمكنك إعادة تشغيل هذه الجولة من Humbug ← أخذ جولة.",

        fetched_models_remove="إزالة",

        fetch_error_invalid_key="مفتاح API غير صالح — يرجى التحقق من مفتاحك والمحاولة مجدداً.",
        fetch_error_access_denied="تم رفض الوصول ({0}) — قد لا يملك مفتاحك صلاحية عرض قائمة النماذج.",
        fetch_error_not_found="نقطة نهاية النماذج غير موجودة ({0}) — تحقق من عنوان URL للواجهة البرمجية.",
        fetch_error_rate_limited="تم تجاوز حد الطلبات ({0}) — انتظر لحظة ثم أعد المحاولة.",
        fetch_error_server_error="خطأ في خادم المزود ({0}) — حاول مجدداً لاحقاً.",
        fetch_error_connection="تعذّر الاتصال — تحقق من عنوان URL وشبكتك.",
        fetch_error_timeout="انتهت مهلة الطلب — استغرق المزود وقتاً طويلاً للاستجابة.",
        fetch_error_generic="فشل: {0}",
        stream_error_interrupted="انقطع الاتصال قبل اكتمال الاستجابة. يرجى إعادة المحاولة.",
        stream_error_no_backend="لا يوجد خدمة ذكاء اصطناعي مهيأة لمزود هذا النموذج — تحقق من إعدادات خدمات الذكاء الاصطناعي.",
        stream_error_access_denied="تم رفض الوصول ({0}) — قد لا يملك حسابك صلاحية الوصول إلى هذا النموذج.",

        mission_control="مركز المهام",
        mission_subtitle="خطّط وفوّض ووافق في مكان واحد",
        mission_hero_title="ما الذي تريد أن يفعله فريق الذكاء الاصطناعي من أجلك؟",
        mission_hero_subtitle=(
            "صِف العمل وسلّمه. يخطّط له قائد المهمة ويستعين بمساعدين ويعمل في الخلفية "
            "ثم يعود إليك بتقرير نهائي. لا تتدخّل إلا عندما يحتاج إلى رأيك."
        ),
        mission_examples_label="جرّب أحد هذه الأمثلة",
        mission_example_1="راجع هذا المشروع واكتب نظرة عامة موجزة على بنيته",
        mission_example_2="اعثر على الاختبارات الفاشلة وأصلحها",
        mission_example_3="أضف التحقق من المدخلات إلى نموذج الإعدادات مع الاختبارات",
        mission_new="مهمة جديدة",
        mission_caption="مهمة",
        mission_status_planning="قيد التخطيط",
        mission_status_running="قيد التنفيذ",
        mission_status_needs_you="بانتظارك",
        mission_status_idle="بانتظار ردّك",
        mission_status_completed="مكتملة",
        mission_status_stopped="متوقفة",
        mission_progress="اكتمل {0}%",
        mission_stop="إيقاف",
        mission_stat_tasks="الخطوات",
        mission_stat_completed="مكتملة",
        mission_stat_in_progress="قيد التنفيذ",
        mission_stat_approvals="بانتظار موافقتك",
        mission_stat_agents="وكلاء نشطون",
        mission_plan_title="خطة المهمة",
        mission_plan_empty="ستظهر الخطة هنا بمجرد أن يصيغها الذكاء الاصطناعي الرئيسي.",
        mission_task_pending="معلّقة",
        mission_task_in_progress="قيد التنفيذ",
        mission_task_completed="مكتملة",
        mission_task_blocked="متعثّرة",
        mission_agents_title="الوكلاء",
        mission_agents_active="{0} نشط",
        mission_agents_empty="لا توجد محادثة مفتوحة لهذه المهمة. افتح المحادثة الرئيسية للمتابعة.",
        mission_agent_lead="قائد المهمة",
        mission_agent_delegate="وكيل مفوَّض",
        mission_agent_thinking="يفكّر…",
        mission_agent_working="يستخدم {0}",
        mission_agent_waiting="بانتظار موافقتك",
        mission_agent_idle="خامل",
        mission_agent_error="توقّف بسبب خطأ",
        mission_inbox_title="صندوق القرارات",
        mission_inbox_empty="لا شيء بانتظار قرارك. تظهر هنا طلبات الموافقة من هذه المهمة ومن المحادثات التي لا تنتمي إلى أي مهمة.",
        mission_approve="موافقة",
        mission_unsure="غير متأكد",
        mission_reject="رفض",
        mission_destructive="إجراء هدّام",
        mission_decision_from="من {0}",
        mission_activity_title="النشاط الأخير",
        mission_activity_empty="سيظهر نشاط هذه المهمة هنا.",
        mission_activity_started="بدأت المهمة",
        mission_activity_plan_updated="تم تحديث الخطة",
        mission_activity_agent_joined="انضمّ {0} إلى المهمة",
        mission_activity_turn_started="بدأ {0} العمل",
        mission_activity_turn_finished="أنهى {0} عمله",
        mission_activity_tool_used="استخدم {0} الأداة {1}",
        mission_activity_approval_requested="طلب {0} استخدام {1}",
        mission_activity_approved="وافقت على {0}",
        mission_activity_rejected="رفضت {0}",
        mission_activity_guidance="أرسلت توجيهات",
        mission_activity_stopped="توقفت المهمة",
        mission_activity_error="توقّف {0} بسبب خطأ",
        mission_composer_new="أخبر Humbug بما تريد تحقيقه… ({key} للإرسال)",
        mission_composer_guidance="أرسل توجيهات إلى قائد المهمة… ({key} للإرسال)",
        mission_send="إرسال",
        mission_error_start="تعذّر بدء المهمة: {0}",
        mission_error_send="تعذّر الإرسال إلى المهمة: {0}",
        mission_decision_stale="تمت الإجابة عن هذا الطلب مسبقًا.",
        mission_lead_message_title="آخر رسالة من قائد المهمة",
        mission_lead_waiting="بانتظار ردّك",
        mission_reply="ردّ",
        mission_composer_reply="اكتب ردّك لقائد المهمة… ({key} للإرسال)",
        mission_task_progress="التقدّم",
        mission_task_evidence="الدليل",
        mission_task_worked_by="يعمل عليها {0}",
        mission_task_no_update="لم يُبلغ القائد عن أي تقدّم في هذه الخطوة بعد.",
        mission_task_open_agent="فتح المحادثة",
        mission_plan_hint="انقر على خطوة لعرض تقدّمها.",
        mission_plan_filter_empty="لا توجد خطوات مطابقة لهذا الفلتر.",
        mission_plan_show_all="عرض الكل",
        mission_activity_auto_continued="طُلب من القائد المتابعة",
        mission_activity_auto_paused="توقّف مؤقتًا: لم تكتمل أي خطوة بعد {0} محاولات متابعة تلقائية",
        mission_status_interrupted="متوقفة بشكل مفاجئ",
        mission_status_replied="تمت الإجابة",
        mission_status_failed="فشلت",
        mission_resume="استئناف",
        mission_interrupted_notice="أُغلق Humbug أثناء تنفيذ هذه المهمة. استأنفها للمتابعة من حيث توقفت.",
        mission_auto_resume="استئناف تلقائي بعد إعادة التشغيل",
        mission_milestone_step="اكتملت الخطوة {0} من {1}: {2}",
        mission_milestone_round="اكتمل: {0}",
        mission_milestone_next="التالي: {0}",
        mission_milestone_planning="التالي: التخطيط لما تبقى من الهدف",
        mission_milestone_follow_up="التالي: التخطيط لطلبك الجديد",
        mission_glance_title="نظرة سريعة",
        mission_glance_now="الآن",
        mission_glance_blocking="ما يعيقها",
        mission_glance_approvals="بانتظارك",
        mission_glance_last_done="آخر ما أُنجز",
        mission_glance_idle="لا شيء قيد التشغيل",
        mission_glance_nothing="لا شيء",
        mission_glance_blocked_decision="قرارك بشأن طلب",
        mission_glance_blocked_paused="أوقفتها مؤقتًا",
        mission_glance_blocked_failed="انتهى آخر رد للقائد بخطأ",
        mission_glance_blocked_interrupted="أُغلق Humbug أثناء العمل",
        mission_glance_blocked_stopped="أوقفتها",
        mission_glance_blocked_step="خطوة متوقفة: {0}",
        mission_glance_approvals_count="{0} طلب",
        mission_glance_approvals_none="لا شيء",
        mission_glance_last_done_none="لا شيء بعد",
        mission_details_show="عرض التفاصيل",
        mission_details_hide="إخفاء التفاصيل",
        mission_working_on_message="أعمل على رسالتك: “{0}”",
        mission_reduce_motion="تقليل الحركة",
        mission_reduce_motion_tip=(
            "اعرض Mission Control ثابتًا: لا شيء ينزلق أو يتلاشى أو ينبض أو يدور أو يُكتب حرفًا حرفًا، "
            "ولا يُحتفل بالمهام المنتهية بقصاصات الورق"
        ),
        mission_activity_resumed="استُؤنفت بعد إغلاق Humbug",
        mission_error_resume="تعذّر استئناف المهمة: {0}",
        mission_decision_target="يعمل على: {0}",
        mission_decision_undoable="يمكن التراجع عنه: يسجّل سجل تغييرات الملفات في Humbug هذا التغيير.",
        mission_decision_not_undoable="لا يمكن لـ Humbug التراجع عنه بعد الموافقة.",
        mission_review_request="مراجعة",
        mission_status_paused="متوقفة مؤقتًا",
        mission_say_paused="متوقف مؤقتًا. سأُنهي ما أعمل عليه الآن، ثم أنتظرك.",
        mission_pause="إيقاف مؤقت",
        mission_pause_tip=(
            "دع ما يعمل الآن ينتهي، ثم انتظر: لا يُوافَق على أي شيء تلقائيًا "
            "ولا يتابع القائد من تلقاء نفسه حتى تتابع أنت"
        ),
        mission_unpause="متابعة",
        mission_paused_notice=(
            "هذه المهمة متوقفة مؤقتًا. لا يُوافَق على أي شيء تلقائيًا "
            "ولن يتابع القائد من تلقاء نفسه حتى تضغط متابعة."
        ),
        mission_activity_paused="أُوقفت مؤقتًا",
        mission_activity_unpaused="تابعت بعد توقف مؤقت",
        mission_everything_banner=(
            "الموافقة التلقائية على كل شيء مفعّلة للمهمة \"{0}\": كل طلب أداة، بما في ذلك حذف الملفات وتشغيل "
            "الأوامر، يُوافَق عليه دون سؤالك."
        ),
        mission_everything_turn_off="إيقاف",
        mission_retry="إعادة المحاولة",
        mission_activity_retried="أُعيدت محاولة الطلب الفاشل",
        mission_error_retry="تعذّرت إعادة محاولة المهمة: {0}",
        mission_switch_to_autonomous="التبديل إلى الوضع المستقل",
        mission_switch_to_classic="التبديل إلى الوضع الكلاسيكي",
        mission_mode_autonomous="مستقل",
        mission_mode_classic="كلاسيكي",
        mission_classic_title="هذه المهمة في الوضع الكلاسيكي",
        mission_classic_body="أنت تتابعها من تبويب محادثتها، لذا لا يتابعها مركز المهام. بدّل إلى الوضع المستقل لإعادتها إليه.",
        mission_task_stop="إيقاف الخطوة",
        mission_task_stopped="متوقفة",
        mission_activity_task_stopped="أوقفت \"{0}\"",
        mission_activity_mode_classic="تم التبديل إلى الوضع الكلاسيكي",
        mission_activity_mode_autonomous="تم التبديل إلى الوضع المستقل",
        mission_error_switch="تعذّر تبديل وضع المهمة: {0}",
        mission_error_stop_task="تعذّر إيقاف الخطوة: {0}",
        mission_auto_approve="موافقة تلقائية",
        mission_auto_approve_off="متوقفة",
        mission_auto_approve_safe="الإجراءات الآمنة",
        mission_auto_approve_all="كل شيء",
        mission_auto_approve_tip=(
            "مع «الإجراءات الآمنة» تتم الموافقة فورًا على تغييرات الملفات التي يسجّلها السجل، أما الطلبات "
            "الآمنة الأخرى فيراجعها مراجع ذكاء اصطناعي لا يوافق إلا عندما تتوافق بوضوح مع الهدف، وما لا "
            "يتأكد منه يُعرض عليك. لا تشمل الإجراءات الآمنة أبدًا الحذف أو النقل أو الاستبدال أو أوامر "
            "الطرفية أو الإرسال عبر الشبكة. ومع «كل شيء» تتم الموافقة على كل طلب دون الرجوع إليك."
        ),
        mission_auto_approve_all_title="الموافقة على كل شيء تلقائيًا؟",
        mission_auto_approve_all_confirm=(
            "لهذه المهمة فقط، ستتم الموافقة على كل طلب دون الرجوع إليك أو إلى أي مراجع، بما في ذلك حذف "
            "الملفات ونقلها واستبدالها، وتنفيذ أوامر الطرفية (التي لا يمكن قصرها على ملفات معينة)، والتفويض "
            "إلى المساعدين، وإرسال البيانات عبر الشبكة. يستمر ذلك ما دامت المهمة تعمل في الوضع المستقل: "
            "اختيار مستوى آخر أو التبديل إلى الوضع الكلاسيكي أو إيقاف المهمة يوقفه. يُسجَّل تأكيدك وكل "
            "موافقة. فعّل هذا فقط لعمل لا تمانع عدم الإشراف عليه."
        ),
        mission_decision_reviewing="مراجعة الذكاء الاصطناعي…",
        mission_decision_review_note="مراجع الذكاء الاصطناعي: {0}",
        mission_activity_auto_approved="وافق مراجع الذكاء الاصطناعي على {0}",
        mission_activity_review_deferred="ترك مراجع الذكاء الاصطناعي {0} لك",
        mission_auto_approve_note_classic="الموافقة التلقائية متوقفة ما دامت هذه المهمة في الوضع الكلاسيكي.",
        mission_auto_approve_note_destructive="هذا الطلب هدّام، والموافقة التلقائية مضبوطة على الإجراءات الآمنة.",
        mission_options="خيارات المهمة",
        mission_rename="إعادة تسمية",
        mission_rename_title="إعادة تسمية المهمة",
        mission_rename_label="اسم المهمة:",
        mission_delete="حذف",
        mission_delete_title="حذف المهمة",
        mission_delete_confirm="هل تريد حذف \"{0}\" من مركز المهام؟ سيتم الاحتفاظ بمحادثاتها.",
        mission_error_rename="تعذّرت إعادة تسمية المهمة: {0}",
        mission_you="أنت",
        mission_plan_history="السجل ({0})",
        mission_plan_history_tip="الخطط التي أنهتها هذه المهمة قبل العمل الحالي",
        mission_plan_history_title="الخطط السابقة",
        mission_plan_empty_next="يصيغ الذكاء الاصطناعي الرئيسي خطة العمل الجديد. الخطط السابقة موجودة في السجل.",
        mission_phase_count="اكتملت {0} من {1} خطوات",
        mission_activity_plan_archived="نُقلت الخطة المكتملة إلى السجل",
        mission_say_reading="أقرأ {0} من أجلك…",
        mission_say_writing="أكتب {0} من أجلك…",
        mission_say_editing="أعدّل {0} من أجلك…",
        mission_say_deleting="أحذف {0}…",
        mission_say_files="أرتّب الملفات من أجلك…",
        mission_say_delegating="أستدعي وكيلًا مساعدًا لهذه المهمة…",
        mission_say_terminal="أشغّل أمرًا من أجلك: {0}",
        mission_say_web="أبحث عن شيء على الويب من أجلك…",
        mission_say_tool="أستخدم {0} من أجلك…",
        mission_say_task="أعمل الآن على «{0}» من أجلك…",
        mission_say_thinking="أفكر في الخطوة التالية…",
        mission_say_planning="مرحبًا، أنا {0}! أُعدّ خطة من أجلك…",
        mission_say_hello="مرحبًا، أنا {0}! أبدأ بالجزء الخاص بي…",
        mission_say_approval="أحتاج إلى موافقتك: {0}",
        mission_say_reviewing="يتحقق مراجع الذكاء الاصطناعي من طلبي: {0}",
        mission_say_done="انتهيت! هل هناك شيء آخر يمكنني فعله من أجلك؟",
        mission_say_reply="تركت لك رسالة. ردّ في الأسفل عندما تكون جاهزًا.",
        mission_say_stopped="توقفت. أرسل لي رسالة لأتابع.",
        mission_say_interrupted="أُغلق Humbug أثناء عملي. اضغط على استئناف وسأتابع من حيث توقفت.",
        mission_say_helper_waiting="أحد مساعديّ يحتاج إلى موافقتك.",
        mission_say_replied="رددتُ عليك أدناه. اسألني المزيد، أو كلّفني بمهمة.",
        mission_say_ready="جاهز متى شئت.",
        mission_say_error="حدث خطأ ما. اضغط إعادة المحاولة، أو أرسل لي رسالة.",
        mission_failed_notice=(
            "انتهى ردي الأخير بخطأ. أعد المحاولة لإرسال الطلب نفسه مرة أخرى، أو افتح محادثتي لمعرفة السبب، "
            "أو أرسل لي رسالة."
        ),
        mission_show_lead_conversation="افتح محادثة القائد",
        mission_say_sleeping="آخذ استراحة مخطط لها مدتها {0}. سأتابع بعد انتهائها…",
        mission_chat_title="المحادثة",
        mission_chat_hint="رسائلك وردود قائد المهمة، كما في علامة تبويب المحادثة",
        mission_chat_you="أنت",
        mission_chat_lead="قائد المهمة",
        mission_results_title="النتائج السابقة",
        mission_results_hint="تُحفظ هنا كل جولة انتهت قبل سؤال متابعة، الأحدث أولًا. انقر على جولة لقراءة تقريرها الكامل.",
        mission_result_number="الجولة {0}",
        mission_result_asked="طلبت: {0}",
        mission_result_no_report="لم يقدّم قائد المهمة تقريرًا نهائيًا لهذه الجولة.",
        mission_say_after_reading="قرأت {0}، وأفكّر الآن في الخطوة التالية…",
        mission_say_after_writing="كتبت {0}، وأتحقق منه ثم أتابع…",
        mission_say_after_editing="عدّلت {0}، وأتحقق منه ثم أتابع…",
        mission_say_after_deleting="حذفت {0}، وأتابع الآن…",
        mission_say_after_files="انتهيت من الملفات، وأفكّر الآن في الخطوة التالية…",
        mission_say_after_delegating="أبلغني مساعدي بالنتيجة، وأقرأ ما وجده…",
        mission_say_after_terminal="شغّلت {0}، وأقرأ النتيجة الآن…",
        mission_say_after_web="وصلني رد من الويب، وأقرأه الآن…",
        mission_say_after_tool="انتهيت من {0}، وأفكّر الآن في الخطوة التالية…",
        mission_say_progress="{0} ({1} إجراءات حتى الآن)",
        mission_say_progress_one="{0} (إجراء واحد حتى الآن)",
        mission_finish_waiting="إنهاء الانتظار",
        mission_wait_left="متبقٍ {0}",
        mission_activity_started_prompt="بدأت المهمة: «{0}»",
        mission_activity_guidance_prompt="أنت: «{0}»",
        mission_composer_hint_enter="Enter لسطر جديد",
        mission_stage_asked="طلبك",
        mission_stage_planning="الذكاء الاصطناعي يخطط",
        mission_stage_working="فريق الذكاء الاصطناعي يعمل",
        mission_stage_done="اكتملت المهمة",
        mission_report_title="تم إنجاز المهمة",
        mission_report_caption="التقرير النهائي من قائد المهمة",
        mission_report_stats="الخطوات المكتملة: {0} من {1}  ·  الوكلاء في الفريق: {2}",
        mission_follow_up="اطرح سؤال متابعة",
        mission_activity_reversible_approved="تمت الموافقة على {0} — مُسجَّل في السجل",
        mission_activity_everything_approved="تمت الموافقة على {0} — الموافقة التلقائية مضبوطة على كل شيء",
        mission_activity_user_message="أنت، إلى {0}: «{1}»",
        mission_say_waiting_helpers="بانتظار أن يُنهي {0} العمل ويرفع تقريره…",
        mission_read_more="اقرأ المزيد",
        mission_activity_open_tip="انقر لمعرفة ما حدث",
        mission_attention="{0} طلب/طلبات تحتاج إلى قرارك",
        mission_review_now="راجِع الآن",
        mission_more_actions="إجراءات أخرى",
        mission_agents_show_list="اعرض كقائمة",
        mission_agents_hide_list="أخفِ القائمة",
        mission_auto_approve_off_note="كل إجراء يغيّر الملفات أو ينفّذ أوامر أو يرسل بيانات ينتظرك.",
        mission_auto_approve_safe_note="تمضي تغييرات الملفات المسجّلة والطلبات الآمنة؛ وأي إجراء مُتلف ينتظرك.",
        mission_auto_approve_all_note=(
            "يعمل الفريق من البداية إلى النهاية دون توقف؛ "
            "ولا ينتظر شيء موافقتك."
        ),
        mission_scroll_left="مرّر المهام إلى اليسار",
        mission_scroll_right="مرّر المهام إلى اليمين",
        mission_waiting_elsewhere="المهمة «{0}» بانتظار قرارك",
        mission_waiting_elsewhere_many="{0} مهام أخرى بانتظار قراراتك",
        mission_go_to_waiting="انتقل إليها"
    )
