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

        # Tab bar context menu items
        close_tabs_to_left="إغلاق التبويبات إلى اليسار",
        close_tabs_to_right="إغلاق التبويبات إلى اليمين",
        close_other_tabs="إغلاق التبويبات الأخرى",
        close_column="إغلاق العمود",

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

        # Mindspace tree
        mindspace_label_none="لا توجد مساحة ذهنية نشطة",
        mindspace_name_tooltip="انقر لفتح مساحة ذهنية مختلفة",
        mindspace_files="ملفات",
        mindspace_vcs="الملفات المتغيرة",
        mindspace_vcs_no_repo="لم يتم العثور على مستودع",

        # VCS / git sidebar
        git_staged_changes="التغييرات المجهّزة",
        git_changes="التغييرات",
        git_commit_placeholder="رسالة الإيداع",
        git_commit_button="إيداع",
        git_stage="تجهيز",
        git_unstage="إلغاء التجهيز",
        git_stage_all="تجهيز الكل",
        git_unstage_all="إلغاء تجهيز الكل",
        git_discard="تجاهل التغييرات",
        git_push="دفع",
        git_pull="سحب",
        git_fetch="جلب",
        git_branch_tooltip="الفرع الحالي — انقر للتبديل أو الإنشاء",
        git_create_branch="إنشاء فرع جديد…",
        git_create_branch_title="إنشاء فرع",
        git_create_branch_prompt="اسم الفرع الجديد:",
        git_error_title="خطأ Git",
        git_confirm_discard_title="تجاهل التغييرات",
        git_confirm_discard_message="تجاهل جميع التغييرات على '{0}'؟ لا يمكن التراجع عن هذا.",
        git_no_staged_title="لا شيء للإيداع",
        git_no_staged_message="لا توجد تغييرات مجهّزة لإيداعها.",
        git_busy_title="Git مشغول",
        git_busy_message="هناك عملية git أخرى قيد التشغيل بالفعل. يرجى الانتظار.",
        git_set_upstream_title="لا يوجد فرع أعلى",
        git_set_upstream_message="الفرع '{0}' ليس له فرع أعلى. هل تريد الدفع وتعيين 'origin' كفرع أعلى؟",
        git_repo_tooltip="المستودع النشط — اختر المشروع المراد إدارته",
        git_manage="إدارة Git",
        git_refresh="تحديث التغييرات",
        git_commit_and_push="إيداع ودفع",
        git_commit_amend="تعديل آخر إيداع",
        git_working="جارٍ العمل…",
        git_stage_all_tooltip="تجهيز جميع التغييرات",
        git_unstage_all_tooltip="إلغاء تجهيز الكل",
        git_history="سجل الإيداعات",
        git_more_actions="إجراءات أخرى",
        git_undo_last_commit="التراجع عن آخر إيداع",
        git_stash_changes="إخفاء التغييرات",
        git_stashes="المخبوءات",
        git_no_stashes="لا توجد مخبوءات",
        git_stash_pop="استرجاع",
        git_stash_apply="تطبيق",
        git_stash_drop="حذف",
        git_revert="التراجع عن الإيداع",
        git_reset="إعادة التعيين إلى هذا الإيداع",
        git_reset_soft="ناعم — الاحتفاظ بكل التغييرات",
        git_reset_mixed="مختلط — إلغاء تجهيز التغييرات",
        git_reset_hard="صارم — تجاهل كل التغييرات",
        git_copy_hash="نسخ معرّف الإيداع",
        git_confirm_reset_hard_title="إعادة تعيين صارمة",
        git_confirm_reset_hard_message="تجاهل جميع التغييرات غير المودعة وإعادة التعيين إلى {0}؟ لا يمكن التراجع عن هذا.",
        git_confirm_drop_title="حذف المخبوء",
        git_confirm_drop_message="حذف المخبوء '{0}'؟ لا يمكن التراجع عن هذا.",
        git_conflicts="تعارضات الدمج",
        git_accept_ours="قبول النسخة الحالية (لنا)",
        git_accept_theirs="قبول النسخة الواردة (لهم)",
        git_mark_resolved="وضع علامة كمحلول",
        git_merge_in_progress="الدمج قيد التقدم — قم بحل التعارضات ثم أودِع",
        git_rebase_in_progress="إعادة الأساس قيد التقدم — قم بحل التعارضات للمتابعة",
        git_abort="إحباط",
        git_confirm_abort_title="إحباط",
        git_confirm_abort_message="إحباط {0} الجاري؟ ستُفقد جميع عمليات حل التعارضات.",
        git_switch="تبديل",
        git_merge_branch="دمج في الفرع الحالي",
        git_rename_branch="إعادة تسمية…",
        git_delete_branch="حذف",
        git_rename_branch_title="إعادة تسمية الفرع",
        git_rename_branch_prompt="اسم الفرع الجديد:",
        git_confirm_delete_branch_title="حذف الفرع",
        git_confirm_delete_branch_message="حذف الفرع '{0}'؟ لا يمكن التراجع عن هذا.",
        git_tags="الوسوم",
        git_new_tag="وسم جديد…",
        git_new_tag_title="إنشاء وسم",
        git_new_tag_prompt="اسم الوسم:",
        git_no_tags="لا توجد وسوم",
        git_delete_tag="حذف",
        git_confirm_delete_tag_message="حذف الوسم '{0}'؟",
        git_remotes="المستودعات البعيدة",
        git_add_remote="إضافة مستودع بعيد…",
        git_add_remote_title="إضافة مستودع بعيد",
        git_add_remote_name_prompt="اسم المستودع البعيد:",
        git_add_remote_url_prompt="رابط المستودع البعيد:",
        git_no_remotes="لا توجد مستودعات بعيدة",
        git_remove_remote="إزالة",
        git_confirm_remove_remote_message="إزالة المستودع البعيد '{0}'؟",
        git_create_tag_here="إنشاء وسم هنا…",
        git_stage_hunks="تجهيز الأجزاء…",
        git_unstage_hunks="إلغاء تجهيز الأجزاء…",
        git_stage_hunks_title="تجهيز الأجزاء",
        git_unstage_hunks_title="إلغاء تجهيز الأجزاء",
        git_set_identity="تعيين الهوية…",
        git_identity_title="هوية المستودع",
        git_identity_name_prompt="اسم المؤلف:",
        git_identity_email_prompt="بريد المؤلف الإلكتروني:",
        git_edit_remote_url="تعديل الرابط…",
        git_edit_remote_title="تعديل رابط المستودع البعيد",
        git_auth_error_title="فشل المصادقة",
        git_auth_error_hint=(
            "تعذّر على Git المصادقة مع المستودع البعيد لهذا المشروع.\n\n"
            "قم بإعداد بيانات اعتماد غير تفاعلية لهذا المشروع:\n"
            "• SSH: أضف مفتاحك إلى ssh-agent ووجّه المستودع البعيد إلى اسم مضيف SSH بديل.\n"
            "• HTTPS: قم بإعداد مساعد بيانات الاعتماد.\n\n"
            "يمكن لكل مشروع استخدام حساب مختلف عبر روابط مستودع بعيد خاصة به."
        ),
        git_set_token="تعيين رمز الوصول…",
        git_token_title="تعيين رمز الوصول",
        git_token_username_prompt="اسم المستخدم البعيد (مثل معرّف GitHub):",
        git_token_prompt="رمز الوصول الشخصي:",
        git_cherry_pick="انتقاء إلى الفرع الحالي",
        git_compare_head="مقارنة مع HEAD",
        git_file_history="سجل الملف",
        git_blame="التوصيف (blame)",
        git_checkout="سحب",
        git_remote_branches="الفروع البعيدة",
        git_no_remote_branches="لا توجد فروع بعيدة",
        git_rebase_onto="إعادة أساس الفرع الحالي على هذا",
        git_rebase_continue="متابعة إعادة الأساس",
        git_push_tags="دفع الوسوم",
        git_force_push="دفع قسري (مع الحماية)",
        git_pull_rebase="سحب (إعادة أساس)",
        git_discard_all="تجاهل كل التغييرات",
        git_clean_untracked="حذف الملفات غير المتعقبة",
        git_add_gitignore="إضافة إلى .gitignore",
        git_confirm_discard_all_message="تجاهل جميع التغييرات غير المودعة في هذا المستودع؟ لا يمكن التراجع.",
        git_confirm_clean_message="حذف جميع الملفات والمجلدات غير المتعقبة؟ لا يمكن التراجع.",
        git_confirm_force_push_message="دفع الفرع الحالي بشكل قسري (مع الحماية)؟ هذا يعيد كتابة السجل البعيد.",
        git_file_history_title="سجل الملف — {0}",
        git_blame_title="التوصيف — {0}",
        git_discard_hunks="تجاهل الأجزاء…",
        git_discard_hunks_title="تجاهل الأجزاء",
        git_stash_with_message="خبّئ مع رسالة…",
        git_stash_message_prompt="رسالة التخبئة",
        git_no_repo_title="لا يوجد تحكم بالمصدر",
        git_no_repo_message="هذا المجلد ليس مستودع git بعد.",
        git_init_repo="تهيئة مستودع",
        git_clone_repo="استنساخ مستودع…",
        git_clone_title="استنساخ مستودع",
        git_clone_url_prompt="رابط المستودع",
        mindspace_conversations="محادثات",
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
        conversations_folder='إنشاء مجلد "conversations"',
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

        # Unified settings dialog
        settings_all_mindspaces="جميع المساحات الذهنية",
        settings_this_mindspace="هذه المساحة الذهنية",
        settings_display="العرض",
        settings_file_access="الوصول إلى الملفات",
        settings_ai_backends="خلفيات الذكاء الاصطناعي",
        settings_ai_model="نموذج الذكاء الاصطناعي",
        settings_ai_tools="أدوات الذكاء الاصطناعي",
        settings_editor="المحرر",
        settings_terminal="الطرفية",
        settings_tabs="علامات التبويب",

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
        error_opening_preview="لا يمكن فتح المعاينة: {}",
        error_saving_mindspace_settings="فشل في حفظ الإعدادات: {}",
        error_saving_user_settings="فشل في حفظ إعدادات المستخدم: {}",
        cancel_conversation="هل أنت متأكد من أنك تريد إيقاف استجابة الذكاء الاصطناعي الحالية؟ سيؤدي هذا "
            "إلى إنهاء المحادثة الجارية وقد يؤدي إلى استجابات غير مكتملة.",

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
        preview_status="معاينة: {path}",
        diff_status="فرق: {path} | {rows} صفوف متغيرة",

        # Mindspace File Tree Edit Menu Errors and Options
        rename="إعادة التسمية",
        delete="حذف",
        open_in_editor="فتح في المحرر",
        open_in_preview="فتح في المعاينة",
        open_in_diff="فتح في الفرق",
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

        # Tool tips
        tooltip_copy_contents="نسخ جميع المحتويات",
        tooltip_save_contents="حفظ المحتويات إلى ملف",
        tooltip_copy_message="نسخ الرسالة إلى الحافظة",
        tooltip_save_message="حفظ الرسالة كملف ماركداون",
        tooltip_fork_message="تفريع المحادثة من هذه الرسالة",
        tooltip_delete_from_message="حذف المحادثة من هذه النقطة",
        tooltip_edit_message="تحرير هذه الرسالة",
        tooltip_edit_file="تحرير هذا الملف",
        tooltip_submit_message="إرسال الرسالة",
        tooltip_stop_message="إيقاف المعالجة الحالية",
        tooltip_settings_message="فتح إعدادات المحادثة",
        tooltip_attach_file="إرفاق ملف",
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

        # Fetch-models errors
        fetch_error_invalid_key="مفتاح API غير صالح — يرجى التحقق من مفتاحك والمحاولة مجدداً.",
        fetch_error_access_denied="تم رفض الوصول ({0}) — قد لا يملك مفتاحك صلاحية عرض قائمة النماذج.",
        fetch_error_not_found="نقطة نهاية النماذج غير موجودة ({0}) — تحقق من عنوان URL للواجهة البرمجية.",
        fetch_error_rate_limited="تم تجاوز حد الطلبات ({0}) — انتظر لحظة ثم أعد المحاولة.",
        fetch_error_server_error="خطأ في خادم المزود ({0}) — حاول مجدداً لاحقاً.",
        fetch_error_connection="تعذّر الاتصال — تحقق من عنوان URL وشبكتك.",
        fetch_error_timeout="انتهت مهلة الطلب — استغرق المزود وقتاً طويلاً للاستجابة.",
        fetch_error_generic="فشل: {0}"
    )
