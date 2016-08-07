{#
Params:
code
initial_lang_code
#}
<div id="dialog_language_edit_content">
    <div class="list-group mod_translation-language-list">
        {% for lang_code, lang in m.translation.language_list_all_stats %}
            {% if lang.is_main %}
                <a class="list-group-item" id="{{ #item.lang_code }}">
                    <i class="fa fa-chevron-right pull-right"></i>
                    <h3>{{ lang.name_en }}</h3>
                    <h4 class="mod_translation-codes text-muted">
                        {% with lang_code ++ lang.sub_languages as all %}
                            {% for lang_code in all %}
                                <span class="mod_translation-code {% if  m.config.i18n.language_list.list[lang_code] %}mod_translation-activated{% endif %}">{{ lang_code }}</span>
                            {% endfor %}
                        {% endwith %}
                    </h4>
                </a>
                {% wire id=#item.lang_code
                    action={replace
                        template="_dialog_language_edit_detail.tpl"
                        target="dialog_language_edit_content"
                        code=lang_code
                        initial_lang_code=initial_lang_code
                    }
                %}
            {% endif %}
        {% endfor %}
    </div>
</div>

{% javascript %}
$.dialogReposition()
{% endjavascript %}

<style>
.mod_translation-language-list h3,
.mod_translation-language-list h4 {
    font-weight: normal;
    margin: 0;
}
.mod_translation-language-list h3 {
    font-size: 1.1em;
    line-height: 1.3;
    color: #000;
}
.mod_translation-language-list h4 {
    font-size: 1em;
    line-height: 1.3;
    margin-top: .2em;
}
.mod_translation-language-list .mod_translation-codes {
    padding-right: 2em; /* make room for arrow */
}
.mod_translation-language-list .mod_translation-code {
    margin-right: .3em;
}
.mod_translation-language-list .fa-chevron-right {
    color: #ccc;
    height: 12px;
    width: 16px;
    text-align: right;
    display: inline-block;
    position: absolute;
    top: 50%;
    right: 10px;
    margin-top: -6px;
    margin-left: 0;
}
.mod_translation-language-list .mod_translation-activated {
    color: #00ab6b; /* light green */
}
</style>
