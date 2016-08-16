{#
Params:
code
initial_lang_code
#}
<div id="dialog_language_edit_content">
    <div class="list-group mod_translation-language-list">
        {% for lang_code, lang_data in m.translation.main_languages %}
            <a class="list-group-item" id="{{ #item.lang_code }}">
                <i class="fa fa-chevron-right pull-right"></i>
                <h3>{{ lang_data.name_en }}</h3>
                {% with lang_data.sublanguages as sublanguages %}
                    <h4 class="mod_translation-codes text-muted">
                        {% with lang_code ++ sublanguages|element:1 as all %}
                            {% for lang_code1 in all %}
                                <span class="mod_translation-code {% if  m.translation.language_list_configured[lang_code1|as_atom] %}mod_translation-added{% endif %}">{{ lang_code1 }}</span>
                            {% endfor %}
                        {% endwith %}
                    </h4>
                {% endwith %}
            </a>
            {% wire id=#item.lang_code
                action={replace
                    template="_dialog_language_edit_detail.tpl"
                    target="dialog_language_edit_content"
                    code=lang_code
                    initial_lang_code=initial_lang_code
                }
            %}
        {% endfor %}
    </div>
</div>

{% javascript %}
$.dialogCenter();
$.dialogScrollTo(0);
{% endjavascript %}
