{#
Params:
code
initial_lang_code
is_sublanguage
#}
{% with
    m.translation.all_languages
    as
    languages
%}
{% with
    language|default:languages[code|to_binary]
    as
    language
%}
<div id="dialog_language_edit_content">
    {% if initial_lang_code == undefined %}
        <a href="" id="mod_translation_details_back">{_ All languages _}</a>
        {% wire id="mod_translation_details_back"
            action={replace
                template="_dialog_language_edit_list.tpl"
                target="dialog_language_edit_content"
                initial_lang_code=initial_lang_code
            }
        %}
    {% endif %}
    <div class="list-group mod_translation-language-detail">
        {% include "_dialog_language_edit_detail_item.tpl" code=code initial_lang_code=initial_lang_code language=language  %}
        {% with language.sublanguages as sublanguages %}
            {% if initial_lang_code == undefined %}
                {% if sublanguages or is_sublanguage %}
                    {% include "_dialog_language_edit_detail_sublanguage_note.tpl" %}
                {% endif %}
                {% for sub_code in sublanguages|element:1 %}
                    {% with languages[sub_code] as sub_language %}
                        {% include "_dialog_language_edit_detail_item.tpl" code=sub_code initial_lang_code=initial_lang_code language=sub_language languages=languages  %}
                    {% endwith %}
                {% endfor %}

            {% endif %}
        {% endwith %}
    </div>
</div>
{% endwith %}
{% endwith %}

{% javascript %}
$.dialogCenter();
$.dialogScrollTo(0);
{% endjavascript %}
