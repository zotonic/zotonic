{#
Params:
code
initial_lang_code
#}
{% with
    m.translation.language_list_all_stats[code]
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
        {% if initial_lang_code == undefined %}
            {% for sub_code in language.sub_languages %}
                {% with m.translation.language_list_all_stats[sub_code] as sub_language %}
                    {% include "_dialog_language_edit_detail_item.tpl" code=sub_code initial_lang_code=initial_lang_code language=sub_language  %}
                {% endwith %}
            {% endfor %}
        {% endif %}
    </div>
</div>
{% endwith %}

{% javascript %}
$.dialogReposition()
{% endjavascript %}

<style>
#mod_translation_details_back {
    display: inline-block;
    margin-bottom: 1.5em;
}
.mod_translation-language-detail h4 {
    margin: .5em 0 1em 0;
}
.mod_translation-language-detail table {
    width: 100%;
}
.mod_translation-language-detail table td {
    padding: .3em 0;
    width: 50%;
    font-size: 1em;
    line-height: 1.3;
}
.mod_translation-language-detail table td:first-child {
    opacity: .6;
}
.mod_translation-language-detail .mod_translation-activated {
    color: #00ab6b; /* light green */
}
.mod_translation-language-detail .mod_translation-code {
    margin-right: .5em;
}
</style>
