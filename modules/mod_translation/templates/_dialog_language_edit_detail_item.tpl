{#
Params:
code
initial_lang_code
language
#}
{% with m.translation.language_list[code] != undefined
    as
    selected
%}
<div class="list-group-item">
    {% wire id=#form.code type="submit" postback={language_edit code=code} delegate="mod_translation" %}
    <form id="{{ #form.code }}" method="POST" action="postback" class="form-horizontal">
        {# Get values from form fields, in case this needs to be editable in the future. #}
        <input type="hidden" name="code" value="{{ code }}" />
        <input type="hidden" name="is_enabled" value="1" />
        <input type="hidden" name="fallback" value="{% if language.territory or language.script %}{{ language.language }}{% endif %}" />

        {% if not selected %}
            {% if initial_lang_code == undefined %}
                <button class="btn btn-primary pull-right" type="submit">{_ Add _}</button>
            {% endif %}
        {% else %}
            <span class="pull-right mod_translation-added">{_ Added _}</span>
        {% endif %}
        <h4>{{ language.name_en }}</h4>
        <table>
            <tr>
                <td>{_ Name in menu _}</td>
                <td>{{ language.name }}</td>
            </tr>
            {% if language.territory %}
                <tr>
                    <td>{_ Territory _}</td>
                    <td>{{ language.territory }}</td>
                </tr>
            {% endif %}
            {% if language.script %}
                <tr>
                    <td>{_ Script _}</td>
                    <td>{{ language.script }}</td>
                </tr>
            {% endif %}
            <tr>
                <td>{_ Language code in URL _}</td>
                <td>{{ code }}</td>
            </tr>
            {% if initial_lang_code and language.sub_languages %}
                <tr>
                    <td>{_ Available sub-languages _}</td>
                    <td>{% for lang_code in language.sub_languages %}
                        {% if m.translation.language_list[lang_code] %}
                            <span class="mod_translation-code mod_translation-added">{{ lang_code }}</span>
                        {% else %}
                            <a href="#" id="{{ #item.lang_code }}" class="mod_translation-code">{{ lang_code }}</a>
                            {% wire id=#item.lang_code
                                action={replace
                                    template="_dialog_language_edit_detail.tpl"
                                    target="dialog_language_edit_content"
                                    code=lang_code
                                    initial_lang_code=undefined
                                }
                            %}
                        {% endif %}
                        {% endfor %}
                    </td>
                </tr>
            {% endif %}
            {% if language.language != code %}
                <tr>
                    <td>{_ Fallback language _}</td>
                    <td>{{ language.language }}</td>
                </tr>
            {% endif %}
        </table>
    </form>
</div>
{% endwith %}
