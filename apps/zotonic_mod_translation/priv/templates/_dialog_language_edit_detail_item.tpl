{#
Params:
code
initial_lang_code
language
languages
#}
{% with m.translation.language_list_configured[code|as_atom] != undefined
    as
    selected
%}
<div class="list-group-item">
    {% wire id=#form.code type="submit" postback={language_add code=code} delegate="mod_translation" %}
    <form id="{{ #form.code }}" method="POST" action="postback" class="form-horizontal">
        {# Get values from form fields, in case this needs to be editable in the future. #}
        <input type="hidden" name="code" value="{{ code }}" />
        <input type="hidden" name="is_enabled" value="1" />

        {% if not selected %}
            {% if initial_lang_code == undefined %}
                <button class="btn btn-primary pull-right mod_translation-add-button" type="submit">{_ Add _}</button>
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
            {% if language.region %}
                <tr>
                    <td>{_ Region _}</td>
                    <td>{{ language.region }}</td>
                </tr>
            {% endif %}
            {% if language.script %}
                <tr>
                    <td>{_ Script _}</td>
                    <td>{{ language.script }}</td>
                </tr>
            {% endif %}
            {% with
                m.translation.default_language,
                language.language|as_atom
                as
                default_language,
                fallback_language
            %}
                {% if (fallback_language != code) or (default_language != code) %}
                    <tr>
                        <td>{_ Fallback language _}</td>
                        <td>
                            {% if fallback_language != code %}
                                {{ fallback_language }}
                                {% if not fallback_language|member:m.translation.enabled_language_codes %}
                                    <em class="mod_translation-warning">{_ not enabled _}</em>, {{ default_language }}
                                {% endif %}
                            {% else %}
                                {{ default_language }}
                                {% if not default_language|member:m.translation.enabled_language_codes %}
                                    <em class="mod_translation-warning">{_ not enabled _}</em>
                                {% endif %}
                            {% endif %}

                        </td>
                    </tr>
                {% endif %}
            {% endwith %}
            <tr>
                <td>{_ Language in URL _}</td>
                <td>{{ code }}</td>
            </tr>
            {% if initial_lang_code and language.sublanguages %}
                <tr>
                    <td>{_ Available sub-languages _}</td>
                    <td>{% for lang_code in language.sublanguages|element:1 %}
                        {% if m.translation.language_list_enabled[lang_code|as_atom] %}
                            <span class="mod_translation-code mod_translation-added">{{ lang_code }}</span>
                        {% else %}
                            <a href="#" id="{{ #item.lang_code }}" class="mod_translation-code">{{ lang_code }}</a>
                            {% wire id=#item.lang_code
                                action={replace
                                    template="_dialog_language_edit_detail.tpl"
                                    target="dialog_language_edit_content"
                                    code=lang_code
                                    initial_lang_code=undefined
                                    is_sublanguage=1
                                }
                            %}
                        {% endif %}
                        {% endfor %}
                    </td>
                </tr>
            {% endif %}
        </table>
    </form>
</div>
{% endwith %}
