{# Table used in admin_translation.tpl -- reloaded on config changes #}
{% with m.translation.default_language as default_code %}
    {% for code, lang in m.translation.language_list_configured %}
        <tr id="{{ #li.code }}" class="{% if not lang.is_enabled %}unpublished{% endif %}">
            <td>
                <div class="drag-handle"></div>
            </td>
            <td>
                <input type="radio" id="{{ #enabled.code }}" name="status-{{ code }}" value="enabled"
                {% if lang.is_enabled %}checked{% endif %}>
            </td>
            <td>
                <input type="radio" id="{{ #editable.code }}" name="status-{{ code }}" value="editable"
                {% if not lang.is_enabled and lang.is_editable %}checked{% endif %}>
            </td>
            <td>
                <input type="radio" id="{{ #disabled.code }}" name="status-{{ code }}" value="disabled"
                {% if not lang.is_enabled and not lang.is_editable %}checked{% endif %}>
            </td>
            <td>
                {{ lang.name_en|default:"-" }}
            </td>
            <td>
                {{ code|default:"-" }}
            </td>
            <td>
                {{ lang.region|default:"<span class='text-muted'>-</span>" }}
            </td>
            <td>
                {{ lang.script|default:"<span class='text-muted'>-</span>" }}
            </td>
            <td>
                <div class="pull-right">
                    {% button class="btn btn-default btn-xs" text=_"Remove"
                        action={
                            dialog_open
                            title=_"Remove language"|append:": "|append:lang.name_en
                            template="_dialog_language_delete.tpl"
                            code=code
                            lang=lang
                        }
                    %}
                    {% button class="btn btn-default btn-xs"text=_"Details"
                        action={
                            dialog_open
                            title=_"Language"|append:": "|append:lang.name_en
                            template="_dialog_language_edit.tpl"
                            code=code
                        }
                    %}
                </div>
            </td>
        </tr>
    {% empty %}
        <tr>
            <td colspan="4">
                {_ No languages configured. _}
            </td>
        </tr>
    {% endfor %}
{% endwith %}
