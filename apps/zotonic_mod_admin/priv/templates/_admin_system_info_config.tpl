<code>{{ file|escape }}</code>
{% if m.modules.active.mod_development %}
{% with m.development.config_source[file] as source %}
    {% if source %}
        {% include "_development_editor_button.tpl"
            editor_file=source.file editor_line=source.line editor_token=source.token
            editor_label=_"Edit" editor_title=_"Edit this configuration file" editor_compact %}
    {% endif %}
{% endwith %}
{% endif %}
