{% if m.acl.is_allowed.use.mod_development %}
<span class="development-editor-launch">
    {% if editor_inline %}<span data-editor-fallback>{{ editor_label }}</span>{% endif %}
    <button id="{{ #open_editor }}" type="button" hidden
            title="{% if editor_title %}{{ editor_title|escape }}{% else %}{_ Open in editor _}{% endif %}"
            class="btn {% if editor_inline %}btn-link btn-inline{% else %}btn-default btn-xs{% endif %}"
            data-editor-file="{{ editor_file|escape }}" data-editor-line="{{ editor_line|default:1|escape }}"
            data-editor-column="{{ editor_column|default:1|escape }}"
            data-editor-source="{{ editor_token|escape }}"
            data-editor-error="{_ Could not open the editor. Check the editor settings on the Development page. _}">
        {% if editor_label %}{{ editor_label }}{% else %}{_ Open in editor _}{% endif %}
    </button>
    {% if not editor_compact %}
        <a id="{{ #editor_settings }}" href="{% url admin_development %}#development-editor-settings" class="btn btn-link btn-inline">{_ Editor settings _}</a>
    {% endif %}
    <span role="status" class="text-danger" hidden></span>
</span>
{% javascript %}
    zotonicDevelopmentEditor.prepare(document.getElementById('{{ #open_editor }}'));
    {% if not editor_compact %}
        document.getElementById('{{ #editor_settings }}').addEventListener('click', (event) => {
            const overlay = event.currentTarget.closest('.modal-overlay');
            const closeButton = overlay && overlay.querySelector('.modal-overlay-close');
            if (closeButton) {
                closeButton.click();
            }
        });
    {% endif %}
{% endjavascript %}
{% endif %}
