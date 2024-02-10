{% with r_language|default:m.rsc[id].language|default:[z_language] as r_language %}
{% with edit_language|default:z_language as edit_language %}
{% with edit_language|member:r_language|if:edit_language:(r_language[1]) as edit_language %}
<ul class="nav nav-tabs language-tabs" id="{{ prefix }}-tabs">
    {% for code,lang in m.translation.language_list_editable %}
        <li class="nav-item tab-{{ code }}" {% if not code|member:r_language %}style="display: none"{% endif %} data-index="{{ forloop.counter0 }}" {% include "_language_attrs.tpl" language=code %}>
            <a href="#{{ prefix }}-{{ code }}" class="nav-link {% if code == edit_language %}active{% endif %}" data-bs-toggle="tab">{{ lang.name|default:code }}</a>
        </li>
    {% endfor %}

    {% all include "_admin_translation_tabs_extra.tpl" %}
</ul>
{% endwith %}
{% endwith %}
{% endwith %}
