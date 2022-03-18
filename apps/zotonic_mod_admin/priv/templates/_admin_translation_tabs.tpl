{% with r_language|default:m.rsc[id].language|default:[z_language] as r_language %}
{% with edit_language|default:z_language as edit_language %}
{% with edit_language|member:r_language|if:edit_language:(r_language[1]) as edit_language %}
<ul class="nav nav-tabs language-tabs" id="{{ prefix }}-tabs">
    {% for code,lang in m.translation.language_list_editable %}
        <li class="tab-{{ code }} {% if code == edit_language %}active{% endif %}" {% if not code|member:r_language %}style="display: none"{% endif %} data-index="{{ forloop.counter0 }}" {% include "_language_attrs.tpl" language=code %}><a href="#{{ prefix }}-{{ code }}" data-toggle="tab">{{ lang.name|default:code }}</a></li>
    {% endfor %}
</ul>
{% endwith %}
{% endwith %}
{% endwith %}

{% javascript %}
    {# Also switch all elements marked with the given language in the widget #}
    $('#{{ prefix }}-tabs').on('shown.bs.tab', function(e) {
        const showLang = e.target.closest('li').getAttribute('lang');
        const hideLang = e.relatedTarget.closest('li').getAttribute('lang');
        const widget = e.target.closest('.widget');
        if (widget) {
            $(widget).find(".widget-content-lang-" + hideLang).hide();
            $(widget).find(".widget-content-lang-" + showLang).show();
        }
    });
{% endjavascript %}


