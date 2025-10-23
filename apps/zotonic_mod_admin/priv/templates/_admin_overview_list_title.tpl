{% block title %}
    <span {% include "_language_attrs.tpl" %}>{{ id.title|default:id.short_title|striptags|default:_"<em>Untitled</em>" }}</span>
{% endblock %}

{% block title_flags %}
    {% all catinclude "_admin_overview_list_flags.tpl" id %}
{% endblock %}
