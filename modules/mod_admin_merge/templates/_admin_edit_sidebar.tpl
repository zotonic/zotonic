{% extends "admin_edit_widget_std.tpl" %}

{# editing rsc username and password #}

{% block widget_title %}
    {_ Merge pages _}
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}

{% block widget_content %}
    <div class="form-group">
        <a href="{% url admin_merge_rsc id=id %}">{_ Merge with another page … _}</a>
    </div>
{% endblock %}
