{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    <p class="help-block">
        {_ This block signals a stop in the flow. The user can't continue further and it is counted as a page break. _}
    </p>
{% endwith %}
{% endblock %}
