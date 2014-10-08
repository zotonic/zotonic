{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Mailing list _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-mailinglist{% endblock %}

{% block widget_content %}

    <div class="form-group">
        <a class="btn btn-default btn-sm" title="{_ Send this page to a mailinglist and view mailinglist statistics. _}" href="{% url admin_mailing_status id=id %}">
            <i class="glyphicon glyphicon-envelope"></i>
            {_ Go to mailinglist page _}
        </a>
    </div>
{% endblock %}
