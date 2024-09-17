{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Mailing list _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-mailinglist{% endblock %}

{% block widget_content %}
    <div class="form-group">
        <a class="btn btn-default btn-sm" title="{_ Send this page to a mailinglist and view mailinglist statistics. _}" href="{% url admin_mailing_status id=id %}">
            <i class="glyphicon glyphicon-envelope"></i>
            {_ Go to mailinglist page _}
        </a>

        <a class="btn btn-default btn-sm" target="_mailing" id="mailing-preview-btn" title="{_ View this page as mailing. _}" href="{% url admin_mailing_preview id=id %}">
            <i class="glyphicon glyphicon-eye-open"></i>
            {_ Preview mailing _}
        </a>
        {% javascript %}
            document.getElementById("mailing-preview-btn").addEventListener("click",
                (e) => {
                    window.open(e.target.getAttribute('href'), 'mailingpreview', 'width=800,height=800');
                    e.preventDefault();
                });
        {% endjavascript %}

        <a id="{{ #mailnow }}" class="btn btn-default btn-sm" title="{_ Send this page to a single address _}" href="#mail">
            <i class="glyphicon glyphicon-envelope"></i>
            {_ Mail _}
        </a>
        {% wire id=#mailnow action={dialog_open template="_dialog_mailing_testaddress.tpl" title=_"Send test to address" id=id} %}
    </div>
{% endblock %}
