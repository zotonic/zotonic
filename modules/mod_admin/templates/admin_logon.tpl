{% extends "admin_base.tpl" %}

{% block title %}
{_ Admin log on _}
{% endblock %}

{% block bodyclass %}noframe{% endblock %}

{% block navigation %}
<div class="navbar navbar-branded navbar-fixed-top">
    <div class="navbar-header">
        <a class="navbar-brand" href="http://{{ m.site.hostname }}" title="{_ visit site _}"><span class="zotonic-logo"><em>Zotonic</em></span></a>
    </div>
</div>
{% endblock %}

{% block content %}
<div class="widget admin-logon">
    <div class="widget-header">{_ Log on to _} {{ m.config.site.title.value|default:"Zotonic" }}</div>
    <div class="widget-content">
        {% include
            "_logon_modal.tpl"
            page=page|default:"/admin"
            logon_context="admin_logon"
        %}
    </div>
</div>
</div>
{% endblock %}
