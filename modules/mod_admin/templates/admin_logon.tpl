{% extends "admin_base.tpl" %}

{% block title %}
{_ Admin log on _}
{% endblock %}

{% block bodyclass %}noframe{% endblock %}

{% block navigation %}
<div class="navbar navbar-default navbar-fixed-top">
    <div class="navbar-header">
        <a class="navbar-brand" href="http://{{ m.site.hostname }}" title="{_ visit site _}"><span class="zotonic-logo"></span></a>
    </div>
</div>
{% endblock %}

{% block content %}
<div class="widget admin-logon">
    <h3 class="widget-header">{_ Log on to _} {{ m.config.site.title.value|default:"Zotonic" }}</h3>
    <div id="logon_box" class="widget-content">
        <div id="logon_error" class="alert alert-block alert-danger"></div>
        {% include "_logon_form.tpl" page=page|default:"/admin" hide_title %}
    </div>
</div>
</div>
{% endblock %}
