{% extends "admin_base.tpl" %}

{% block title %}
{_ Admin log on _}
{% endblock %}

{% block bodyclass %}noframe{% endblock %}

{% block navigation %}
<div class="navbar navbar-fixed-top">
    <div class="navbar-inner">
        <div class="container-fluid">
            <a class="brand" href="http://{{ m.site.hostname }}" title="{_ visit site _}"><img alt="zotonic logo" src="/lib/images/admin_zotonic.png" width="106" height="20"></a>
        </div>
    </div>
</div>
{% endblock %}

{% block content %}
<div class="widget admin-logon">
    <h3 class="widget-header">{_ Log on to _} {{ m.config.site.title.value|default:"Zotonic" }}</h3>
    <div id="logon_box" class="widget-content">
        <div id="logon_error" class="alert alert-block alert-error"></div>
        {% include "_logon_form.tpl" page=page|default:"/admin" hide_title %}
    </div>
</div>
</div>
{% endblock %}
