{% extends "base.tpl" %}

{% block html_head_extra %}
{% lib "css/logon.css" %}
{% endblock %}

{% block title %}
{_ Log on to _} {{ m.config.site.title.value|default:"Zotonic" }}
{% endblock %}

{% block page_class %}z-page-logon{% endblock %}

{% block content_area %}
    {% include "_logon.tpl" %}
{% endblock %}
