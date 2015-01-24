{% extends "base.tpl" %}

{% block html_head_extra %}
{% lib
    "css/z.icons.css"
    "css/logon.css"
%}
{% endblock %}

{% block title %}
{{ m.rsc.page_logon.title|default:[_"Log on to", " ", m.config.site.title.value|default:"Zotonic"] }}
{% endblock %}

{% block page_class %}z-page-logon{% endblock %}

{% block content_area %}
    {% include "_logon.tpl" style_boxed=1 %}
{% endblock %}
