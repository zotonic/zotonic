{% extends "base.tpl" %}

{% block html_head_extra %}
{% lib
    "css/z.icons.css"
    "css/logon.css"
%}
{% endblock %}

{% block title %}
{{ m.rsc.page_logon.title|default:[_"Sign in to", " ", m.site.title|default:"Zotonic"] }}
{% endblock %}

{% block html_attr %}
    {% if {logon_done p=q.p}|url as logon_done_url %}
        data-onauth="{{ logon_done_url|escape }}"
    {% else %}
        data-onauth="{{ q.p|default:"#reload"|escape }}"
    {% endif %}
{% endblock %}

{% block content_area %}
    {% include
        "_logon_config.tpl"
        logon_modal=0
    %}
{% endblock %}
