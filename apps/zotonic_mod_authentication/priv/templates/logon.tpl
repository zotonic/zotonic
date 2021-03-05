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
    {% with page|default:q.p as page %}
        {% if page == "#reload" or error_code == 401 %}
            data-onauth="#reload"
        {% elseif {logon_done p=page}|url as logon_done_url %}
            data-onauth="{{ logon_done_url|escape }}"
        {% else %}
            data-onauth="{{ page|default:"#reload"|escape }}"
        {% endif %}
    {% endwith %}
{% endblock %}

{% block content_area %}
    {% include
        "_logon_config.tpl"
        logon_modal=0
    %}
{% endblock %}
