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
    {% if q.zotonic_dispatch == 'logon' and m.acl.user %}
        <h1>{_ You are signed in _}</h1>

        <p>{_ Your username is _} <b>{{ m.identity[m.acl.user].username|escape }}</b></h1>
        <ul class="list-unstyled">
            <li>
                <a href="{% url logoff %}">{_ Sign out _}</a>
            </li>
            <li>
                <a href="{% url home %}">{_ Go to the home page _}</a>
            </li>
            {% if m.acl.user %}
                <li>
                    <a href="{{ m.acl.user.page_url }}">{_ Go to the your profile page _}</a>
                </li>
            {% endif %}
            {% if m.acl.is_allowed.use.mod_admin %}
                <li>
                    <a href="{% url admin %}">{_ Go to the admin _}</a>
                </li>
            {% endif %}
        </ul>
    {% else %}
        {% include
            "_logon_config.tpl"
            logon_modal=0
        %}
    {% endif %}
{% endblock %}
