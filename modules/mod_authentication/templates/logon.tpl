{% extends "base.tpl" %}

{% block html_head_extra %}
{% lib "css/logon.css" %}
{% endblock %}

{% block title %}
{_ Log on to _} {{ m.config.site.title.value|default:"Zotonic" }}
{% endblock %}

{% block content_area %}

<div id="logon_box">
    
    <div id="logon_error">
	{% include "_logon_error.tpl" reason=error_reason %}
    </div>

    <div id="logon_form">
        {% if q.f == "reminder" %}
        {% include "_logon_password_reminder.tpl" %}
        {% elseif q.f == "password_reset" %}
        {% include "_logon_password_reset.tpl" %}
        {% else %}
        {% include "_logon_form.tpl" %}
        {% endif %}
    </div>    
</div>

<div class="logon_bottom">
    <ul id="logon_methods">
        {% all include "_logon_extra.tpl" %}
    </ul>

    {% all include "_logon_link.tpl" %}

</div>

{# Use a real post for all forms on this page, and not AJAX or Websockets. This will enforce all cookies to be set correctly. #}
{% javascript %}
z_only_post_forms = true;
{% endjavascript %}

{% endblock %}
