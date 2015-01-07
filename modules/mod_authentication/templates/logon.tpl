{% extends "base.tpl" %}

{% block html_head_extra %}
{% lib "css/logon.css" %}
{% endblock %}

{% block title %}
{{ m.rsc.page_logon.title|default:[_"Log on to", " ", m.config.site.title.value|default:"Zotonic"] }}
{% endblock %}

{% block content_area %}
<div id="logon_box" style="display:none">

    <div id="logon_error">
	{% include "_logon_error.tpl" reason=error_reason %}
    </div>

    {% if m.rsc.page_logon.body %}
        <div class="alert">{{ m.rsc.page_logon.body }}</div>
    {% endif %}
    <ul id="logon_methods">
        {% all include "_logon_extra.tpl" %}
    </ul>

    <h2><span>{_ or _}</span></h2>

    <div id="logon_form">
    {% if zotonic_dispatch == `logon_reminder` %}
        {% include "_logon_password_reminder.tpl" %}
    {% elseif zotonic_dispatch == `logon_reset` %}
        {% include "_logon_password_reset.tpl" %}
    {% else %}
        {% include "_logon_form.tpl" %}
    {% endif %}
    </div>
</div>

<div class="logon_bottom">
    {% all include "_logon_link.tpl" %}
</div>

{# Hightlight the most recently used authentication method (if any) #}
{% if m.persistent.auth_method %}
    {% javascript %}
        $('#logon_methods li').css('opacity', '0.6');
        $('#logon_{{ m.persistent.auth_method }}').css('opacity', '1');
    {% endjavascript %}
{% endif %}

{# Use a real post for all forms on this page, and not AJAX or Websockets. This will enforce all cookies to be set correctly. #}
{% javascript %}
    z_only_post_forms = true;

    if ($('#logon_methods li').length == 0) {
        $('#logon_box > h2').hide();
    }
    $('#logon_box').fadeIn();
{% endjavascript %}

{# Set a listener on the session changes - needed for logon via external auth methods #}
{% javascript %}
    z_transport_delegate_register('reload', function(_status) {
        $('body').mask();
        z_transport("controller_logon", "ubf", { msg: "logon_redirect", page: '{{ q.p|escapejs }}' });
    });
{% endjavascript %}

{% endblock %}
