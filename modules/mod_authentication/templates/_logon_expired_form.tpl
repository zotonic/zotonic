{% include "_logon_expired_form_title.tpl" %}

<div id="logon_error" class="alert alert-danger">
    {% include "_logon_error.tpl" reason=error_reason %}
</div>

{% wire id="password_expired" type="submit" postback={expired secret=secret username=username} delegate=`controller_logon` %}
<form id="password_expired" method="post" action="postback">
    {% include "_logon_reset_form_fields.tpl" %}
</form>

{% javascript %}
setTimeout(function() {
    z_init_postback_forms();
}, 100);
{% endjavascript %}
