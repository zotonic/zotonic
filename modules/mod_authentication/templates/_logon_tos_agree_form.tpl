{% wire id="tos_agree_form" type="submit"
        postback={logon_tos_agree user_id=user_id wire_args=wire_args secret=secret}
        delegate=`controller_logon`
%}
<form id="tos_agree_form" method="post" action="postback" class="z_logon_form">
    <input type="hidden" name="page" value="{{ q.page|default:page|escape }}" />

    {% include "_logon_tos_agree_form_fields.tpl" user_id=user_id %}
</form>
{% javascript %}
setTimeout(function() {
    z_init_postback_forms();
}, 100);
{% endjavascript %}
