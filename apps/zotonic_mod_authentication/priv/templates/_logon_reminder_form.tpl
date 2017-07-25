{% if use_wire %}
    {#
        Use a wired postback when we are not using the default logon page with
        its logon_controller.
    #}
    {% wire id="password_reminder" type="submit" postback={reminder} delegate=`mod_authentication` %}
{% endif %}
<form id="password_reminder" method="post" action="postback">
    {% include form_fields_tpl %}
</form>
