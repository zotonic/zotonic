{% wire id="password_reminder" type="submit" postback={reminder} delegate=`mod_authentication` %}
<form id="password_reminder" method="post" action="postback" data-onsubmit-topic="model/auth-ui/post/form/reminder">
    {% include form_fields_tpl %}
</form>
