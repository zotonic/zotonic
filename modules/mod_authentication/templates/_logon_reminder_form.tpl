{% wire id="password_reminder" type="submit" postback={reminder} delegate=`controller_logon` %}
<form id="password_reminder" method="post" action="postback">
    {% include form_fields_tpl %}
</form>
