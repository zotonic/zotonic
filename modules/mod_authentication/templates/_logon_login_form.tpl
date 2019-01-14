<iframe src="/lib/images/spinner.gif" id="logonTarget" name="logonTarget" style="display:none"></iframe>

{% wire id="logon_form" type="submit" postback={logon} delegate=`controller_logon` %}
<form id="logon_form" method="post" action="postback" class="z_logon_form" target="logonTarget">
    <input type="hidden" name="page" value="{{ page|escape }}" />
    <input type="hidden" name="handler" value="username" />

    {% include form_fields_tpl %}
</form>

{% javascript %}
$("#logon_form form").unmask();
{% endjavascript %}
