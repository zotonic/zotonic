{#
{% if use_wire %}
    { #
        Use a wired postback when we are not using the default logon page with
        its logon_controller.
    # }
    {% wire id="logon_form" type="submit" postback={logon} delegate=`mod_authentication` %}
{% endif %}
#}

<form id="logon_form" class="z_logon_form"
      method="post" action="#" target="logonTarget"
      onsubmit="cotonic.ui.on('auth/logon-form', true, event)"
      data-handler="username">
    <input type="hidden" name="page" value="{{ page|escape }}" />
    <input type="hidden" name="handler" value="username" />

    {% include form_fields_tpl %}
</form>
<iframe src="/lib/images/spinner.gif" id="logonTarget" name="logonTarget" style="display:none"></iframe>

{#
{% javascript %}
$("#logon_form form").unmask();
{% endjavascript %}
#}
