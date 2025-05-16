{#
      This form posts all fields to the auth worker. The worker communicates with the
      controller_authentication using fetch, outside of the MQTT channel.
      The controller_authentication responds with the new authentication status, if a
      2FA code is needed or if there was any error. If succesfull the controller sets
      the z.auth cookie.

      Depending on the 'is_one_step_logon' configuration, the form requests the username
      and password field at once, or first the username and then (after some checks) the
      password and/or 2FA code.

      The auth-ui worker reacts on the changing auth state by re-rendering the logon
      template and updating the html.
#}
<form id="logon_form" class="z_logon_form {% if is_show_passcode %}z-logon-passcode{% endif %}" method="post" action="#" target="logonTarget"
      data-onsubmit-topic="model/auth/post/form/logon"
      {% if q.options.is_username_checked %}
            data-onsubmit-message="{{ %{
                  username: q.options.username|default:q.username,
                  step: 2
            }|to_json|escape }}"
      {% endif %}
      >
    {% include form_fields_tpl %}

    {% if q.authuser %}
          <input type="hidden" name="authuser" value="{{ q.authuser|escape }}">
    {% endif %}
</form>
<iframe src="" id="logonTarget" name="logonTarget" style="display:none"></iframe>
