{% if m.auth2fa[id].is_totp_enabled %}
    <p><span class="fa fa-check-circle"></span> {_ Two-factor authentication is enabled for this account. _}</p>

    <p>
      {% if m.auth2fa[id].is_allowed_reset %}
          {% button class="btn btn-default"
                    text=_"Remove two-factor..."
                    postback={auth2fa_remove_confirm id=id}
                    delegate=`mod_auth2fa`
          %}
      {% else %}
          {_ Only the user themselves or an admin can remove the two-factor authentication. _}
      {% endif %}
    </p>
{% else %}
    <p class="text-danger"><span class="fa fa-warning"></span> {_ Two-factor authentication is not enabled for this account. _}</p>

    {% if id == m.acl.user %}
        <p>
          {% button class="btn btn-default"
                  text=_"Enable two-factor authentication..."
                  postback={request_2fa}
                  delegate=`mod_auth2fa`
          %}
        </p>
    {% endif %}

{% endif %}

{% if id == 1 %}
    <p class="text-muted">{_ Only the admin can change the two-factor authentication of the admin. _}</p>
{% endif %}
