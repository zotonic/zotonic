<input type="text" id="instagram_access_token" name="access_token" value="{{ m.config.mod_instagram.access_token.value|escape }}" class="form-control" />

<p class="help-block">
    {% if m.identity[m.acl.user].instagram %}
        <a href="#copy" id="{{ #copytoken }}">{_ Use the access token for the Instagram account _}: <b>{{ m.identity[m.acl.user].instagram.propb.username|escape }}</b></a>
        {% wire id=#copytoken 
                action={set_value target="instagram_access_token" value=m.identity[m.acl.user].instagram.propb.access_token}
        %}
    {% else %}
        {_ To obtain an access token you need to connect with Instagram. _}
    {% endif %}
</p>

{% with m.identity[m.acl.user].all_types as idn_types %}
    <ul class="list-unstyled social-login-list">
        {% include "_logon_extra_instagram.tpl" is_connect identity_types=idn_types %}
    </ul>
{% endwith %}
