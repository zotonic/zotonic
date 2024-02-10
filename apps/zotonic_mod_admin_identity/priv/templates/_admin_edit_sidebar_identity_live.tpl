{% if m.acl.is_allowed.use.mod_admin_identity or (id == m.acl.user and id.is_editable) %}
    {% if id != 1 %}
        <div class="form-group">
            <div>
                {% button class="btn btn-outline-secondary"
                          action={dialog_set_username_password id=id}
                          text=_"Set username / password"
                %}
                {% if m.acl.is_admin and m.identity[id].is_user and id != m.acl.user %}
                    {% button class="btn btn-outline-secondary"
                              action={confirm
                                    text=_"Click OK to log on as this user. You will be redirected to the home page if this user has no rights to access the admin system."
                                    postback={switch_user id=id}
                                    delegate=`mod_admin_identity`}
                              text=_"Log on as this user" %}
                {% endif %}
            </div>
        </div>
    {% endif %}
{% endif %}

{% if m.modules.active.mod_translation %}
    <div class="form-group">
        <label class="control-label" for="pref_language">{_ Language _}</label>
        <div>
            <select class="form-control" id="pref_language" name="pref_language">
                <option></option>
                {% for code,lang in m.translation.language_list_enabled %}
                    <option {% if id.pref_language == code %}selected{% endif %} value="{{ code }}">{{ lang.name }}</option>
                {% endfor %}
            </select>
        </div>
    </div>
{% endif %}

{% if m.modules.active.mod_l10n %}
    <div class="form-group">
        <label class="control-label" for="pref_tz">{_ Timezone _}</label>
        <div>
            <select class="form-control" id="pref_tz" name="pref_tz">
                <option></option>
                {% include "_l10n_timezone_options.tpl" timezone=id.pref_tz %}
            </select>
        </div>
    </div>
{% endif %}

<div class="form-group">
    <div class="alert alert-info">
        {% with m.identity[id].user_info as user_info %}
            {% if user_info.username %}
                {% trans "Username <b>{username}</b>, last logon at {date}."
                         username=user_info.username|escape
                         date=user_info.visited|date:_"Y-m-d H:i"
                %}
            {% elseif m.identity[id].is_user %}
                {_ This person is also a user. _}
            {% else %}
                {_ This person is not yet a user. _}
            {% endif %}
        {% endwith %}
    </div>
</div>
