{% if m.acl.is_allowed.use.mod_admin_identity or id == m.acl.user %}
    <div class="form-group">
        <div>
            {% button class="btn btn-default"
                      action={dialog_set_username_password id=id}
                      text=_"Set username / password"
            %}
            {% if m.acl.is_admin and m.identity[id].is_user and id != m.acl.user and id != 1 %}
                {% button class="btn btn-default"
                          action={confirm
                                text=_"Click OK to log on as this user. You will be redirected to the home page if this user has no rights to access the admin system."
                                postback={switch_user id=id}
                                delegate=`mod_admin_identity`}
                          text=_"Log on as this user" %}
            {% endif %}
        </div>
    </div>
{% endif %}

{% if m.modules.active.mod_translation %}
    <div class="form-group">
        <label class="control-label" for="pref_language">{_ Language _}</label>
        <div>
            <select class="form-control" id="pref_language" name="pref_language">
                <option></option>
                {% for code,lang in m.translation.language_list_enabled %}
                    <option {% if id.pref_language == code %}selected{% endif %} value="{{ code }}">{{ lang.name }}</a>
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
        {% if m.identity[id].is_user %}
            {_ This person is also a user. _}
        {% else %}
            {_ This person is not yet a user. _}
        {% endif %}
    </div>
</div>