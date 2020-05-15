{% extends "admin_base.tpl" %}

{% block title %}Two-factor Authentication Configuration{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Two-factor authentication configuration _}</h2>

    {% if not m.acl.use.mod_admin_config %}
        <p class="alert alert-danger">
            {_ You need to be allowed to edit the system configuration to view or change the two-factor authentication configuration. _}
        </p>
    {% else %}
        <p>{_ Here you can define which users need or should use two-factor authentication when signing in. _}</p>
    {% endif %}

    <p>{_ You can use two-factor authentication apps such as <a href="https://support.google.com/accounts/answer/1066447">Google Authenticator</a> or <a href="https://duo.com/product/trusted-users/two-factor-authentication/duo-mobile\">Duo Mobile</a>." _}</p>
</div>

<div>
    {% if m.acl.use.mod_admin_config %}
        <h3>{_ Default configuration _}</h3>

        <p>{_ Define who should use two-factor authentication, this is the default setting for all users. _}</p>

        <div class="form-group">
            <div>
                {% wire id="opt2fa"
                    action={config_toggle module="mod_auth2fa" key="mode"}
                %}
                <label class="radio-inline">
                    <input name="2fa_mode" type="radio" id="opt2fa" value="0" {% if not m.auth2fa.mode %}checked="checked"{% endif %} />
                    {_ Optional _}
                </label>
            </div>

            <div>
                {% wire id="ask2fa"
                    action={config_toggle module="mod_auth2fa" key="mode"}
                %}
                <label class="radio-inline">
                    <input name="2fa_mode" type="radio" id="ask2fa" value="1" {% if m.auth2fa.mode  == '1' %}checked="checked"{% endif %} />
                    {_ Ask after signing in _}
                </label>
            </div>

            <div>
                {% wire id="force2fa"
                    action={config_toggle module="mod_auth2fa" key="mode"}
                %}
                <label class="radio-inline">
                    <input name="2fa_mode" type="radio" id="force2fa" value="2" {% if m.auth2fa.mode == '2' %}checked="checked"{% endif %} />
                    {_ Force two-factor authentication _}
                </label>
            </div>
        </div>

        {% if m.modules.active.mod_acl_user_groups %}
            <h3>{_ User group configuration _}</h3>

            <p>{_ It is possible to force two-factor authentication for a specific user group, regardless of the setting above. _}</p>
            <p>{_ Check the user groups for which two-factor authentication should be forced. _}</p>

            <ul class="list-unstyled">
                {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                    {% with cg.id as cg_id %}
                    <li>
                        <label class="checkbox-inline">
                            {{ cg.indent }}
                            <input type="checkbox" id="{{ #cg.cg_id }}" {% if cg_id.acl_2fa %}checked{% endif %} value="2" {% if not cg_id.is_editable %}disabled{% endif %}>
                            {{ cg_id.title }}
                        </label>
                        {% wire id=#cg.cg_id
                                postback={auth2fa_ug id=cg_id}
                                delegate=`mod_auth2fa`
                        %}
                    </li>
                    {% endwith %}
                {% endfor %}
            </ul>
        {% endif %}
    {% endif %}
</div>
{% endblock %}
