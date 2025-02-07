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

    <p>{_ You can use two-factor authentication apps such as <a rel="noopener noreferrer" target="_blank" href="https://support.google.com/accounts/answer/1066447">Google Authenticator</a> or <a rel="noopener noreferrer" target="_blank" href="https://duo.com/product/trusted-users/two-factor-authentication/duo-mobile">Duo Mobile</a>. _}</p>
</div>

{% if m.acl.use.mod_admin_config %}
    <div class="widget">
        <h3 class="widget-header">{_ Default configuration _}</h3>
        <div class="widget-content">
            <p class="help-block">{_ Define who should use two-factor authentication, this is the default setting for all users. _}</p>

            <div class="form-group">
                <div>
                    {% wire id="opt2fa"
                        action={config_toggle module="mod_auth2fa" key="mode"}
                    %}
                    <label class="radio-inline">
                        <input name="2fa_mode" type="radio" id="opt2fa" value="0" {% if not m.auth2fa.mode %}checked="checked"{% endif %} />
                        {_ Optional _}
                    </label>
                    <p class="help-block">{_ The two-factor authentication can be added per user in the admin or (if available on the site) on their profile page. _}</p>
                </div>

                <div>
                    {% wire id="ask2fa"
                        action={config_toggle module="mod_auth2fa" key="mode"}
                    %}
                    <label class="radio-inline">
                        <input name="2fa_mode" type="radio" id="ask2fa" value="1" {% if m.auth2fa.mode  == 1 %}checked="checked"{% endif %} />
                        {_ Ask after logging in _}
                    </label>
                    <p class="help-block">{_ On the first page load a dialog appears for setting the two-factor authentication. _}</p>
                </div>

                <div>
                    {% wire id="nag2fa"
                        action={config_toggle module="mod_auth2fa" key="mode"}
                    %}
                    <label class="radio-inline">
                        <input name="2fa_mode" type="radio" id="nag2fa" value="2" {% if m.auth2fa.mode == 2 %}checked="checked"{% endif %} />
                        {_ Ask on every page _}
                    </label>
                    <p class="help-block">{_ On each page load a dialog appears for setting the two-factor authentication. _}</p>
                </div>

                <div>
                    {% wire id="force2fa"
                        action={config_toggle module="mod_auth2fa" key="mode"}
                    %}
                    <label class="radio-inline">
                        <input name="2fa_mode" type="radio" id="force2fa" value="3" {% if m.auth2fa.mode == 3 %}checked="checked"{% endif %} />
                        {_ Force two-factor authentication _}
                    </label>
                    <p class="help-block">{_ The user will be prompted to set the two-factor authentication before being allowed to log in. _}</p>
                </div>
            </div>
        </div>
    </div>
{% endif %}

{% if m.modules.active.mod_acl_user_groups and m.acl.use.mod_admin_config %}
    <div class="widget">
        <h3 class="widget-header">{_ User group configuration _}</h3>
        <div class="widget-content">
            <p class="help-block">{_ It is possible to force two-factor authentication for a specific user group, regardless of the setting above. _}</p>

            <table class="table table-hover widget-content">
                <thead>
                    <tr class="active">
                        <th class="col-sm-4">{_ User group _}</th>
                        <th class="col-sm-2 text-center">{_ Optional _}</th>
                        <th class="col-sm-2 text-center">{_ Ask after logging in _}</th>
                        <th class="col-sm-2 text-center">{_ Ask on every page _}</th>
                        <th class="col-sm-2 text-center">{_ Force two-factor authentication _}</th>
                    </tr>
                </thead>
                <tbody>
                {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                    {% with cg.id as cg_id %}
                        <tr>
                            <td class="active">{{ cg.indent }}{{ cg_id.title }}</td>

                            {% for option in [0, 1, 2, 3] %}
                                {% with [#cg.cg_id, option]|join:"-" as radio_id %}
                                    <td class="text-center">
                                        <input
                                            type="radio"
                                            name="{{ #cg.cg_id }}"
                                            id="{{ radio_id }}"
                                            {% if cg_id.acl_2fa == option %}checked{% endif %}
                                            value="{{ option }}"
                                            {% if not cg_id.is_editable %}disabled{% endif %}
                                        />
                                    </td>
                                    {% wire id=radio_id
                                        postback={auth2fa_ug id=cg_id}
                                        delegate=`mod_auth2fa`
                                    %}
                                {% endwith %}
                            {% endfor %}
                        </tr>
                    {% endwith %}
                {% endfor %}
                </tbody>
            </table>
        </div>
    </div>
{% endif %}
{% endblock %}
