{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth2 Application _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ OAuth2 Applications _}</h2>

    <p>{_ These applications are used by 3rd parties to: _}</p>

    <ul>
        <li>{_ Access content on this website._}</li>
        <li>{_ Ask users on this website for their credentials to authenticate on their website or App. _}</li>
    </ul>

    <p>{_ To use one of these applications, the App ID and secret must be registered with the 3rd party. _}</p>
</div>

{% if m.acl.is_admin %}
    <div class="well z-button-row">
        <button id="app-new" class="btn btn-primary">
            {_ Make a new App _}
        </button>
        {% wire id="app-new"
                action={dialog_open
                    title=_"Make a new App"
                    template="_dialog_oauth2_app_new.tpl"
                }
        %}
    </div>

    {% if q.app_id %}
        {% wire action={dialog_open
                    title=_"Edit App"
                    template="_dialog_oauth2_app.tpl"
                    app_id=q.app_id|to_integer
                }
        %}
    {% endif %}

{% endif %}

{% if m.acl.is_admin %}
    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th>{_ Enabled _}</th>
                <th width="30%">{_ Description _}</th>
                <th>{_ User _}</th>
                <th>{_ Modified on _}</th>
                <th>{_ Created on _}</th>
                <th>{_ User count _}</th>
                <th></th>
            </tr>
        </thead>
        <tbody id="apps-list">
            {% include "_oauth2_apps_list.tpl" %}
        </tbody>
    </table>
{% else %}
    <p class="alert alert-danger">
        <strong>{_ Not allowed. _}</strong>
        {_ Only admnistrators can view OAuth2 applications. _}
    </p>
{% endif %}

{% endblock %}
