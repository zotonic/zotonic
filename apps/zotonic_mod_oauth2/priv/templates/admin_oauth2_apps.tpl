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
        <tbody>
            {% for app in m.oauth2.apps %}
                {% with app.id as id %}
                    <tr id="{{ #app.id }}" class="clickable">
                        <td>{% if app.is_enabled %}√{% else %}-{% endif %}</td>
                        <td>{{ app.description|escape }}</td>
                        <td>
                            {% if app.user_id %}
                                <a href="{% url admin_edit_rsc id=app.user_id %}">
                                    {% include "_name.tpl" id=app.user_id %}
                                    ({{ app.user_id }})
                                </a>
                            {% endif %}
                        </td>
                        <td>{{ app.modified|date:_"d M Y, H:i" }}</td>
                        <td>{{ app.created|date:_"d M Y, H:i" }}</td>
                        <td>{{ app.token_count }}</td>
                        <td>
                            <button class="btn btn-default" id="{{ #edit.id }}">
                                {_ Edit App _}
                            </button>
                            {% wire id=#edit.id
                                    action={dialog_open
                                        template="_dialog_oauth2_app.tpl"
                                        title=_"Edit OAuth2 App"
                                        app_id=app.id
                                    }
                            %}
                            <a class="btn btn-default" href="{% url admin_oauth2_apps_tokens appid=app.id %}">
                                {_ Access tokens _}
                            </a>
                        </td>
                    </tr>
                {% endwith %}
            {% endfor %}
        </body>
    </table>
{% else %}
    <p class="alert alert-danger">
        <strong>{_ Not allowed. _}</strong>
        {_ Only admnistrators can view OAuth2 applications. _}
    </p>
{% endif %}

{% endblock %}
