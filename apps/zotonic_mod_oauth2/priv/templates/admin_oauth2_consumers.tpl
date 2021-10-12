{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth2 Consumer Tokens _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ OAuth2 Consumer Tokens _}</h2>

    <p>
        {_ Registration of tokens to access remote websites. _}<br>
        {_ With a consumer token this website can: _}
    </p>

    <ul>
        <li>{_ Import content from the remote website to this website. _}</li>
        <li>{_ Allow users registered on the remote website to authenticate on this website. _}</li>
    </ul>
</div>

{% if m.acl.is_admin %}
    <div class="well z-button-row">
        <button id="app-new" class="btn btn-primary">
            {_ Register a new website _}
        </button>
        {% wire id="app-new"
                action={dialog_open
                    title=_"Register a new website"
                    template="_dialog_oauth2_consumer_new.tpl"
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
                <th>{_ Name _}</th>
                <th>{_ Domain _}</th>
                <th width="25%">{_ Description _}</th>
                <th>{_ Auth _}</th>
                <th>{_ Import _}</th>
                <th>{_ Added by _}</th>
                <th>{_ Created on _}</th>
                <th>{_ User count _}</th>
                <th></th>
            </tr>
        </thead>
        <tbody>
            {% for app in m.oauth2_consumer.consumers %}
                {% with app.id as id %}
                    <tr id="{{ #app.id }}" class="clickable">
                        <td>{{ app.name|escape }}</td>
                        <td>{{ app.domain|escape }}</td>
                        <td>{{ app.description|escape }}</td>
                        <td>{% if app.is_use_auth %}√{% else %}-{% endif %}</td>
                        <td>{% if app.is_use_import %}√{% else %}-{% endif %}</td>
                        <td>
                            {% if app.user_id %}
                                <a href="{% url admin_edit_rsc id=app.user_id %}">
                                    {% include "_name.tpl" id=app.user_id %}
                                    ({{ app.user_id }})
                                </a>
                            {% endif %}
                        </td>
                        <td>{{ app.created|date:_"d M Y, H:i" }}</td>
                        <td>{{ app.token_count }}</td>
                    </tr>
                    {% wire id=#app.id
                            action={dialog_open
                                template="_dialog_oauth2_consumer.tpl"
                                title=_"Edit OAuth2 Consumer"
                                app_id=app.id
                            }
                    %}
                {% endwith %}
            {% endfor %}
        </body>
    </table>
{% else %}
    <p class="alert alert-danger">
        <strong>{_ Not allowed. _}</strong>
        {_ Only admnistrators can view OAuth2 consumers. _}
    </p>
{% endif %}

{% endblock %}
