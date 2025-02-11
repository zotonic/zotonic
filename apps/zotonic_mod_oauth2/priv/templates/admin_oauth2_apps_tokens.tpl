{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth2 Application - Tokens _}{% endblock %}

{% block content %}

{% with m.oauth2.apps[q.appid] as app %}

<div class="admin-header">
    <h2>{_ OAuth2 Applications &gt; Tokens _}</h2>

    <p>{_ Tokens to let other systems access this website of behalf of an user. _}</p>

    <p>
        <a href="{% url admin_oauth2_apps %}">{_ OAuth2 Applications _}</a>
        &gt; {{ app.description|escape }}
    </p>
</div>

{% if m.acl.is_admin %}
    <div class="well z-button-row">
        <button id="token-new" class="btn btn-primary">
            {_ Make a new App token _}
        </button>
        {% wire id="token-new"
                action={dialog_open
                    title=_"Add a token"
                    template="_dialog_oauth2_app_token_new.tpl"
                    app_id=app.id
                }
        %}
    </div>
{% endif %}

{% if m.acl.is_admin %}
    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th>{_ Permission _}</th>
                <th>{_ User _}</th>
                <th>{_ Valid till _}</th>
                <th width="30%">{_ Note _}</th>
                <th>{_ Created on _}</th>
                <th></th>
            </tr>
        </thead>
        <tbody>
            {% for token in m.oauth2.apps[app.id].tokens %}
                {% with token.id as id %}
                    <tr id="{{ #token.id }}" class="clickable">
                        <td>
                            {% if token.is_read_only %}
                                <span class="glyphicon glyphicon-eye-open" title="{_ Read access _}"></span>
                            {% else %}
                                <span title="{_ Read &amp; write access _}">
                                    <span class="glyphicon glyphicon-eye-open"></span>
                                    <span class="glyphicon glyphicon-pencil"></span>
                                </span>
                            {% endif %}
                        </td>
                        <td>
                            {% if token.user_id %}
                                <a href="{% url admin_edit_rsc id=token.user_id %}">
                                    {% include "_name.tpl" id=token.user_id %}
                                    ({{ token.user_id }})
                                </a>
                            {% endif %}
                        </td>
                        <td>{{ token.valid_till|date:_"d M Y, H:i" }}</td>
                        <td>{{ token.note|escape }}</td>
                        <td>{{ token.created|date:_"d M Y, H:i" }}</td>
                        <td>
                            <button id="{{ #delete.id }}" class="btn btn-xs btn-default">
                                {_ Delete _}
                            </button>
                            {% wire id=#delete.id
                                    action={confirm
                                        text=_"Are you sure you want to delete this access token?"
                                        ok=_"Delete"
                                        is_danger
                                        postback={oauth2_app_token_delete token_id=token.id}
                                        delegate=`mod_oauth2`
                                    }
                            %}
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
{% endwith %}

{% endblock %}
