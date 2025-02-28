{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth2 Clients - Tokens _}{% endblock %}

{% block content %}

{% with m.oauth2_consumer.consumers[q.appid] as app %}

<div class="admin-header">
    <h2>{% trans "OAuth2 Clients &gt; Tokens to access {domain}" domain=app.domain|escape %}</h2>

    <p>{% trans "Tokens to access or authenticate with <b>{domain}</b> of behalf of a user." domain=app.domain|escape %}</p>

    <p>
        <a href="{% url admin_oauth2_consumers %}">{_ OAuth2 Clients _}</a>
        &gt; {{ app.description|escape }}
    </p>
</div>

{% if m.acl.is_admin %}
    <div class="well z-button-row">
        <button id="token-new" class="btn btn-primary">
            {_ Add a new token _}
        </button>
        {% wire id="token-new"
                action={dialog_open
                    title=_"Add a token"
                    template="_dialog_oauth2_consumer_token_new.tpl"
                    app_id=app.id
                }
        %}
    </div>
{% endif %}

{% if m.acl.is_admin %}
    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th>{_ User _}</th>
                <th>{_ Valid till _}</th>
                <th>{_ Created on _}</th>
                <th></th>
            </tr>
        </thead>
        <tbody>
            {% for token in m.oauth2_consumer.consumers[app.id].tokens %}
                {% with token.id as id %}
                    <tr id="{{ #token.id }}" class="clickable">
                        <td>
                            {% if token.user_id %}
                                <a href="{% url admin_edit_rsc id=token.user_id %}">
                                    {% include "_name.tpl" id=token.user_id %}
                                    ({{ token.user_id }})
                                </a>
                            {% endif %}
                        </td>
                        <td>{{ token.expires|date:_"d M Y, H:i" }}</td>
                        <td>{{ token.created|date:_"d M Y, H:i" }}</td>
                        <td>
                            <button id="{{ #delete.id }}" class="btn btn-xs btn-default">
                                {_ Delete _}
                            </button>
                            {% wire id=#delete.id
                                    action={confirm
                                        text=_"Are you sure you want to delete this client token?"
                                        ok=_"Delete"
                                        is_danger
                                        postback={oauth2_consumer_token_delete app_id=app.id id=token.id}
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
