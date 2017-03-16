{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth tokens for _} {{ app.application_title|escape }}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ OAuth tokens for _} {{ app.application_title|escape }}</h2>
    <p>
        <a href="{% url admin_oauth %}">&lt; {_ Registered OAuth applications _}</a>
    </p>
</div>

<table>
    <tr>
        <th>{_ Consumer key _} &nbsp;</th>
        <td><tt>{{ app.consumer_key }}</tt></td>
    </tr>
    <tr>
        <th>{_ Consumer secret _} &nbsp;</th>
        <td>
            <tt>{{ app.consumer_secret }}</tt>
            &nbsp;
            <a class="btn btn-xs btn-danger" id="{{ #reset }}" href="#">{_ Reset Secret _}</a>
            {% wire id=#reset
                    action={confirm
                            title=_"Reset Secret"
                            text=_"Replacing the Consumer Secret with a new one will stop access for all consumers."
                            ok=_"Reset Secret"
                            postback={reset_consumer_secret app_id=app_id}
                            delegate=`controller_oauth_apps`}
            %}

        </td>
    </tr>
    <tr>
        <th>{_ Access to _}</th>
        <td>
            {% for perm in m.oauth_perms.humanreadable[app_id] %}
                {{ perm.desc|escape }}{% if not forloop.last %}<br/>{% endif %}
            {% endfor %}
        </td>
    </tr>
</table>

<br/>

{% if app.application_descr or app.application_uri %}
<p>
    {{ app.application_descr|escape }}
    {% if app.application_uri %}
        {% if app.application_descr %}<br/>{% endif %}
        <a href="{{ app.application_uri|escape }}" target="_blank">{{ app.application_uri|escape }}</a>
    {% endif %}
</p>
{% endif %}

{% if not tokens or tokens[1].token_secret %}
    <p>
        {_ You can add access for Anonymous users. _}
        {_ Anonymous users use the Consumer Key and Secret without extra tokens to gain access to the API. _}
        <a class="btn btn-xs btn-default" id="{{ #anon }}" href="#">{_ Allow Anonymous Access _}</a>
        {% wire id=#anon
                postback={ensure_anonymous_token app_id=app_id}
                delegate=`controller_oauth_apps`
        %}
    </p>
{% else %}
    <p class="alert alert-info">
        <b>{_ Anonymous users have access. _}</b>
        {_ Anybody with the Consumer Key and Secret has anonymous access to the API. _}

        <a class="btn btn-xs btn-default" id="{{ #revoke_anon }}" href="#">{_ Revoke _}</a>
        {% wire id=#revoke_anon
                action={confirm
                    text=_"Are you sure you want to delete this token?"
                    ok=_"Delete"
                    postback={delete_token token_id=tokens[1].id}
                    delegate=`controller_oauth_apps`
                }
        %}
    </p>
{% endif %}

{% if tokens %}
    <p>{_ The following users are using this application: _}</p>

    <table class="table" width="100%">
        <thead>
            <tr>
                <th>{_ User _}</th>
                <th>{_ In use since _}</th>
                <th>{_ Options _}</th>
            </tr>
        </thead>
        <tbody>
            {% for token in tokens %}
            <tr>
                <td>
                    {% if token.token_secret %}
                        <a href="{% url admin_edit_rsc id=token.user_id %}">{{ m.rsc[token.user_id].title }}</a>
                    {% else %}
                        <i>{_ Anonymous access _}</i>
                    {% endif %}
                </td>
                <td>{{ token.timestamp|date:"Y-d-m H:i" }}</td>
                <td>
                    <a class="btn btn-xs btn-default" id="{{ #revoke.id }}" href="#">{_ Revoke _}</a>
                    {% wire id=#revoke.id 
                            action={confirm
                                text=_"Are you sure you want to delete this token?"
                                ok=_"Delete"
                                postback={delete_token token_id=token.id}
                                delegate=`controller_oauth_apps`
                            }
                    %}
                </td>
            </tr>
            {% endfor %}
        </tbody>
    </table>
{% else %}
    <p class="text-muted">{_ This application is not yet used by anybody. _}</p>
{% endif %}

{% endblock %}

