{% extends "base.tpl" %}

{% block title %}{_ Sessions _}{% endblock %}

{% block content %}
    <h1>{_ Your current sessions _}</h1>

    <p>
        {_ Below are all your active sessions. _}
        {_ Note that if you close your browser a session stays active for some time. _}
    </p>
    <p>
        {_ To stop a session, click on ”Stop Session” or log out from the browser with the session. _}
    </p>

    <div id="logon-sessions">
        {% include "_logon_sessions.tpl" %}
    </div>

    <p>
        <br>
        <span class="text-muted">
            {_ Automatically refreshed every 10 seconds. _}
        </span>

        <button class="btn btn-default btn-xs" id="sess-reload">
            <span class="glyphicon glyphicon-refresh"></span> {_ Refresh Now _}
        </button>
        {% wire id="sess-reload"
                action={mask target="logon-sessions"}
                action={update target="logon-sessions" template="_logon_sessions.tpl" unmask_id="logon-sessions"}
        %}
    </p>

    {% wire name="sess-reload"
            action={update target="logon-sessions" template="_logon_sessions.tpl" unmask_id="logon-sessions"}
    %}
    {% javascript %}
        setInterval(function() {
            z_event("sess-reload");
        }, 10000);
    {% endjavascript %}

{% endblock %}
