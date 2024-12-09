{% extends "base_simple.tpl" %}

{% block title %}{_ Sessions overview _}{% endblock %}

{% block content %}
<div class="container">

<nav class="navbar navbar-default">
    <div class="container">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                <span class="sr-only">{_ Toggle navigation _}</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </button>
            <a class="navbar-brand" href="/">
                {{ m.site.title }} &mdash; <small>{{ m.req.host|escape }}</small>
            </a>
        </div>
    </div>
</nav>

<h1>{_ Sessions overview _}</h1>

<p>
    {_ On this page you can see the recent logins and the active sessions for your account: _}
    <a href="{{ m.acl.user.page_url }}"><strong>{{ m.acl.user.id.title }}</strong> (#{{ m.acl.user }})</a>
</p>

<div class="row">
    <div class="col-md-12 col-lg-12 col-sm-12">
        <div class="panel panel-info">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Active sessions in the last hour _}</h3>
            </div>
            <div class="panel-body">
                <p>
                    {_ Sessions that have been used by a browser in the last hour. _}<br>
                    {_ Note: sessions will appear here only when actively used by a browser tab, it’s recommended to keep this page open for a while. _}
                </p>

                <table id="active_sessions" class="table">
                    <thead>
                        <tr>
                            <th scope="col"></th>
                            <th scope="col">{_ Last seen _}</th>
                            <th scope="col">{_ IP Address_}</th>
                            <th scope="col">{_ Browser info _}</th>
                        </tr>
                    </thead>

                    <tbody id="active_sessions_rows">
                    </tbody>
                </table>

                <p>
                    {% button class="btn btn-danger" text=_"Interrupt all active sessions"
                        action={
                            dialog_open
                            title=_"Are you sure?"
                            template="_dialog_session_close.tpl"
                        }
                    %}
                </p>

                <p class="help-block">
                   <span class="glyphicon glyphicon-info-sign"></span> {_ Interrupted sessions might still be visible in the sessions overview, but will not be updated anymore. _}
                </p>
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div class="col-md-12 col-lg-12 col-sm-12">
        <div class="panel panel-info">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Last known logins _}</h3>
            </div>
            <div class="panel-body">

                <p class="help-block">
                    {_ Below are the last known logins to your account. _}<br>
                    {_ Note: these are periodically deleted. _}
                </p>

                <table id="known_logons" class="table">
                    <thead>
                        <tr>
                            <th scope="col">#</th>
                            <th scope="col">{_ Logon time _}</th>
                            <th scope="col">{_ IP Address _}</th>
                            <th scope="col" width="60%">{_ Browser info _}</th>
                        </tr>
                    </thead>

                    <tbody>
                        {% for user_agent, ip_address, logon_time in m.identity[m.acl.user.id].logon_history %}
                            <tr>
                                <td scope="row">{{ forloop.counter }}</td>
                                <td>
                                    {{ logon_time|date:_"Y-m-d" }}
                                    <span class="text-muted">{{ logon_time|date:_"H:i:s" }}</span>
                                </td>
                                <td>{{ ip_address }}</td>
                                <td class="text-muted">{{ user_agent|escape }}</td>
                            </tr>
                        {% empty %}
                            <tr>
                                <td scope="row">{_ No recent known logons _}</td>
                                <td>-</td>
                                <td>-</td>
                                <td>-</td>
                            </tr>
                        {% endfor %}
                    </tbody>
                </table>
            </div>
        </div>
    </div>
</div>

{% javascript %}
cotonic.broker.subscribe("bridge/origin/user/{{ m.acl.user.id }}/sessions/+sessionId", function(msg, bindings) {
    cotonic.broker.call("model/sessionId/get").then((sessionMsg) => {
            const reported_session_id = bindings.sessionId;
            const this_session_id = sessionMsg.payload;
            const date = new Date(msg.payload.timestamp);

            let tr = document.getElementById("session-" + reported_session_id);
            if (tr) {
                tr.remove();
            }

            tr = document.createElement("tr");
            tr.id = "session-" + reported_session_id;
            tr.setAttribute('timestamp', msg.payload.timestamp);

            let td = document.createElement("td");
            if (reported_session_id == this_session_id) {
                let span = document.createElement('span');
                span.className = 'glyphicon glyphicon-user';
                span.title = '{_ This is you _}';
                td.append(span);
            }
            tr.append(td);

            td = document.createElement("td");
            td.textContent = date.toLocaleTimeString();
            tr.append(td);

            td = document.createElement("td");
            td.textContent = msg.payload.ip_address ?? '';
            tr.append(td);

            td = document.createElement("td");
            td.className = 'text-muted';
            td.textContent = msg.payload.user_agent ?? '';
            tr.append(td);

            document.getElementById("active_sessions_rows").prepend(tr);

            // Sort table
            let rows = document.querySelectorAll("#active_sessions_rows tr");
            let rowsArray = Array.from(rows);
            let sorted = rowsArray.sort(sorter);
            function sorter(a,b) {
                return b.getAttribute('timestamp').localeCompare(a.getAttribute("timestamp"));
            }
            sorted.forEach(e => document.querySelector("#active_sessions_rows").appendChild(e));
        });
});

setTimeout(() => cotonic.broker.publish("model/auth/post/check"), 500);
{% endjavascript %}

{% endblock %}

