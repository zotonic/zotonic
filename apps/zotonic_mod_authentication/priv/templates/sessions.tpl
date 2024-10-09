{% extends "base_simple.tpl" %}

{% block title %}{_ Session overview _}{% endblock %}

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

<h1>{_ Session overview _}</h1>

<p>
    {_ On this page you can see the recent logins and the active sessions for your user. _}<br>
</p>

<div class="row">
    <div class="col-md-12 col-lg-12 col-sm-12">
        <div class="panel panel-info">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Active sessions in the last hour _}</h3>
            </div>
            <div class="panel-body">
                <p class="text-muted">
                    {_ These sessions are currenly being used in a browser, logged in as your user, _}
                    <strong>#{{ m.acl.user.id }} ({{ m.acl.user.id.title }})</strong><br>
                    {_ Note: sessions will appear here only when actively used by a browser tab, it's recommended to keep this page open for a while. _}
                </p>

                <table id="active_sessions" class="table">
                    <thead>
                        <tr>
                            <th scope="col">{_ Session ID _}</th>
                            <th scope="col">{_ IP Address_}</th>
                            <th scope="col">{_ Last seen _}</th>
                            <th scope="col" width="50%">{_ User Agent _}</th>
                        </tr>
                    </thead>

                    <tbody id="active_sessions_rows">
                    </tbody>
                </table>
                {% button class="btn btn-danger" text=_"Interrupt all active sessions"
                    action={
                        dialog_open
                        title=_"Are you sure?"
                        template="_dialog_session_close.tpl"
                    }
                %}
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div class="col-md-12 col-lg-12 col-sm-12">
        <div class="panel panel-info">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Last known logons _}</h3>
            </div>
            <div class="panel-body">

                <p class="text-muted">
                    {_ Below are the last known logons from your user account. _}<br>
                    {_ Note: these are periodically deleted. _}
                </p>

                <table id="known_logons" class="table">
                    <thead>
                        <tr>
                            <th scope="col">#</th>
                            <th scope="col">{_ IP Address _}</th>
                            <th scope="col">{_ Logon time _}</th>
                            <th scope="col" width="50%">{_ User Agent _}</th>
                        </tr>
                    </thead>

                    <tbody>
                        {% for user_agent, ip_address, logon_time in m.identity[m.acl.user.id].logon_history %}
                            <tr>
                                <td scope="row">{{ forloop.counter }}</td>
                                <td>{{ ip_address }}</td>
                                <td>{{ logon_time|date:_"Y-m-d H:i:s" }}</td>
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

            let tr = document.getElementById(reported_session_id);
            if (tr) {
                tr.remove();
            }

            tr = document.createElement("tr");
            tr.id = reported_session_id;

            let td = document.createElement("td");
            if (reported_session_id == this_session_id) {
                let span = document.createElement('span');
                span.className = 'text-muted';
                span.textContent = reported_session_id;
                td.append(span);
                td.append(document.createElement('br'));
                let you = document.createElement('b');
                you.textContent = ' {{ _"(this is you)" }}';
                td.append(you);
            } else {
                let span = document.createElement('span');
                span.className = 'text-muted';
                span.textContent = reported_session_id;
                td.append(span);
            }
            tr.append(td);

            td = document.createElement("td");
            td.textContent = msg.payload.ip_address ?? '';
            tr.append(td);

            td = document.createElement("td");
            let date = new Date(msg.payload.timestamp);
            td.textContent = date.toLocaleTimeString();
            tr.append(td);

            td = document.createElement("td");
            td.className = 'text-muted';
            td.textContent = msg.payload.user_agent ?? '';
            tr.append(td);

            document.getElementById("active_sessions_rows").prepend(tr);
        });
});

setTimeout(() => cotonic.broker.publish("model/auth/post/check"), 500);
{% endjavascript %}

{% endblock %}

