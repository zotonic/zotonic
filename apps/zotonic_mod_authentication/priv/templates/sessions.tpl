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
                <h3 class="panel-title">{_ Active sessions _}</h3>
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
                            <th scope="col">{_ User Agent _}</th>
                            <th scope="col">{_ IP Address _}</th>
                            <th scope="col">{_ Logon time _}</th>
                        </tr>
                    </thead>

                    <tbody>
                        {% for user_agent, ip_address, logon_time in m.identity[m.acl.user.id].logon_history %}
                            <tr>
                                <th scope="row">{{ forloop.counter }}</th>
                                <td>{{ user_agent }}</td>
                                <td>{{ ip_address }}</td>
                                <td>{{ logon_time|date:_"d M Y, H:i" }}</td>
                            </tr>
                        {% empty %}
                            <tr>
                                <th scope="row">{_ No recent known logons _}</th>
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
cotonic.broker.subscribe("bridge/origin/user/{{ m.acl.user.id }}/session_extended", function(msg) {
    reported_client_id = msg.payload;
    this_client_id = cotonic.mqtt_session.findSession().clientId;

    clientRow = document.getElementById(reported_client_id);
    if (clientRow) {
        // do nothing if the client is already listed
    } else {
        newRowTr = document.createElement("tr");
        newRowTr.id = reported_client_id;
        newRowTh = document.createElement("th");
        if (reported_client_id == this_client_id) {
            rowContent = document.createTextNode(this_client_id + " (this is you)")
        } else {
            rowContent = document.createTextNode(reported_client_id)
        };
        newRowTh.appendChild(rowContent);
        newRowTr.appendChild(newRowTh);
        document.getElementById("active_sessions_rows").appendChild(newRowTr);
    };
});

// trigger an auth refresh on page load, so we get our client to appear right away
cotonic.broker.publish("model/auth/post/refresh", {});
{% endjavascript %}

{% endblock %}

