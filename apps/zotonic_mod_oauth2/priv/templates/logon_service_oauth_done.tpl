{% extends "logon_service.tpl" %}

{% block content %}
    <div id="oauth-status">
        <h1>
            {_ Authenticating... _}
            <img src='/lib/images/spinner.gif' class='loading'>
        </h1>
    </div>
    {% worker
            name="oauth"
            src="js/zotonic.oauth.worker.js"
            args=worker_args
    %}
{% endblock %}
