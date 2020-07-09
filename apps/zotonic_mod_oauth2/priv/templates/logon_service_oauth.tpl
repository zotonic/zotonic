{% extends "logon_service.tpl" %}

{% block content %}
    {% inherit %}

    {% worker
            name="oauth"
            src="js/zotonic.oauth.worker.js"
            args=worker_args
    %}
{% endblock %}
