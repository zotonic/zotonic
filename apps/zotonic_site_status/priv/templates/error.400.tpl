{% extends "error.tpl" %}

{% block content %}
    <div class="col-md-6 col-md-offset-3">
        <div class="jumbotron">
            <h1>{_ Powered by _} <span class="zotonic-logo"><em>Zotonic</em></span></h1>
            <p>
                {_ Welcome, visitor! The page you are
                currently looking at is the default page for a
                Zotonic web server. The fact that you are seeing
                this page could mean that the website you are
                trying to visit has not been configured
                correctly. _}
            </p>
            <p>
                {_ If you feel that you are here by mistake,
                please hit the "back" button in your browser or
                try a different address. _}
            </p>
        </div>
    </div>

    <div class="col-md-6 col-md-offset-3">
       <div class="panel panel-default">
            <div class="panel-body">
                <p style="text-align: center">
                    <a href="{% url zotonic_status %}" class="btn btn-default">{_ Manage this server _}</a>
                </p>
            </div>
        </div>
    </div>
{% endblock %}
