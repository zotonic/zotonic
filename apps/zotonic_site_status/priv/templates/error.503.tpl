{% extends "error.tpl" %}

{% block html_head_extra %}
    <meta http-equiv="refresh" content="60">
{% endblock %}

{% block content %}
    <div class="col-md-6 col-md-offset-3">
        <div class="jumbotron">
            <h1>{_ Powered by _} <span class="zotonic-logo"><em>Zotonic</em></span></h1>
            <p>
                {_ The site you are trying to visit is experiencing some problems. Please try again later. _}
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
