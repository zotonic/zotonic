{% extends "admin_base.tpl" %}

{% block title %}{_ Development - Function Call Tracing _}{% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Function Call Tracing _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Function Call Tracing _}</h2>
    <p>{_ Live tracing of function calls on the server. _}</p>
</div>

{% if m.acl.is_allowed.use.mod_development %}
    {% if m.acl.user == 1 %}
        {% if m.development.function_tracing_enabled %}
            <div class="well">
                <p class="help-block">
                    {_ Enter the module and function you want to trace. If no function is given, all functions in the module will be traced. Enter how many calls to trace before stopping. _}
                </p>

                {% wire id="starttrace"
                        type="submit"
                        postback=`function_trace`
                        delegate=`mod_development`
                %}
                <form id="starttrace" class="form form-inline" action="postback">
                    <input class="form-control" autofocus type="text" name="module" placeholder="module" value="" required>
                    <input class="form-control" type="text" name="function" placeholder="function" value="">
                    <input class="form-control" type="number" name="count" id="count" placeholder="count" value="100" style="width: 10ch" required>
                    {% validate id="count" type={presence} type={numericality minimum=1 maximum=1000} %}
                    <button type="submit" class="btn btn-primary">{_ Start Tracing _}</button>
                </form>
            </div>

            <div class="widget">
                <div class="widget-content">
                    <pre id="trace" style="white-space: pre-wrap;">Trace output will appear here...</pre>
                </div>
            </div>
        {% else %}
            <p class="alert alert-info">
                {_ Function call tracing has been disabled. Use the following to enable function call tracing: _}<br>
                <code>bin/zotonic setconfig zotonic function_tracing_enabled true</code>
            </p>
        {% endif %}
    {% else %}
        <p class="alert alert-info">
            {_ Only the admin user can use function call tracing. _}
        </p>
    {% endif %}

    {% javascript %}
        cotonic.broker.subscribe("development/function_trace_output", function(message) {
            const traceElem = document.getElementById("trace");
            const text = html_escape(message.payload.data);
            traceElem.innerHTML += text;
        });
    {% endjavascript %}
{% else %}
    <div class="alert alert-danger">
        {_ You do not have permission to access development tools. _}
    </div>
{% endif %}
{% endblock %}
