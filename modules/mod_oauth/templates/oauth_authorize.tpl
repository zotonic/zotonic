{% extends "base.tpl" %}

{% block title %} Authorize this request {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
        <h1>Allow {{ token.application_title|default:"Untitled" }}?</h1>

        <p>The application <em>{{ token.application_title|default:"Untitled" }}</em> wants to access your account.</p>

        {% if token.application_descr %}
        <p>{{ token.application_descr }}</p>
        {% endif %}

        {% if token.application_uri %}
        <p><a href="{{ token.application_uri }}">More information about this program</a></p>
        {% endif %}            

        <p>This application wants access to the following services:</p>
        <ul>
        {% for perm in m.oauth_perms.humanreadable[token.application_id] %}
            <li>{{ perm.desc }}</li>
        {% endfor %}
        </ul>

        <!-- FIXME: Show information about how this application will use your account. -->

        <form method="post">
            <button type="submit">Allow!</button>
            <button type="reset">Disallow</button>
        </form>
        
	</div>
{% endblock %}

