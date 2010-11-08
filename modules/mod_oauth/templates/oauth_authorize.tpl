{% extends "base.tpl" %}

{% block title %}{_ Authorize this request _}{% endblock %}

{% block content %}
	<div id="content" class="zp-100">
        <h1>{_ Allow _} {{ token.application_title|default:_"Untitled" }}?</h1>

        <p>{_ The application _} <em>{{ token.application_title|default:_"Untitled" }}</em> {_ wants to access your account. _}</p>

        {% if token.application_descr %}
        <p>{{ token.application_descr }}</p>
        {% endif %}

        {% if token.application_uri %}
        <p><a href="{{ token.application_uri }}">{_ More information about this program _}</a></p>
        {% endif %}

        <p>{_ This application wants access to the following services: _}</p>
        <ul>
        {% for perm in m.oauth_perms.humanreadable[token.application_id] %}
            <li>{{ perm.desc }}</li>
        {% endfor %}
        </ul>

        <!-- FIXME: Show information about how this application will use your account. -->

        <form method="post">
            <button type="submit">{_ Allow! _}</button>
            <button type="reset">{_ Disallow _}</button>
        </form>

	</div>
{% endblock %}

