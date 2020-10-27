{% extends "base_simple.tpl" %}

{# Template shown in popup window when redirecting to an external identity service #}

{% block title %}{_ Authenticated _}{% endblock %}

{% block content %}
    {% if try_reload %}
        <div class="container">
            <p class="alert">{_ One moment, please... _}</p>
        </div>
        {% javascript %}
            if (sessionStorage.is_reload_done) {
                window.close();
            } else {
                sessionStorage.is_reload_done = true;
                setTimeout(function() { z_reload(); }, 50);
            }
        {% endjavascript %}
    {% else %}
    	<div class="container">
    		<p class="alert">{_ Authentication has been done with _} {{ service }}</p>
    	</div>
    	{% javascript %}
    		window.close();
    	{% endjavascript %}
    {% endif %}
{% endblock %}
