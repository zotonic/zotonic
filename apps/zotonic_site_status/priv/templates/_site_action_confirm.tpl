{% if site_action == "restart" %}
    {% trans "Are you sure you want to restart the site <b>{site}</b>?" site=site|to_binary|escape %}
{% else %}
    {% trans "Are you sure you want to stop the site <b>{site}</b>?" site=site|to_binary|escape %}
{% endif %}
