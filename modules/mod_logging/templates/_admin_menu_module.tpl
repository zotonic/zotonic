{% if m.acl.is_admin %}
<li><a href="{% url admin_log %}" {% if selected == "log" %}class="current"{% endif %}>{_ Log _}</a></li>
{% endif %}
