{% if m.acl.is_admin or m.acl.is_supervisor %}
<li><a href="{% url admin_pubsub %}" {% if page_admin_pubsub %}class="current"{% endif %}>Publish/Subscribe</a></li>
{% endif %}
