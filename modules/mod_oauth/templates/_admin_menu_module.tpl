{% if m.acl.use.mod_oauth %}
<li><a href="{% url admin_oauth %}" {% if page_admin_oauth %}class="current"{% endif %}>{_ API access _}</a></li>
{% endif %}
