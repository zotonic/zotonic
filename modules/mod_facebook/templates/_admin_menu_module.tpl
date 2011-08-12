{% if m.acl.use.mod_facebook %}
        <li><a href="{% url admin_facebook %}" {% if page_admin_facebook %}class="current"{% endif %} title=_"Facebook">Facebook</a></li>
{% endif %}
