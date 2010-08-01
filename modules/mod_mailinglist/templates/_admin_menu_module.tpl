{% if m.acl.use.mod_mailinglist %}
<li><a href="{% url admin_mailinglist %}" {% if page_admin_mailinglist %}class="current"{% endif %}>Mailing lists</a></li>
{% endif %}
