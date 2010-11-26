{% if m.acl.use.mod_mailinglist %}
<li><a href="{% url admin_mailinglist %}" {% if page_admin_mailinglist %}class="current"{% endif %}>{_ Mailing lists _}</a></li>
{% endif %}
