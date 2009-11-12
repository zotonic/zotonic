{% if m.acl.is_admin or m.acl.is_supervisor %}
<li><a href="{% url admin_predicate %}" {% if page_admin_predicate %}class="current"{% endif %}>Predicates</a></li>
{% endif %}
