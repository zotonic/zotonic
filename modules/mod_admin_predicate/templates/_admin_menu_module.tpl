{% if m.acl.is_admin or m.acl.is_supervisor %}
<li><a href="{% url admin_predicate %}" {% ifequal selected "predicate" %}class="current"{% endifequal %}>Predicates</a></li>
{% endif %}
