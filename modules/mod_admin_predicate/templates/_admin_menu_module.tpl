{% if m.acl.insert.predicate %}
<li><a href="{% url admin_predicate %}" {% ifequal selected "predicate" %}class="current"{% endifequal %}>Predicates</a></li>
{% endif %}
