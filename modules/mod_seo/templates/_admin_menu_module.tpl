{% if m.acl.use.mod_seo %}
	<li><a href="{% url admin_seo %}" {% if page_admin_seo %}class="current"{% endif %} title=_"Search Engine Optimization">SEO</a></li>
{% endif %}
