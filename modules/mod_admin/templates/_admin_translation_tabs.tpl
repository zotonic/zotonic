<ul class="ui-tabs-nav">
{% for code,lang in m.config.i18n.language_list.list %}
	{% if lang.is_enabled %}
	<li class="tab-{{ code }}" {% if not code|member:r_language %}style="display: none"{% endif %}><a href="#{{ prefix }}-{{ code }}">{{ lang.language }}</a></li>
	{% endif %}
{% endfor %}
</ul>
