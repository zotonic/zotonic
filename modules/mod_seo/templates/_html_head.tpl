{% if id %}
	<link rel="shorturl" href="{% url id id=id %}" />
	<link rel="canonical" href="{{ m.rsc[id].page_url }}" />
{% endif %}

{% if m.config.seo.noindex.value %}
	<meta name="robots" content="noindex,nofollow" />
{% elseif id and id.language and m.modules.info.mod_translation.enabled and not z_language|member:id.language %}
	{# Take one of the alternative urls, provided by mod_translation #}
	<meta name="robots" content="noindex" />
{% else %}
	{% with m.config.seo.keywords.value as keywords %}
	{% with m.config.seo.description.value as description %}
		{% if id %}
			{% if m.rsc[id].seo_noindex %}
				{% if not m.config.seo.noindex.value %}<meta name="robots" content="noindex" />{% endif %}
			{% else %}
				{% with m.rsc[id].seo_keywords as seo_keywords %}
					{% if seo_keywords %}
						<meta name="keywords" content="{{ seo_keywords }}, {{ keywords }}" />
					{% else %}
						<meta name="keywords" content="{% for predicate in id.op %}{% if predicate /= "depiction" %}{% for oid in id.o[predicate] %}{{ oid.title }}, {% endfor %}{% endif %}{% endfor %}{{ keywords }}" />
					{% endif %}
					<meta name="description" content="{{ id.seo_desc|default:id.summary|default:description|escape }}" />
				{% endwith %}
			{% endif %}
		{% else %}
			{% if keywords %}
				<meta name="keywords" content="{{ keywords }}" />
			{% endif %}
			{% if description %}
				<meta name="description" content="{{ description }}" />
			{% endif %}
		{% endif %}
	{% endwith %}
	{% endwith %}
{% endif %}
