{% if id and id.language %}{% for code in id.language %}{% if z_language != code %}
	<link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url with z_language = code }}" title="{{ m.rsc[id].title with z_language = code }}" />
{% endif %}{% endfor %}{% endif %}
