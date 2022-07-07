{% if m.config.site.title.value %}
	<meta property="og:site_name" content="{{ m.config.site.title.value }}"/>
{% endif %}
{% if id %}
	<meta property="og:title" content="{{ id.title }}"/>
	<meta property="og:description" content="{{ id|summary:160 }}"/>
	<meta property="og:url" content="{{ id.page_url_abs }}"/>
	{% if id.depiction %}
	<meta property="og:image" content="{% image_url id.depiction mediaclass="facebook-og" use_absolute_url %}"/>
	{% endif %}
{% endif %}
