{% if m.config.site.title.value %}
	<meta property="og:site_name" content="{{ m.config.site.title.value }}"/>
{% endif %}
{% if id %}
	<meta property="og:title" content="{{ id.title }}"/>
	<meta property="og:url" content="http://{{ m.site.hostname }}{{ id.page_url }}"/>
	{% if id.depiction %}
	<meta property="og:image" content="http://{{ m.site.hostname }}{% image_url id.depiction mediaclass="facebook-og" %}"/>
	{% endif %}
{% endif %}
