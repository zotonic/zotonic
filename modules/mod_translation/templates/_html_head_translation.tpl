{% with m.config.mod_translation.rewrite_url.value|default_if_none:1 as rewrite_url %}
{% if id and id.is_a.query and q.page %}
    {% if rewrite_url %}{% for code in id.language %}
       <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}?page={{ q.page|escape }}">
    {% endfor %}{% endif %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}?page={{ q.page|escape }}">
{% elseif id and zotonic_dispatch == `home` and rewrite_url %}
    {% for code,_ in m.translation.language_list_enabled %}
       <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}">
    {% endfor %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}">
{% elseif id and id.language %}
    {% if rewrite_url %}{% for code in id.language %}
	   <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}">
    {% endfor %}{% endif %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}">
{% elseif id.exists %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}">
{% endif %}
{% endwith %}
