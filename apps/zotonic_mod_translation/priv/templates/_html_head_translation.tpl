{% if id and id.is_a.query and q.page %}
    {% if m.translation.rewrite_url %}{% for code in id.language %}
       <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}?page={{ q.page|escape }}" title="{{ m.rsc[id].title with z_language = code }}">
    {% endfor %}{% endif %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}?page={{ q.page|escape }}" title="{{ m.rsc[id].title with z_language = `x-default` }}">
{% elseif id and id.language %}
    {% if m.translation.rewrite_url %}{% for code in id.language %}
	   <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}" title="{{ m.rsc[id].title with z_language = code }}">
    {% endfor %}{% endif %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}" title="{{ m.rsc[id].title with z_language = `x-default` }}">
{% elseif id.exists %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}" title="{{ m.rsc[id].title with z_language = `x-default` }}">
{% endif %}
