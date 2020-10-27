{% if id and id.is_a.query and q.page %}
    {% for code in id.language %}{% if z_language != code or not q.z_language %}
       <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}?page={{ q.page|escape }}" title="{{ m.rsc[id].title with z_language = code }}" />
    {% endif %}{% endfor %}{% if q.z_language %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}?page={{ q.page|escape }}" title="{{ m.rsc[id].title with z_language = `x-default` }}" />{% endif %}
{% elseif id and id.language %}
    {% for code in id.language %}{% if z_language != code or not q.z_language %}
	   <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}" title="{{ m.rsc[id].title with z_language = code }}" />
    {% endif %}{% endfor %}{% if q.z_language %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}" title="{{ m.rsc[id].title with z_language = `x-default` }}" />{% endif %}
{% elseif id.exists %}
    <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = `x-default` }}" title="{{ m.rsc[id].title with z_language = `x-default` }}" />
{% endif %}
{% lib "css/mod_translation.css" %}
