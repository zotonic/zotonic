{# The variable z_content_language is set by controller_page.
 # The z_content_language is set to the current z_language if the page is the home page or a
 # collection.
 #}
{% if not seo_noindex and m.translation.rewrite_url %}
{% with m.translation.default_language as default_language %}
{% with m.translation.x_default_language as x_default %}
{% if id %}
    {% if id.is_a.query %}
        {% for code in m.translation.enabled_language_codes %}
           <link rel="alternate" hreflang="{{ code }}" href="{{ m.req.raw_path|set_url_language:code }}">{% endfor %}
        <link rel="alternate" hreflang="x-default" href="{{ m.req.raw_path|set_url_language:x_default }}">
    {% elseif not z_content_language or zotonic_dispatch == `home` or id.is_a.collection %}
        {% for code in m.translation.enabled_language_codes %}
           <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}">{% endfor %}
        <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = x_default }}">
    {% elseif id.language and z_content_language %}
        <link rel="alternate" hreflang="{{ default_language }}" href="{{ m.rsc[id].page_url_abs with z_language = default_language }}">
        {% for code in id.language %}{% if code /= default_language %}
    	   <link rel="alternate" hreflang="{{ code }}" href="{{ m.rsc[id].page_url_abs with z_language = code }}">{% endif %}{% endfor %}
        <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = x_default }}">
    {% elseif id.exists %}
        <link rel="alternate" hreflang="x-default" href="{{ m.rsc[id].page_url_abs with z_language = x_default }}">
    {% endif %}
{% else %}
    {% for code in m.translation.enabled_language_codes %}
       <link rel="alternate" hreflang="{{ code }}" href="{{ m.req.raw_path|set_url_language:code }}">{% endfor %}
    <link rel="alternate" hreflang="x-default" href="{{ m.req.raw_path|set_url_language:x_default }}">
{% endif %}
{% endwith %}
{% endwith %}
{% endif %}
