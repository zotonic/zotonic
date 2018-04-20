{% if id %}
	<link rel="shortlink" href="{% block shortlink %}{% url id id=id %}{% endblock %}" />
	<link rel="canonical" href="{% block canonical %}{{ m.rsc[id].page_url }}{% endblock %}" />
{% endif %}

{% if m.seo.noindex or noindex %}
	<meta name="robots" content="noindex,nofollow" />
{% elseif id and id.language and m.modules.active.mod_translation and not z_language|member:id.language %}
	{# Take one of the alternative urls, provided by mod_translation #}
	<meta name="robots" content="noindex" />
{% else %}
	{% with m.seo.description as description %}
		{% if id %}
			{% if m.rsc[id].seo_noindex %}
				{% if not m.seo.noindex %}<meta name="robots" content="noindex" />{% endif %}
			{% else %}
                <meta name="description" content="{{ id.seo_desc|default:id.summary|default:description|escape }}" />
			{% endif %}
		{% elseif description %}
            <meta name="description" content="{{ description }}" />
		{% endif %}
	{% endwith %}

    {% if zotonic_dispatch == 'home' %}
        {% include "schema_org/types/website.tpl" %}
    {% endif %}
    {% catinclude "schema_org/schema.tpl" id %}
{% endif %}
{% with m.seo.bing.webmaster_verify as wmv %}{% if wmv %}
	<meta name="msvalidate.01" content="{{ wmv }}" />
{% endif %}{% endwith %}
{% with m.seo.google.webmaster_verify as wmv %}{% if wmv %}
	<meta name="google-site-verification" content="{{ wmv }}" />
{% endif %}{% endwith %}
{% if not m.acl.is_admin and not notrack %}
    {% if m.seo.google.analytics as ga %}
        <script>
            window.ga=window.ga||function(){(ga.q=ga.q||[]).push(arguments)};ga.l=+new Date;
            ga('create', "{{ ga|escapejs }}", 'auto', {% include "_ga_params.tpl" %});
            ga('set', 'anonymizeIp', true);
            ga('send', 'pageview');
        </script>
        <script async src='https://www.google-analytics.com/analytics.js'></script>
    {% endif %}
{% endif %}
