{% if id %}
	<link rel="shorturl" href="{% url id id=id %}" />
	<link rel="canonical" href="{{ m.rsc[id].page_url }}" />
{% endif %}

{% if m.config.seo.noindex.value or noindex %}
	<meta name="robots" content="noindex,nofollow" />
{% elseif id and id.language and m.modules.active.mod_translation and not z_language|member:id.language %}
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
{% with m.config.seo_bing.webmaster_verify.value as wmv %}{% if wmv %}
	<meta name="msvalidate.01" content="{{ wmv }}" />
{% endif %}{% endwith %}
{% with m.config.seo_google.webmaster_verify.value as wmv %}{% if wmv %}
	<meta name="google-site-verification" content="{{ wmv }}" />
{% endif %}{% endwith %}
{% if m.acl.user /= 1 and not notrack %}
	{% with m.config.seo_google.analytics.value as ga %}{% if ga %}
	<script type="text/javascript">
		var _gaq = _gaq || [];
		_gaq.push(['_setAccount', "{{ ga }}"]);
		_gaq.push(['_trackPageview']);
		{% if m.acl.user %}_gaq.push(['_setCustomVar',1,'User','True',1]);{% endif %}
		(function() {
			var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
			ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
			var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
		})();
	</script>
	{% endif %}{% endwith %}
{% endif %}
