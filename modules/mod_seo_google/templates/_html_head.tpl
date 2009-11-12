{% with m.config.seo_google.webmaster_verify.value as wmv %}{% if wmv %}
		<meta name="verify-v1" content="{{ wmv }}" />
		<meta name="google-site-verification" content="{{ wmv }}" />
{% endif %}{% endwith %}