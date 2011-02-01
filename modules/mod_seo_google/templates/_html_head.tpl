{% with m.config.seo_google.webmaster_verify.value as wmv %}{% if wmv %}
	<meta name="google-site-verification" content="{{ wmv }}" />
{% endif %}{% endwith %}
{% if m.acl.user /= 1 %}
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
{% endif %}{% endwith %}{% endif %}