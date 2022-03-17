{% if id and id.is_a.query and q.page %}
    <link rel="canonical" href="{% block canonical %}{{ m.rsc[id].page_url_abs }}{% endblock %}?page={{ q.page|escape }}" />
    <link rel="shortlink" href="{% block shortlink %}{% url id id=id absolute_url %}{% endblock %}" />
{% elseif id %}
	<link rel="canonical" href="{% block canonical %}{{ m.rsc[id].page_url_abs }}{% endblock %}" />
    <link rel="shortlink" href="{% block shortlink %}{% url id id=id absolute_url %}{% endblock %}" />
{% endif %}
{% if m.seo.noindex or noindex %}
    <meta name="robots" content="noindex,nofollow">
{% elseif id and id.language and m.modules.active.mod_translation and not z_language|member:id.language %}
    {# Take one of the alternative urls, provided by mod_translation #}
    <meta name="robots" content="noindex">
{% else %}
    {% with m.seo.keywords as keywords %}
    {% with m.seo.description.value as description %}
        {% if id %}
            {% if m.rsc[id].seo_noindex %}
                <meta name="robots" content="noindex">
            {% else %}
                {% with m.rsc[id].seo_keywords as seo_keywords %}
                    {% if seo_keywords %}
                        <meta name="keywords" content="{{ seo_keywords }}, {{ keywords|escape }}">
                    {% else %}
                        <meta name="keywords" content="{% for oid in id.o.subject %}{{ oid.title }}, {% endfor %}{{ keywords|escape }}">
                    {% endif %}
                    <meta name="description" content="{{ id.seo_desc|default:(id|summary)|default:(description|escape)|truncate:150 }}">
                {% endwith %}
            {% endif %}
        {% else %}
            {% if keywords %}
                <meta name="keywords" content="{{ keywords|escape }}">
            {% endif %}
            {% if description %}
                <meta name="description" content="{{ description|escape|truncate:150 }}">
            {% endif %}
        {% endif %}
    {% endwith %}
    {% endwith %}
{% endif %}
{% if m.seo.bing.webmaster_verify as wmv %}
    <meta name="msvalidate.01" content="{{ wmv }}">
{% endif %}
{% if m.seo.google.webmaster_verify as wmv %}
    <meta name="google-site-verification" content="{{ wmv }}">
{% endif %}
{% if m.seo.yandex.webmaster_verify as wmv %}
    <meta name="yandex-verification" content="{{ wmv }}">
{% endif %}
{% if not m.acl.is_admin and not notrack %}
    {% if m.seo.google.analytics as ga %}
        {% if ga|match:"^G-" %}
            <script async src="https://www.googletagmanager.com/gtag/js?id={{ ga|urlencode }}"></script>
            <script nonce="{{ m.req.csp_nonce }}">
              window.dataLayer = window.dataLayer || [];
              function gtag(){dataLayer.push(arguments);}
              gtag('js', new Date());

              gtag('config', '{{ ga|escapejs }}', { 'anonymize_ip': true });
            </script>
        {% else %}
            <script nonce="{{ m.req.csp_nonce }}">
                var GA_LOCAL_STORAGE_KEY = 'ga:clientId';
                var ga_options = {% include '_ga_params.tpl' %};
                window.ga=window.ga||function(){(ga.q=ga.q||[]).push(arguments)};ga.l=+new Date;
                if (window.localStorage) {
                  ga_options.storage = 'none';
                  ga_options.clientId = localStorage.getItem(GA_LOCAL_STORAGE_KEY);
                  ga('create', '{{ ga|escapejs }}', ga_options);
                  ga(function(tracker) {
                    localStorage.setItem(GA_LOCAL_STORAGE_KEY, tracker.get('clientId'));
                  });
                }
                else {
                  ga('create', '{{ ga|escapejs }}', 'auto', ga_options);
                }
                ga('set', 'anonymizeIp', true);
                ga('send', 'pageview');
            </script>
            <script async src='https://www.google-analytics.com/analytics.js'></script>
        {% endif %}
    {% endif %}
    {% if m.seo.google.gtm as gtm %}
        <script nonce="{{ m.req.csp_nonce }}">
        (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
        new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
        j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
        'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
        })(window,document,'script','dataLayer','{{ gtm|escapejs }}');
        </script>
    {% endif %}
{% endif %}
