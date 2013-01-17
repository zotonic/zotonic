<?xml version="1.0" encoding="UTF-8"?>
<urlset
      xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9
            http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd">

{% with m.site.hostname|default:"localhost" as hostname %}

	<url>
	  <loc>{{ m.site.protocol }}://{{ hostname }}/</loc>
	  <lastmod>{{ m.rsc.home.modified|default:now|date:"c" }}</lastmod>
	  <changefreq>daily</changefreq>
	  <priority>1.00</priority>
	</url>
	
	{% for id in result %}
		{% if not m.rsc[id].seo_noindex %}
			{% with m.rsc[id].page_url_abs as page_url %}
				{% if page_url /= "/" and id.page_path /= "/" %}
	<url>
	  <loc>{{ page_url|escapexml }}</loc>
	  <lastmod>{{ m.rsc[id].modified|date:"c" }}</lastmod>
	  <changefreq>daily</changefreq>
	  <priority>{% if m.rsc[id].page_path %}0.8{% else %}0.5{% endif %}</priority>
	</url>
				{% endif %}
			{% endwith %}
		{% endif %}
	{% endfor %}
{% endwith %}

</urlset>
