<?xml version="1.0" encoding="UTF-8"?>
<urlset
      xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9
            http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd">

{% with m.modules.active.mod_translation as is_i18n %}
{% with m.site.hostname|default:"localhost" as hostname %}

	<url>
	  <loc>{{ m.site.protocol }}://{{ hostname }}/</loc>
	  <lastmod>{{ m.rsc.home.modified|default:now|date:"c" }}</lastmod>
	  <changefreq>daily</changefreq>
	  <priority>1.0</priority>
	</url>

	{% for id in result %}
		{% if id.is_visible and not id.seo_noindex %}
			{% with id.page_url_abs as page_url %}
			{% with id.category_id as category_id %}
				{% if page_url /= "/" and (id.page_path /= "/" or is_i18n) %}
{% if is_i18n and id.language %}
	{% for lang in id.language %}
	<url>
	  <loc>{{ id.page_url_abs|escapexml with z_language=lang }}</loc>
	  <lastmod>{{ id.modified|date:"c" }}</lastmod>
	  <changefreq>{{ category_id.seo_sitemap_changefreq|default:'weekly' }}</changefreq>
	  <priority>{% if category_id.seo_sitemap_priority %}{{ category_id.seo_sitemap_priority }}{% elseif id.page_path %}0.8{% else %}0.5{% endif %}</priority>
	</url>
	{% endfor %}
{% else %}
	<url>
	  <loc>{{ page_url|escapexml }}</loc>
	  <lastmod>{{ id.modified|date:"c" }}</lastmod>
	  <changefreq>{{ category_id.seo_sitemap_changefreq|default:'weekly' }}</changefreq>
	  <priority>{% if category_id.seo_sitemap_priority %}{{ category_id.seo_sitemap_priority }}{% elseif id.page_path %}0.8{% else %}0.5{% endif %}</priority>
	</url>
{% endif %}
				{% endif %}
			{% endwith %}
			{% endwith %}
		{% endif %}
	{% endfor %}
{% endwith %}
{% endwith %}

</urlset>
