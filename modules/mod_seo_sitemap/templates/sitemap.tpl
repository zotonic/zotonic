<?xml version="1.0" encoding="UTF-8"?>
<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
{% for url in m.seo_sitemap.urlsets %}
   <sitemap>
      <loc>{{ url.loc|escapexml }}</loc>
      {% if url.lastmod %}<lastmod>{{ url.lastmod|date:"c" }}</lastmod>{% endif %}
   </sitemap>
{% endfor %}
</sitemapindex>
