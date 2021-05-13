<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
{% for url in m.seo_sitemap.urlset[q.s][q.o][q.c] %}
   <url>
      <loc>{{ url.loc|escapexml }}</loc>
      {% if url.lastmod %}<lastmod>{{ url.lastmod|date:"c" }}</lastmod>{% endif %}
      {% if url.changefreq %}<lastmod>{{ url.changefreq|escapexml }}</changefreq>{% endif %}
      {% if url.priority|is_defined %}<lastmod>{{ url.priority|to_binary|escapexml }}</priority>{% endif %}
   </url>
{% endfor %}
</urlset>
