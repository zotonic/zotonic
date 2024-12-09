{
    "name": "{{ m.site.title | default:"Zotonic" }}",
    "start_url": "/",
    "scope": "/",
    "display": "standalone",
    "icons": [ {% for size in [144, 192, 512] %}
      {
        "type": "image/png",
        "src": "{% image_url 'lib/images/maskable_icon.png' width=size height=size lossless %}",
        "sizes": "{{ size }}x{{ size }}",
        "purpose": "any maskable"
      }{% if not forloop.last %},{% endif %}
    {% endfor %} ]
}
