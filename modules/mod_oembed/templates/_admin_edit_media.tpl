{% if medium.oembed_url %}
<p class="clear">
    {% if medium.oembed.html %}
    {{ medium.oembed.html }}
    {% else %}
    {% image medium width=500 %}
    {% endif %}
</p>
{% endif %}
