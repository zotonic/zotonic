{% if medium.oembed_url %}
<p class="clear">
    <span class="label label-info">{_ Embed URL _}:</span>
    <a href="{{ medium.oembed_url }}">{{ medium.oembed_url }}</a>
</p>
<p class="clear">
    {% if medium.oembed.html %}
        {{ medium.oembed.html }}
    {% elseif medium.oembed.error %}
        <p><span class="label label-warning">
                {_ Embedding of media item failed with HTTP code _} {{ medium.oembed.code }}:
            </span>&nbsp;
            {{ medium.oembed.body }}
        </p>
    {% else %}
    {% image medium width=500 %}
    {% endif %}
</p>
{% endif %}
