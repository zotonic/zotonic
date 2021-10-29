{% if medium.oembed.error %}
    <p>
        <span class="label label-warning">
            {_ Embedding of media item failed with HTTP code _} {{ medium.oembed.code }}:
        </span>&nbsp;
        {{ medium.oembed.body|strip|escape }}
    </p>
{% endif %}

{% if not medium.media_import and media %}
    <!-- Older content not imported with media_import -->
    <p class="clear">
        <span class="label label-info">{_ Embed _}</span>
        {% if medium.oembed_url|match:'^https?:' %}
            <a href="{{ medium.oembed_url|escape }}" target="_blank" rel="noopener noreferrer">{{ medium.oembed_url|truncate:80:'…'|escape }}</a>
        {% else %}
            <span class="text-muted">{{ medium.oembed_url|truncate:80:'…'|escape }}</span>
        {% endif %}
    </p>
{% endif %}
