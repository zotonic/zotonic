{% if medium.oembed.error %}
    <p>
        <span class="label label-warning">
            {_ Embedding of media item failed with HTTP code _} {{ medium.oembed.code }}:
        </span>&nbsp;
        {{ medium.oembed.body|strip|escape }}
    </p>
{% endif %}
