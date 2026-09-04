{% if medium.mime == "text/html-video-embed" %}
    <tr class="active">
        <th colspan="2">{_ Video embed _}</th>
    </tr>
    {% if medium.video_embed_service %}
        <tr>
            <th>{_ Service _}</th>
            <td>{{ medium.video_embed_service|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.video_embed_id %}
        <tr>
            <th>{_ Video ID _}</th>
            <td>{{ medium.video_embed_id|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.video_embed_site %}
        <tr>
            <th>{_ Site _}</th>
            <td>{{ medium.video_embed_site|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.video_embed_author %}
        <tr>
            <th>{_ Author _}</th>
            <td>{{ medium.video_embed_author|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.video_embed_published %}
        <tr>
            <th>{_ Published _}</th>
            <td>{{ medium.video_embed_published|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.media_import %}
        <tr>
            <th>{_ Imported from _}</th>
            <td class="admin-url-break">
                <a href="{{ medium.media_import|escape }}" target="_blank" rel="noopener noreferrer">
                    {{ medium.media_import|escape }}
                </a>
                <a class="btn btn-xs btn-default"
                   data-onclick-topic="model/clipboard/post/copy"
                   data-text="{{ medium.media_import|escape }}"
                   title="{_ Copy the URL to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>
            </td>
        </tr>
    {% endif %}
    {% if medium.video_embed_canonical_url %}
        <tr>
            <th>{_ Canonical URL _}</th>
            <td class="admin-url-break">
                <a href="{{ medium.video_embed_canonical_url|escape }}" target="_blank" rel="noopener noreferrer">
                    {{ medium.video_embed_canonical_url|escape }}
                </a>
                <a class="btn btn-xs btn-default"
                   data-onclick-topic="model/clipboard/post/copy"
                   data-text="{{ medium.video_embed_canonical_url|escape }}"
                   title="{_ Copy the URL to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>
            </td>
        </tr>
    {% endif %}
    {% if medium.video_embed_final_url and medium.video_embed_final_url != medium.video_embed_canonical_url %}
        <tr>
            <th>{_ Final URL _}</th>
            <td class="admin-url-break">
                <a href="{{ medium.video_embed_final_url|escape }}" target="_blank" rel="noopener noreferrer">
                    {{ medium.video_embed_final_url|escape }}
                </a>
                <a class="btn btn-xs btn-default"
                   data-onclick-topic="model/clipboard/post/copy"
                   data-text="{{ medium.video_embed_final_url|escape }}"
                   title="{_ Copy the URL to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>
            </td>
        </tr>
    {% endif %}
    {% if medium.video_embed_thumbnail_url %}
        <tr>
            <th>{_ Thumbnail URL _}</th>
            <td class="admin-url-break">
                <a href="{{ medium.video_embed_thumbnail_url|escape }}" target="_blank" rel="noopener noreferrer">
                    {{ medium.video_embed_thumbnail_url|escape }}
                </a>
                <a class="btn btn-xs btn-default"
                   data-onclick-topic="model/clipboard/post/copy"
                   data-text="{{ medium.video_embed_thumbnail_url|escape }}"
                   title="{_ Copy the URL to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>
            </td>
        </tr>
    {% endif %}
    {% if medium.video_embed_tags %}
        <tr>
            <th>{_ Tags _}</th>
            <td>
                {% for tag in medium.video_embed_tags %}
                    {% if not forloop.first %}, {% endif %}{{ tag|escape }}
                {% endfor %}
            </td>
        </tr>
    {% endif %}
{% endif %}
