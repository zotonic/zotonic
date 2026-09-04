{% if medium.mime == "text/html-oembed" and medium.oembed %}
    <tr class="active">
        <th colspan="2">{_ OEmbed _}</th>
    </tr>
    {% if medium.oembed.error %}
        <tr>
            <th>{_ Error _}</th>
            <td>
                {% if medium.oembed.code %}
                    {{ medium.oembed.code|escape }}:
                {% endif %}
                {{ medium.oembed.body|strip|escape }}
            </td>
        </tr>
    {% endif %}
    {% if medium.oembed.provider_name %}
        <tr>
            <th>{_ Provider _}</th>
            <td>
                {% if medium.oembed.provider_url %}
                    <a href="{{ medium.oembed.provider_url|escape }}" target="_blank" rel="noopener noreferrer">
                        {{ medium.oembed.provider_name|escape }}
                    </a>
                    <a class="btn btn-xs btn-default"
                       data-onclick-topic="model/clipboard/post/copy"
                       data-text="{{ medium.oembed.provider_url|escape }}"
                       title="{_ Copy the URL to the clipboard. _}">
                        <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                    </a>
                {% else %}
                    {{ medium.oembed.provider_name|escape }}
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.oembed.type %}
        <tr>
            <th>{_ Type _}</th>
            <td>{{ medium.oembed.type|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.oembed.title %}
        <tr>
            <th>{_ Title _}</th>
            <td>{{ medium.oembed.title|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.oembed.author_name %}
        <tr>
            <th>{_ Author _}</th>
            <td>
                {% if medium.oembed.author_url %}
                    <a href="{{ medium.oembed.author_url|escape }}" target="_blank" rel="noopener noreferrer">
                        {{ medium.oembed.author_name|escape }}
                    </a>
                    <a class="btn btn-xs btn-default"
                       data-onclick-topic="model/clipboard/post/copy"
                       data-text="{{ medium.oembed.author_url|escape }}"
                       title="{_ Copy the URL to the clipboard. _}">
                        <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                    </a>
                {% else %}
                    {{ medium.oembed.author_name|escape }}
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.oembed.duration %}
        <tr>
            <th>{_ Duration _}</th>
            <td>{{ medium.oembed.duration|format_duration }}</td>
        </tr>
    {% endif %}
    {% if medium.oembed.video_id %}
        <tr>
            <th>{_ Video ID _}</th>
            <td>{{ medium.oembed.video_id|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.oembed.thumbnail_url %}
        <tr>
            <th>{_ Thumbnail URL _}</th>
            <td class="admin-url-break">
                <a href="{{ medium.oembed.thumbnail_url|escape }}" target="_blank" rel="noopener noreferrer">
                    {{ medium.oembed.thumbnail_url|escape }}
                </a>
                <a class="btn btn-xs btn-default"
                   data-onclick-topic="model/clipboard/post/copy"
                   data-text="{{ medium.oembed.thumbnail_url|escape }}"
                   title="{_ Copy the URL to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>
            </td>
        </tr>
    {% endif %}
    {% if medium.oembed.thumbnail_width or medium.oembed.thumbnail_height %}
        <tr>
            <th>{_ Thumbnail size _}</th>
            <td>{{ medium.oembed.thumbnail_width|escape }} &times; {{ medium.oembed.thumbnail_height|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.oembed.version %}
        <tr>
            <th>{_ Version _}</th>
            <td>{{ medium.oembed.version|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.oembed_url %}
        <tr>
            <th>{_ OEmbed URL _}</th>
            <td class="admin-url-break">
                <a href="{{ medium.oembed_url|escape }}" target="_blank" rel="noopener noreferrer">
                    {{ medium.oembed_url|escape }}
                </a>
                <a class="btn btn-xs btn-default"
                   data-onclick-topic="model/clipboard/post/copy"
                   data-text="{{ medium.oembed_url|escape }}"
                   title="{_ Copy the URL to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>
            </td>
        </tr>
    {% endif %}
{% endif %}
