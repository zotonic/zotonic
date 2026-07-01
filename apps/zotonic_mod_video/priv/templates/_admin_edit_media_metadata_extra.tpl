{% if medium.mime|match:'^video/' %}
    <tr class="active">
        <th colspan="2">{_ Video _}</th>
    </tr>
    {% if medium.format_long_name or medium.format_name %}
        <tr>
            <th>{_ Format _}</th>
            <td>
                {{ medium.format_long_name|default:medium.format_name|escape }}
                {% if medium.format_name and medium.format_long_name %}
                    <span class="text-muted">({{ medium.format_name|escape }})</span>
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.video_codec %}
        <tr>
            <th>{_ Video codec _}</th>
            <td>
                {{ medium.video_codec|escape }}
                {% if medium.video_profile %}
                    <span class="text-muted">({{ medium.video_profile|escape }})</span>
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.video_pixel_format %}
        <tr>
            <th>{_ Pixel format _}</th>
            <td>{{ medium.video_pixel_format|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.video_frame_rate %}
        <tr>
            <th>{_ Frame rate _}</th>
            <td>{{ medium.video_frame_rate|format_number }} fps</td>
        </tr>
    {% endif %}
    {% if medium.video_bit_rate %}
        <tr>
            <th>{_ Video bit rate _}</th>
            <td>
                {% if medium.video_bit_rate > 1024 %}
                    {{ (medium.video_bit_rate / 1000)|round|format_integer }} kbps
                {% else %}
                    {{ medium.video_bit_rate|format_integer }} bps
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.audio_codec %}
        <tr>
            <th>{_ Audio codec _}</th>
            <td>{{ medium.audio_codec|escape }}</td>
        </tr>
    {% endif %}
    {% if medium.audio_channels or medium.audio_channel_layout %}
        <tr>
            <th>{_ Audio channels _}</th>
            <td>
                {% if medium.audio_channels %}{{ medium.audio_channels|escape }}{% endif %}
                {% if medium.audio_channel_layout %}
                    <span class="text-muted">{{ medium.audio_channel_layout|escape }}</span>
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.audio_sample_rate %}
        <tr>
            <th>{_ Audio sample rate _}</th>
            <td>
                {% if medium.audio_sample_rate > 1024 %}
                    {{ (medium.audio_sample_rate / 1000)|round|format_integer }} kHz
                {% else %}
                    {{ medium.audio_sample_rate|format_integer }} Hz
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.audio_bit_rate %}
        <tr>
            <th>{_ Audio bit rate _}</th>
            <td>
                {% if medium.audio_bit_rate > 1024 %}
                    {{ (medium.audio_bit_rate / 1000)|round|format_integer }} kbps
                {% else %}
                    {{ medium.audio_bit_rate|format_integer }} bps
                {% endif %}
            </td>
        </tr>
    {% endif %}
    {% if medium.tags %}
        <tr class="active">
            <th colspan="2">{_ Video tags _}</th>
        </tr>
        {% for name, value in medium.tags|sort %}
            <tr>
                <th>{{ name|to_binary|replace:"_":" "|capfirst|escape }}</th>
                <td>{{ value|escape }}</td>
            </tr>
        {% endfor %}
    {% endif %}
{% endif %}
