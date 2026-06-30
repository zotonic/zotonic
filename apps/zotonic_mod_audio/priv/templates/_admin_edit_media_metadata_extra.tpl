{% if medium.mime|match:'^audio/' %}
    {% if medium.bit_rate %}
        <tr>
            <th>{_ Audio bit rate _}</th>
            <td>{{ medium.bit_rate|format_integer }} bps</td>
        </tr>
    {% endif %}
    {% if medium.tags %}
        <tr class="active">
            <th colspan="2">{_ Audio tags _}</th>
        </tr>
        {% for name, value in medium.tags|sort %}
            <tr>
                <th>{{ name|to_binary|replace:"_":" "|capfirst|escape }}</th>
                <td>{{ value|escape }}</td>
            </tr>
        {% endfor %}
    {% endif %}
{% endif %}
