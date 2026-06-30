{% if medium.exif %}
    <tr class="active">
        <th colspan="2">EXIF</th>
    </tr>
    {% for name, value in medium.exif|sort %}
        <tr>
            <th>{{ name|to_binary|replace:"_":" "|capfirst|escape }}</th>
            <td>{{ value|media_exif_value:name|escape }}</td>
        </tr>
    {% endfor %}
{% endif %}
