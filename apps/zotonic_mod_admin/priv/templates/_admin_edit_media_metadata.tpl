{% if medium.mime %}
    <tr>
        <th>{_ MIME type _}</th>
        <td>{{ medium.mime|escape }}</td>
    </tr>
{% endif %}
{% if medium.width and medium.height %}
    <tr>
        <th>{_ Dimensions _}</th>
        <td>{{ medium.width }} x {{ medium.height }} {_ pixels _}</td>
    </tr>
{% endif %}
{% if medium.orientation %}
    <tr>
        <th>{_ Orientation _}</th>
        <td>
            {% if medium.orientation == 1 %}
                {_ Normal _}
            {% elseif medium.orientation == 2 %}
                {_ Mirrored horizontally _}
            {% elseif medium.orientation == 3 %}
                {_ Rotated 180 degrees _}
            {% elseif medium.orientation == 4 %}
                {_ Mirrored vertically _}
            {% elseif medium.orientation == 5 %}
                {_ Mirrored horizontally, rotated 270 degrees _}
            {% elseif medium.orientation == 6 %}
                {_ Rotated 90 degrees _}
            {% elseif medium.orientation == 7 %}
                {_ Mirrored horizontally, rotated 90 degrees _}
            {% elseif medium.orientation == 8 %}
                {_ Rotated 270 degrees _}
            {% else %}
                {{ medium.orientation|escape }}
            {% endif %}
            <span class="text-muted">({{ medium.orientation|escape }})</span>
        </td>
    </tr>
{% endif %}
{% if medium.duration %}
    <tr>
        <th>{_ Duration _}</th>
        <td>{{ medium.duration|format_duration }}</td>
    </tr>
{% endif %}
{% if medium.size %}
    <tr>
        <th>{_ File size _}</th>
        <td>{{ medium.size|filesizeformat }}</td>
    </tr>
{% endif %}
{% if medium.filename %}
    <tr>
        <th>{_ Filename _}</th>
        <td>{{ medium.filename|escape }}</td>
    </tr>
{% endif %}
{% if medium.created %}
    <tr>
        <th>{_ Uploaded on _}</th>
        <td>{{ medium.created|date:"Y-m-d H:i:s" }}</td>
    </tr>
{% endif %}
{% if medium.digest %}
    <tr>
        <th>{_ SHA-256 checksum _}</th>
        <td>
            <code>{{ medium.digest|escape }}</code>
            <a class="btn btn-xs btn-default"
               data-onclick-topic="model/clipboard/post/copy"
               data-text="{{ medium.digest|escape }}"
               title="{_ Copy the SHA-256 checksum to the clipboard. _}">
                <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
            </a>
        </td>
    </tr>
{% endif %}
