<h2 style="color: #0c0; font-weight: bold;margin:0">Debug</h2>
<table style="border: 2px solid #0c0; padding: 2px">
    {% for name, value in vars %}
    <tr>
        <td style="padding-right:2px; border-bottom: 1px dotted #0c0; font-weight: bold;">{{ name }}</td>
        <td style="border-bottom: 1px dotted #0c0">
            <tt>{{ value }}</tt>
        </td>
    </tr>
    {% endfor %}
</table>
