<h2 style="color: rgb(0, 177, 0); font-weight: bold; margin: 0; line-height: 1;">Debug</h2>
<table style="border: 2px solid rgb(0, 177, 0); padding: 2px; margin: 10px 0; font-size: 13px;">
    {% for name, value in vars %}
    <tr>
        <td style="padding: 4px 10px 4px 4px; border-bottom: 1px solid rgba(0, 177, 0, .25); font-weight: bold;">{{ name }}</td>
        <td style="padding: 4px; border-bottom: 1px solid rgba(0, 177, 0, .25)">
            <tt>{{ value }}</tt>
        </td>
    </tr>
    {% endfor %}
</table>
