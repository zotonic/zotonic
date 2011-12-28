<script>window.graphs={};</script>

<table width="100%">
    {% for typea, typeb in types|chunk:2 %}
    <tr>
        <td width="50%">
            {% include "_statsgraph.tpl" type=typea nodes=nodes stats=stats %}
        </td>
        <td width="50%">
            {% include "_statsgraph.tpl" type=typeb nodes=nodes stats=stats %}
        </td>
    </tr>
    {% endfor %}
</table>



