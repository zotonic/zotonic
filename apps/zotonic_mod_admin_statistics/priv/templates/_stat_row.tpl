{# 
 # parameters:
 #    title
 #    id
 #}
<tr>
    <td class="meta" {% if not id %}colspan="2" style="text-align:center"{% endif %}>{{ title }}</td>
    {% if id %}
        <td id="{{ id }}" style="text-align:right"></td>
    {% endif %}
</tr>
 
