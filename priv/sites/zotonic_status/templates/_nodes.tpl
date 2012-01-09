<tr>
    <td>&nbsp;</td>
    {% for node in nodes %}
    <th class="node-header node-{% if node_up[node] %}up{% else %}down{% endif %}">{{ node|make_list|replace:["@","<span>"] }}</span></th>
    {% endfor %}
</tr>

{% for service in services %}
<tr>
    <th>
        {{ service }}
    </th>

    {% for node in nodes %}

    {% if node_up[node] %}
    {% if service|member:node_services[node] %}
    <td class="running"><span class="running status">running</span></td>
    {% else %}
    <td class="failed"><span class="failed status">stopped</span></td>
    {% endif %}
    {% else %}
    <td class="stopped"><span class="stopped status">down</span></td>
    {% endif %}

    {% endfor %}
</tr>
{% endfor %}

