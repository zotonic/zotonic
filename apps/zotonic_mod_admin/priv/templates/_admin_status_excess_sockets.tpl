{% if m.acl.is_admin %}
    {% with m.admin_status.excess_sockets as peers %}
        {% if peers %}
            <p class="text-warning"><strong>{{ peers|length }}</strong> {_ IP addresses have more than 250 open sockets. _}</p>
            <ul>
                {% for peer in peers|sort:'desc' %}
                    <li><code>{{ peer.ip|escape }}</code>: {{ peer.count }} {_ open sockets _}</li>
                {% endfor %}
            </ul>
        {% else %}
            <p>{_ No IP addresses have more than 250 open sockets. _}</p>
        {% endif %}
        {% button class="btn btn-default btn-sm" text=_"Refresh" action={update target="admin-excess-sockets" template="_admin_status_excess_sockets.tpl"} %}
        <small class="text-muted">{_ Updated _}: {{ now|date:"H:i:s" }}</small>
    {% endwith %}
{% endif %}
