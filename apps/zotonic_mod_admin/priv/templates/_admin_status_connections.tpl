<div class="widget">
    <div class="widget-header">{_ Network Connections _}</div>
    <div class="widget-content">

        {% with m.modules.active.mod_geoip as is_ip2country %}
            <p>
                {_ Network Connections _}: <b>{{ m.admin_status.tcp_connection_count }}</b>
            </p>

            <table class="table table-sm">
                <thead>
                    <tr>
                        <th>{_ IP Address _}</th>
                        {% if is_ip2country %}
                            <th>{_ Country _}</th>
                        {% endif %}
                        <th class="text-right">{_ Amount _}</th>
                    </tr>
                </thead>
                <tbody>
                    {% for item in m.admin_status.group_sockets | sort:'desc' | slice:[,10] %}
                    <tr>
                        <td>{{ item.ip|escape }}</td>
                        {% if is_ip2country %}
                            <td>{{ m.l10n.country_name[ item.ip|ip2country ]|default:"-" }}</td>
                        {% endif %}
                        <td class="text-right">{{ item.count }}</td>
                    </tr>
                    {% endfor %}
                </tbody>
            </table>

        {% endwith %}
        <hr>
        {% if m.acl.is_admin %}
            {% compose "_admin_maintenance_action.tpl" title=_"Excess connections" scope=_"All sites on this node" description=_"Close all sockets from IP addresses with more than 250 open sockets. This can disconnect clients on other sites." %}
                {% block status %}
                    <div id="admin-excess-sockets" aria-live="polite">{% include "_admin_status_excess_sockets.tpl" %}</div>
                {% endblock %}
                {% block action %}
                    {% button class="btn btn-default" text=_"Close excess connections"
                        action={confirm title=_"Close excess connections" text=_"Disconnect peers with more than 250 open sockets across all sites on this node?" is_danger action={admin_tasks task='close_sockets'}} %}
                {% endblock %}
            {% endcompose %}
        {% endif %}
    </div>
</div>
