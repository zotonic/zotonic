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
    </div>
</div>
