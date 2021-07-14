<h4>{_ Memory _}</h4>

<table class="table table-sm">
    <thead>
        <tr>
            <th>{_ Allocated _}</th>
            <th>{_ Used _}</th>
            <th>{_ Unused _}</th>
            <th>{_ Usage _}</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>{{ m.admin_status.memory.allocated | filesizeformat }}</td>
            <td>{{ m.admin_status.memory.used | filesizeformat }}</td>
            <td>{{ m.admin_status.memory.unused | filesizeformat }}</td>
            <td>{{ (m.admin_status.memory.usage * 100) | round }}%</td>
        </tr>
    </tbody>
</table>


<br>

<h4>{_ Disks _}</h4>

<div {% if m.admin_status.disks.alert %}class="border-danger"{% endif %}>
    <table class="table table-sm">
        <thead>
            <tr>
                <th>{_ Path _}</th>
                <th>{_ Size _}</th>
                <th>{_ Usage _}</th>
            </tr>
        </thead>
        <tbody>
            {% for d in m.admin_status.disks%}
                <tr {% if d.alert %}style="font-weight: bold; font-size:1.1em;" class="text-danger"{% endif %}>
                    <td>{{ d.disk|escape }}</td>
                    <td>{{ (d.size * 1024)|filesizeformat }}</td>
                    <td>
                        {% if d.alert %}
                            <span class="fa fa-warning"></span>
                        {% endif %}
                        {{ d.percent_used }}%</td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
    <p class="help-block"><span class="fa fa-info-circle"></span> {_ Refreshed every 30 minutes. _}</p>
</div>

<br>

<h4>{_ Connections _}</h4>

{% with m.modules.active.mod_geoip as is_ip2country %}
<div class="row">
    <div class="col-md-6">
        <table class="table table-sm">
            <tbody>
                <tr>
                    <th class="text-right">{_ Network Connections _}</th>
                    <td class="text-right">{{ m.admin_status.tcp_connection_count }}</td>
                </tr>
                {#
                <tr>
                    <th class="text-right">{_ Session Count _}</th>
                    <td class="text-right">{{ m.admin_status.session_count }}</td>
                </tr>
                <tr>
                    <th class="text-right">{_ Page Count _}</th>
                    <td class="text-right">{{ m.admin_status.page_count }}</td>
                </tr>
                #}
            </tbody>
        </table>
    </div>

    <div class="col-md-6">
        <table class="table table-sm">
            <thead>
                <tr>
                    <th>{_ IP Address _}</th>
                    {% if is_ip2country %}
                        <th>{_ Country _}</th>
                    {% endif %}
                    <th class="text-right">{_ Count _}</th>
                </tr>
            </thead>
            <tbody>
                {% for item in m.admin_status.group_sockets | sort:'desc' | slice:[,10] %}
                <tr>
                    <td>{{ item.ip }}</td>
                    {% if is_ip2country %}
                        <td>{{ m.l10n.country_name[ item.ip|ip2country ]|default:"-" }}</td>
                    {% endif %}
                    <td class="text-right">{{ item.count }}</td>
                </tr>
                {% endfor %}
            </tbody>
        </table>

        <div class="form-group">
            <div>
                {% button class="btn btn-default" text=_"Close Sockets" action={admin_tasks task='close_sockets'} %}
                <span class="help-block">{_ Close all sockets from ip addresses which have over &gt; 250 open sockets. _}</span>
            </div>
        </div>
    </div>
</div>
{% endwith %}
