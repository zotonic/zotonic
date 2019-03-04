<div class="container">
<div class="col-md-3">
    <table class="table table-sm">
        <thead>
            <tr>
                <th colspan="2" class="text-center">{_ Memory _}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <th class="text-right">{_ Allocated _}</th>
                <td class="text-right">{{ m.admin_status.memory.allocated | filesizeformat }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Used _}</th>
                <td class="text-right">{{ m.admin_status.memory.used | filesizeformat }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Unused _}</th>
                <td class="text-right">{{ m.admin_status.memory.unused | filesizeformat }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Usage _}</th>
                <td class="text-right">{{ (m.admin_status.memory.usage * 100) | round }}%</td>
            </tr>
        </tbody>
    </table>
</div>

<div class="col-md-3">
    <table class="table table-sm">
        <thead>
            <tr>
                <th colspan="2" class="text-center">{_ Connections and Sessions _}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <th class="text-right">{_ Network Connections _}</th>
                <td class="text-right">{{ m.admin_status.tcp_connection_count }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Session Count _}</th>
                <td class="text-right">{{ m.admin_status.session_count }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Page Count _}</th>
                <td class="text-right">{{ m.admin_status.page_count }}</td>
            </tr>
        </tbody>
    </table>
</div>
</div>


<div class="container">
<div class="col-md-6">
    <table class="table table-sm">
        <thead>
            <tr>
                <th colspan="2">{_ Number of open sockets grouped per ip-address (top 10) _}</th>
            </tr>
            <tr>
                <td>{_ IP Address _}</td>
                <td class="text-right">{_ Count _}</td>
            </tr>
        </thead>
        <tbody>
            {% for item in m.admin_status.group_sockets | sort:[`count`] | slice:[,10] %}
            <tr>
                <td>{{ item.ip }}</td>
                <td class="text-right">{{ item.count }}</td>
            </tr>
            {% endfor %}
        </tbody>
    </table>
</div>
</div>

<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Close Sockets" action={admin_tasks task='close_sockets'} %}
        <span class="help-block">{_ Close all sockets from ip addresses which have over &gt; 250 open sockets. _}</span>
    </div>
</div>
