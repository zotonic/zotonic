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
                <td class="text-right">{{ m.admin_status.memory.allocated | readable_bytes }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Used _}</th>
                <td class="text-right">{{ m.admin_status.memory.used | readable_bytes }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Unused _}</th>
                <td class="text-right">{{ m.admin_status.memory.unused | readable_bytes }}</td>
            </tr>
            <tr>
                <th class="text-right">{_ Usage _}</th>
                <td class="text-right">{{ (m.admin_status.memory.usage * 100) | round }}%</td>
            </tr>
        </tbody>
    </table>
</div>

<div class="container col-md-3">
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
