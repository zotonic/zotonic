{% if m.acl.is_admin %}
{% with m.filestore.stats as stats %}
    <table class="table table-striped">
        <thead>
            <tr>
                <th></th>
                <th>{_ Media Resources _}</th>
                <th>{_ Local Files _}</th>
                <th>{_ Cloud Files _}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <th>{_ Files _}</th>
                <td>{{ stats.archived }}</td>
                <td>{{ stats.local }}</td>
                <td>{{ stats.cloud }}</td>
            </tr>
            <tr>
                <th>{_ Storage _}</th>
                <td>{{ stats.archive_size|filesizeformat }}</td>
                <td>{{ stats.local_size|filesizeformat }}</td>
                <td>{{ stats.cloud_size|filesizeformat }}</td>
            </tr>
        </tbody>

        <thead>
            <tr>
                <th></th>
                <th><br>{_ Upload Queue _}</th>
                <th><br>{_ Download Queue _}</th>
                <th><br>{_ Deletion queue _}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <th>{_ Files _}</th>
                <td id="s3queue">{{ stats.queued|default:0 }}</td>
                <td id="s3queue-local">{{ stats.queued_local|default:0 }}</td>
                <td>{{ stats.queued_deleted|default:0 }}</td>
            </tr>
        </tbody>
    </table>

{% endwith %}
{% endif %}
