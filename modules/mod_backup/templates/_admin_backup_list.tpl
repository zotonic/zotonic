{% if m.acl.use.mod_backup %}
    <table class="table">
        <thead>
            <tr>
                <th width="100%">{_ Date _}</th>
            </tr>
        </thead>

        <tbody>
            {% for id, date, is_full_backup, in_progress in m.backup.list_backups %}
                <tr id="{{ #li.id }}">
                    <td>
                        <div class="pull-right">
                            {% if in_progress %}
                                <span class="label label-info">{_ this backup is in progress _}</span>
                            {% else %}
                                <a class="btn btn-default btn-xs" href="{% url backup_download star=[id, ".sql"] %}">{_ download database _}</a>
                                {% if is_full_backup %}
                                    <a class="btn btn-default btn-xs" href="{% url backup_download star=[id, ".tar.gz"] %}">{_ download files _}</a>
                                {% endif %}
                            {% endif %}
                        </div>
                        {{ date|date:"M d Y, H:i" }}
                    </td>
                </tr>
            {% empty %}
                <tr>
                    <td>
                        {_ No backups present. _}
                    </td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
{% endif %}
