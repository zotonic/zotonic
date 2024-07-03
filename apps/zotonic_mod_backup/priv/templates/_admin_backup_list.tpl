{% if m.acl.use.mod_backup %}
    <table class="table">
        <thead>
            <tr>
                <th width="100%">{_ Date _}</th>
            </tr>
        </thead>

        <tbody>
            {% for backup in m.backup.list_backups %}
                <tr>
                    <td>
                        <div class="pull-right">
                            {% if backup.is_running %}
                                <span class="label label-info">{_ this backup is in progress _}</span>
                            {% else %}
                                {% if backup.is_encrypted %}
                                    <span class="label label-info">{_ Encrypted _}</span>
                                {% endif %}

                                {% if backup.is_files_present %}
                                    <a {% if backup.is_encrypted %}
                                           href="{% url backup_download star=backup.name++".tar.gz.enc" %}"
                                       {% else %}
                                            href="{% url backup_download star=backup.name++".tar.gz" %}"
                                       {% endif %}
                                       download class="btn btn-default btn-xs">{_ download files _}</a>
                                {% endif %}

                                {% if backup.is_database_present %}
                                    <a {% if backup.is_encrypted %}
                                           href="{% url backup_download star=backup.name++".sql.gz.enc" %}"
                                       {% else %}
                                           href="{% url backup_download star=backup.name++".sql.gz" %}"
                                       {% endif %} 
                                       download class="btn btn-default btn-xs">{_ download database _}</a>
                                {% endif %}
                            {% endif %}
                        </div>

                        {{ backup.timestamp|date:_"Y-m-d H:i – l" }}

                        {% if is_filestore_enabled %}
                            <br>
                            {% if backup.is_filestore_uploaded %}
                                <span class="label label-success">
                                    {_ Uploaded to filestore _}
                                </span>
                            {% else %}
                                <span class="label label-warning">
                                    {_ Not uploaded to filestore _}
                                </span>
                            {% endif %}
                        {% endif %}
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
