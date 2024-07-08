{% extends "admin_base.tpl" %}

{% block title %} {_ Admin Backups _} {% endblock %}

{% block content %}
    {% with m.acl.is_admin as is_editable %}
        <div class="admin-header">

            <h2>{_ Backups _}</h2>

            <p>
                {_ Backup and restore options for your content. _}
                {% if m.backup.directory as dir %}
                    <br>{_ Directory _}: <tt>{{ dir|escape }}</tt>
                {% endif %}
            </p>

        </div>

        <div class="row">
            <div class="col-lg-8 col-md-6">
                {% include "_admin_backup_widget_backup_now.tpl" %}

                <div class="widget">
                    <div class="widget-content">
                        {% live template="_admin_backup_list.tpl" topic="bridge/origin/model/backup/event/#"
                                is_filestore_enabled=is_filestore_enabled %}
                    </div>
                </div>
            </div>

            <div class="col-lg-4 col-md-6">
                {% if is_editable %}
                    {% include "_admin_backup_widget_config_export_panel.tpl" %}
                    {% include "_admin_backup_widget_config_backup_frequency.tpl" %}
                    {% include "_admin_backup_widget_config_encryption.tpl" %}
                {% endif %}
            </div>
        </div>
    {% endwith %}
{% endblock %}
