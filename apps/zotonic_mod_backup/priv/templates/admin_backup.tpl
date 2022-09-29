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
                <div class="widget">
                    <div class="widget-content">
                        {% live template="_admin_backup_list.tpl" topic="bridge/origin/model/backup/event/#" is_filestore_enabled=is_filestore_enabled %}
                    </div>
                </div>
            </div>

            <div class="col-lg-4 col-md-6">
                {% if is_editable %}
                    <div class="widget">
                        <div class="widget-content">
                            {# TODO: make this a mod_admin_config scomp/include #}
                            <div class="checkbox">
                                <label>
                                    <input id="backup_panel" name="backup_panel" type="checkbox" value="1" {% if m.backup.admin_panel %}checked="checked"{% endif %} /> {_ Show import/export panel in the admin. _}
                                </label>
                            </div>
                            {% wire id="backup_panel" postback=`config_backup_panel` %}

                            <div class="checkbox">
                                <label>
                                    <input id="backup_daily" name="backup_daily" type="checkbox" value="1" {% if m.backup.daily_dump %}checked="checked"{% endif %}/>
                                    {% if is_filestore_enabled %}
                                        {_ Make a daily backup of the database. _}
                                    {% else %}
                                        {_ Make a daily backup of the database and uploaded files. _}
                                    {% endif %}
                                </label>
                            </div>
                            {% wire id="backup_daily" postback=`config_backup_daily` %}
                        </div>
                    </div>
                {% endif %}

                <div class="widget">
                    <div class="widget-content">
                        <p>
                            {_ At any moment you can make a backup of your system. _}
                            {% if not is_filestore_enabled %}
                                {_ The backup comprises two parts, the database and the uploaded files. _}
                            {% endif %}<br/>
                            {_ You can have one backup per day of the week, older ones will be overwritten. _}
                        </p>

                        {% if backup_config.ok and is_editable %}
                            <div class="form-group">
                                {% button class="btn btn-primary" text=_"Start backup now"
                                          action={backup_start is_full_backup} %}
                                {% if not is_filestore_enabled %}
                                    {% button class="btn btn-primary" text=_"Start database-only backup now"
                                              action={backup_start is_full_backup=false} %}
                                {% endif %}
                            </div>
                        {% elseif not backup_config.ok %}
                            <div class="alert alert-danger">
                                <p>
                                    <strong>{_ Warning _}:</strong> {_ Your backup is not correctly configured. The backup module will not work until the problem(s) below have been resolved: _}
                                </p>
                                <ul>
                                    {% if not backup_config.db_dump %}
                                        <li>{_ The "pg_dump" command was not found in the path. Set in zotonic.config the "pg_dump" config key to the path to <tt>pg_dump</tt> and return to this page. _}</li>
                                    {% endif %}
                                    {% if not backup_config.archive %}
                                        <li>{_ The "tar" command was not found in the path. Set in zotonic.config the "tar" config key to the path to <tt>tar</tt> and return to this page. _}</li>
                                    {% endif %}
                                </ul>
                            </div>
                        {% endif %}

                        {% if is_filestore_enabled  %}
                            <div class="alert alert-warning">
                                <strong>{_ Warning _}:</strong>

                                {_ Cloud file store is enabled. The local files will not be backed up. Ensure that your cloud file store system has a proper backup. _}
                            </div>
                        {% endif %}
                    </div>
                </div>
            </div>
        </div>
    {% endwith %}
{% endblock %}
