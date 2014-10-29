{% extends "admin_base.tpl" %}

{% block title %} {_ Admin Backups _} {% endblock %}

{% block content %}
    {% with m.acl.is_admin as is_editable %}
        <div class="admin-header">

            <h2>{_ Backups _}</h2>

            <p>{_ Backup and restore options for your content. _}</p>
        </div>

        <div class="row">

            <div class="col-lg-8 col-md-6">

                <table class="table">
                    <thead>
                        <tr>
                            <th width="100%">{_ Date _}</th>
                        </tr>
                    </thead>

                    <tbody>
                        {% for id,date,in_progress in backups %}
                            <tr id="{{ #li.id }}">
                                <td>
                                    <div class="pull-right">
                                        {% if in_progress %}
                                            <span class="label label-info">{_ this backup is in progress _}</span>
                                        {% else %}
                                            <a class="btn btn-default btn-xs" href="{% url backup_download star=[id, ".sql"] %}">{_ download database _}</a>
                                            <a class="btn btn-default btn-xs" href="{% url backup_download star=[id, ".tar.gz"] %}">{_ download files _}</a>
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
            </div>

            <div class="col-lg-4 col-md-6">
                {% if is_editable %}
                    <div class="well">
                        {# TODO: make this a mod_admin_config scomp/include #}
                        <div class="checkbox"><label>
                                <input id="backup_panel" name="backup_panel" type="checkbox" value="1" {% if m.config.mod_backup.admin_panel.value %}checked="checked"{% endif %} /> {_ Show import/export panel in the admin. _}
                            </label></div>
                        {% wire id="backup_panel" postback=`config_backup_panel` %}
                        <div class="checkbox"><label>
                                <input id="backup_daily" name="backup_daily" type="checkbox" value="1" {% if m.config.mod_backup.daily_dump.value %}checked="checked"{% endif %}/> {_ Make a daily backup of the database and uploaded files. _}
                            </label></div>
                        {% wire id="backup_daily" postback=`config_backup_daily` %}
                    </div>
                {% endif %}
                
                <div class="well">
                    <p>{_ At any moment you can make a backup of your system. _} {_ The backup comprises two parts, the database and the uploaded files. _}<br/> 
                        {_ You can have 10 backups, older ones will be deleted automatically. _}</p>
                    
                    {% if backup_config.ok and is_editable %}
                        {% button class="btn btn-primary" text=_"Start backup now" action={backup_start} %}

                    {% elseif not backup_config.ok %}
                        <div class="alert alert-danger">
                            <strong>{_ Warning: _}</strong> {_ Your backup is not correctly configured. The backup module will not work until the problem(s) below have been resolved: _}
                            {% if not backup_config.db_dump %}<br/><strong>{_ The "pg_dump" command was not found in the path. Set the "pg_dump" config key to the path to pg_dump and return to this page. _}</strong>{% endif %}
                            {% if not backup_config.archive %}<br/><strong>{_ The "tar" command was not found in the path. Set the "tar" config key to the path to pg_dump and return to this page. _}</strong>{% endif %}
                        </div>
                    {% endif %}
                </div>
                
                {% if `mod_filestore`|member:m.modules.enabled  %}
                    {% if m.filestore.stats.cloud > 0 %}
                        <div class="alert alert-warning">
                            <strong>{_ Warning _}</strong>

                            {{ _"This site has cloud file store enabled, and there are <strong>$1</strong> media files on this system that are only stored in the cloud and not on this machine. These files will not backed up!"|replace_args:[m.filestore.stats.cloud|make_list] }}
                        </div>
                    {% endif %}
                {% endif %}
            </div>
            
        </div>
    {% endwith %}
{% endblock %}
