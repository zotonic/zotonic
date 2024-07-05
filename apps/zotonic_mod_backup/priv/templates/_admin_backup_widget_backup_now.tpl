<div class="widget">
    <div class="widget-content">
        <p>
        {_ At any moment you can make a backup of your system. _}
        {% if not is_filestore_enabled %}
            {_ The backup comprises three parts, the database, the configuration,
               and the uploaded files. _}
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

        {% if m.backup.encrypt_backups and not m.backup.has_encrypt_password %}
            <p class="help-block alert-danger">
            <i class="fa fa-info-circle"></i>

            {_ Encryption is enabled, but there is no encryption password. Please
               make sure there is an encryption password in the settings. _}
            </p>
        {% endif %}
        {% if is_filestore_enabled  %}
            <p class="help-block">
            <i class="fa fa-info-circle"></i>

            {_ Cloud file store is enabled. The local files will not be backed up. Ensure that your cloud file store system has a proper backup. _}
            </p>
        {% endif %}
    </div>
</div>
