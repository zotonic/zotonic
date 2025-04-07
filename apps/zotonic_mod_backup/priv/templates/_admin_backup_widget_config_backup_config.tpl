<div class="widget">
    <div class="widget-content">
        <p>{_ Automatic daily backups of the database and uploaded files can be enabled. The backup is done at night and the last 7 backups are kept. _}</p>

        {% if is_config_locked %}
            <p class="alert alert-info"><span class="glyphicon glyphicon-info-sign"></span>
                {_ The backup module uses a global Zotonic configuration. The configuration cannot be changed here. _}
            </p>
            <p>{_ The backup configuration is set to: _}</p>
            <p>
                <b>
                    {% if m.backup.daily_dump == '2' %}
                        {_ Make a daily backup of the database. _}
                    {% elseif m.backup.daily_dump == '1' %}
                        {_ Make a daily backup of the database and uploaded files. _}
                    {% else %}
                        {_ Do not make a daily backup. _}
                    {% endif %}
                </b>
            </p>
            {% if is_filestore_enabled %}
                <p class="help-block">
                <i class="fa fa-info-circle"></i>
                {_ Cloud file store is enabled. Backups of the database are uploaded to the cloud file store. Local files are copied to the cloud and not backed up locally. _}
                </p>
            {% endif %}
        {% elseif is_filestore_enabled %}
            <div class="radio">
                <label>
                    <input id="backup_daily0" name="backup_daily" type="radio" value="0" {% if not m.backup.daily_dump %}checked="checked"{% endif %}>
                    {_ Do not make a daily backup. _}
                </label><br>
                <label>
                    <input id="backup_daily2" name="backup_daily" type="radio" value="2" {% if m.backup.daily_dump %}checked="checked"{% endif %}>
                    {_ Make a daily backup of the database. _}
                </label><br>
                <label class="text-muted">
                    <input id="backup_daily1" name="backup_daily" type="radio" value="1" disabled>
                    {_ Make a daily backup of the database and uploaded files. _}
                </label>
                {% wire id="backup_daily0" postback=`config_backup_daily` %}
                {% wire id="backup_daily2" postback=`config_backup_daily` %}
            </div>
            <p class="help-block">
            <i class="fa fa-info-circle"></i>
            {_ Cloud file store is enabled. Backups of the database are uploaded to the cloud file store. Local files are copied to the cloud and not backed up locally. _}
            </p>
        {% else %}
            <div class="radio">
                <label>
                    <input id="backup_daily0" name="backup_daily" type="radio" value="0" {% if not m.backup.daily_dump %}checked="checked"{% endif %}>
                    {_ Do not make a daily backup. _}
                </label><br>
                <label>
                    <input id="backup_daily2" name="backup_daily" type="radio" value="2" {% if m.backup.daily_dump == '2' %}checked="checked"{% endif %}>
                    {_ Make a daily backup of the database. _}
                </label><br>
                <label>
                    <input id="backup_daily1" name="backup_daily" type="radio" value="1" {% if m.backup.daily_dump == '1' %}checked="checked"{% endif %}>
                    {_ Make a daily backup of the database and uploaded files. _}
                </label>
                {% wire id="backup_daily0" postback=`config_backup_daily` %}
                {% wire id="backup_daily2" postback=`config_backup_daily` %}
                {% wire id="backup_daily1" postback=`config_backup_daily` %}
            </div>
        {% endif %}

        <hr>

        <p>
        {_ By default the backup of the database and files are not encrypted. It is
        possible to encrypt the backups. This makes it safer to store them on an 
        external server. _}
        </p>

        {% if is_config_locked %}
            <p>{_ The encryption configuration is set to: _}</p>
            <p>
                <b>
                    {% if m.backup.encrypt_backups %}
                        {_ Encrypt backups. _}
                    {% else %}
                        {_ Do not encrypt backups. _}
                    {% endif %}
                </b>
            </p>
        {% else %}
            <div class="checkbox">
                <label>
                    <input id="encrypt_backups" name="encrypt_backups" type="checkbox" value="1" {% if m.backup.encrypt_backups %}checked="checked"{% endif %} /> {_ Encrypt Backups _}
                </label>
                {% wire id="encrypt_backups" postback=`config_encrypt_backups` %}
            </div>
        {% endif %}

        {% if m.backup.encrypt_backups or not is_config_locked %}
            <hr>
            <p class="encryption-extra-explanation">
            <strong>{_ Note: _}</strong>
            <mark>{_ Please make sure you store the backup encryption password in a safe <u>external</u> location. _}</mark> {_ It can be found in the  _} <a href="{% url admin_config %}">{_ System Configuration _}</a> {_ or in the Zotonic configuration files. _}
            </p>
        {% endif %}
    </div>
</div>
    </div>
</div>
