<div class="widget">
    <div class="widget-content">
        <p>{_ Automatic daily backups of the database and uploaded files can be enabled. This is done at night and the last 7 backups are kept. _}</p>
        {% if is_filestore_enabled %}
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
    </div>
</div>
