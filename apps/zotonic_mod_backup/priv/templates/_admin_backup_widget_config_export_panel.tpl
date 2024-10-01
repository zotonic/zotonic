<div class="widget">
    <div class="widget-content">
        <p>
        {_ With the import/export panel you can download a page as a file, see page revisions and restore a previous version. _}
        </p>
        <div class="checkbox">
            <label>
                <input id="backup_panel" name="backup_panel" type="checkbox" value="1" {% if m.backup.admin_panel %}checked="checked"{% endif %} /> {_ Show import/export panel on the edit pages in the admin. _}
            </label>
            {% wire id="backup_panel" postback=`config_backup_panel` %}
        </div>

        <p class="help-block">
        <i class="fa fa-info-circle"></i> {% trans "Revisions are kept for {n} months." n=m.backup_revision.retention_months %}<br>
        <i class="fa fa-info-circle"></i> {% trans "Revisions of active users are kept for {n} days." n=m.backup_revision.user_retention_days %}<br>
        <i class="fa fa-info-circle"></i> {% trans "Revisions of deleted users are kept for {n} days." n=m.backup_revision.deleted_user_retention_days %}
        </p>
    </div>
</div>
