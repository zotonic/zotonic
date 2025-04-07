<div class="widget">
    <div class="widget-content">
        <p>
        {_ By default the backup of the database and files are not encrypted. It is
        possible to encrypt the backups. This makes it safer to store them on an 
        external server. _}
        </p>

        {% if is_config_locked %}
            <p class="alert alert-info"><span class="glyphicon glyphicon-info-sign"></span>
                {_ The backup module uses a global Zotonic configuration. It cannot be changed here. _}
            </p>

            <p>{_ The configuration is set to: _}</p>
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
