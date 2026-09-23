{% if m.acl.is_admin %}
    {% with m.admin_status.migrations as migrations %}
        {% for migration in migrations %}
            {% if migration.is_needed or migration.is_running %}
                <div class="widget" id="migration-{{ migration.id|escape }}">
                    <div class="widget-header"><h3>{_ Migration _}: {{ migration.title|escape }}</h3></div>
                    <div class="widget-content">
                        <p>{{ migration.description|escape }}</p>
                        {% if migration.is_running %}
                            <p role="status"><strong>{_ Migration queued or running _}</strong></p>
                        {% else %}
                            <p><strong>{_ Migration needed _}</strong></p>
                            {% if migration.can_start %}
                                {% button class="btn btn-primary" text=_"Start migration"
                                    postback={migration_start id=migration.id} delegate="mod_admin" %}
                            {% endif %}
                        {% endif %}
                        {% if migration.url %}<a href="{{ migration.url|escape }}">{_ System status _}</a>{% endif %}
                        <p class="help-block">{_ Refresh this page to check progress. If migration remains pending, check the server logs. _}</p>
                    </div>
                </div>
            {% endif %}
        {% endfor %}
    {% endwith %}
{% endif %}
