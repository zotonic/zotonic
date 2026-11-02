{% if m.acl.is_admin %}
    {% with m.filestore.health as health %}
        {% if health.high_error_rate or health.prolonged %}
            <div class="alert alert-warning" role="alert">
                <strong>{_ Cloud file store needs attention _}</strong>
                {% if health.high_error_rate %}
                    <p>{_ Remote file reads are failing frequently. _}
                        {_ Failed reads in the last 15 minutes: _} {{ health.failures|escape }} / {{ health.attempts|escape }}.</p>
                {% endif %}
                {% if health.prolonged %}
                    <p>{_ Files with unresolved read errors older than 24 hours: _} {{ health.prolonged|escape }}.
                        <a href="{% url admin_filestore %}#filestore-errors">{_ View errors _}</a></p>
                {% endif %}
                <p>{_ Check the storage service and its access permissions. Failed reads are retried when requested, with a delay of up to ten minutes. _}</p>
                <a href="{% url admin_filestore %}">{_ Cloud file store configuration _}</a>
            </div>
        {% endif %}
    {% endwith %}
{% endif %}
