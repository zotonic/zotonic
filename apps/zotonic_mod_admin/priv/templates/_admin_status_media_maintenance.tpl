{% if m.acl.use.mod_admin_config %}
    {% with m.admin_status.media_maintenance as maintenance %}
        {% if maintenance %}
            <dl class="dl-horizontal">
                <dt>{_ Media deduplication _}</dt>
                <dd>
                    {% include "_admin_status_media_maintenance_state.tpl" status=maintenance.migration %}
                </dd>
                <dt>{_ Preview cleanup _}</dt>
                <dd>
                    {% include "_admin_status_media_maintenance_state.tpl" status=maintenance.previews %}
                </dd>
            </dl>
        {% else %}
            <p class="text-warning">{_ Media maintenance status is unavailable. _}</p>
        {% endif %}
    {% endwith %}
    <p class="help-block">{_ Media maintenance runs in the background. Waiting tasks resume automatically. Remote deletion follows the filestore retention policy. _}</p>
    {% wire type="interval" interval=5000 action={update target="admin-status-media-maintenance" template="_admin_status_media_maintenance.tpl"} %}
{% endif %}
