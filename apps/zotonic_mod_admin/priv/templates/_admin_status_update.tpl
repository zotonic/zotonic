{% if m.acl.is_admin %}
{% with m.admin_status.code_update as update %}
    {% if update.blocked %}
        <div class="alert alert-danger" role="alert">
            <strong>{_ Code updates need attention _}</strong>
            {% if update.restart_required %}
                {_ The update coordinator restarted. Restart the Zotonic node before deploying again. _}
            {% else %}
                {_ A deployment failed. Automatic updates and schema upgrades are waiting for a successful rebuild. Check the server log and retry the deployment or rebuild through Zotonic. _}
            {% endif %}
            <span>{_ Applies to all sites on this node. _}</span>
        </div>
    {% elseif update.active %}
        <div class="alert alert-info" role="status">
            {_ Code is being updated. Background changes will resume when activation finishes. _}
            <span>{_ Applies to all sites on this node. _}</span>
        </div>
    {% endif %}
{% endwith %}
{% endif %}
