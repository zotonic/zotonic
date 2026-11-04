{% extends "admin_base.tpl" %}
{% block title %}{_ Maintenance _}{% endblock %}
{% block content %}
<div class="admin-header">
    <h2>{_ Maintenance _}</h2>
    <p>{_ Rebuild derived data and maintain this site. Queued work can be followed on the Status page. _}</p>
</div>
{% include "_admin_system_navigation.tpl" %}
{% include "_admin_status_update.tpl" %}
{% compose "_admin_maintenance_panel.tpl" title=_"Caches" %}
    {% block tools %}
        {% compose "_admin_maintenance_action.tpl" title=_"Site caches" description=_"Clear cached templates, dispatch rules and in-memory site data. The shared file index is also refreshed." %}
            {% block action %}{% button class="btn btn-default" text=_"Flush caches" action={admin_tasks task='flush'} %}{% endblock %}
        {% endcompose %}
    {% endblock %}
{% endcompose %}
{% if m.acl.use.mod_admin_config %}
    {% compose "_admin_maintenance_panel.tpl" title=_"Media" %}
        {% block tools %}
            <div id="admin-status-media-maintenance" aria-live="polite">
                {% include "_admin_status_media_maintenance.tpl" %}
            </div>
            {% compose "_admin_maintenance_action.tpl" title=_"Deduplicate media files" description=_"Move originals and extracted preview images into content-addressable storage. Redundant local and remote copies are cleaned up in the background. Remote files without a checksum are skipped; no files are downloaded to calculate checksums." %}
                {% block action %}
                    {% button class="btn btn-default" text=_"Deduplicate media files" action={admin_tasks task='migrate_to_cafs'} %}
                {% endblock %}
            {% endcompose %}
            {% compose "_admin_maintenance_action.tpl" title=_"Generated previews" description=_"Delete all resized image variants, including variants in remote storage. Uploaded originals and preview images extracted from movies, PDFs and embeds are kept. Previews regenerate when requested. Remote deletion follows the filestore retention policy." %}
                {% block action %}
                    {% button class="btn btn-danger" text=_"Delete generated previews"
                        action={confirm title=_"Delete generated previews" text=_"Delete all generated image sizes? Preview originals are kept. Image requests may be slower while previews regenerate." is_danger action={admin_tasks task='delete_generated_previews'}} %}
                {% endblock %}
            {% endcompose %}
        {% endblock %}
    {% endcompose %}
{% endif %}
{% compose "_admin_maintenance_panel.tpl" title=_"Search" panel_id="search" %}
    {% block tools %}
        <div id="admin-status-indexing" aria-live="polite">{% include "_admin_status_indexing.tpl" %}</div>
        <hr>
        {% compose "_admin_maintenance_action.tpl" title=_"Search indices" description=_"Queue every page for search indexing. Large sites can take a long time to finish." %}
            {% block action %}{% button class="btn btn-default" text=_"Rebuild search indices" action={admin_tasks task='pivot_all'} %}{% endblock %}
        {% endcompose %}
        {% all include "_admin_maintenance_search.tpl" %}
    {% endblock %}
{% endcompose %}
{% if not m.admin.is_notrack_refers %}
    {% compose "_admin_maintenance_panel.tpl" title=_"Content repair" %}
        {% block tools %}
            {% compose "_admin_maintenance_action.tpl" title=_"Embedded-page references" description=_"Find embedded page references and rebuild their refers connections. This runs in the background." %}
                {% block action %}{% button class="btn btn-default" text=_"Rebuild references" postback={ensure_refers} delegate=`mod_admin` %}{% endblock %}
            {% endcompose %}
        {% endblock %}
    {% endcompose %}
{% endif %}
{% all include "_admin_maintenance.tpl" %}
<details class="widget">
    <summary class="widget-header">{_ Advanced repair _}</summary>
    <div class="widget-content">
        <p>{_ Use these operations to repair installation data, not for routine maintenance. _}</p>
        {% if m.acl.use.mod_admin_modules %}
            <p>{_ Individual modules can be reinstalled from the Modules list. _} <a href="{% url admin_modules %}">{_ Open Modules list _}</a></p>
        {% endif %}
        {% compose "_admin_maintenance_action.tpl" title=_"Site datamodel" description=_"Run the site's schema installation again. The site will stop and restart." %}
            {% block action %}
                {% button class="btn btn-default" text=_"Reinstall site datamodel"
                    action={confirm title=_"Reinstall site datamodel" text=_"This runs the site's schema installation again and stops and restarts the site. Continue?" is_danger action={admin_tasks task='site_reinstall'}} %}
            {% endblock %}
        {% endcompose %}
        <hr>
        {% compose "_admin_maintenance_action.tpl" title=_"Zotonic datamodel" description=_"Run the Zotonic core datamodel installation again for this site's database." %}
            {% block action %}
                {% button class="btn btn-default" text=_"Reinstall Zotonic datamodel"
                    action={confirm title=_"Reinstall Zotonic datamodel" text=_"Run the Zotonic core datamodel installation again for this site's database?" is_danger action={admin_tasks task='zotonic_reinstall'}} %}
            {% endblock %}
        {% endcompose %}
    </div>
</details>
{% endblock %}
