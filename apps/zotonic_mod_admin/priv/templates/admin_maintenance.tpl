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
        {% if m.acl.is_admin %}
        <h4>{_ Network connections _} <small>{_ All sites on this node _}</small></h4>
        <p>{_ Close all sockets from IP addresses with more than 250 open sockets. This can disconnect clients on other sites. _}</p>
        {% button class="btn btn-default" text=_"Close excess connections"
            action={confirm title=_"Close excess connections" text=_"Disconnect peers with more than 250 open sockets across all sites on this node?" is_danger action={admin_tasks task='close_sockets'}} %}
        {% endif %}
        <h4>{_ Site datamodel _}</h4>
        <p>{_ Run the site's schema installation again. The site will stop and restart. _}</p>
        {% button class="btn btn-default" text=_"Reinstall site datamodel"
            action={confirm title=_"Reinstall site datamodel" text=_"This runs the site's schema installation again and stops and restarts the site. Continue?" is_danger action={admin_tasks task='site_reinstall'}} %}
        <h4>{_ Zotonic datamodel _}</h4>
        <p>{_ Run the Zotonic core datamodel installation again for this site's database. _}</p>
        {% button class="btn btn-default" text=_"Reinstall Zotonic datamodel"
            action={confirm title=_"Reinstall Zotonic datamodel" text=_"Run the Zotonic core datamodel installation again for this site's database?" is_danger action={admin_tasks task='zotonic_reinstall'}} %}
    </div>
</details>
{% endblock %}
