{% if m.acl.is_admin or m.acl.use.mod_search %}
{% compose "_admin_maintenance_action.tpl" title=_"Search facets" description=_"Rebuild the faceted search table by reindexing all pages." %}
    {% block action %}
        {% button class="btn btn-default" text=_"Rebuild search facets" postback={facet_rebuild} delegate=`mod_search` %}
    {% endblock %}
{% endcompose %}
{% endif %}
