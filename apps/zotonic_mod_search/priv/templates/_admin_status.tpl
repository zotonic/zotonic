{% if m.acl.is_admin or m.acl.use.mod_search %}
<div class="form-group">
    <div>
        {% button class="btn btn-outline-secondary" text=_"Rebuild search facets"
                  postback={facet_rebuild}
                  delegate=`mod_search`
        %}
        <p class="help-block">{_ Check the table for the faceted search and fill it by reindexing all pages. _}</p>
    </div>
</div>
{% endif %}
