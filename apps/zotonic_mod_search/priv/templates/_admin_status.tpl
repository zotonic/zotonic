{% if m.acl.is_admin or m.acl.use.mod_search %}
<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Rebuild search facets"
                  postback={facet_rebuild}
                  delegate=`mod_search`
        %}
        <span class="help-block">{_ Check the table for the faceted search and fill it by reindexing all pages. _}</span>
    </div>
</div>
{% endif %}
