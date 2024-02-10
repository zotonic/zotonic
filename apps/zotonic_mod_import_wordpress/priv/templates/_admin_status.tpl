{% if m.acl.use.mod_import_wordpress %}
<div class="form-group">
    <div>
        {% button class="btn btn-outline-secondary" text=_"WordPress import" action={dialog_open title=_"Import WXR file" template="_dialog_import_wordpress.tpl"} %}
        <p class="help-block">{_ Import a WordPress WXR export file into Zotonic. _}</p>
    </div>
</div>
{% endif %}
