{% if m.acl.use.mod_import_wordpress %}
<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"WordPress import" action={dialog_open title=_"Import WXR file" template="_dialog_import_wordpress.tpl"} %}
        <span class="help-block">{_ Import a WordPress WXR export file into Zotonic. _}</span>
    </div>
</div>
{% endif %}
