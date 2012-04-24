{% if m.acl.use.mod_import_wordpress %}
<div class="control-group">
    <div class="controls">
        {% button class="btn" text=_"Wordpress import" action={dialog_open title=_"Import WXR file" template="_dialog_import_wordpress.tpl"} %} 
        <span class="help-inline">{_ Import a Wordpress WXR export file into Zotonic. _}</span>
    </div>
</div>
{% endif %}
