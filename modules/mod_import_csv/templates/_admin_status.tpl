{% if m.acl.use.mod_import_wordpress %}
<div class="control-group">
    <div class="controls">
        {% button class="btn" text=_"CSV import" action={dialog_open title=_"Import CSV file" template="_dialog_import_csv.tpl"} %} 
        <span class="help-inline">{_ Import a CSV file into Zotonic. _}</span>
    </div>
</div>
{% endif %}

