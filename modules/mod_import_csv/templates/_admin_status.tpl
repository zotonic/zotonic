{% if m.acl.use.mod_import_csv %}
<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"CSV import" action={dialog_open title=_"Import CSV file" template="_dialog_import_csv.tpl"} %} 
        <span class="help-block">{_ Import a CSV file into Zotonic. _}</span>
    </div>
</div>
{% endif %}

