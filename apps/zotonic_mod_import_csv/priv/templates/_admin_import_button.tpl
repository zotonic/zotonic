{% if m.acl.use.mod_import_csv %}
    <div class="form-group">
        {% button class="btn btn-primary" text=_"Import CSV file"++"..."
                  action={dialog_open title=_"Import CSV file"
                  template="_dialog_import_csv.tpl"}
        %}
        <p class="help-block">{_ Import data from a CSV file. _}</p>
    </div>
{% endif %}
