{% if m.acl.use.mod_import_csv %}
    <div class="form-group">
        {% button class="btn btn-primary" text=_"Import CSV or XLSX file"++"..."
                  action={dialog_open title=_"Import CSV XLSX file"
                  template="_dialog_import_csv.tpl"}
        %}
    </div>
    <p class="help-block">
        {_ Import data from a CSV or XLSX file. _}
        {_ Depending on the filename the data can be imported as pages or as special data. _}
    </p>
{% endif %}
