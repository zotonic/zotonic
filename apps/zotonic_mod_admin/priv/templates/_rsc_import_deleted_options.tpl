<div class="checkbox">
    <label>
        <input type="checkbox" id="{{ #import_deleted }}" name="z_import_deleted" value="1" {% if not import_edges %}disabled{% endif %} {% if import_options.is_import_deleted %}checked{% endif %}>
        {_ Always import all resources, including manually deleted resources. _}

        <a href="#" class="z-btn-help do_dialog"
            title="{_ Importing deleted resources _}" aria-label="{_ Importing deleted resources _}"
            data-dialog="{{ %{
                title: _"Importing deleted resources",
                text: _"Applies to the selected connection depth, including future updates. By default, manually deleted resources stay deleted; automatically removed dependent resources can be imported again.",
                level: 10
            }|escape }}"></a>
    </label>
</div>

{% javascript %}
    {
        const checkbox = document.getElementById('{{ #import_deleted }}');
        checkbox.form.addEventListener('change', (event) => {
            if (event.target.name === 'z_import_edges') {
                checkbox.disabled = event.target.value === '0';
            }
        });
    }
{% endjavascript %}
