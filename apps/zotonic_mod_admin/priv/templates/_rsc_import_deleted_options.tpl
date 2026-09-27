<div class="checkbox">
    <label>
        <input type="checkbox" name="z_import_deleted" value="1" {% if import_options.is_import_deleted %}checked{% endif %}>
        {_ Always import all resources, including manually deleted resources. _}
    </label>
    <p class="help-block">{_ Applies to the selected connection depth, including future updates. By default, manually deleted resources stay deleted; automatically removed dependent resources can be imported again. _}</p>
</div>
