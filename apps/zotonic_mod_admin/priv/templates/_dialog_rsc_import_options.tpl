{% if id.is_editable %}
    {% wire id=#form type="submit" postback={import_refresh id=id} delegate=`z_admin_rsc_import` %}
    <form id="{{ #form }}" method="post" action="postback">
        <div class="modal-body">
            <p>{_ Fetch the latest version from the original website using these import options. _}</p>
            <div class="form-group">
                <label for="{{ #edges }}">{_ Connections _}</label>
                <select id="{{ #edges }}" name="z_import_edges" class="form-control">
                    <option value="0" {% if not import_options.import_edges %}selected{% endif %}>{_ Do not import connections. _}</option>
                    <option value="1" {% if import_options.import_edges == 1 %}selected{% endif %}>{_ Import only direct connections (shallow copy). _}</option>
                    <option value="10" {% if import_options.import_edges > 1 %}selected{% endif %}>{_ Follow connections and import all (deep copy). _}</option>
                </select>
            </div>
            <div class="checkbox"><label>
                <input type="checkbox" name="import_medium" value="1" {% if not import_options.is_no_medium_download %}checked{% endif %}>
                {_ Download media from the original website. _}
            </label></div>
            {% all include "_rsc_import_subscription_options.tpl" id=id %}
        </div>
        <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
            <button type="submit" class="btn btn-primary">{_ Fetch new version _}</button>
        </div>
    </form>
{% endif %}
