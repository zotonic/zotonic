{% if id.is_editable and not id.is_authoritative %}
    {% with m.websub.status[id] as subscription %}
        <div class="checkbox"><label>
            <input type="checkbox" name="z_import_subscribe" value="1" {% if subscription.is_enabled %}checked{% endif %}>
            {_ Automatically fetch updates from the original website. _}
        </label></div>
        <div class="checkbox">
            <label>
                <input type="checkbox" name="z_import_subscribe_haspart" value="1" {% if import_options.is_subscribe_haspart %}checked{% endif %}>
                {_ Also subscribe to imported collection items (haspart). _}
            </label>
        </div>
        <p class="help-block">{_ Requires automatic updates for this page and imported connections. Item subscriptions are independent and remain active when items are removed from the collection. _}</p>
    {% endwith %}
{% endif %}
