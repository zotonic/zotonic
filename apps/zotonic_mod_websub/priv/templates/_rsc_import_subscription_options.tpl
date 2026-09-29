{% if id.is_editable and not id.is_authoritative %}
    {% with m.websub.status[id] as subscription %}
        <div class="form-group form__subscribe">
            <label class="control-label" for="{{ #subscribe }}">{_ Subscribe _}</label>
            <div class="checkbox"><label>
                <input type="checkbox" id="{{ #subscribe }}" name="z_import_subscribe" value="1" {% if subscription.is_enabled %}checked{% endif %}>
                {_ Automatically fetch updates from the original website. _}
            </label></div>
            <div id="{{ #subscribe_options }}" {% if not subscription.is_enabled %}hidden{% endif %}>
                <div class="checkbox">
                    <label>
                        <input type="checkbox" name="z_import_subscribe_connections" value="1" {% if import_options.is_subscribe_connections %}checked{% endif %}>
                        {_ Also subscribe to connected resources. _}
                        <a href="#" class="z-btn-help do_dialog"
                            title="{_ Subscribing to connected resources _}" aria-label="{_ Subscribing to connected resources _}"
                            data-dialog="{{ %{
                                title: _"Subscribing to connected resources",
                                text: _"Follows the Connections option. Connected resources keep their subscriptions when disconnected from this page.",
                                level: 10
                            }|escape }}"></a>
                    </label>
                </div>
            </div>
        </div>
        {% javascript %}
            document.getElementById('{{ #subscribe }}').addEventListener('change', (event) => {
                document.getElementById('{{ #subscribe_options }}').hidden = !event.target.checked;
            });
        {% endjavascript %}
    {% endwith %}
{% endif %}
