<div class="modal-body">
    <div class="form-group">
        <label class="control-label">{_ Editor version _}</label>
        <div>
            {% for id, version, date in [
                ["c", "4.1.6",   "Oct 08 2014"],
                ["b", "4.0.26",  "May 06 2014"],
                ["a", "3.5.0",   "May 03 2012"]
            ] %}
                <div class="radio">
                    <label>
                        <input type="radio" name="version" id="{{ #config.id }}" value="{{ version|escape }}" {% if m.config.mod_editor_tinymce.version.value|escape==version%} checked{% endif %} /> {{ version }} <span class="text-muted">{{ date }}</span>
                    </label>
                    {% wire
                        id=#config.id
                        type="click"
                        action={
                            config_toggle
                            module="mod_editor_tinymce"
                            key="version"
                        }
                    %}
                </div>
            {% endfor %}
        </div>
    </div>
</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>

