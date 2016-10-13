<div class="modal-body">
    <div class="form-group">
        <label class="control-label">{_ Editor version _}</label>
        <div>
            {% for id, version, version_txt, date_txt in [
                ["d", "newest", "always use newest available", ""],
                ["c", "4.3.7",  "4.3.7", "Mar 03 2016"]
            ] %}
                <div class="radio">
                    <label>
                        <input type="radio" name="version" id="{{ #config.id }}" value="{{ version|escape }}" {% if m.config.mod_editor_tinymce.version.value|escape==version%} checked{% endif %} /> {{ version_txt }} <span class="text-muted">{{ date_txt }}</span>
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

