<div class="modal-body">
    <div class="form-group">
        <label class="control-label">{_ Editor version _}</label>
        <div>
            {% for id, version, version_txt, date_txt in [
                ["e", "newest", _"always use newest available", ""],
                ["d", "5.10.2", "5.10.2", "Nov 17 2021"],
                ["c", "4.9.3",  "4.9.3",  "Jan 31 2019"]
            ] %}
                <div class="radio">
                    <label>
                        <input type="radio" name="version" id="{{ #config.id }}" value="{{ version|escape }}" {% if m.editor_tinymce.version|escape==version%} checked{% endif %} /> {{ version_txt }} <span class="text-muted">{{ date_txt }}</span>
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

