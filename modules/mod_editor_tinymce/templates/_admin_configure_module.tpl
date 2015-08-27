<div class="modal-body">
    <div class="form-group">
        <label class="control-label">{_ Editor version _}</label>
        <div>
            {% for id, version, version_txt, date_txt in [
                ["e", "newest",       "always use newest available", ""],
                ["d", "4.2.4",  "4.2.4", "Aug 17 2015"],
                ["c", "4.1.6",  "4.1.6", "Oct 08 2014"],
                ["b", "4.0.26", "4.0.26", "May 06 2014"],
                ["a", "3.5.0",  "3.5.0", "May 03 2012"]
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

