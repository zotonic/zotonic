<div class="modal-body">
    <div class="form-group">
        <label class="control-label">{_ Choose editor version _}</label>
        <div>
            
            <select class="form-control" name="version" id="editor-version">
                <option value="3.5.0"{% if m.config.mod_editor_tinymce.version.value|escape=="3.5.0"%} selected="selected"{% endif %}>3.5.0 (2012-05-03)</option>
                <option value="4.0.26"{% if m.config.mod_editor_tinymce.version.value|escape=="4.0.26"%} selected="selected"{% endif %}>4.0.26 (2014-05-06)</option>
            </select>
            {% wire
                id="editor-version"
                type="change"
                action={
                    config_toggle
                    module="mod_editor_tinymce"
                    key="version"
                }
            %}
        </div>
    </div>
</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>

