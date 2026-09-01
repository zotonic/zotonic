<div class="checkbox">
    <label>
        <input type="checkbox" id="block-{{ name }}-is_required" name="blocks[].is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %}>
        {_ Required, this question must be answered. _}
    </label>
</div>

<div class="checkbox">
    <label>
        <input type="checkbox" id="block-{{ name }}-is_editor_only" name="blocks[].is_editor_only" value="1" {% if blk.is_editor_only %}checked="checked"{% endif %}>
        {_ Only editors can answer _}
    </label>
</div>

{% if not is_resultless %}
    <div class="checkbox">
        <label>
            <input type="checkbox" id="block-{{ name }}-is_hide_result" name="blocks[].is_hide_result" value="1" {% if blk.is_hide_result %}checked="checked"{% endif %}>
            {_ Hide from results _}
        </label>
    </div>
{% endif %}
