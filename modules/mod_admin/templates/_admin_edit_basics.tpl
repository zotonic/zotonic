{% with m.rsc[id] as r %}
<div class="item-wrapper">
    <h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
        <span class="title">Basics</span>
        <span class="arrow">make smaller</span>
    </h3>
    <div class="item">
        <fieldset class="admin-form">
            <input type="hidden" name="id" value="{{ id }}" />
            <div class="form-item clearfix">
                <label for="field-title">{_ Title _}</label>
                <input type="text" id="field-title" name="title" value="{{ r.title }}" {% if not is_editable %}disabled="disabled"{% endif %}/>
            </div>

            <div class="form-item clearfix">
                <label for="field-summary">{_ Summary _}</label>
                <textarea rows="2" cols="10" id="field-summary" name="summary" class="intro" {% if not is_editable %}disabled="disabled"{% endif %}>{{ r.summary }}</textarea>
            </div>
        </fieldset>
    </div>
</div>
{% endwith %}
