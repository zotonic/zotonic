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
                <label for="field-title">Title</label>
                <input type="text" id="field-title" name="title" value="{{ r.title }}" />
            </div>

            <div class="form-item clearfix">
                <label for="field-summary">Summary</label>
                <textarea rows="2" cols="10" id="field-summary" name="summary" class="intro">{{ r.summary }}</textarea>
            </div>

            {% button action={zmedia id=id media_div_id=#media subject_id=id} text="Add media to body" id="zmedia-open-dialog" style="display:none" %}
            <div class="form-item clearfix">
                <textarea rows="10" cols="10" id="rsc-body" name="body" class="body tinymce">{{ r.body|escape }}</textarea>
            </div>

            {% include "_admin_save_buttons.tpl" %}

        </fieldset>
    </div>
</div>
{% endwith %}
