{% with m.rsc[id] as r %}
<div class="item-wrapper">
    <h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
        <span class="title">Body text</span>
        <span class="arrow">make smaller</span>
    </h3>
    <div class="item">
        <fieldset class="admin-form">
            {% button action={zmedia id=id media_div_id=#media subject_id=id} text="Add media to body" id="zmedia-open-dialog" style="display:none" %}
            <div class="form-item clearfix">
                {% if is_editable %}
                <textarea rows="10" cols="10" id="rsc-body" name="body" class="body tinymce">{{ r.body|escape }}</textarea>
                {% else %}
                {{ r.body }}
                {% endif %}
            </div>

            {% include "_admin_save_buttons.tpl" %}

        </fieldset>
    </div>
</div>
{% endwith %}
