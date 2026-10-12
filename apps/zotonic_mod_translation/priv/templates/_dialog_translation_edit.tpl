{% if id.is_editable %}
{% wire id=#form type="submit" postback={translation_save id=id
    property=field.property block=args.block language=language target=args.target}
    delegate=`mod_translation` %}
<form id="{{ #form }}" method="post" action="postback" class="form do_formdirty"
      data-translation-edit-form>
    {% include "_admin_edit_version.tpl" id=id edit_version=edit_version version_form=#form is_dialog %}
    <div class="form-group">
        <label for="{{ #text }}">{{ field.property|escape }} ({{ language|escape }})</label>
        {% include "_translation_edit_field.tpl" field=field field_id=#text language=language %}
    </div>
    <div class="modal-footer">
        {% button text=_"Cancel" action={dialog_close level=1} %}
        {% button type="submit" class="btn btn-primary" text=_"Save" %}
    </div>
</form>
{% javascript %}
    z_editor.init();
{% endjavascript %}
{% endif %}
