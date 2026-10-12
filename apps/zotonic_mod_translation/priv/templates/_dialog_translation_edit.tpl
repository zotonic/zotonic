{% if id.is_editable %}
    {% wire id=#form
            type="submit"
            postback={translation_save id=id
                property=field.property
                block=args.block
                language=language
                target=args.target
            }
            delegate=`mod_translation` %}
    <form id="{{ #form }}" method="post" action="postback" class="form do_formdirty" data-translation-edit-form>
        {% include "_admin_edit_version.tpl" id=id edit_version=edit_version version_form=#form is_dialog %}
        {% if source_languages and m.translation.has_translation_service %}
            <details class="form-group" data-translation-source
                 data-id="{{ id }}" data-property="{{ field.property|escape }}"
                 data-block="{{ args.block|escape }}" data-language="{{ language|escape }}"
                 data-field="{{ #text }}"
                 data-confirm="{_ Replace the edited text with an automatic translation? _}"
                 data-busy="{_ Translating... _}"
                 data-error="{_ Translation failed. Your text has not been changed. _}"
                 data-changed="{_ The text changed while translating. Please try again. _}">
                <summary>{_ Translate from _}</summary>
                <label class="sr-only" for="{{ #source }}">{_ Source language _}</label>
                <div class="input-group">
                    <select id="{{ #source }}" class="form-control" data-translation-language>
                        {% for code in source_languages %}
                            <option value="{{ code|escape }}">{{ m.translation.localized_name[code]|escape }} ({{ code|escape }})</option>
                        {% endfor %}
                    </select>
                    <span class="input-group-btn">
                        <button type="button" class="btn btn-primary" data-translation-start>{_ Translate _}</button>
                    </span>
                </div>
                <p class="help-block">{_ If you automatically translate texts then your texts will be sent to a remote translation service. _}</p>
                <p class="help-block" role="status" aria-live="polite" data-translation-status></p>
            </details>
        {% endif %}
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
