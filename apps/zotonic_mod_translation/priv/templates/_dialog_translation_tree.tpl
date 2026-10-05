{% if id.is_editable %}
<p>{_ Current translations _} ({{ tree.total }} {% if tree.total == 1 %}{_ page _}{% else %}{_ pages _}{% endif %}):</p>
<ul>
    {% for code, count in tree.languages %}
        <li>
            {{ m.translation.localized_name[code]|escape }} ({{ code|escape }}) — {{ count }} {% if count == 1 %}{_ page _}{% else %}{_ pages _}{% endif %}
            <button type="button" class="btn btn-link text-danger" data-translation-tree-remove="{{ code|escape }}" data-tree-id="{{ id }}" title="{_ Delete translation _}">
                <span class="fa fa-trash" aria-hidden="true"></span> {_ Delete _}
            </button>
        </li>
    {% endfor %}
</ul>
<p class="help-block">{_ Removing a language affects all pages in this tree, except pages where it is the only language. _}</p>

<hr>

<form data-translation-tree-form="{{ id }}">

    <div class="row">
        <div class="col-md-4">
            <label>{_ From existing language _}</label>
            <select class="form-control" name="src" required style="margin-right:5px;">
                {% for code, count in tree.languages %}
                    <option value="{{ code|escape }}">{{ m.translation.localized_name[code]|escape }} ({{ code|escape }})</option>
                {% endfor %}
            </select>
        </div>
        <div class="col-md-4">
            <label>{_ To new language _}</label>
            <select class="form-control" name="dst" required>
                <option value=""></option>
                {% for code, lang in m.translation.language_list_editable %}
                    <option value="{{ code|escape }}">{{ m.translation.localized_name[code]|escape }} ({{ code|escape }})</option>
                {% endfor %}
            </select>
        </div>
        <div class="col-md-4">
            <label>{_ Method _}</label>
            <select class="form-control" name="method" required>
                {% if m.translation.has_translation_service %}
                    <option value="translate">{_ Automatic translation _}</option>
                {% endif %}
                <option value="copy">{_ Copy texts _}</option>
                <option value="empty">{_ Leave texts empty _}</option>
            </select>
        </div>
    </div>

    <div class="form-group">
        <br>
        <label class="checkbox">
            <input type="checkbox" name="overwrite" value="1"> {_ Overwrite existing texts with new translations _}
        </label>
    </div>

    <p class="help-block">{_ Copying and translating texts is used to fill in blank text fields in the destination language. Existing texts will never be overwritten unless the “overwrite existing texts” checkbox is checked. _}</p>

    {% if m.translation.has_translation_service %}
        <p class="help-block">{_ If you automatically translate texts then your texts will be sent to a remote translation service. _}</p>
    {% endif %}

    <p class="help-block">{_ This changes saved pages immediately. Pages you cannot edit are skipped. Copying and automatic translation skip pages without the source language. Editing is disabled while the operation runs. _}</p>

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
        <button class="btn btn-primary" type="submit">{_ Add translation _}</button>
    </div>
</form>
{% endif %}
