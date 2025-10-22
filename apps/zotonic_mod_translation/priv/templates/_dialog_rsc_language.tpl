{% with m.translation.language_list_editable|language_sort_localized as language_list_editable %}
{% if id.is_editable %}
    <p>{_ Current translations _}:</p>
    <ul id="{{ #current }}">
        {% for code, lang in language_list_editable %}
            {% if lang.code_bin|member:q.enabled %}
                <li lang="{{ code }}">
                    {{ lang.name }} <span class="text-muted">/ {{ lang.name_localized }} ({{ code }})</span>

                    <button class="btn btn-link" title="{_ Delete translation _}" id="{{ #del.code }}">
                        <span class="text-danger"><span class="glyphicon glyphicon-remove-sign"></span></span>
                    </button>

                    {% wire id=#del.code
                            action={confirm
                                title=_"Delete translation"
                                level=1
                                text=[
                                    _"Are you sure you want to delete:",
                                    " <b>", lang.name_localized,
                                    "</b> <span class='text-muted'>(", code, ")</li></span><br><br>",
                                    _"All translated texts will be removed."
                                ]
                                is_danger
                                ok=_"Delete"
                                action={publish
                                    topic="model/translation/post/disable"
                                    language=lang.code_bin
                                }
                                action={script script=[
                                    "$('#", #current, " li[lang=", lang.code_bin, "]').remove();",
                                    "$('#form-new-language select[name=src] option[value=", lang.code_bin, "]').remove();"
                                ]}
                                action={dialog_close}
                            }
                    %}
                </li>
            {% endif %}
        {% endfor %}
    </ul>

    <p class="help-block">
        {_ Removing a language will hide it on the edit form. If the page is saved then the translation will be deleted from the database. _}
    </p>

    <hr>

    <p>{_ Add a new translation _}:</p>

    <form id="form-new-language" action="GET" class="form"
          data-onsubmit-topic="model/translation/post/submit">

        <input type="hidden" name="action" value="dialog_close">

        <div class="row">
            <div class="col-md-4">
                <div class="form-group">
                    <label>{_ From existing language _}</label>
                    <select name="src" class="form-control">
                        {% for code, lang in language_list_editable %}
                            {% if lang.code_bin|member:q.enabled %}
                                <option value="{{ code }}" {% if code == q.language %}selected{% endif %}>
                                    {{ lang.name_localized }} ({{ code }})
                                </option>
                            {% endif %}
                        {% endfor %}
                    </select>
                </div>
            </div>
            <div class="col-md-4">
                <div class="form-group">
                    <label>{_ To new language _}</label>
                    <select name="dst" class="form-control" required>
                        <option></option>
                        {% for code, lang in language_list_editable %}
                            <option value="{{ code }}">
                                {{ lang.name_localized }} ({{ code }})
                            </option>
                        {% endfor %}
                    </select>
                </div>
            </div>
            <div class="col-md-4">
                <div class="form-group">
                    <label>{_ Method _}</label>
                    <select name="method" class="form-control" required>
                        {% if m.translation.has_translation_service %}
                            <option value="translate">{_ Automatic translation _}</option>
                        {% endif %}
                        <option value="copy">{_ Copy texts _}</option>
                        <option value="empty">{_ Leave texts empty _}</option>
                    </select>
                </div>
            </div>
        </div>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" value="1" name="overwrite">
                {_ Overwrite existing texts with new translations _}
            </label>
        </div>

        {% if m.translation.has_translation_service %}
            <p class="help-block">
                {_ If you automatically translate texts then your texts will be sent to a remote translation service. _}
            </p>
        {% endif %}
        <p class="help-block">
            {_ Copying and translating texts is used to fill in blank text fields in the destination language. Existing texts will never be overwritten unless the “overwrite existing texts” checkbox is checked. _}
        </p>

        <div class="modal-footer">
            {% button class="btn btn-default"
                      text=_"Cancel"
                      action={dialog_close}
            %}
            {% button class="btn btn-primary"
                      type="submit"
                      text=_"Add translation"
            %}
        </div>
    </form>
{% else %}
    <ul>
        {% for code, lang in language_list_editable %}
            {% if lang.code_bin|member:q.enabled %}
                <li>
                    {{ lang.name_localized }} <span class="text-muted">/ {{ lang.name }} ({{ code }})</span>
                </li>
            {% endif %}
        {% endfor %}
    </ul>
{% endif %}
{% endwith %}

