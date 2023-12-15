{% if id.is_editable %}
    <p>{_ Current translations _}:</p>
    <ul id="{{ #current }}">
        {% for code, lang in m.translation.language_list_editable %}
            {% if lang.code_bin|member:q.enabled %}
                <li lang="{{ code }}">
                    {{ lang.name }} <span class="text-muted">/ {{ lang.name_en }} ({{ code }})</span>

                    <button class="btn btn-link" title="{_ Delete translation _}" id="{{ #del.code }}">
                        <span class="text-danger"><span class="fa fa-trash"></span></span>
                    </button>

                    {% wire id=#del.code
                            action={confirm
                                title=_"Delete translation"
                                level=1
                                text=[
                                    _"Are you sure you want to delete:",
                                    " <b>", lang.name,
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
                        {% for code, lang in m.translation.language_list_editable %}
                            {% if lang.code_bin|member:q.enabled %}
                                <option value="{{ code }}" {% if code == q.language %}selected{% endif %}>
                                    {{ lang.name }} ({{ code }})
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
                        {% for code, lang in m.translation.language_list_editable %}
                            <option value="{{ code }}">
                                {{ lang.name }} ({{ code }})
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

        <p class="help-block">
            {% if m.translation.has_translation_service %}
                {_ If you automatically translate texts then your texts will be sent to the remote translation service for automatic translation. _}
            {% endif %}
            {_ Copying and translating texts will never overwrite an existing text and can be used to fill in blank text fields in the desitination language. _}
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
        {% for code, lang in m.translation.language_list_editable %}
            {% if lang.code_bin|member:q.enabled %}
                <li>
                    {{ lang.name }} <span class="text-muted">/ {{ lang.name_en }} ({{ code }})</span>
                </li>
            {% endif %}
        {% endfor %}
    </ul>
{% endif %}
