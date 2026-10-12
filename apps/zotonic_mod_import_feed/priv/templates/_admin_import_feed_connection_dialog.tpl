{% if m.acl.use.mod_import_feed %}
    {% wire id=#predicate_form type="submit" postback=`select_connection_predicate` delegate=`mod_import_feed` %}
    <form id="{{ #predicate_form }}" method="post" action="postback">
        <div class="form-group">
            <label for="{{ #predicate }}">{_ Predicate _}</label>
            <select id="{{ #predicate }}" name="predicate" class="form-control" required>
                <option value="">{_ Select a predicate _}</option>
                {% for name, p in m.predicate %}
                    {% if not p.id.is_connections_hide %}
                        <option value="{{ p.id }}">{{ p.title }}</option>
                    {% endif %}
                {% endfor %}
            </select>
        </div>

        <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
            <button class="btn btn-primary" type="submit">{_ Select page _}</button>
        </div>
    </form>

{% endif %}
