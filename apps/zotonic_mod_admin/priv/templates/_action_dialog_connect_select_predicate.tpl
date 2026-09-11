{# Select a predicate before finding a subject or object for the connection. #}
<div class="form-group">
    <label for="{{ #predicate }}">{_ Predicate _}</label>
    <select id="{{ #predicate }}" class="form-control">
        <option value="">{_ Select a predicate _}</option>
        {% for name, p in m.predicate %}
            {% if not p.id.is_connections_hide
                and ((subject_id and m.predicate.is_valid_subject_subcategory[name][subject_id.category_id])
                    or (object_id and m.predicate.is_valid_object_subcategory[name][object_id.category_id])) %}
                <option value="{{ name|escape }}">{{ p.title }}</option>
            {% endif %}
        {% endfor %}
    </select>
    {% wire id=#predicate type="change"
        action={update target=#find_connection
            template="_action_dialog_connect_select_predicate_find.tpl"
            subject_id=subject_id
            object_id=object_id
        }
    %}
</div>

<div id="{{ #find_connection }}">
    {% include "_action_dialog_connect_select_predicate_find.tpl" %}
</div>
