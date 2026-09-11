{# Resolve the selected predicate before rendering signed connection postbacks. #}
{% with m.rsc[q.triggervalue].id as predicate_id %}
    {% if predicate_id.is_a.predicate
        and not predicate_id.is_connections_hide
        and ((subject_id and m.predicate.is_valid_subject_subcategory[predicate_id][subject_id.category_id])
            or (object_id and m.predicate.is_valid_object_subcategory[predicate_id][object_id.category_id]))
    %}
        {% with subject_id|if:(m.predicate.object_category[predicate_id]|first):(m.predicate.subject_category[predicate_id]|first) as category %}
        {% include "_action_dialog_connect.tpl"
            intent="connect"
            subject_id=subject_id
            object_id=object_id
            predicate=predicate_id.name
            category=category
            tabs_enabled=["find"]
        %}
        {% endwith %}
    {% else %}
        <p class="help-block">
            {% if subject_id %}
                {_ Select a predicate to find pages this page can connect to. _}
            {% else %}
                {_ Select a predicate to find pages that can refer to this page. _}
            {% endif %}
        </p>
        <div class="modal-footer">
            <button id="{{ #close }}" type="button" class="btn btn-default">{_ Close _}</button>
            {% wire id=#close action={dialog_close} %}
        </div>
    {% endif %}
{% endwith %}
