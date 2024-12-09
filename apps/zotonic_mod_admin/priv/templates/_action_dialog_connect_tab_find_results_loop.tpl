{% if result|length %}
    {% with predicate|as_atom as predicate %}
        <div class="row">
        {% for id in result %}
            {% if id.is_visible
                and (intent != 'connect'
                    or (subject_id and m.acl.is_allowed.link[subject_id][predicate][id])
                    or (object_id and m.acl.is_allowed.link[id][predicate][object_id]) )
            %}
                {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id
                    predicate=predicate
                    subject_id=subject_id
                    object_id=object_id
                %}
            {% endif %}
        {% endfor %}
        </div>
    {% endwith %}
{% else %}
    {_ No pages found. _}
{% endif %}
