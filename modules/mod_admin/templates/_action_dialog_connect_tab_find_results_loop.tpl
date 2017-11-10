{% if result|length %}
    {% with predicate|as_atom as predicate %}
        {% for row in result|make_list|is_visible|chunk:3 %}
            <div class="row">
                {% for id in row %}
                    {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id
                        predicate=predicate
                        subject_id=subject_id
                        object_id=object_id
                    %}
                {% endfor %}
            </div>
        {% endfor %}
    {% endwith %}
{% else %}
    {_ No pages found. _}
{% endif %}
