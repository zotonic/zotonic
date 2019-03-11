{% if result|length %}
    {% with predicate|as_atom as predicate %}
        {% for id in result|make_list|is_visible %}
            {% catinclude "_action_dialog_new_rsc_tab_find_results_item.tpl" id
                predicate=predicate
                subject_id=subject_id
                object_id=object_id
            %}
        {% endfor %}
    {% endwith %}
{% elseif result.page == 1 %}
    {_ No pages found. _}
{% endif %}
