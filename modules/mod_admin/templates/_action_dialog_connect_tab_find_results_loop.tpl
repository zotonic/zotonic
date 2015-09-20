{% if result|length %}
    {% with predicate|as_atom as predicate %}
        {% for row in result|make_list|chunk:3 %}
            <div class="row">
                {% for id, score in row %}
                    {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id %}
                {% endfor %}
            </div>
        {% endfor %}
    {% endwith %}
{% else %}
    {_ No pages found. _}
{% endif %}