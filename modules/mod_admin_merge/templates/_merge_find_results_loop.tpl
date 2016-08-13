{% if result|length %}
    {% for row in result|make_list|chunk:3 %}
        <div class="row">
            {% for id in row %}
                {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id is_connected=(not id.is_editable) %}
            {% endfor %}
        </div>
    {% endfor %}
{% else %}
    {_ No pages found. _}
{% endif %}
