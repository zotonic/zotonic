{# included by the quick search 'search_view.tpl' #}

{% if m.acl.use.mod_admin_identity %}
    {% if q.qs|match:".*@.*" %}
        {% if m.identity.lookup.email[q.qs] as idns %}
            <li class="list-header">
                {_ Matching e-mail _}
            </li>
            {% for idn in idns %}
            {% with idn.rsc_id as id %}
                {% if id.is_visible %}
                    <li class="search-view-list-item">
                        {% catinclude "_search_view_list_item.tpl" id %}
                    </li>
                {% endif%}
            {% endwith %}
            {% endfor %}
        {% endif %}
    {% endif %}
{% endif %}