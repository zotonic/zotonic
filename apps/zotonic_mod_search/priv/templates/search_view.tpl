{% block content %}
    {% with q.page|default:1 as qpage %}
        {% if q.qs|trim|length > 0 %}
            <ul class="search-view-results">
                {% with m.rsc[q.qs].id as id %}
                    {% if id %}
                        <li class="list-header">
                            {_ Matching id, name or path _}
                        </li>
                        <li class="search-view-list-item">
                            {% catinclude "_search_view_list_item.tpl" id %}
                        </li>
                    {% endif %}
                    {% if m.rsc['-'].lookup.page_path[q.qs] as path %}
                        {% if path.id =/= id %}
                            <li class="list-header">
                                {% if path.is_redirect %}
                                    {_ Matching previous page path _}
                                {% else %}
                                    {_ Matching page path _}
                                {% endif %}
                            </li>
                            <li class="search-view-list-item">
                                {% catinclude "_search_view_list_item.tpl" path.id %}
                            </li>
                        {% endif %}
                    {% endif %}
                {% endwith %}

                {% all include "_search_view.tpl" %}

                {% if m.search.paged.query::%{
                            text: q.qs,
                            cat_exclude: [ "meta" ],
                            page: q.page,
                            pagelen: 20
                        }
                        as result
                %}
                    <li class="list-header">
                        {_ Matching text _}
                    </li>
                    {% for id in result %}
                        {% if id.is_visible %}
                            <li class="search-view-list-item">
                                {% catinclude "_search_view_list_item.tpl" id %}
                            </li>
                        {% endif %}
                    {% endfor %}
                {% endif %}
            </ul>
        {% endif %}
    {% endwith %}
{% endblock %}
