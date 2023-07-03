{% block content %}
    {% with q.page|default:1 as qpage %}
        {% if q.qs|trim|length > 0 %}
            <ul class="search-view-results">
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
