{% block content %}
    {% with q.page|default:1 as qpage %}
        {% if q.qs|trim|length > 0 %}
            <ul class="search-view-results">
                {% with m.search.paged.query::%{
                            text: q.qs,
                            cat: [ "text", "media" ],
                            page: q.page,
                            pagelen: 20
                        }
                        as result %}
                            {% for id in result %}
                                <li class="search-view-list-item">
                                    {% catinclude "_search_view_list_item.tpl" id %}
                                </li>
                            {% endfor %}
                {% endwith %}
            </ul>
        {% endif %}
    {% endwith %}
{% endblock %}
