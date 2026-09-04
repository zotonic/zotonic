{% with text|default:q.text as search_text %}
{% with link_form_id|default:q.link_form_id as link_form_id %}
{% with m.search::%{
        text: search_text
        cat: `person`
        pagelen: 100
    } as result
%}
    {% if result %}
        <table class="table table-striped">
            {% for id in result %}
                <tr>
                    <td>
                        <a href="{% url admin_edit_rsc id=id %}" target="_blank" rel="noopener">
                            {% include "_name.tpl" %} <i class="fa fa-external-link"></i>
                        </a><br>
                        <span class="text-muted">
                            {{ id.category_id.title }}
                            {% if m.identity[id].is_user %}
                                ({_ member _})
                            {% endif %}
                            &ndash; <small>{{ id.modified|date:_"Y-m-d" }}</small>
                        </span>
                    </td>
                    <td>
                        {% if id.address_country %}
                            {{ id.address_street_1 }}
                            {% if id.address_street_2 %}
                                {{ id.address_street_2 }}<br>
                            {% endif %}
                            {{ id.address_city }}<br>
                            {{ m.l10n.country_name[id.address_country] }}
                        {% endif %}
                    </td>
                    <td>
                        <button type="submit"
                                class="btn btn-default pull-right"
                                form="{{ link_form_id|escape }}"
                                value="{{ id }}"
                                name="id">
                            {_ Select _}
                        </button>
                    </td>
                </tr>
            {% endfor %}
        </table>
    {% else %}
        <p class="text-muted">{_ No persons found. _}</p>
    {% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
