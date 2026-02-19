<details {% if q.show_details %}open{% endif %}>
    <summary class="graph-active-resource-header">
        {_ Selected page _}
    </summary>
    <div class="graph-active-resource-detail">
        {% with m.rsc[q.id].id as id %}
            {% if id.is_visible %}
                    <h4>
                        {{ id.title|default:id.short_title|default:_"Untitled"|truncate:80 }}
                    </h4>

                    <p class="text-muted">
                        {{ id.category_id.title }}
                        &middot;
                        {% include "_name.tpl" id=id.creator_id %}
                    </p>

                    <p>
                        <a href="{% url admin_edges_graph id=id %}" class="btn btn-default btn-xs">
                             ⋺ {_ Graph _}
                        </a>
                        <a href="{% url admin_edit_rsc id=id %}" target="admin-edit" class="btn btn-default btn-xs">
                            {_ Edit _} <span class="fa fa-external-link"></span>
                        </a>
                        <a href="{{ id.page_url }}" target="admin-view" class="btn btn-default btn-xs">
                            {_ View _} <span class="fa fa-external-link"></span>
                        </a>
                    </p>

                    {% if id.depiction as depiction %}
                        {% media depiction mediaclass="admin-media" %}
                    {% endif %}

                    <p>{{ id|summary }}</p>

                    <h4>{_ Path _}</h4>

                    <ol class="graph-active-resource-path">
                        {% for pid in q.path_nodes|reversed %}
                            {% with m.rsc[pid].id as id %}
                                <li>
                                    <a href="{% url admin_edit_rsc id=id %}" target="admin-edit">
                                        {{ id.title|default:id.short_title|default:_"Untitled"|truncate:80 }}
                                        <span class="fa fa-external-link"></span>
                                    </a>
                                </li>
                            {% endwith %}
                        {% endfor %}
                    </ol>
            {% else %}
                <p>
                    {_ No such page or page not visible. _}
                </p>
            {% endif %}
        {% endwith %}
    </div>
</details>
