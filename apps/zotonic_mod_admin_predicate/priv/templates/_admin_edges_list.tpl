<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th width="35%">
                {_ Subject _}
                {% if m.rsc[q.qhassubject].id as id %}
                    <a class="btn btn-default btn-xs" href="{% url admin_edges qpredicate=q.qpredicate qhasobject=q.qhasobject %}" title="{_ Remove subject filter _}">
                        &times; {{ id.title|truncatechars:40 }}
                    </a>
                {% endif %}
            </th>
            <th width="30%">
                {_ Predicate _} &rarr;
                {% if m.rsc[q.qpredicate].id as id %}
                    <a class="btn btn-default btn-xs" href="{% url admin_edges qhassubject=q.qhassubject qhasobject=q.qhasobject %}" title="{_ Remove predicate filter _}">
                        &times; {{ id.title }}
                    </a>
                {% endif %}
            </th>
            <th width="35%">
                {_ Object _}
                {% if m.rsc[q.qhasobject].id as id %}
                    <a class="btn btn-default btn-xs" href="{% url admin_edges qpredicate=q.qpredicate qhassubject=q.qhassubject %}" title="{_ Remove object filter _}">
                        &times; {{ id.title|truncatechars:40 }}
                    </a>
                {% endif %}
            </th>
        </tr>
    </thead>

    <tbody>
    {% for edge in result %}
        {% with edge.id as id %}
        {% if edge.subject_id.is_visible or edge.object_id.is_visible %}
        <tr id="{{ #tr.id }}">
            <td class="{% if not edge.subject_id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=edge.subject_id %}">
                <a class="btn btn-link pull-right{% if q.qhassubject == edge.subject_id %} disabled{% endif %}" style="margin-left: 10px" href="{% url admin_edges qhassubject=edge.subject_id %}" {% if q.qhassubject == edge.subject_id %}aria-disabled="true" tabindex="-1"{% endif %} aria-label="{_ Show all outgoing connections _}" title="{_ Show all outgoing connections _}">
                    ○→
                </a>
                <a class="btn btn-link pull-left" style="margin-right: 10px" href="{% url admin_edges qhasobject=edge.subject_id %}" aria-label="{_ Show all incoming connections _}" title="{_ Show all incoming connections _}">
                    →○
                </a>
                {% catinclude "_rsc_item.tpl" edge.subject_id show_medium %}
            </td>
            <td class="clickable">
                <div>
                    <strong {% include "_language_attrs.tpl" %}>{{ edge.predicate_id.title }}</strong>
                    {% if m.acl.is_allowed.link[edge.subject_id] %}
                        <div class="btn-group btn-group-xs pull-right">
                            {% all include "_admin_edge_action.tpl" edge=edge tr_id=#tr.id %}
                        </div>
                    {% endif %}
                </div>
                <div class="text-muted">
                    <span class="edge-date">
                        {{ edge.created|date:_"d M Y, H:i" }}
                    </span>
                    {% with edge.creator_id as creator_id %}
                        {% if creator_id %}
                            {_ by _}
                            <a class="edge-creator" href="{% url admin_edit_rsc id=creator_id %}">
                                {% include "_name.tpl" id=creator_id %}
                            </a>
                        {% endif %}
                    {% endwith %}
                </div>
            </td>
            <td class="{% if not edge.object_id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=edge.object_id %}">
                <a class="btn btn-link pull-right" style="margin-left: 10px" href="{% url admin_edges qhassubject=edge.object_id %}" aria-label="{_ Show all outgoing connections _}" title="{_ Show all outgoing connections _}">
                    ○→
                </a>
                <a class="btn btn-link pull-left{% if q.qhasobject == edge.object_id %} disabled{% endif %}" style="margin-right: 10px" href="{% url admin_edges qhasobject=edge.object_id %}" {% if q.qhasobject == edge.object_id %}aria-disabled="true" tabindex="-1"{% endif %} aria-label="{_ Show all incoming connections _}" title="{_ Show all incoming connections _}">
                    →○
                </a>
                {% catinclude "_rsc_item.tpl" edge.object_id show_medium %}
            </td>
        </tr>
        {% endif %}
        {% endwith %}
    {% empty %}
        <tr>
            <td>
                {% if m.rsc[q.qhassubject].id as id %}
                    <a class="btn btn-link pull-right disabled" style="margin-left: 10px" href="{% url admin_edges qhassubject=id %}" aria-disabled="true" tabindex="-1" aria-label="{_ Show all outgoing connections _}" title="{_ Show all outgoing connections _}">
                        ○→
                    </a>
                    <a class="btn btn-link pull-left" style="margin-right: 10px" href="{% url admin_edges qhasobject=id %}" aria-label="{_ Show all incoming connections _}" title="{_ Show all incoming connections _}">
                        →○
                    </a>
                    {% catinclude "_rsc_item.tpl" id show_medium %}
                {% endif %}
            </td>
            <td>
                <span class="text-muted">
                    {% if q.qpredicate %}
                        {_ No page connections with the predicate: _} {{ m.rsc[q.qpredicate].title }}
                    {% else %}
                        {_ No page connections found. _}
                    {% endif %}
                </span>
            </td>
            <td>
                {% if m.rsc[q.qhasobject].id as id %}
                    <a class="btn btn-link pull-right" style="margin-left: 10px" href="{% url admin_edges qhassubject=id %}" aria-label="{_ Show all outgoing connections _}" title="{_ Show all outgoing connections _}">
                        ○→
                    </a>
                    <a class="btn btn-link pull-left disabled" style="margin-right: 10px" href="{% url admin_edges qhasobject=id %}" aria-disabled="true" tabindex="-1" aria-label="{_ Show all incoming connections _}" title="{_ Show all incoming connections _}">
                        →○
                    </span>
                    {% catinclude "_rsc_item.tpl" id show_medium %}
                {% endif %}
            </td>
        </tr>
    {% endfor %}
    </tbody>
</table>
