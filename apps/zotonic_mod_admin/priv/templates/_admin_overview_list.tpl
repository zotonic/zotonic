{#
Params:
qsort
qcat
#}

{% if (not q.page or q.page == "1") and q.qs %}

    {# Try exact match on id #}
    {% if m.rsc[q.qs].id as id %}
        {% if id.is_visible %}
            <h2>{_ Unique Page _}</h2>

            <table class="table table-striped do_adminLinkedTable">
                <thead>
                    <tr>
                        <th width="30%">
                            {_ Title _}
                        </th>
                        <th width="15%">
                            {_ Category _}
                        </th>
                        <th width="15%" class="hidden-xs">
                            {_ Created on _}
                        </th>
                        <th width="15%">
                            {_ Modified on _}
                        </th>
                        <th width="25%" class="hidden-xs">
                            {_ Modified by _}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    <tr class="{% if not id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
                        <td>
                            <span {% include "_language_attrs.tpl" %}>{{ id.title|striptags|default:_"<em>Untitled</em>" }}</span>
                            {% if id.is_protected %}
                                &nbsp; <span class="fa fa-lock text-muted" title="{_ Protected, not deletable _}"></span>
                            {% endif %}

                            {% if id == 1 or id.is_a.meta or id.content_group_id.name == 'system_content_group' %}
                                <span class="label label-warning pull-right hidden-xs" title="{_ This is system content. _}">
                                    {{ id.name|default:_"system content" }}
                                </span>
                            {% elif id.name %}
                                <span class="label label-default pull-right hidden-xs">
                                    {{ id.name }}
                                </span>
                            {% endif %}
                            {% if m.identity[id].username as username %}
                                <span class="label label-info pull-right hidden-xs">
                                    {{ username | escape }}
                                </span>
                            {% endif %}
                        </td>
                        <td>
                            {% if qcat %}
                                {% catinclude "_admin_overview_list_data.tpl" id %}
                            {% else %}
                                {% include "_admin_overview_list_data.tpl" %}
                            {% endif %}
                        </td>
                        <td class="hidden-xs">{{ id.created|date:_"d M Y, H:i" }}</td>
                        <td>{{ id.modified|date:_"d M Y, H:i" }}</td>
                        <td>
                            <span class="hidden-xs">{{ id.modifier_id.title|default:"-" }}</span>
                            <span class="pull-right buttons">
                                <a href="{{ id.page_url }}" class="btn btn-default btn-xs">{_ view _}</a>
                                <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                            </span>
                        </td>
                    </tr>
                </tbody>
            </table>
        {% endif %}
    {% endif %}

    {# Let other modules inject results on the first page #}
    {% all include "_admin_overview_list_page1.tpl" %}
{% endif %}


{# Normal search result #}

<h2>{_ Search results _}</h2>

<p>
    <span id="sel-count">0</span> <span class="text-muted">{_ Selected _}</span>

    <button class="btn btn-default btn-xs" id="csel-update">{_ Update _}...</button>
    <button class="btn btn-default btn-xs" id="csel-delete">{_ Delete _}...</button>
</p>

<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th width="2%">
                <input id="check-all" type="checkbox" value="1">
            </th>
            <th width="30%">
                {% include "_admin_sort_header.tpl" field="pivot.title" caption=_"Title" type=type|default:"string" %}
            </th>
            <th>
                {% catinclude "_admin_sort_header.tpl" m.category[qcat].is_a field=field|default:"category_id" type=type|default:"string" caption=_"Category" qsort=qsort %}
            </th>
            <th>
                {% include "_admin_sort_header.tpl" field="publication_start" caption=_"Published on" type="date" qsort=qsort %}
            </th>
            <th class="hidden-xs">
                {% include "_admin_sort_header.tpl" field="created" caption=_"Created on" type="date" qsort=qsort %}
            </th>
            <th>
                {% include "_admin_sort_header.tpl" field="modified" caption=_"Modified on" type="date" qsort=qsort %}
            </th>
            <th width="20%" class="hidden-xs">
                {% include "_admin_sort_header.tpl" field="modifier_id" caption=_"Modified by" type=type|default:"string" qsort=qsort %}
            </th>
            <th></th>
        </tr>
    </thead>

    <tbody>
    {% for id in result|is_visible %}
        <tr class="{% if not id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
            <td class="not-clickable">
                <input type="checkbox" value="{{ id }}" name="csel">
            </td>
            <td>
                <span {% include "_language_attrs.tpl" %}>{{ id.title|default:id.short_title|striptags|default:_"<em>Untitled</em>" }}</span>
                {% if id.is_protected %}
                    &nbsp; <span class="fa fa-lock text-muted" title="{_ Protected, not deletable _}"></span>
                {% endif %}

                {% if id == 1 or id.is_a.meta or id.content_group_id.name == 'system_content_group' %}
                    <span class="label label-warning pull-right hidden-xs" title="{_ This is system content. _}">
                        {{ id.name|default:_"system content" }}
                    </span>
                {% elseif id.name %}
                    <span class="label label-default pull-right hidden-xs">
                        {{ id.name }}
                    </span>
                {% endif %}
                {% if m.identity[id].username as username %}
                    <span class="label label-info pull-right hidden-xs">{{ username | escape }}</span>
                {% endif %}
            </td>
            <td>
                {% if qcat %}
                    {% catinclude "_admin_overview_list_data.tpl" id %}
                {% else %}
                    {% include "_admin_overview_list_data.tpl" %}
                {% endif %}
            </td>
            <td class="hidden-xs">{{ id.publication_start|date:_"d M Y, H:i" }}</td>
            <td class="hidden-xs">{{ id.created|date:_"d M Y, H:i" }}</td>
            <td>{{ id.modified|date:_"d M Y, H:i" }}</td>
            <td class="hidden-xs">{% include "_name.tpl" id=id.modifier_id %}</td>
            <td class="text-right">
                <span class="buttons">
                    <a href="{{ id.page_url }}" class="btn btn-default btn-xs">{_ view _}</a>
                    <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                </span>
            </td>
        </tr>
    {% empty %}
        <tr>
            <td colspan="5">
                {_ No pages found. _}
            </td>
        </tr>
    {% endfor %}
    </tbody>
</table>

{% include "_admin_overview_bulk_actions.tpl" %}

