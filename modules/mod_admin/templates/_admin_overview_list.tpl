<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th width="30%">
                {% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" %}
            </th>
            <th width="15%">
                {% include "_admin_sort_header.tpl" field="category_id" caption=_"Category" %}
            </th>
            <th width="15%">
                {% include "_admin_sort_header.tpl" field="created" caption=_"Created on" %}
            </th>
            <th width="15%">
                {% include "_admin_sort_header.tpl" field="modified" caption=_"Modified on" %}
            </th>
            <th width="25%">
                {% include "_admin_sort_header.tpl" field="modifier_id" caption=_"Modified by" %}
            </th>
        </tr>
    </thead>

    <tbody>
        {% for id in result %}
        {% if m.rsc[id].is_visible %}
        <tr id="{{ #tr.id }}" class="{% if not m.rsc[id].is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
            <td><span {% include "_language_attrs.tpl" %}>{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span></td>
            <td>{{ m.rsc[m.rsc[id].category_id].title }}</td>
            <td>{{ m.rsc[id].created|date:"d M Y, H:i" }}</td>
            <td>{{ m.rsc[id].modified|date:"d M Y, H:i" }}</td>
            <td>
                {{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}
                <span class="pull-right">
                    <a href="{{ m.rsc[id].page_url }}" class="btn btn-mini">{_ view _}</a>
                    <a href="{% url admin_edit_rsc id=id %}" class="btn btn-mini">{_ edit _}</a>
                </span>
            </td>
        </tr>
        {% endif %}
        {% empty %}
        <tr>
            <td colspan="5">
                {_ No pages found. _}
            </td>
        </tr>
        {% endfor %}
    </tbody>
</table>

