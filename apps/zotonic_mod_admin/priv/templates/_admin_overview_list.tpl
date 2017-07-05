{#
Params:
qsort
qcat
custompivot
#}
<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th width="30%">
                {% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" type=type|default:"string" %}
            </th>
            <th width="15%">
                {% catinclude "_admin_sort_header.tpl" m.category[qcat].is_a field=field|default:"category_id" type=type|default:"string" caption=_"Category" qsort=qsort %}
            </th>
            <th width="15%">
                {% include "_admin_sort_header.tpl" field="created" caption=_"Created on" type="date" qsort=qsort %}
            </th>
            <th width="15%">
                {% include "_admin_sort_header.tpl" field="modified" caption=_"Modified on" type="date" qsort=qsort %}
            </th>
            <th width="25%">
                {% include "_admin_sort_header.tpl" field="modifier_id" caption=_"Modified by" type=type|default:"string" qsort=qsort %}
            </th>
        </tr>
    </thead>

    <tbody>
    {% for id in result|is_visible %}
        <tr id="{{ #tr.id }}" class="{% if not id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
            <td>
                {% if id.name %}
                    <span class="label label-default pull-right">
                        {{ id.name }}
                    </span>
                {% endif %}
                <span {% include "_language_attrs.tpl" %}>{{ id.title|striptags|default:"<em>untitled</em>" }}</span>
            </td>
            <td>
                {% if qcat %}
                    {% catinclude "_admin_overview_list_data.tpl" id %}
                {% else %}
                    {% include "_admin_overview_list_data.tpl" %}
                {% endif %}
            </td>
            <td>{{ id.created|date:_"d M Y, H:i" }}</td>
            <td>{{ id.modified|date:_"d M Y, H:i" }}</td>
            <td>
                {{ id.modifier_id.title|default:"-" }}
                <span class="pull-right buttons">
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
