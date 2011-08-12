<ul class="short-list">
    <li class="headers clearfix">
        <span class="zp-20">
            {% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" %}
        </span>
        <span class="zp-15">
            {_ Performer _}
        </span>
        <span class="zp-15">
            {% include "_admin_sort_header.tpl" field="pivot_date_start" caption=_"Start date" %}
        </span>
        <span class="zp-10">
            {% include "_admin_sort_header.tpl" field="category_id" caption=_"Category" %}
        </span>
        <span class="zp-15">
            {% include "_admin_sort_header.tpl" field="modified" caption=_"Modified on" %}
        </span>
        <span class="zp-25">
            {% include "_admin_sort_header.tpl" field="modifier_id" caption=_"Modified by" %}
        </span>
    </li>

    {% for id in result %}
    {% if m.rsc[id].is_visible %}
    <li id="{{ #li.id }}" class="clearfix {% if not m.rsc[id].is_published %} unpublished{% endif %}">
        <a href="{% url admin_edit_rsc id=id %}" class="row">
            <span class="zp-20">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
            <span class="zp-15">{{ m.rsc[id].o.performer.title|default:"-" }}</span>
            <span class="zp-15">{{ m.rsc[id].date_start|date:"d M Y, H:i"|default:"-" }}</span>
            <span class="zp-10">{{ m.rsc[m.rsc[id].category_id].title }}</span>
            <span class="zp-15">{{ m.rsc[id].modified|date:"d M Y, H:i" }}</span>
            <span class="zp-15">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
        </a>
        <span class="button-area">
            <a href="{{ m.rsc[id].page_url }}" class="button">{_ view _}</a>
            <a href="{% url admin_edit_rsc id=id %}" class="button">{_ edit _}</a>
        </span>
    </li>
    {% endif %}
    {% empty %}
    <li>
        {_ No pages found. _}
    </li>
    {% endfor %}
</ul>
