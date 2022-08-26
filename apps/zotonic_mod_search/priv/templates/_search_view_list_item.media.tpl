{#
    This is a special variation on the '_search_view_list_item.tpl' template.
    It is used for pages with category 'website'.
    And is included as:

        {% catinclude "_search_view_list_item.tpl" id %}

    Where 'id' contains the id of a page in category 'website' or a sub-category
#}
{% if id.is_visible %}
<div class="list-item list-item-media {% if is_highlight or id.is_featured %} featured{% endif %}">
    {% image id class="float-left" mediaclass="small-crop" crop=crop link=link alt=id.title %}
    <span class="list-item__title">{{ id.title|default:_"Untitled" }}</span>
    <small class="text-muted list-item__meta"> {{ id.category_id.title }} </small>
    <div class="btn-group" role="group">
        <a href="{{ id.page_url }}" class="btn btn-default btn-secondary">
            {_ view _}
        </a>
        {% if m.acl.is_admin %}
            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-secondary">
                {_ edit _}
            </a>
        {% endif %}
    </div>

</div>
{% endif %}
