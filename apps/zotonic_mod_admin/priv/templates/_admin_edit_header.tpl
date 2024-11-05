{# Showed on top of the resource edit page #}
<div class="admin-header">
    <div class="admin-header-meta">
        <a class="btn btn-default btn-xs"
           data-onclick-topic="model/location/post/redirect/back"
           href="{% url admin %}"
        >
            &lt; {_ Back _}
        </a>
        &nbsp;
        <span class='admin-edit-dates'>
            {_ Created: _} {{ id.created|date:"Y-m-d H:i" }}
            {% if id.creator_id %}
                {_ by _} <a href="{% url admin_edit_rsc id=id.creator_id %}">{% include "_name.tpl" id=id.creator_id %}</a>
            {% endif %}
            &middot;
            {_ Modified: _} {{ id.modified|date:"Y-m-d H:i" }}
            {% if id.modifier_id %}
                {_ by _} <a href="{% url admin_edit_rsc id=id.modifier_id %}">{% include "_name.tpl" id=id.modifier_id %}</a>
            {% endif %}
        </span>
        &middot;
        <span class="text-muted">{_ id: _} {{ id }}</span>
    </div>

    {% with id.depiction as depict %}
        {% if depict %}
            {% image depict mediaclass="admin-leader-image" class="admin-leader" title=depict.id.title %}
        {% endif %}

        {% with id.category_id as cat_id %}
            <div class="{% if depict %}admin-header-has-image{% endif %}">
                <h2 {% include "_language_attrs.tpl" %}>
                    {{ id.title|default:id.short_title|default:("<em>" ++ _"untitled" ++ "</em>")}}

                {% if id.is_protected %}
                    &nbsp; <small class="fa fa-lock text-muted small" title="{_ Protected, not deletable _}"></small>
                {% endif %}

                </h2>
                <a class='btn btn-default btn-xs admin-btn-category' href="javascript:;" id="changecategory" title="{_ Change category _}">
                    <span class="text-muted">{_ Category: _}</span>
                    {% for cid in m.category[cat_id].path|reversed %}
                        {{ cid.title }} &gt;
                    {% endfor %}
                    {{ cat_id.title }}
                </a>
                {% wire id="changecategory"
                    action={dialog_open
                        title=[ _"Category:", " ", cat_id.title|default:_"untitled" ]
                        template="_action_dialog_change_category.tpl"
                        id=id
                        cat_id=cat_id
                    }
                %}

                <a class="btn btn-default btn-xs" href="{% url admin_overview_rsc qcat=cat_id %}">
                    {% trans "Show all {title} pages" title=cat_id.title %}
                </a>
            </div>
        {% endwith %}
    {% endwith %}
</div>
