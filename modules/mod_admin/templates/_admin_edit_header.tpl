{#
Params:
id
cat_id
languages
#}
<div class="admin-header">
    <div class="admin-header-meta">
        <span class='admin-edit-dates'>
            {_ Created: _} {{ id.created|date:"Y-m-d H:i" }}
            {% if id.creator_id %}
                {_ by _} <a href="{% url admin_edit_rsc id=id.creator_id %}">{{ id.creator_id.title }}</a>
            {% endif %}
            &middot;
            {_ Modified: _} {{ id.modified|date:"Y-m-d H:i" }}
            {% if id.modifier_id %}
                {_ by _} <a href="{% url admin_edit_rsc id=id.modifier_id %}">{{ id.modifier_id.title }}</a>
            {% endif %}
        </span>
    </div>

    {% with id.depiction as depict %}
        {% if depict %}
            {% image depict mediaclass="admin-leader-image" class="admin-leader" title=depict.id.title %}
        {% endif %}

        <div class="{% if depict %}admin-header-has-image{% endif %}">
            <h2 {% include "_language_attrs.tpl" %}>
                {{ id.title|striptags|default:("<em>" ++ _"untitled" ++ "</em>")}}
            </h2>
            <a class='btn btn-default btn-xs admin-btn-category' href="javascript:;" id="changecategory" title="{_ Change category _}">{_ Category: _} {{ id.category_id.title|default:_"untitled" }}</a>
            {% wire id="changecategory"
                action={dialog_open
                    title=[_"Category:", " ", id.category_id.title|default:_"untitled"]
                    template="_action_dialog_change_category.tpl"
                    id=id
                    cat_id=id.category_id
                    is_editable = is_editable
                                and not id.is_a.category
                                and not id.is_a.predicate
                                and (m.acl.insert[id.category.name|as_atom] or not id.category_id.exists)
                }
            %}
        </div>
    {% endwith %}
</div>
