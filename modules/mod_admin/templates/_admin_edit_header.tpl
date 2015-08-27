{#
Params:
r
id
is_editable
languages
#}
<div class="admin-header">
    {% with id.depiction as depict %}
        {% if depict %}
            {% image depict mediaclass="admin-leader-image" class="admin-leader" title=depict.id.title %}
        {% endif %}

        <div class="{% if depict %}admin-header-has-image{% endif %}">
            <div class="admin-header-meta">
                {% if is_editable and m.acl.insert[r.category.name|as_atom] and not r.is_a.category and not r.is_a.predicate %}
                    <a class='btn btn-default btn-xs admin-btn-category' href="javascript:;" id="changecategory" title="{_ Change category _}">{_ Category: _} {{ m.rsc[r.category_id].title }}</a>
                    {% wire id="changecategory" action={dialog_open title=_"Change category" template="_action_dialog_change_category.tpl" id=id cat_id=r.category_id } %}
                {% else %}
                    <span class="admin-btn-category">{_ Category: _} {{ m.rsc[r.category_id].title }}</span>
                {% endif %}
    
                <span class='admin-edit-dates'>
                    {_ Modified: _} {{ r.modified|date:"Y-m-d H:i" }} {_ by _} <a href="{% url admin_edit_rsc id=r.modifier_id %}">{{ m.rsc[r.modifier_id].title }}</a> &middot;
                    {_ Created: _} {{ r.created|date:"Y-m-d H:i" }} {_ by _} <a href="{% url admin_edit_rsc id=r.creator_id %}">{{ m.rsc[r.creator_id].title }}</a>
                </span>
            </div>

            <h2 {% include "_language_attrs.tpl" %}>
                {{ r.title|striptags|default:_"<em>untitled</em>" }}
            </h2>          

        </div>
    {% endwith %}
</div>